{*********************************************************}
{* FlashFiler: Import/Export unit                        *}
{*********************************************************}

{$I fsdefine.inc}

Unit fsclimex;

Interface

Uses
  Windows,
  Db,
  dbConsts,
  Forms,
  SysUtils,
  Classes,
  IniFiles,
  TypInfo,
  fssrbde,
  fsdbbase,
  fsdb,
  fsstdate,
  fsconst,
  fsclbase,
  fslldate,
  fsllexcp,
  fsconvff,
  fsclintf,
  fsllbase,
  fssrbase,
  fslldict,
  fsserverclass,
  fsfunInterp;

Const
  DefDateMask = 'MM/DD/YYYY';
  DefDblDelims = False;
  DefDelimitor = '"';
  DefError = 'ERROR';
  DefExt = '.SCH';
  DefMaxLineLength = 8 * 1024; { Max line length assumed by ASCII import routines }
  DefSeparator = ',';
  DefEpoch: Integer = 1969; {!!.05}
  DefYieldInterval = 1;

Type
  TffieFileType = (ftCSV, ftASCII, ftBINARY, ftBTF, ftVARBTF);

  TffieNativeFieldType = (nftUnknown,
    nftChar,
    nftASCIIFloat,
    nftASCIINumber,
    nftASCIIBool,
    nftASCIILongInt,
    nftASCIIAutoInc,
    nftASCIIDate,
    nftASCIITime,
    nftASCIITimestamp,
    nftInt8,
    nftInt16,
    nftInt32,
    nftUInt8,
    nftUInt16,
    nftUInt32,
    nftAutoInc8,
    nftAutoInc16,
    nftAutoInc32,
    nftAutoInc64,
    nftReal,
    nftSingle,
    nftDouble,
    nftExtended,
    nftBCD,
    nftComp,
    nftCurrency,
    nftBoolean,
    nftDateTime1,
    nftDateTime2,
    nftStDate,
    nftStTime,
    nftLString,
    nftZString,
    nftUnicode,
    nftBinary);

  {===== Schema File Classes =====}

  TffieFieldItem = Class
    fiTargetFieldNo: Smallint;
    fiFieldName: TffDictItemName;
    fiNativeTypeDesc: String[20];
    fiNativeType: TffieNativeFieldType;
    fiNativeSize: Smallint;
    fiNativeDecPl: Smallint;
    fiNativeOffset: Smallint;
    fiDateMask: String[25];
  End;

  TffSchemaFieldList = Class(TFSSpecObject)
  Private
    FList: TList;
    Function GetCount: Integer;
  Protected
    Function GetFieldItem(aIndex: Integer): TffieFieldItem;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Add(aFieldItem: TffieFieldItem);
    Property Count: Integer Read GetCount;
    Property Items[aIndex: Integer]: TffieFieldItem Read GetFieldItem;
  End;

  TffSchemaFile = Class(TIniFile)
  Protected {private}
    FFilename: TFileName;
    FFields: TffSchemaFieldList;
    FMainSection: String;
    FRecLength: Longint;
    FBTFDelFlag: Boolean;
    Function GetDateMask: String;
    Function GetDblDelims: Boolean;
    Function GetDelimiter: AnsiChar;
    Function GetFileType: TffieFileType;
    Function GetSeparator: AnsiChar;
    Procedure LoadFields;
    Procedure SetDateMask(aValue: String);
    Procedure SetDblDelims(aValue: Boolean);
    Procedure SetDelimiter(aValue: AnsiChar);
    Procedure SetFileType(aValue: TffieFileType);
    Procedure SetRecLength(aValue: Longint);
    Procedure SetSeparator(aValue: AnsiChar);
  Public
    Constructor Create(aFileName: String);
    Destructor Destroy; Override;
    Procedure BindDictionary(aDictionary: TFSInfoDict);
    Function GetSourceFieldPtr(aBufPtr: Pointer; aFieldNo: Integer): Pointer;
    Procedure MakeIntoDictionary(aDictionary: TFSInfoDict);
    Property BTFDelFlag: Boolean Read FBTFDelFlag;
    Property DateMask: String Read GetDateMask Write SetDateMask;
    Property DblDelims: Boolean Read GetDblDelims Write SetDblDelims;
    Property Delimiter: AnsiChar Read GetDelimiter Write SetDelimiter;
    Property Fields: TffSchemaFieldList Read FFields;
    Property FileType: TffieFileType Read GetFileType Write SetFileType;
    Property RecordLength: Longint Read FRecLength Write SetRecLength;
    Property Section: String Read FMainSection;
    Property Separator: AnsiChar Read GetSeparator Write SetSeparator;
  End;

  {===== Stream Classes for File I/O =====}

  TffFileStream = Class(TFileStream)
  Protected
  Protected
    Function GetNumRecords: Longint; Virtual; Abstract;
    Function GetPercentCompleted: Word; Virtual;
    Function GetRecordLength: Longint; Virtual; Abstract;
  Public
    Function Read(Var Buffer; Count: Longint): Longint; Override;
    Function ReadRec(Var Rec): Boolean; Virtual; Abstract;
    Property NumRecords: Longint Read GetNumRecords;
    Property PercentCompleted: Word Read GetPercentCompleted;
    Property RecordLength: Longint Read GetRecordLength;
  End;

  TffFixedFileStream = Class(TffFileStream)
  Protected {private}
    FRecLength: Longint;
    FNumRecs: Longint;
  Protected
    Function GetNumRecords: Longint; Override;
    Function GetRecordLength: Longint; Override;
  Public
    Constructor Create(Const aFileName: String; aMode: Word; aRecLength: Longint);
    Function ReadRec(Var Rec): Boolean; Override;
  End;

  TffFixedASCIIStream = Class(TffFixedFileStream)
  Protected {private}
  Protected
    CRLF: Boolean;
  Public
    Function ReadRec(Var Rec): Boolean; Override;
  End;

  TffFixedBTFStream = Class(TffFixedFileStream)
  Protected {private}
    FNumSkipped: Longint;
    DelFieldAvail: Boolean;
  Protected
  Public
    Constructor Create(Const aFileName: String; aMode: Word; aDelFlag: Boolean);
    Function ReadRec(Var Rec): Boolean; Override;
    Property NumSkipped: Longint Read FNumSkipped;
  End;

  TffVaryingFileStream = Class(TffFileStream)
  Protected
  Public
    Function ReadRec(Var Rec): Boolean; Override;
  End;

  {===== Field Conversion Classes to Parse Records =====}

  TffFieldConverter = Class
  Protected { private }
    FBuffer: Pointer;
    FBufLen: Longint;
    FSchema: TffSchemaFile;
    FDict: TFSInfoDict;
  Public
    Procedure Init(aFieldBuf: Pointer;
      aBufLen: Longint;
      aSchema: TffSchemaFile;
      aDictionary: TFSInfoDict);
    Procedure AdjustMaskAndValue(aMask, aValue: TffShStr;
      Var aDateMask, aDateValue,
      aTimeMask, aTimeValue: TffShStr);
    { Translates a FF date/time mask into one suitable for SysTools conversion
      routines (expands token characters out to the correct number of digitis
      for each element) }
    Function ConvertField(aSourcePtr: Pointer;
      aSourceType: TffieNativeFieldType;
      aSourceSize: Integer;
      aTargetFFType: TfsFieldType;
      aTargetSize: Integer;
      aDateMask: TffShStr;
      aRangeError: boolean): TffResult;
  End;

  {===== Engine Classes =====}

  TffieProgressPacket = Record
    ppNumRecs: DWORD;
    ppTotalRecs: DWORD;
  End;

  TffieYieldEvent = Procedure(aProgressPacket: TffieProgressPacket) Of Object;

  TffInOutEngine = Class
  Protected {private}
    FDataFile: TffFullFileName;
    FLogFile: TextFile;
    FLogFilename: TFileName;
    FLogCount: Longint;
    FSchema: TffSchemaFile;
    FStream: TffFileStream;
    FTerminated: Boolean;
    FYieldInterval: Word;
    FImportFilename: TFileName;
    FOnYield: TffieYieldEvent;
  Protected
  Public
    Constructor Create(Const aFileName: TffFullFileName;
      aMode: Word);
    Destructor Destroy; Override;
    Procedure PostLog(S: String);
    Procedure Terminate;

    Property LogFilename: TFilename Read FLogFilename;
    Property LogCount: Longint Read FLogCount;
    Property Schema: TffSchemaFile Read FSchema;
    Property Stream: TffFileStream Read FStream;
    Property Terminated: Boolean Read FTerminated;
    Property YieldInterval: Word Read FYieldInterval Write FYieldInterval;
    Property OnYield: TffieYieldEvent
      Read FOnYield Write FOnYield;
  End;

  TffExportEngine = Class(TffInOutEngine)
  Protected
  Public
  End;

  TffImportEngine = Class(TffInOutEngine)
  Protected
    FieldConverter: TffFieldConverter;
  Public
    Constructor Create(Const aFileName: TffFullFileName);
    { Creates the import engine.  aFilename is the full path and
      filename for the file to import. }
    Destructor Destroy; Override;

    Procedure Import(aTable: TFSTable; aBlockInserts: Word; aRangeError: boolean);
    { Loads the import file into the given table. Importing only works with
      an existing table. If the import is aborted, the partially loaded
      table remains. }
  End;

Implementation

Function StripQuotes(S: TffShStr): TffShStr;
Begin
  S := FFShStrTrim(S);
  If Copy(S, 1, 1) = '"' Then
    Delete(S, 1, 1);
  If COpy(S, Length(S), 1) = '"' Then
    Delete(S, Length(S), 1);
  Result := S;
End;

{ TffSchemaFieldList }

Procedure TffSchemaFieldList.Add(aFieldItem: TffieFieldItem);
Begin
  FList.Add(aFieldItem);
End;

Constructor TffSchemaFieldList.Create;
Begin
  FList := TList.Create;
End;

Destructor TffSchemaFieldList.Destroy;
Begin
  FList.Free;
End;

Function TffSchemaFieldList.GetCount: Integer;
Begin
  Result := FList.Count;
End;

Function TffSchemaFieldList.GetFieldItem(aIndex: Integer): TffieFieldItem;
Begin
  Result := TffieFieldItem(FList.Items[aIndex]);
End;

{ TffSchemaFile }

Constructor TffSchemaFile.Create(aFileName: String);
Var
  Dir: String;
  FCB: TextFile;
  Rec: TffShStr;
Begin
  If Not FileExists(aFileName) Then
    FSRaiseException(EfsClientException, fsStrResClient, fsccImport_NoSchemaFile, [aFilename]);

  { TIniFile will look in the WINDOWS directory if no path is given }
  If ExtractFilePath(aFileName) = '' Then
    Begin
      GetDir(0, Dir);
      aFileName := Dir + '\' + aFileName;
    End;
  FFileName := aFileName;

  Inherited Create(FFileName);

  {FMainSection := ChangeFileExt(ExtractFileName(aFileName), '');}
  { Get section header }
  FMainSection := '';
  AssignFile(FCB, FFileName);
  Reset(FCB);
  Try
    Repeat
      ReadLn(FCB, Rec);
      Rec := FFShStrTrim(Rec);
    Until Rec <> '';
    If (Length(Rec) > 2) And (Rec[1] = '[') And (Rec[Length(Rec)] = ']') Then
      FMainSection := Copy(Rec, 2, Length(Rec) - 2)
    Else
      FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadSchemaHeader, [Rec]);
  Finally
    CloseFile(FCB);
  End;
  //FileType:=ftCSV;

  FFields := TffSchemaFieldList.Create;
  LoadFields;

  { Check to see if the first field of a BTF file is the delete flag }
  With Fields.Items[0] Do
    FBTFDelFlag := (FileType In [ftBTF, ftVARBTF]) And
      (Uppercase(fiFieldName) = 'DELFLAG') And
      (fiNativeType = nftInt32);

  { Get the record length of a fixed ASCII file }
  FRecLength := 0;
  If FileType In [ftASCII, ftBINARY] Then
    Begin
      FRecLength := ReadInteger(FMainSection, 'RECLENGTH', 0);
      If FRecLength = 0 Then
        Begin

          { reclength required for typed binary files }
          If FileType = ftBinary Then
            FSRaiseExceptionNoData(EfsClientException, fsStrResClient, fsccImport_RECLENGTHRequired);

          { For fixed ASCII, reclength defined by size and position of
            last field with an assumed CRLF }
          With FFields.Items[FFields.Count - 1] Do
            FRecLength := fiNativeOffset + fiNativeSize + 2;
        End;
    End;
End;

Destructor TffSchemaFile.Destroy;
Var
  I: Integer;
Begin
  If Assigned(FFields) Then
    For I := 0 To FFields.Count - 1 Do
      FFields.Items[I].Free;

  FFields.Free;
  Inherited Destroy;
End;

Procedure TffSchemaFile.BindDictionary(aDictionary: TFSInfoDict);
Var
  I: Integer;
  NoMatches: Boolean;
Begin
  NoMatches := True;
  For I := 0 To FFields.Count - 1 Do
    If Not ((I = 0) And BTFDelFlag) Then
      With FFields.Items[I] Do
        Begin
          fiTargetFieldNo := aDictionary.GetFieldFromName(fiFieldName);
          If fiTargetFieldNo <> -1 Then
            NoMatches := False;
        End;
  If NoMatches Then
    FSRaiseExceptionNoData(EfsClientException, fsStrResClient, fsccImport_NoMatchingFields);
End;

Function TffSchemaFile.GetDateMask: String;
Begin
  Result := ReadString(FMainSection, 'DATEMASK', DefDateMask);
End;

Function TffSchemaFile.GetDblDelims: Boolean;
Begin
  Result := ReadBool(FMainSection, 'DBLDELIMS', DefDblDelims);
End;

Function TffSchemaFile.GetDelimiter: AnsiChar;
Begin
  Result := ReadString(FMainSection, 'DELIMITER', DefDelimitor)[1];
End;

Function TffSchemaFile.GetFileType: TffieFileType;
Var
  S: String;
Begin
  S := ReadString(FMainSection, 'FILETYPE', '');
  If S = '' Then
    FSRaiseExceptionNoData(EfsClientException, fsStrResClient, fsccImport_FILETYPEMissing);
  Result := TffieFileType(GetEnumValue(TypeInfo(TffieFileType), 'ft' + S));
  If Ord(Result) = -1 Then
    FSRaiseExceptionNoData(EfsClientException, fsStrResClient, fsccImport_FILETYPEInvalid);
End;

Function TffSchemaFile.GetSeparator: AnsiChar;
Begin
  Result := ReadString(FMainSection, 'SEPARATOR', DefSeparator)[1];
End;

Procedure TffSchemaFile.LoadFields;

  Function BuildField(FieldEntry: TffShStr): TffieFieldItem;
  Var
    FieldID: TffShStr;
    Temp: TffShStr;
  Begin

    { Parse the FIELD string from the schema file }
    Result := TffieFieldItem.Create;
    With Result Do
      Begin
        fiTargetFieldNo := -1;

        { Field ID }
        FFShStrSplit(FieldEntry, '=', Temp, FieldEntry);
        FieldID := FFShStrTrim(Temp);

        { Field name }
        FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
        fiFieldName := FFShStrTrim(Temp);
        If fiFieldName = '' Then
          FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadFieldName, [FieldID, fiFieldName]);

        { Import datatype }
        FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
        fiNativeTypeDesc := Uppercase(FFShStrTrim(Temp));

        { Import field size }
        FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
        Try
          fiNativeSize := StrToInt(FFShStrTrim(Temp));
        Except
          FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadSize, [FieldID, Temp]);
        End;

        { Import decimal places }
        FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
        Try
          fiNativeDecPl := StrToInt(FFShStrTrim(Temp));
        Except
          FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadDecPl, [FieldID, Temp]);
        End;

        { Import offset }
        FFShStrSplit(FieldEntry, ',', Temp, FieldEntry);
        Try
          fiNativeOffset := StrToInt(FFShStrTrim(Temp));
        Except
          FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadOffset, [FieldID, Temp]);
        End;

        fiDateMask := '';

        { The following tokens are valid for any import filetype }
        If fiNativeTypeDesc = 'CHAR' Then
          fiNativeType := nftChar
        Else If fiNativeTypeDesc = 'DATE' Then
          Begin
            fiNativeType := nftASCIIDate;
            fiDateMask := StripQuotes(FieldEntry);
          End
        Else If fiNativeTypeDesc = 'TIME' Then
          Begin
            fiNativeType := nftASCIITime;
            fiDateMask := StripQuotes(FieldEntry);
          End
        Else If fiNativeTypeDesc = 'TIMESTAMP' Then
          Begin
            fiNativeType := nftASCIITimeStamp;
            fiDateMask := StripQuotes(FieldEntry);
          End

            { The following tokens are valid only for ASCII import files }
        Else If FileType In [ftASCII, ftCSV] Then
          Begin
            If fiNativeTypeDesc = 'BOOL' Then
              fiNativeType := nftASCIIBool
            Else If fiNativeTypeDesc = 'FLOAT' Then
              fiNativeType := nftASCIIFloat
            Else If fiNativeTypeDesc = 'NUMBER' Then
              fiNativeType := nftASCIINumber
            Else If fiNativeTypeDesc = 'LONGINT' Then
              fiNativeType := nftASCIILongInt
            Else If fiNativeTypeDesc = 'AUTOINC' Then
              fiNativeType := nftASCIIAutoInc
            Else
              FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadFieldtype, [FieldID, fiNativeTypeDesc]);
          End

            { The following datatype tokens only apply to Binary and BTF files }
        Else If FileType In [ftBINARY, ftBTF, ftVARBTF] Then
          Begin
            If fiNativeTypeDesc = 'BOOL' Then
              fiNativeType := nftBoolean
            Else If fiNativeTypeDesc = 'FLOAT' Then
              Begin
                Case fiNativeSize Of
                  4: fiNativeType := nftSingle;
                  6: fiNativeType := nftReal;
                  8: fiNativeType := nftDouble;
                  10: fiNativeType := nftExtended;
                  Else
                    FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadFloatSize, [FieldID]);
                End;
              End
            Else If fiNativeTypeDesc = 'INTEGER' Then
              Begin
                Case fiNativeSize Of
                  1: fiNativeType := nftInt8;
                  2: fiNativeType := nftInt16;
                  4: fiNativeType := nftInt32;
                  8: fiNativeType := nftComp;
                  Else
                    FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadIntegerSize, [FieldID]);
                End;
              End
            Else If fiNativeTypeDesc = 'UINTEGER' Then
              Begin
                Case fiNativeSize Of
                  1: fiNativeType := nftUInt8;
                  2: fiNativeType := nftUInt16;
                  4: fiNativeType := nftUInt32;
                  Else
                    FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadUIntegerSize, [FieldID]);
                End;
              End
            Else If fiNativeTypeDesc = 'AUTOINC' Then
              Begin
                Case fiNativeSize Of
                  1: fiNativeType := nftAutoInc8;
                  2: fiNativeType := nftAutoInc16;
                  4: fiNativeType := nftAutoInc32;
                  Else
                    FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadAutoIncSize, [FieldID]);
                End;
              End
            Else If fiNativeTypeDesc = 'STRING' Then
              fiNativeType := nftLString
            Else If fiNativeTypeDesc = 'ASCIIZ' Then
              fiNativeType := nftZString
            Else If fiNativeTypeDesc = 'UNICODE' Then
              fiNativeType := nftUnicode
            Else If fiNativeTypeDesc = 'CURRENCY' Then
              fiNativeType := nftCurrency
            Else If fiNativeTypeDesc = 'DATETIME1' Then
              fiNativeType := nftDateTime1
            Else If fiNativeTypeDesc = 'DATETIME2' Then
              fiNativeType := nftDateTime2
            Else If fiNativeTypeDesc = 'STDATE' Then
              fiNativeType := nftStDate
            Else If fiNativeTypeDesc = 'STTIME' Then
              fiNativeType := nftStTime
            Else If fiNativeTypeDesc = 'BINARY' Then
              fiNativeType := nftBinary
            Else
              FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadFieldtype, [FieldID, fiNativeTypeDesc]);
          End
        Else
          FSRaiseException(EfsClientException, fsStrResClient, fsccImport_BadFieldtype, [FieldID, fiNativeTypeDesc]);
      End;
  End;
Var
  SchemaFields: TStringList;
  I: Integer;
Begin
  SchemaFields := TStringList.Create;
  Try

    { Get all the field descriptors into a stringlist }
    SchemaFields.LoadFromFile(FFileName);

    { Traverse the stringlist and grab all the field descriptors in order }
    For I := 0 To SchemaFields.Count - 1 Do
      If FFCmpShStrUC(FFShStrTrim(SchemaFields[I]), 'FIELD', 5) = 0 Then
        Fields.Add(BuildField(SchemaFields[I]));
  Finally
    SchemaFields.Free;
  End;

  If Fields.Count = 0 Then
    FSRaiseExceptionNoData(EfsClientException, fsStrResClient, fsccImport_NoFields);
End;

Function TffSchemaFile.GetSourceFieldPtr(aBufPtr: Pointer; aFieldNo: Integer): Pointer;
Begin
  Result := Nil;
  Case FileType Of
    ftASCII, ftBINARY, ftBTF:
      Result := PChar(aBufPtr) + Fields.Items[aFieldNo].fiNativeOffset;
    ftCSV: ;
    ftVARBTF: ;
  End;
End;

Procedure TffSchemaFile.MakeIntoDictionary(aDictionary: TFSInfoDict);
Var
  I: Integer;
  FieldType: TfsFieldType;
  Units, DecPl: Integer;
Begin
  For I := 0 To Fields.Count - 1 Do
    If Not ((I = 0) And BTFDelFlag) Then
      Begin
        With Fields.Items[I] Do
          Begin
            Units := 0;
            DecPl := 0;
            Case fiNativeType Of
              nftChar:
                Begin
                  If fiNativeSize = 1 Then
                    Begin
                      FieldType := fstSingleChar;
                      Units := 1;
                    End
                  Else
                    Begin
                      FieldType := fstShortString;
                      Units := fiNativeSize;
                    End;
                End;
              nftASCIIFloat:
                Begin
                  FieldType := fstDouble;
                  DecPl := fiNativeDecPl;
                End;
              nftASCIINumber:
                FieldType := fstInt16;
              nftASCIIBool:
                FieldType := fstBoolean;
              nftASCIILongInt:
                FieldType := fstInt32;
              nftASCIIAutoInc:
                FieldType := fstAutoInc32;
              nftASCIIDate:
                FieldType := fstDateTime;
              nftASCIITime:
                FieldType := fstDateTime;
              nftASCIITimestamp:
                FieldType := fstDateTime;
              nftInt8:
                FieldType := fstInt8;
              nftInt16:
                FieldType := fstInt16;
              nftInt32:
                FieldType := fstInt32;
              nftAutoInc8,
                nftAutoInc16,
                nftAutoInc32:
                FieldType := fstAutoInc32;
              nftUInt8:
                FieldType := fstUInt8;
              nftUInt16:
                FieldType := fstUInt16;
              nftUInt32:
                FieldType := fstUInt32;
              nftReal:
                Begin
                  FieldType := fstDouble;
                  DecPl := fiNativeDecPl;
                End;
              nftSingle:
                Begin
                  FieldType := fstSingle;
                  DecPl := fiNativeDecPl;
                End;
              nftDouble:
                Begin
                  FieldType := fstDouble;
                  DecPl := fiNativeDecPl;
                End;
              nftExtended:
                Begin
                  FieldType := fstExtended;
                  DecPl := fiNativeDecPl;
                End;
              nftAutoInc64:
                FieldType := fstAutoInc64;
              nftComp:
                FieldType := fstInt64;
              nftCurrency:
                Begin
                  FieldType := fstCurrency;
                  DecPl := fiNativeDecPl;
                End;
              {nftBcd:
                begin
                  FieldType := fstBinaryDecimals;
                  DecPl := fiNativeDecPl;
                end; }
              nftBoolean:
                FieldType := fstBoolean;
              nftDateTime1,
                nftDateTime2:
                FieldType := fstDateTime;
              nftLString:
                Begin
                  If fiNativeSize = 2 Then
                    FieldType := fstSingleChar
                  Else If fiNativeSize <= 256 Then
                    FieldType := fstShortString
                  Else
                    FieldType := fstNullString;
                  Units := fiNativeSize - 1;
                End;
              nftZString:
                Begin
                  FieldType := fstNullString;
                  Units := fiNativeSize - 1;
                End;
              nftUnicode:
                If fiNativeSize = 2 Then
                  FieldType := fstSingleWideChar
                Else
                  Begin
                    FieldType := fstWideString;
                    Units := (fiNativeSize - 2) Div 2;
                  End;
              nftStDate:
                FieldType := fstDate;
              nftStTime:
                FieldType := fstTime;
              Else
                FieldType := fstArrayUInt8;
                Units := fiNativeSize;
            End;

            aDictionary.AddField(fiFieldName, '', FieldType, Units, DecPl, False, Nil, blNone, '', rNone, False, duNormal);
          End;
      End;
End;

Procedure TffSchemaFile.SetDateMask(aValue: String);
Begin
  WriteString(FMainSection, 'DATEMASK', aValue);
End;

Procedure TffSchemaFile.SetDblDelims(aValue: Boolean);
Begin
  WriteBool(FMainSection, 'DBLDELIMS', aValue);
End;

Procedure TffSchemaFile.SetDelimiter(aValue: AnsiChar);
Begin
  WriteString(FMainSection, 'DELIMITER', aValue);
End;

Procedure TffSchemaFile.SetFileType(aValue: TffieFileType);
Var
  S: String;
Begin
  S := GetEnumName(TypeInfo(TffieFileType), Integer(aValue));
  Delete(S, 1, 2);
  WriteString(FMainSection, 'FILETYPE', S);
End;

Procedure TffSchemaFile.SetRecLength(aValue: Longint);
Begin
  FRecLength := aValue;
End;

Procedure TffSchemaFile.SetSeparator(aValue: AnsiChar);
Begin
  WriteString(FMainSection, 'SEPARATOR', aValue);
End;

{ TffFileStream }

Function TffFileStream.GetPercentCompleted: Word;
Begin
  Result := Round(Position * 100.0 / Size);
End;

Function TffFileStream.Read(Var Buffer; Count: Longint): Longint;
Begin
  If (Position = Size - 1) Then
    Begin
      Result := Inherited Read(Buffer, 1);
      If Byte(Buffer) = $1A {EOF} Then
        Result := 0;
    End
  Else
    Result := Inherited Read(Buffer, Count);
End;

{ TffFixedFileStream }

Constructor TffFixedFileStream.Create(Const aFileName: String;
  aMode: Word;
  aRecLength: Longint);
Begin
  Inherited Create(aFileName, aMode);

  If aRecLength > 0 Then
    Begin
      FRecLength := aRecLength;
      FNumRecs := Size Div RecordLength;
    End;
End;

Function TffFixedFileStream.GetNumRecords: Longint;
Begin
  Result := FNumRecs;
End;

Function TffFixedFileStream.GetRecordLength: Longint;
Begin
  Result := FRecLength;
End;

Function TffFixedFileStream.ReadRec(Var Rec): Boolean;
Begin
  Result := Read(Rec, RecordLength) <> 0;
End;

{ TffFixedASCIIStream }

Function TffFixedASCIIStream.ReadRec(Var Rec): Boolean;
Var
  Buffer: Word;
Begin
  { Determine if we need to account for a CR+LF at the end of each record }
  If Position = 0 Then
    Begin
      Result := Read(Rec, RecordLength - 2) <> 0;
      Read(Buffer, 2);
      CRLF := Buffer = $0A0D;
    End
  Else
    Begin
      If CRLF Then
        Begin
          Result := Read(Rec, RecordLength - 2) <> 0;
          Position := Position + 2;
        End
      Else
        Result := Read(Rec, RecordLength) <> 0;
    End;
End;

{ TffFixedBTFStream }

Constructor TffFixedBTFStream.Create(Const aFileName: String;
  aMode: Word;
  aDelFlag: Boolean);
Begin
  Inherited Create(aFileName, aMode, 0);

  DelFieldAvail := aDelFlag;

  { Absorb the BTF header record }
  Position := 8;
  Read(FNumRecs, SizeOf(FNumRecs));
  Read(FRecLength, SizeOf(FRecLength));
  Position := FRecLength;
End;

Function TffFixedBTFStream.ReadRec(Var Rec): Boolean;
Begin
  Repeat
    Inc(FNumSkipped);
    Result := Inherited ReadRec(Rec);
    { Skip deleted records}
  Until Not Result Or (Not DelFieldAvail Or (Longint(Rec) = 0));
  Dec(FNumSkipped);
End;

{ TffVaryingFileStream }

Function TffVaryingFileStream.ReadRec(Var Rec): Boolean;
Begin
  Result := False;
End;

{ TffFieldConverter }

Procedure TffFieldConverter.Init(aFieldBuf: Pointer;
  aBufLen: Longint;
  aSchema: TffSchemaFile;
  aDictionary: TFSInfoDict);
Begin
  FBuffer := aFieldBuf;
  FBufLen := aBufLen;
  FSchema := aSchema;
  FDict := aDictionary;
End;

Procedure TffFieldConverter.AdjustMaskAndValue(aMask, aValue: TffShStr;
  Var aDateMask, aDateValue,
  aTimeMask, aTimeValue: TffShStr);
{ Translates a FF date/time mask into one suitable for SysTools conversion
routines (expands token characters out to the correct number of digitis
for each element) }
Var
  I, J, K, N: Integer;
  ValueIdx: Integer;
  LastDateCharAt,
    LastTimeCharAt,
    FirstDateCharAt,
    FirstTimeCharAt: Smallint;
  MaskStart,
    ValueStart: Integer;
  NewMask: String;
  Found: Boolean;
  NoDelimitersFound: Boolean;
Begin
  aDateMask := '';
  aDateValue := '';
  aTimeMask := '';
  aTimevalue := '';
  NewMask := '';

  { Match number of digits in the mask with number of
    digits in the data }
  MaskStart := 1;
  ValueStart := 1;
  I := 1;
  NoDelimitersFound := True;
  While I <= Length(aMask) Do
    Begin
      { look for the next delimiter in the mask }
      If Pos(aMask[I], 'DMYhmst') = 0 Then
        Begin
          NoDelimitersFound := False;
          If I - MaskStart = 0 Then
            Begin
              {Error}
              Exit;
            End;

          { aMask[I] is our delimiter; find the position of this delimiter
            in the value  }
          ValueIdx := ValueStart;
          Found := (aValue[ValueIdx] = aMask[I]);
          While Not Found And (ValueIdx < Length(aValue)) Do
            Begin
              Inc(ValueIdx);
              Found := aValue[ValueIdx] = aMask[I];
            End;

          { Count the digits in this element of the value }
          N := ValueIdx - ValueStart;
          If Not Found Or (N = 0) Then
            Begin
              {error}
              Exit;
            End;

          NewMask := NewMask + FFShStrRepChar(aMask[I - 1], N) + aMask[I];
          MaskStart := I + 1;
          ValueStart := ValueIdx + 1;
        End;
      Inc(I);
    End;

  If NoDelimitersFound Then
    NewMask := aMask
  Else
    Begin
      { Handle end-of-mask case }
      N := Length(aValue) - ValueStart + 1;
      NewMask := NewMask + FFShStrRepChar(aMask[Length(aMask)], N);
    End;

  {-- Special handling for "seconds" token; truncate fractional seconds --}
  For I := 1 To Length(NewMask) Do
    { find start of "seconds" mask }
    If NewMask[I] = 's' Then
      Begin
        { Find the end of the "seconds" mask }
        J := I + 1;
        While (NewMask[J] = 's') And (J <= Length(NewMask)) Do
          Inc(J);

        { Find first nondigit character in the "seconds" data }
        K := I;
        While (K < J) And (Pos(aValue[K], '0123456789') <> 0) Do
          Inc(K);

        If K <> J Then
          Begin
            { Truncate mask and data }
            Delete(NewMask, K, J - K);
            Delete(aValue, K, J - K);
          End;
        Break;
      End;

  {-- Break up the date and time components --}
  LastDateCharAt := 0;
  LastTimeCharAt := 0;
  FirstDateCharAt := 0;
  FirstTimeCharAt := 0;

  { Find the bounds of each component in the mask }
  For I := 1 To Length(NewMask) Do
    Begin
      If Pos(NewMask[I], 'DMY') <> 0 Then
        LastDateCharAt := I;
      If Pos(NewMask[I], 'hmst') <> 0 Then
        LastTimeCharAt := I;

      J := Length(NewMask) - I + 1;
      If Pos(NewMask[J], 'DMY') <> 0 Then
        FirstDateCharAt := J;
      If Pos(NewMask[J], 'hmst') <> 0 Then
        FirstTimeCharAt := J;
    End;

  { Return date components }
  If FirstDateCharAt <> 0 Then
    Begin
      aDateMask := Copy(NewMask, FirstDateCharAt, LastDateCharAt - FirstDateCharAt + 1);
      aDateValue := Copy(aValue, FirstDateCharAt, LastDateCharAt - FirstDateCharAt + 1);
    End;

  { Return time components }
  If FirstTimeCharAt <> 0 Then
    Begin
      aTimeMask := Copy(NewMask, FirstTimeCharAt, LastTimeCharAt - FirstTimeCharAt + 1);
      aTimeValue := Copy(aValue, FirstTimeCharAt, LastTimeCharAt - FirstTimeCharAt + 1);
    End;
End;

Function TffFieldConverter.ConvertField(aSourcePtr: Pointer;
  aSourceType: TffieNativeFieldType;
  aSourceSize: Integer;
  aTargetFFType: TfsFieldType;
  aTargetSize: Integer;
  aDateMask: TffShStr;
  aRangeError: boolean): TffResult;
Var
  I: Integer;
  MinUnits: Integer;
  SourceFSType: TfsFieldType;
  vFloat: Extended;
  vDouble: Double;
  vSmallInt: Smallint;
  vLongInt: Longint;
  vDateValue,
    vTimeValue: TffShStr;
  vDateMask,
    vTimeMask: TffShStr;
  Da, Mo, Yr: Integer;
  Hr, Mn, Sc: Integer;
  IsBlank: Boolean;

  Function ExtractAsciiField(aPtr: PChar; aSize: Smallint): TffShStr;
  Var
    HoldChar: Char;
  Begin
    HoldChar := aPtr[aSize];
    aPtr[aSize] := #0;
    Result := FFStrPasLimit(aPtr, aSize);
    aPtr[aSize] := HoldChar;
  End;

Begin
  FillChar(FBuffer^, FBufLen, #0);
  Result := 0;

  { ASCII import fields that are totally blank are treated as nulls }
  If FSchema.FileType = ftASCII Then
    Begin
      IsBlank := True;
      For I := 0 To aSourceSize - 1 Do
        Begin
          IsBlank := FFCmpB(PByte(Longint(aSourcePtr) + I)^, $20) = 0;
          If Not IsBlank Then
            Break;
        End;

      If IsBlank Then
        Begin
          Result := DBIERR_FIELDISBLANK;
          Exit;
        End;
    End;

  Case aSourceType Of
    nftChar:
      Begin
        MinUnits := FFMinI(aSourceSize, aTargetSize);
        Case aTargetFFType Of
          fstSingleChar:
            Char(FBuffer^) := Char(aSourcePtr^);
          fstShortString:
            TffShStr(FBuffer^) := FFShStrTrimR(ExtractAsciiField(aSourcePtr, MinUnits));
          fstNullString, fstVarNullString:
            Move(aSourcePtr^, FBuffer^, MinUnits);
          fstSingleWideChar:
            WideChar(FBuffer^) := FFCharToWideChar(Char(aSourcePtr^));
          fstWideString,fstVarWideString:
            Begin
              { Note: the length of a "wide" field is the number of bytes
                it occupies, not the number of wide chars it will hold. }
              MinUnits := FFMinI(aSourceSize - 1, (aTargetSize Div SizeOf(WideChar)) - 1);
              FFShStrLToWideStr(FFShStrTrimR(TffShStr(aSourcePtr^)), FBuffer, MinUnits);
            End;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    nftASCIIFloat:
      Begin
        vFloat := {!!.02}
        StrToFloat(Trim(ExtractAsciiField(aSourcePtr, aSourceSize))); {!!.02}
        Case aTargetFFType Of
          fstSingle:
            Single(FBuffer^) := vFloat;
          fstDouble:
            Double(FBuffer^) := vFloat;
          fstExtended:
            Extended(FBuffer^) := vFloat;
          fstCurrency: Int64(FBuffer^) := Round(vFloat * 10000.0); {!!.03}
          // fftBinaryDecimals: int64(FBuffer^) := vFloat * 10000.0;               {!!.03}
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    nftASCIINumber:
      Begin
        vSmallInt :=
          StrToInt(Trim(ExtractAsciiField(aSourcePtr, aSourceSize))); {!!.02}
        Case aTargetFFType Of
          fstUInt8, fstInt8:
            Byte(FBuffer^) := vSmallInt;
          fstUInt16, fstInt16:
            TffWord16(FBuffer^) := vSmallInt;
          fstUInt32, fstInt32:
            TffWord32(FBuffer^) :=
              StrToInt(Trim(ExtractAsciiField(aSourcePtr, aSourceSize))); {!!.02}
          fstInt64, fstAutoinc64,fstRecVersion:
            Int64(FBuffer^) :=
              StrToInt64(Trim(ExtractAsciiField(aSourcePtr, aSourceSize))); {!!.02}
          fstCurrency:
            Begin
              Int64(FBuffer^) :=
                StrToInt(Trim(ExtractAsciiField(aSourcePtr, aSourceSize))); {!!.02}
              Int64(FBuffer^) := Round(Int64(FBuffer^) * 10000.0);
            End;
          fstAutoInc32:
            Longint(FBuffer^) := vSmallInt;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    nftASCIIBool:
      If aTargetFFType = fstBoolean Then
        Boolean(FBuffer^) := (Char(aSourcePtr^) In ['T', 't', 'Y', 'y', '1'])
      Else
        Result := DBIERR_INVALIDFLDXFORM;

    nftASCIILongInt,
      nftASCIIAutoInc:
      Begin
        vLongInt :=
          StrToInt(Trim(ExtractAsciiField(aSourcePtr, aSourceSize))); {!!.02}
        Case aTargetFFType Of
          fstUInt32, fstInt32:
            TffWord32(FBuffer^) := vLongInt;
          fstInt64, fstAutoinc64,fstRecVersion:
            Int64(FBuffer^) := vLongInt;
          fstCurrency:
            Begin
              Int64(FBuffer^) := vLongInt;
              Int64(FBuffer^) := Round(Int64(FBuffer^) * 10000.0);
            End;
          fstAutoInc32:
            Longint(FBuffer^) := vLongInt;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    nftASCIIDate:
      Begin
        AdjustMaskAndValue(aDateMask, ExtractAsciiField(aSourcePtr, aSourceSize),
          vDateMask, vDateValue,
          vTimeMask, vTimeValue);
        DateStringToDMY(vDateMask, vDateValue, Da, Mo, Yr, DefEpoch);
        If (Yr = 0) And (Mo = 0) And (Da = 0) Then
          Begin
            Result := DBIERR_FIELDISBLANK;
            Exit;
          End;
        {if Yr < 100 then Yr := Yr + DefEpoch;}{!!.05 - Deleted}
        Yr := ResolveEpoch(Yr, DefEpoch); {!!.05 - Added}
        Case aTargetFFType Of
          fstDateTime:
            { TDateTime values are stored in the buffer as Delphi 1 dates }
            TDateTime(FBuffer^) := EncodeDate(Yr, Mo, Da) + 693594.0;
          fstDate:
            TStDate(FBuffer^) := DMYToStDate(Da, Mo, Yr, DefEpoch);
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    nftASCIITime:
      Begin
        AdjustMaskAndValue(aDateMask, ExtractAsciiField(aSourcePtr, aSourceSize),
          vDateMask, vDateValue,
          vTimeMask, vTimeValue);
        TimeStringToHMS(vTimeMask, vTimeValue, Hr, Mn, Sc);
        Case aTargetFFType Of
          fstDateTime:
            TDateTime(FBuffer^) := EncodeTime(Hr, Mn, Sc, 0);
          fstTime:
            TStTime(FBuffer^) := HMSToStTime(Hr, Mn, Sc);
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    nftASCIITimestamp:
      Begin
        AdjustMaskAndValue(aDateMask, ExtractAsciiField(aSourcePtr, aSourceSize),
          vDateMask, vDateValue,
          vTimeMask, vTimeValue);
        DateStringToDMY(vDateMask, vDateValue, Da, Mo, Yr, DefEpoch);
        If (Yr = 0) And (Mo = 0) And (Da = 0) Then
          Begin
            Result := DBIERR_FIELDISBLANK;
            Exit;
          End;
        {if Yr < 100 then Yr := Yr + DefEpoch;}{!!.05 - Deleted}
        Yr := ResolveEpoch(Yr, DefEpoch); {!!.05 - Added}
        TimeStringToHMS(vTimeMask, vTimeValue, Hr, Mn, Sc);
        If Hr < 0 Then
          Hr := 0;
        If Mn < 0 Then
          Mn := 0;
        If Sc < 0 Then
          Sc := 0;
        Case aTargetFFType Of
          fstDateTime:
            { TDateTime values are stored in the buffer as Delphi 1 dates }
            TDateTime(FBuffer^) := EncodeDate(Yr, Mo, Da) + 693594.0 +
              EncodeTime(Hr, Mn, Sc, 0);
          fstDate:
            TStDate(FBuffer^) := DMYToStDate(Da, Mo, Yr, DefEpoch);
          fstTime:
            TStTime(FBuffer^) := HMSToStTime(Hr, Mn, Sc);
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    nftReal:
      Begin
        vDouble := Real(aSourcePtr^);
        Case aTargetFFType Of
          fstSingle:
            Single(FBuffer^) := vDouble;
          fstDouble:
            Double(FBuffer^) := vDouble;
          fstExtended:
            Extended(FBuffer^) := vDouble;
          fstCurrency {,fstBinaryDecimals}:
            Begin
              Int64(FBuffer^) := Round(vDouble * 10000.0);
            End;
          Else
            Result := DBIERR_INVALIDFLDXFORM;
        End;
      End;

    Else
      Begin

        { All remaining datatypes are native to FlashFiler.  Map datatypes and
          use the FF restructure conversion routine. }

        Case aSourceType Of
          nftInt8: SourceFSType := fstInt8;
          nftInt16: SourceFSType := fstInt16;
          nftInt32: SourceFSType := fstInt32;
          nftUInt8: SourceFSType := fstUInt8;
          nftUInt16: SourceFSType := fstUInt16;
          nftUInt32: SourceFSType := fstUInt32;
          nftAutoInc8,
            nftAutoInc16,
            nftAutoInc32: SourceFSType := fstAutoInc32;
          nftSingle: SourceFSType := fstSingle;
          nftDouble: SourceFSType := fstDouble;
          nftExtended: SourceFSType := fstExtended;
          nftComp: SourceFSType := fstInt64;
          nftAutoInc64: SourceFSType := fstAutoInc64;
          nftCurrency: SourceFSType := fstCurrency;
          //nftBcd     :  SourceFSType := fstBinaryDecimals;
          nftBoolean: SourceFSType := fstBoolean;
          nftDateTime1: SourceFSType := fstDateTime;
          nftDateTime2:
            Begin
              SourceFSType := fstDateTime;
              { TDateTime values must be written to the record buffer as
                Delphi 1 values }
              TDateTime(aSourcePtr^) := TDateTime(aSourcePtr^) + 693594.0;
            End;
          nftLString: SourceFSType := fstShortString;
          nftZString: SourceFSType := fstNullString;
          nftUnicode:
            If aSourceSize = 2 Then
              SourceFSType := fstSingleWideChar
            Else
              SourceFSType := fstWideString;
          nftStDate: SourceFSType := fstDate;
          nftStTime: SourceFSType := fstTime;
          Else
            SourceFSType := fstArrayUInt8;
        End;

        Result := FSConvertSingleField(aSourcePtr,
          FBuffer,
          SourceFSType,
          aTargetFFType,
          aSourceSize,
          aTargetSize,
          -1,
          -1,
          -1,
          rNone,
          rNone,
          aRangeError);
      End;
  End;
End;

{ TffInOutEngine }

Constructor TffInOutEngine.Create(Const aFileName: TffFullFileName;
  aMode: Word);
Begin
  FLogFilename := ChangeFileExt(aFilename, '.LG');
  DeleteFile(FLogFilename);
  FLogCount := 0;
  FTerminated := False;

  FYieldInterval := DefYieldInterval;
  FImportFilename := aFileName;
  FSchema := TffSchemaFile.Create(ChangeFileExt(aFileName, DefExt));
  Case FSchema.FileType Of
    ftASCII:
      FStream := TffFixedASCIIStream.Create(aFileName, aMode, FSchema.RecordLength);
    ftBINARY:
      FStream := TffFixedFileStream.Create(aFilename, aMode, FSchema.RecordLength);
    ftBTF:
      Begin
        FStream := TffFixedBTFStream.Create(aFileName, aMode, FSchema.BTFDelFlag);
        FSchema.RecordLength := FStream.RecordLength;
      End;
    ftCSV: ;
    ftVARBTF: ;
  End;
End;

Destructor TffInOutEngine.Destroy;
Begin
  If FLogCount <> 0 Then
    CloseFile(FLogFile);

  FStream.Free;
  FSchema.Free;
  Inherited Destroy;
End;

Procedure TffInOutEngine.PostLog(S: String);
Begin
  If LogCount = 0 Then
    Begin
      AssignFile(FLogFile, FLogFilename);
      Rewrite(FLogFile);
    End;
  WriteLn(FLogFile, S);
  Inc(FLogCount);
End;

Procedure TffInOutEngine.Terminate;
Begin
  FTerminated := True;
End;

{ TffImportEngine }

Constructor TffImportEngine.Create(Const aFileName: TffFullFileName);
Begin
  Inherited Create(aFileName, fmOpenRead);
  FieldConverter := TffFieldConverter.Create;
End;

Destructor TffImportEngine.Destroy;
Begin
  FieldConverter.Free;
  Inherited Destroy;
End;

Procedure TffImportEngine.Import(aTable: TFStable; aBlockInserts: Word; aRangeError: boolean);
Var
  RecBuffer: PByteArray;
  FldBuffer: Pointer;
  FldBufLen: Longint;
  FFTable: TFSTable;
  F: Integer;
  Result: TffResult;
  DateMask: TffShStr;
  ProgressPacket: TffieProgressPacket;
  Status: TffResult;
  IsNull: Boolean;
  DoExplicitTrans: Boolean;
  InTransaction: Boolean;
  AutoIncField: Integer;
  AutoIncHighValue, ati: Int64;
  Step: Longint;
  Cursor: TfsSrBaseCursor;
  SetImp: TffUserRights;
Begin

  If aTable.CursorID = 0 Then
    DatabaseError(SDataSetClosed);

  If Not aTable.Active Then
    DatabaseError(SDataSetClosed);

  SetImp := atable.Session.Client.Rights;
  {If Not (arImportData In SetImp) Then
    Begin
      Check(DBIERR_NOTSUFFTABLERIGHTS);
      Exit;
    End;}

  { If we only have one insert per transaction, then let the server
    do implicit transactions; it'll be faster }
  If aBlockInserts = 0 Then
    aBlockInserts := 1;
  DoExplicitTrans := (aBlockInserts > 1);

  FFTable := aTable;
  Schema.BindDictionary(FFTable.Dictionary);

  { See if we'll need to deal with an autoinc field }
  AutoIncHighValue := 0;
  Step := 1;
  If Not FFTable.Dictionary.HasAutoIncField(AutoIncField) Then
    AutoIncField := -1;

  { Find the largest target field }
  FldBufLen := 0;
  For F := 0 To Schema.Fields.Count - 1 Do
    With Schema.Fields.Items[F] Do
      If fiTargetFieldNo <> -1 Then
        FldBufLen := FFMaxDW(FFTable.Dictionary.FieldLength[fiTargetFieldNo], FldBufLen);

  { Allocate field buffer }
  FFGetMem(FldBuffer, FldBufLen);
  Try

    { Bind the field converter }
    FieldConverter.Init(FldBuffer, FldBufLen, Schema, FFTable.Dictionary);

    { Allocate record buffer }
    FFGetMem(RecBuffer, FStream.RecordLength);
    Try
      With ProgressPacket Do
        Begin
          ppTotalRecs := Stream.NumRecords;
          ppNumRecs := 0;
        End;

      InTransaction := False;
      Try

        { For each record in the import file... }
        While FStream.ReadRec(RecBuffer^) Do
          Begin
            Inc(ProgressPacket.ppNumRecs);

            { Check to see if we need to send the progress status }
            If (ProgressPacket.ppNumRecs Mod YieldInterval) = 0 Then
              If Assigned(FOnYield) Then
                Begin
                  FOnYield(ProgressPacket);
                  Application.ProcessMessages;

                  { Check for user termination }
                  If Terminated Then
                    Begin
                      If InTransaction Then
                        aTable.Database.Rollback;
                      Exit;
                    End;
                End;

            { Blocks inserts within a transaction }
            If DoExplicitTrans And Not InTransaction Then
              Begin
                aTable.Database.StartTransaction;
                InTransaction := True;
              End;

            aTable.Insert;

            { Set all fields to default (null) values }
            aTable.ClearFields;

            { Find all fields in the import file }
            For F := 0 To Schema.Fields.Count - 1 Do
              Begin
                With Schema.Fields.Items[F], FFTable.Dictionary Do
                  Begin
                    If fiTargetFieldNo <> -1 Then
                      Begin

                        { If we have an ASCII date/time field, fetch the mask }
                        DateMask := '';
                        If fiNativeType In [nftASCIIDate,
                          nftASCIITime,
                          nftASCIITimestamp] Then
                          Begin
                            DateMask := fiDateMask;
                            If DateMask = '' Then
                              DateMask := Schema.DateMask;
                          End;

                        { Convert the field into FF datatype }
                        Status := FieldConverter.ConvertField(Schema.GetSourceFieldPtr(RecBuffer, F),
                          fiNativeType,
                          fiNativeSize,
                          FieldType[fiTargetFieldNo],
                          FieldLength[fiTargetFieldNo],
                          DateMask,
                          aRangeError);
                        With FFTable.Dictionary Do
                          Begin
                            If Status = 0 Then
                              Begin

                                { All's well, save the field data to the record buffer }
                                SetRecordField(fiTargetFieldNo,
                                  Pointer(aTable.ActiveBuffer),
                                  FldBuffer);

                                { Check for AutoInc field and retain largest value observed }
                                If fiTargetFieldNo = AutoIncField Then
                                  Begin
                                    If FFCmpDW(PInt64(FldBuffer)^, AutoIncHighValue) > 0 Then
                                      AutoIncHighValue := PInt64(FldBuffer)^;
                                  End;
                              End
                            Else
                              Begin

                                { Assign null for this field }
                                SetRecordField(fiTargetFieldNo,
                                  Pointer(aTable.ActiveBuffer),
                                  Nil);
                                Case Status Of
                                  DBIERR_INVALIDFLDXFORM:
                                    If ProgressPacket.ppNumRecs = 1 Then
                                      PostLog(Format('Field %s datatype %s is incompatible ' +
                                        'with target field datatype %s',
                                        [fiFieldName,
                                        fiNativeTypeDesc,
                                          GetEnumName(TypeInfo(TfsFieldType), Ord(FieldType[fiTargetFieldNo]))
                                          ]));
                                End;
                              End;
                          End;
                      End;
                  End;
              End;

            { Clean up "required" fields that are null; assign binary zero value }
            FillChar(FldBuffer^, FldBufLen, #0);
            With FFTable.Dictionary Do
              Begin
                For F := 0 To FieldCount - 1 Do
                  Begin
                    GetRecordField(F, Pointer(aTable.ActiveBuffer), IsNull, Nil);
                    If IsNull And FieldRequired[F] Then
                      If Not (FieldType[F] In [fstBLOB..ffcLastBLOBType]) Then
                        { set nonBLOB fields to zeros }
                        SetRecordField(F, Pointer(aTable.ActiveBuffer), FldBuffer);
                    { Required BLOB fields are going to fail if not loaded
                      by the import }
                  End;
              End;

            { Post the changes }
            aTable.Post;
            If AutoIncField <> -1 Then
              Begin
                // Check(aTable.gettablea .GetTableAutoInc(Ati, Step));
                Check(aTable.SetTableAutoIncValue(AutoIncHighValue, Step));
              End;

            { See if it's time to commit the transaction }
            If InTransaction And ((ProgressPacket.ppNumRecs Mod aBlockInserts) = 0) Then
              Begin
                aTable.Database.Commit;
                InTransaction := False;
              End;
          End;

        { Residual inserts need to be posted? }
        If InTransaction Then
          aTable.Database.Commit;
      Except
        On E: Exception Do
          Begin
            If InTransaction Then
              aTable.Database.Rollback;
            Raise;
          End;
      End;

      { Check to see if we need to send the final progress status }
      If (ProgressPacket.ppNumRecs Mod YieldInterval) <> 0 Then
        If Assigned(FOnYield) Then
          Begin
            FOnYield(ProgressPacket);
            Application.ProcessMessages;
          End;
    Finally
      FFFreeMem(RecBuffer, FStream.RecordLength);
    End;
  Finally
    FFFreeMem(FldBuffer, FldBufLen);
  End;
End;

End.

