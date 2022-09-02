{$I FsDEFINE.INC}

{$Z+}

Unit fssqldb;

Interface
Uses
  Windows,
  SysUtils,
  Classes,
  fsllbase,
  fslleng,
  fssrbde,
  fsserverclass,
  fssrlock,
  fslldict,
  fsstdate,
  fshash,
  fssrbase,
  fsfunInterp,
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  fssrcur,
  fsindexhelper,
  fszlib;

Const
  fscl_MaxSqlSortDepth = 64;
Type
  PStDate = ^TStDate;
  PStTime = ^TStTime;
  TBTable = Array[0..255] Of Byte; {Table used by Boyer-Moore search routines} {!!.11}
  PBTable = ^TBTable; {!!.11}

Type
  {Type for describing a field for creating temporary tables
   - see CreateTemporaryTable below.}
  PFSSqlFieldDefProxyRec = ^TFSSqlFieldDefProxyRec;
  TFSSqlFieldDefProxyRec = Record
    FieldName: String;
    DisplayName: String;
    FieldType: TfsFieldType;
    FieldUnits: Integer;
    Decimals: Integer;
    BlobLevelComp: TDataCompLevel;
    Round: TRound;
  End;

  TfsSqlFieldDefList = Class(TFSSpecObject)
  Protected
    FieldList: TfsPointerList;
    Function GetCount: Integer;
    Function GetFieldDecimals(Index: Integer): Integer;
    Function GetFieldName(Index: Integer): String;
    Function GetFieldDisplayName(Index: Integer): String;
    Function GetFieldType(Index: Integer): TfsFieldType;
    Function GetFieldUnits(Index: Integer): Integer;
    Function GetFieldBlob(Index: Integer): TDataCompLevel;
    Function GetFieldRound(Index: Integer): TRound;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function CompareTypes(Other: TfsSqlFieldDefList): Boolean;
    Procedure AddField(Const aName, aDisplayName: String; aType: TfsFieldType; aUnit: Integer; aDec: Integer;
      aBlobLevelComp: TDataCompLevel; aRound: TRound);
    Property Count: Integer Read GetCount;
    Property FieldName[Index: Integer]: String Read GetFieldName;
    Property FieldDisplayName[Index: Integer]: String Read GetFieldDisplayName;
    Property FieldType[Index: Integer]: TfsFieldType Read GetFieldType;
    Property FieldUnits[Index: Integer]: Integer Read GetFieldUnits;
    Property FieldDecimals[Index: Integer]: Integer Read GetFieldDecimals;
    Property BlobLevelComp[Index: Integer]: TDataCompLevel Read GetFieldBlob;
    Property FieldRound[Index: Integer]: TRound Read GetFieldRound;
  End;

  TfsSqlSortArray = Array[0..pred(fscl_MaxSqlSortDepth)] Of Integer;

  TFSSqlTableProxy = Class;

  {Interface between SQL engine and a table field definition}
  TFSSqlFieldProxy = Class(TFSSpecObject)
  Private
    Procedure SetBlobValue(Const Value: Variant); {!!.13}
  Protected
    FCursorID: TFFCursorID;
    FIndex: Integer;
    FIsTarget: Boolean;
    FOwnerTable: TFSSqlTableProxy;
    FSrcField: TFSSqlFieldProxy;
    FSrcIndex: Integer;
    TypeKnown: Boolean;
    FType: TfsFieldType;
    Procedure ReadField(Var IsNull: Boolean);
    Procedure WriteField;
    Procedure WriteFieldDirect(Buffer: PffByteArray);
    Function GetBlobValue: Variant; {!!.11} {!!.13}
    Function BLOBBmSearch(Const Table: TBTable;
      Const SearchPhrase: String;
      IgnoreCase: Boolean {!!.13}
      ): Boolean; {!!.11}
  Public
    FieldBuffer: PffByteArray;
    FieldBufferLength: Integer;
    Constructor Create(AnOwnerTable: TFSSqlTableProxy; AnIndex: Integer; ACursorID: TFFCursorID);
    Destructor Destroy; Override;
    Property Index: Integer Read FIndex;
    Function Name: String;
    Function GetDisplayName: String;
    Function IsNull: Boolean;
    Function GetSize: Integer;
    Function GetDecimals: Integer;
    Function GetPrecision: Integer;
    Function GetBlobLevel: TDataCompLevel;
    Function GetRound: TRound;
    Function GetType: TfsFieldType;
    Function GetValue(aArrayIndex: Integer): Variant;
    Procedure Value(aArrayIndex: Integer; Var Result: Variant);
    Procedure SetValue(Const Value: Variant; aArrayIndex: Integer);
    Property IsTarget: Boolean Read FIsTarget Write FIsTarget;
    { If this is a field in the result set table (i.e., a target field) then
      this property returns True. }
    Property OwnerTable: TFSSqlTableProxy Read FOwnerTable;
    Function QualName: String;
    Property SrcField: TFSSqlFieldProxy Read FSrcField Write FSrcField;
    { If this is a target field that refers to a source field then this
      property references the source field. }
    Property SrcIndex: Integer Read FSrcIndex Write FSrcIndex;
    { If this is a target field that refers to a simple expression then
      this property identifies the index of the simple expression in
      protected variable FSX. }
    Function CanUpdate: Boolean;
    Procedure SetDefault; {!!.11}
    Procedure SetFieldToNull;
    Function BMMatch(Const Table: TBTable; Const SearchPhrase: String;
      IgnoreCase: Boolean {!!.13}
      ): Boolean; {!!.11}
  End;

  TFSSqlDatabaseProxy = Class;

  TFSCopyValidator = Function: Boolean Of Object;

  TFSSqlTableIterator = Function(Cookie: TffWord32): Boolean Of Object;

  {Interface between SQL engine and a table definition}
  TFSSqlTableProxy = Class(TFSSpecObject)
  Protected
    FCursorID: TFFCursorID;
    FName: String;
    FAlias: String;
    KeyBuffer1,
      KeyBuffer2,
      RecordBuffer: PffByteArray;
    FDataBase: TFSSqlDatabaseProxy;
    FEngine: TFSBaseServerEngine;
    FIndex: Integer;
    FLeaveOpen: Boolean;
    FRecordLen: Longint;
    NoRecord: Boolean;
    FOwner: TObject;
    FSelect: TFSSpecObject;
    FGroup: TFSSpecObject;
    FIndexID: Word;
    FOnlineCursor: boolean;
    fIsProcedure: Boolean;
    Function SortOnAllFields(aList: TList; Const CaseSensitive: Boolean): TffResult; {!!.13}
  Public
    RecFlag: Byte;
    NormalFieldList: TList;
    DistinctFieldList: TList;
    DistinctFieldName: TStringList;
    Procedure Iterate(Iterator: TFSSqlTableIterator; Cookie: TffWord32);
    Constructor Create(AOwner: TObject;
      ADataBase: TFSSqlDatabaseProxy; ACursorID: TFFCursorID; Const AName,
      AAlias: String); {!!.11}
    Destructor Destroy; Override;
    Property Select: TFSSpecObject Read FSelect Write fSelect;
    Property Group: TFSSpecObject Read FGroup Write fGroup;
    Property OnlineCursor: boolean Read fOnlineCursor Write fOnlineCursor;
    Property IsProcedure: Boolean Read fIsProcedure Write fIsProcedure;
    Property IndexID: Word Read FIndexID Write FIndexID;
    Property Name: String Read FName;
    Property Alias: String Read FAlias; {!!.11}
    Property CursorID: TFFCursorID Read FCursorID Write FCursorID;
    Property LeaveCursorOpen: Boolean Read FLeaveOpen Write FLeaveOpen;
    Function FieldCount: Integer;
    Function Field(Index: Integer): TFSSqlFieldProxy;
    Function FieldByName(Const Name: String): TFSSqlFieldProxy;

    Function DistinctFieldCount: Integer;
    Function DistinctField(Index: Integer): TFSSqlFieldProxy;
    Function DistinctFieldByName(Const Name: String): TFSSqlFieldProxy;
    Procedure ClearDistinct;
    Procedure Close;
    Function Delete: TffResult; {!!.11}
    Function CurrDelete: TffResult;
    Function First: Boolean;
    Function Last: Boolean;
    Function Next: Boolean;
    Function Prior: Boolean;
    Function NextPrior(TD: TTopDirection): Boolean;
    Function IsRecordLocked: boolean;
    Function IsUndeletedRecord: boolean;
    Function IsProtectDeleteRecord: boolean;
    Function IsProtectUpdateRecord: boolean;
    Function IsMarkAsBadRecord: boolean;

    Procedure SetRange(Const StartValues, EndValues: Array Of Variant;
      Const LowCount, HighCount: Integer;
      Const IncludeLowLimit, IncludeHighLimit,
      IndexAsc: Boolean; aPartialLen: Integer = 0);
    Function EnsureWritable: TffResult; {!!.11}
    { Verify the table may be modified. }{!!.11}
    Function EOF: Boolean;
    Procedure Insert;
    {- create a new record where all fields are initially NULL}
    Function InsertPost: TffResult;
    {- actually insert the record.
     - Currently, Insert and Post will only be performed
       on temporary tables created by the SQL statement itself.}
    Function InsertPostNoDefaults: TffResult;
    Function Update: TffResult; {!!.11}
    { - update the current record buffer in the table}
    Procedure SetIndex(KeyNum: Integer);
    {- switch to specified key, 0..pred(GetNumIndexes) means an actual index;
       -1 means physical order (i.e. use no defined ordering) }
    Function GetSegments: Integer;
    {- return number of fields in the currently active index}
    Function CopyValidated(AOwner: TObject; Validator: TFSCopyValidator): TFSSqlTableProxy; {!!.10}
    {- return a copy of the table with only records that are valid
       as per the called Validator function}
    Function CopySortedOnAllFields(AOwner: TObject): TFSSqlTableProxy;
    Function GetCurrentRecordID: Tffint64;
    Function GetRecordByID(ID: Tffint64;
      Const LockType: TfsSrcLockType): TffResult;
    Function IndexesOnField(F: TFSSqlFieldProxy; MustBeCaseInsensitive: Boolean;
      Var IndexRefs: Array Of Integer): Integer;
    Procedure GetIndexProperties(Const Index: Integer;
      Var Unique, IgnoreCase, IndexAsc: Boolean;
      Var IndexFieldCount: Integer; Var IndexFields: Array Of Integer);
    Function FindIndex(Const IndexName: String): Longint;
    {Begin !!.13}
    Function Sort(Const SortListCount: Integer;
      Const SortList: TfsSqlSortArray;
      Const CaseSensitive: Boolean): TffResult;
    Function OrderSort(Const SortListCount: Integer;
      Const SortList: TfsSqlSortArray;
      Const SortCase: TfsSqlSortArray;
      Const SortSize: TfsSqlSortArray;
      Const SortNull: TfsSqlSortArray): TffResult;
    Function CopyLimit(AOwner: TObject;
      Const RecCount, LimitCountT, LimitCountC, LimitCountD, LimitStart: Int64;
      TopDirection: TTopDirection = tdNone;
      DivBy: Longint = 0; AtOne: Boolean = False;
      Order: boolean = False; Distinct: boolean = False): TFSSqlTableProxy;
    Function CopyUnique(AOwner: TObject;
      Const CaseSensitive: Boolean): TFSSqlTableProxy;
    Function HasDuplicates(Const CaseSensitive: Boolean): Boolean;
    {End !!.13}
    Function ExtractFieldDef: TfsSqlFieldDefList;
    Function ExtractDistinctFieldDef: TfsSqlFieldDefList;
    Function GetRecordCount: Integer;
    Property Engine: TFSBaseServerEngine Read FEngine Write FEngine;
    Procedure NullRecord;
    Property Owner: TObject Read FOwner Write FOwner;
    Property PRecordBuffer: PffByteArray Read RecordBuffer;
    Procedure SetDefaults;
    Procedure SetDefault(aField: String);
    Procedure SetNull(aField: String);

    Function NextAutoInc: Int64; // next value
    Function ReadLastAutoInc: Int64; // Last value
  End;

  {Interface between SQL engine and the database}
  TFSSqlDatabaseProxy = Class(TFSSpecObject)
  Protected
    FEngine: TFSServer;
    FDatabaseID: TFFDatabaseID;
    FLastAutoInc: Int64;
  Public
    Property Engine: TFSServer Read FEngine;
    Property LastAutoInc: Int64 Read FLastAutoInc Write FLastAutoInc;
    Constructor Create(Engine: TFSServer; DatabaseID: TFFDatabaseID);
    Destructor Destroy; Override;
    Function TableByName(AOwner: TObject;
      Const S: String;
      Const OpenMode: TffOpenMode;
      Const ExclContentLock: Boolean;
      Const AAlias: String;
      Const aIndexName: String): TFSSqlTableProxy;
    {- find a table by name. if the table does not exist, NIL
       is returned}
    Function TableByCursorID(AOwner: TObject; aCursorID: TffCursorID): TFSSqlTableProxy;
    {- find a table by CursorID. if the table does not exist, NIL
        is returned}
    Function CreateTemporaryTableWithIndex(
      AOwner: TObject;
      Const FieldDef: TfsSqlFieldDefList;
      IndexFields: Integer; IndexColumns: TfsSqlSortArray):
      TFSSqlTableProxy;
    {- create a temporary table as per the specified field and
       key segment lists. Return a proxy object, which gives
       access to the (initially empty) table. When the proxy
       object is freed, the tables can (should) be deleted.
       FieldList is a TList containing PFSSqlFieldDefProxyRec
       instances (see above). Each entry describes a field in the
       table. KeyList is a TList containing PFFSqlKeySegmentDefProxyRec
       instances (see above). Each entry describes a key segment}
    Function CreateTemporaryTableWithoutIndex(
      AOwner: TObject;
      Const FieldDef: TfsSqlFieldDefList): TFSSqlTableProxy;

    Function StartTransaction(Const Tables: Array Of TFSSqlTableProxy): TffResult;
    Function Commit: TffResult;
    Function AbortTransaction: TffResult;
    Function InTransaction: boolean;
    Function Alias: String;
  End;

Type
  TFSVariantList = Class
  Protected
    List: TfsPointerList;
  Public
    Constructor Create(Capacity: Integer);
    Destructor Destroy; Override;
    Function GetValue(Index: Integer): Variant;
    Procedure SetValue(Index: Integer; Const Value: Variant);
    Procedure Value(Index: Integer; Var Result: Variant);
  End;

Const
  fsNRHashMaxRecords = MaxInt Div sizeof(TffInt64);
  fsMaxSourceTables = MaxInt Div sizeof(TFSSqlTableProxy);
Type
  TfsNRecordHashNode = Class(TfsHashNode)
    Destructor Destroy; Override;
  End;

  TfsNRecordHashEntry = Array[0..pred(fsNRHashMaxRecords)] Of TffInt64;
  PfsNRecordHashEntry = ^TfsNRecordHashEntry;
  TfsTableArray = Array[0..pred(fsMaxSourceTables)] Of TFSSqlTableProxy;
  PfsTableArray = ^TfsTableArray;

  TfsNRecordHash = Class(TfsBaseHashTable)
    {- a data structure for keeping track of duplicate
       record combinations when doing joins}
  Protected
    FSourceTables: PfsTableArray;
    EntrySlots: Integer;
    Function fhCompareKey(Const aKey1: Pointer;
      Const aKey2: Pointer): Boolean; Override;

    Function fhCreateNode: TfsHashNode; Override;
    Procedure fhFreeKeyPrim(aKey: pointer); Override;

    Function fhGetIndex(Const AKey: Pointer;
      Const ACount: Integer): Integer; Override;
    {calculate the index, ie hash, of the key}

  Public
    Constructor Create;
    {$IFDEF DCC4OrLater} Reintroduce;
    {$ENDIF}
    Destructor Destroy; Override;
    Procedure AddTable(Const SourceTable: TFSSqlTableProxy);
    Procedure Add;
    Function Exists: Boolean;
  End;

Type
  TFSFieldCopier = Class(TFSSpecObject)
  Protected
    FSourceList, FTargetList, FCompatible, FBlob: TfsPointerList;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Procedure Execute;
    Procedure Add(SourceField, TargetField: TFSSqlFieldProxy);
  End;

Procedure fsCopyField(Const SourceField, TargetField: TFSSqlFieldProxy);
Function fsCompatibleFields(Const SourceField, TargetField: TFSSqlFieldProxy): Boolean;
Procedure fsBMMakeTableS(Const MatchString: ShortString; Var BT: TBTable); {!!.11}

Implementation

Uses
  FsLLExcp,
  FsSrCvex,
  fsutil;

{$I FsCONST.INC}

{ TFSSqlDatabaseProxy }

Constructor TFSSqlDatabaseProxy.Create(Engine: TFSServer;
  DatabaseID: TFFDatabaseID);
Begin
  Inherited Create;
  FEngine := Engine;
  FDatabaseID := DatabaseID;
  FLastAutoInc := 0;
End;

Function TFSSqlDatabaseProxy.StartTransaction(Const Tables: Array Of TFSSqlTableProxy): TffResult;
Var
  CursorIDs: TfsPointerList;
  Inx: Integer;
Begin
  Assert(FEngine <> Nil);
  Assert(FEngine Is TFSBaseServerEngine);
  If Tables[0] = Nil Then
    Begin
      Result := DBIERR_NONE;
      FEngine.TransactionStartSQL(FDatabaseID, False)
    End
  Else
    Begin
      { Build the list of cursor IDs. }
      CursorIDs := TfsPointerList.Create;
      Try
        For Inx := Low(Tables) To High(Tables) Do
          CursorIDs.Append(Pointer(Tables[Inx].CursorID));
        Result := FEngine.TransactionStartWith(FDatabaseID, False, CursorIDs);
      Finally
        CursorIDs.Free;
      End;
    End;
End;

Function TFSSqlDatabaseProxy.InTransaction: boolean;
Var
  TrLevel: Longint;
  err: TffResult;

Begin
  Assert(FEngine <> Nil);
  Assert(FEngine Is TFSBaseServerEngine);
  TrLevel := -1;
  err := FEngine.InTransaction(FDatabaseID, TrLevel);
  If err = DBIERR_NOACTIVETRAN Then
    Result := False
  Else
    Begin
      Assert(Err = 0);
      Result := TrLevel >= 0;
    End;
End;

Function TFSSqlDatabaseProxy.AbortTransaction: TffResult;
Begin
  Assert(FEngine <> Nil);
  Assert(FEngine Is TFSBaseServerEngine);
  Result := FEngine.TransactionRollbackSQL(FDatabaseID, False);
End;

Function TFSSqlDatabaseProxy.Commit: TffResult;
Begin
  Assert(FEngine <> Nil);
  Assert(FEngine Is TFSBaseServerEngine);
  Result := FEngine.TransactionCommitSQL(FDatabaseID, False);
End;

Destructor TFSSqlDatabaseProxy.Destroy;
Begin
  Inherited Destroy;
End;

Function TFSSqlDatabaseProxy.CreateTemporaryTableWithIndex(
  AOwner: TObject;
  Const FieldDef: TfsSqlFieldDefList;
  IndexFields: Integer; IndexColumns: TfsSqlSortArray): TFSSqlTableProxy;
Var
  Dictionary: TFSInfoDict;
  i: Integer;
  KeySegList: TFFFieldList;
  FieldsAscDesc: TffFieldList;
  FieldsCase, FieldsSize, FieldsFlags, FieldsNullTop: TffFieldList;
  FldIHList: TFFFieldIHList;
  Cursor: TfsSrBaseCursor;
  IsAutoinc: boolean;
  Auto64, Step64: Int64;
Begin
  Auto64 := 0;
  Step64 := 0;

  IsAutoinc := False;
  Dictionary := TFSInfoDict.Create(fscl_64k);
  Try
    For i := 0 To pred(FieldDef.Count) Do
      Begin
        If Not IsAutoinc Then
          If FieldDef.FieldType[i] In [fstAutoInc32, fstAutoInc64] Then
            Begin
              Auto64 := FieldDef.FieldUnits[i];
              Step64 := FieldDef.FieldDecimals[i];
              IsAutoinc := True;
            End;
        Dictionary.AddField(FieldDef.FieldName[i], FieldDef.FieldDisplayName[i], FieldDef.FieldType[i],
          FieldDef.FieldUnits[i], FieldDef.FieldDecimals[i], False, Nil, FieldDef.BlobLevelComp[i], '',
          FieldDef.FieldRound[i], False, duNormal);
        If FieldDef.FieldRound[i] <> rnone Then Dictionary.IsAnyRound := True;
      End;

    For i := 0 To pred(IndexFields) Do
      Begin
        KeySegList[i] := IndexColumns[i];
        FieldsAscDesc[i] := 1; //asc true
        FieldsCase[i] := 0; //asc true
        FldIHList[i] := '';
        FieldsSize[i] := 0;
        FieldsFlags[i] := 0;
        FieldsNullTop[i] := 1;
      End;

    Dictionary.AddIndex('key0', '', 0, IndexFields,
      KeySegList, FieldsAscDesc, FieldsCase, FieldsSize,
      FieldsFlags, FieldsNullTop, FldIHList, True);
    Dictionary.EngineDeleteType := edtNotUndelete;
    Cursor := TfsSrcCursor.Create(TFSServer(FEngine),
      TFsSrcDatabase(FDatabaseID),
      FFGetRemainingTime);

    Cursor.Build('', Dictionary, omReadWrite, smExclusive,
      False, True, [fffaTemporary, fffaBLOBChainSafe], 0);

    Cursor.CloseTable := True;
    Result := TFSSqlTableProxy.Create(AOwner, Self, Cursor.CursorID, '', ''); {!!.11}
    Result.Engine := FEngine;
    If IsAutoinc Then
      Begin
        StartTransaction(Result);
        Cursor.SetAutoInc(Auto64, Step64);
        commit;
      End;
  Finally
    Dictionary.Free;
  End;
End;

Function TFSSqlDatabaseProxy.CreateTemporaryTableWithoutIndex(AOwner: TObject;
  Const FieldDef: TfsSqlFieldDefList): TFSSqlTableProxy;
Var
  Dictionary: TFSInfoDict;
  i: Integer;
  Cursor: TfsSrBaseCursor;
  IsAutoinc: boolean;
  Auto64, Step64: Int64;
  GRemain: Longint;
Begin
  IsAutoinc := False;
  Dictionary := TFSInfoDict.Create(fscl_64k);
  Try
    For i := 0 To pred(FieldDef.Count) Do
      Begin
        If Not IsAutoinc Then
          If FieldDef.FieldType[i] In [fstAutoInc32, fstAutoInc64] Then
            Begin
              Auto64 := FieldDef.FieldUnits[i];
              Step64 := FieldDef.FieldDecimals[i];
              IsAutoinc := True;
            End;
        Dictionary.AddField(FieldDef.FieldName[i], FieldDef.FieldDisplayName[i], FieldDef.FieldType[i],
          FieldDef.FieldUnits[i], FieldDef.FieldDecimals[i], False, Nil, FieldDef.BlobLevelComp[i], '',
          FieldDef.FieldRound[i], False, duNormal);
        If FieldDef.FieldRound[i] <> rnone Then Dictionary.IsAnyRound := True;
      End;
    Dictionary.EngineDeleteType := edtNotUndelete;
    {$IFDEF DCC7OrLater}
    {$WARNINGS OFF}
    {$ENDIF}
    GRemain := FFGetRemainingTime;
    Cursor := TfsSimpleSqlResultSet.Create(TFSServer(FEngine), TFsSrcDatabase(FDatabaseID), GRemain);
    {$IFDEF DCC7OrLater}
    {$WARNINGS OFF}
    {$ENDIF}
    Cursor.Build('', Dictionary, omReadWrite, smExclusive,
      False, True, [fffaTemporary, fffaBLOBChainSafe], 0);

    Cursor.CloseTable := True;
    Result := TFSSqlTableProxy.Create(AOwner, Self, Cursor.CursorID, '', '');
    Result.Engine := FEngine;
    If IsAutoinc Then
      Begin
        StartTransaction(Result);
        Cursor.SetAutoInc(Auto64, Step64);
        commit;
      End;
  Finally
    Dictionary.Free;
  End;
End;

Function TFSSqlDatabaseProxy.TableByName(AOwner: TObject;
  Const S: String;
  Const OpenMode: TffOpenMode;
  Const ExclContentLock: Boolean;
  Const AAlias: String;
  Const aIndexName: String): TFSSqlTableProxy;

Var
  Cursor: TfsSrBaseCursor;
  CurID: TffCursorID;
  aProc, aProcParam: String;
  SysT, ETable: boolean;
  i: Integer;

  Function aFindIndex(Const IndexName: String): Longint;
  Var
    i: Integer;
  Begin
    Result := -1;
    If Cursor <> Nil Then
      For i := 0 To pred(Cursor.Dictionary.IndexCount) Do
        Begin
          If AnsiUpperCase(Cursor.Dictionary.IndexDescriptor[i].idName) =
            AnsiUpperCase(IndexName) Then
            Begin
              Result := i;
              System.Break;
            End;
        End;
  End;
Begin
  Cursor := Nil;
  Result := Nil;
  CurID := 0;
  aProcParam := '';
  aProc := '';
  Try
    Assert(FEngine <> Nil);
    Assert(FEngine Is TFSServer);
    Assert(FDatabaseID <> 0);
    Assert(TObject(FDatabaseID) Is TFsSrcDatabase);

    FEngine.DatabaseTableExists(FDatabaseID, S, ETable);
    If Not ETable Then
      aProc := IsProcedure(S, aProcParam);

    If aProc <> '' Then
      Begin
        Cursor := TfsSrcCursor.Create(TFSServer(FEngine), TFsSrcDatabase(FDatabaseID),
          FFGetRemainingTime);
        Try

          Cursor.ExecProcedure(aProc, aProcParam, CurID);
          If CurID > 0 Then
            Begin
              Result := TFSSqlTableProxy.Create(AOwner, Self, CurID, S, AAlias);
              Result.Engine := FEngine;
              Result.IsProcedure := True;
            End;
        Finally
          Cursor.Free;
        End;
      End
    Else
      Begin
        SysT := (UpperCase(S) <> 'SYS$CONSTRAINTS') And (UpperCase(S) <> 'SYS$TRIGGERS');
        Cursor := TfsSrcCursor.Create(TFSServer(FEngine), TFsSrcDatabase(FDatabaseID), FFGetRemainingTime);
        Cursor.Open(S, '', 0, OpenMode, smShared, False, ExclContentLock, [], SysT);

        If aIndexName <> '' Then
          Begin
            i := aFindIndex(aIndexName);
            If i = -1 Then
              Begin
                Raise Exception.CreateFmt('Error:', [aIndexName + ' is wrong ' + s]);
              End
            Else
              Begin
                Cursor.SwitchToIndex(i, False);
                Cursor.SetToBegin;
              End;
          End;
        Result := TFSSqlTableProxy.Create(AOwner, Self, Cursor.CursorID, S, AAlias);
        Result.Engine := FEngine;
        Result.IndexID := cursor.IndexID;
      End;

  Except
    On E: Exception Do
      Begin
        ConvertServerExceptionEx(E, FEngine.EventLog, FEngine.IsReadOnly);

        If Assigned(Result) Then
          Result.free;
        If Assigned(Cursor) Then
          Cursor.Free;
        Result := Nil;
      End;
  End;
End;

Function TFSSqlDatabaseProxy.TableByCursorID(AOwner: TObject; aCursorID: TffCursorID): TFSSqlTableProxy;
Begin
  Result := Nil;
  Try
    Assert(FEngine <> Nil);
    Assert(FEngine Is TFSServer);
    Assert(FDatabaseID <> 0);
    Assert(aCursorID <> 0);
    Assert(TObject(FDatabaseID) Is TFsSrcDatabase);

    Result := TFSSqlTableProxy.Create(AOwner, Self, aCursorID, TfsSrBaseCursor(aCursorID).TableName, '');
    Result.Engine := FEngine;

  Except
    On E: Exception Do
      Begin
        ConvertServerExceptionEx(E, FEngine.EventLog, FEngine.IsReadOnly);

        If Assigned(Result) Then
          Result.free;
        Result := Nil;
      End;
  End;
End;

Function TFSSqlDatabaseProxy.Alias: String;
Begin
  Assert(FDatabaseID <> 0);
  Assert(TObject(FDatabaseID) Is TFsSrcDatabase);
  Result := TFsSrcDatabase(FDatabaseID).Alias;
End;

{ TFSVariantList }

Constructor TFSVariantList.Create(Capacity: Integer);
Var
  I: Integer;
Begin
  Inherited Create;
  List := TfsPointerList.Create;
  List.Capacity := Capacity;
  List.Count := Capacity;
  For i := 0 To pred(List.Capacity) Do
    List[i] := Nil;
End;

Destructor TFSVariantList.Destroy;
Var
  i: Integer;
  P: Pointer;
Begin
  For i := 0 To pred(List.Count) Do
    If List[i] <> Nil Then
      Begin
        Finalize(PVariant(List[i])^);
        P := List[i];
        FFFreeMem(P, sizeof(Variant));
      End;
  List.Free;
  Inherited;
End;

Function TFSVariantList.GetValue(Index: Integer): Variant;
Begin
  Assert(List[Index] <> Nil);
  {$IFDEF IsNoVariantInt64}
  Case TVarData(PVariant(List[Index])^).Vtype Of
    vt_e80:
      Begin
        TVarData(Result).Vtype := VT_E80;
        Decimal(Result).ext80 := Decimal(PVariant(List[Index])^).ext80;
      End;
    vt_decimal:
      Begin
        TVarData(Result).Vtype := vt_decimal;
        Decimal(Result).lo64 := Decimal(PVariant(List[Index])^).lo64;
      End;
    Else
      Result := PVariant(List[Index])^;
  End;
  {$ELSE}
  Result := PVariant(List[Index])^;
  {$ENDIF}
End;

Procedure TFSVariantList.Value(Index: Integer; Var Result: Variant);
Begin
  Assert(List[Index] <> Nil);
  {$IFDEF IsNoVariantInt64}
  Case TVarData(PVariant(List[Index])^).Vtype Of
    vt_e80:
      Begin
        TVarData(Result).Vtype := VT_E80;
        Decimal(Result).ext80 := Decimal(PVariant(List[Index])^).ext80;
      End;
    vt_decimal:
      Begin
        TVarData(Result).Vtype := vt_decimal;
        Decimal(Result).lo64 := Decimal(PVariant(List[Index])^).lo64;
      End;
    Else
      Result := PVariant(List[Index])^;
  End;
  {$ELSE}
  Result := PVariant(List[Index])^;
  {$ENDIF}
End;

Procedure TFSVariantList.SetValue(Index: Integer; Const Value: Variant);
Var
  PV: PVariant;
Begin
  If List[Index] = Nil Then
    Begin
      FFGetZeroMem(PV, sizeof(Variant));
      List[Index] := PV;
    End;
  {$IFDEF IsNoVariantInt64}
  Case TVarData(Value).Vtype Of
    vt_e80:
      Begin
        TVarData(PVariant(List[Index])^).Vtype := VT_E80;
        Decimal(PVariant(List[Index])^).ext80 := Decimal(Value).ext80;
      End;
    vt_decimal:
      Begin
        TVarData(PVariant(List[Index])^).Vtype := vt_decimal;
        Decimal(PVariant(List[Index])^).lo64 := Decimal(Value).lo64;
      End;
    Else
      PVariant(List[Index])^ := Value;
  End;
  {$ELSE}
  PVariant(List[Index])^ := Value;
  {$ENDIF}
End;

{ TFSSqlTableProxy }

Function TFSSqlTableProxy.ExtractFieldDef: TfsSqlFieldDefList;
Var
  i: Integer;
Begin
  Result := TfsSqlFieldDefList.Create;
  For i := 0 To pred(NormalFieldList.Count) Do
    Result.AddField(Field(i).Name, '', Field(i).GetType, Field(i).GetSize,
      Field(i).GetDecimals, Field(i).GetBlobLevel, Field(i).getRound);
End;

Function TFSSqlTableProxy.ExtractDistinctFieldDef: TfsSqlFieldDefList;
Var
  i: Integer;
Begin
  Result := TfsSqlFieldDefList.Create;
  For i := 0 To pred(DistinctFieldList.Count) Do
    Result.AddField(DistinctField(i).Name, '', DistinctField(i).GetType, DistinctField(i).GetSize,
      DistinctField(i).GetDecimals, DistinctField(i).GetBlobLevel, DistinctField(i).getRound);
End;

Function TFSSqlTableProxy.CopySortedOnAllFields(
  AOwner: TObject): TFSSqlTableProxy;
Var
  i: Integer;
  FieldDefList: TfsSqlFieldDefList;
  {$IFOPT C+}
  CopyResult: TffResult;
  {$ENDIF}
  IndexColumns: TfsSqlSortArray;
Begin
  FieldDefList := ExtractFieldDef;
  Try
    For i := 0 To pred(NormalFieldList.Count) Do
      IndexColumns[i] := i;

    Result := FDatabase.CreateTemporaryTableWithIndex(AOwner, FieldDefList,
      NormalFieldList.Count, IndexColumns);

  Finally
    FieldDefList.Free;
  End;

  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Assert(Result.FCursorID <> 0);
  Assert(TObject(Result.FCursorID) Is TfsSrBaseCursor);

  {$IFOPT C+}
  CopyResult :=
    {$ENDIF}
  TfsSrBaseCursor(Result.FCursorID).CopyRecords(
    TfsSrBaseCursor(FCursorID), ffbcmCreateLink, Nil, 0, 0, 0);

  {$IFOPT C+}
  Assert(CopyResult = DBIERR_NONE);
  {$ENDIF}

  Result.SetIndex(0);
End;

Function TFSSqlTableProxy.SortOnAllFields(aList: TList; Const CaseSensitive: Boolean): TffResult; {!!.13}
Var
  aCount: Integer;
  i: Integer;
  KeyArray: TfsSqlSortArray;
Begin
  aCount := FFMinI(aList.Count, fscl_MaxIndexFlds);
  For i := 0 To pred(aCount) Do
    Begin
      KeyArray[i] := TFSSqlFieldProxy(aList[i]).Index + 1;
      {KeyArray values are +1 to allow for specifying descending sorting on column 0
       (by negating)}
    End;

  Result := Sort(aCount, KeyArray, CaseSensitive); {!!.13}
End;

Function TFSSqlTableProxy.CopyLimit(AOwner: TObject;
  Const RecCount,
  LimitCountT,
  LimitCountC,
  LimitCountD,
  LimitStart: Int64;
  TopDirection: TTopDirection = tdNone;
  DivBy: Longint = 0;
  AtOne: Boolean = False;
  Order: boolean = False;
  Distinct: boolean = False): TFSSqlTableProxy;

Var
  i, DivB, increc: Longint;
  FieldCopier: TfsFieldCopier;
  FieldDefList: TfsSqlFieldDefList;
  rcount, LStart: Int64;

  Procedure aTop;
  Begin
    If LimitCountT = -1 Then
      rcount := -2
    Else
      rcount := 0;
    LStart := 0;
    DivB := 0;
    If AtOne Then
      DivB := DivBy - 1;

    Repeat
      Inc(DivB);
      If DivBy > 1 Then
        Begin
          If DivB = DivBy Then
            Begin
              DivB := 0;
              Inc(LStart);
              If LimitStart > 0 Then
                Begin
                  If LStart >= LimitStart Then
                    Begin
                      If LimitCountT >= 0 Then
                        inc(rcount);
                      Result.Insert;
                      FieldCopier.Execute;
                      Result.InsertPost;
                    End;
                End
              Else
                Begin
                  If LimitCountT >= 0 Then
                    inc(rcount);
                  Result.Insert;
                  FieldCopier.Execute;
                  Result.InsertPost;
                End;
            End;
        End
      Else
        Begin
          Inc(LStart);
          If LimitStart > 0 Then
            Begin
              If LStart >= LimitStart Then
                Begin
                  If LimitCountT >= 0 Then
                    inc(rcount);
                  Result.Insert;
                  FieldCopier.Execute;
                  Result.InsertPost;
                End;
            End
          Else
            Begin
              If LimitCountT >= 0 Then
                inc(rcount);
              Result.Insert;
              FieldCopier.Execute;
              Result.InsertPost;
            End;
        End;
      Inc(increc);
    Until Not Next Or (rcount >= LimitCountT);
  End;

  Procedure aCenter;
  Var
    cstart: Longint;
  Begin
    LStart := 0;
    rcount := 0;
    cstart := (RecCount Div 2) - (LimitCountC Div 2);
    DivB := 0;
    If AtOne Then
      DivB := DivBy - 1;
    // set position
    Repeat
      If increc >= cstart Then
        Begin
          Inc(DivB);
          If DivBy > 1 Then
            Begin
              If DivB = DivBy Then
                Begin
                  DivB := 0;
                  Inc(LStart);
                  If LimitStart > 0 Then
                    Begin
                      If LStart >= LimitStart Then
                        Begin
                          If LimitCountC >= 0 Then
                            inc(rcount);
                          Result.Insert;
                          FieldCopier.Execute;
                          Result.InsertPost;
                        End;
                    End
                  Else
                    Begin
                      If LimitCountC >= 0 Then
                        inc(rcount);
                      Result.Insert;
                      FieldCopier.Execute;
                      Result.InsertPost;
                    End;
                End;
            End
          Else
            Begin
              Inc(LStart);
              If LimitStart > 0 Then
                Begin
                  If LStart >= LimitStart Then
                    Begin
                      If LimitCountC >= 0 Then
                        inc(rcount);
                      Result.Insert;
                      FieldCopier.Execute;
                      Result.InsertPost;
                    End;
                End
              Else
                Begin
                  If LimitCountC >= 0 Then
                    inc(rcount);
                  Result.Insert;
                  FieldCopier.Execute;
                  Result.InsertPost;
                End;
            End;
        End;
      Inc(increc);
    Until Not Next Or (rcount >= LimitCountC);
  End;

  Procedure aDown;
  Begin
    If LimitCountD = -1 Then
      rcount := -2
    Else
      rcount := 0;
    LStart := 0;
    DivB := DivBy;
    If DivB < 1 Then DivB := 1;
    Last;
    LStart := 0;
    DivB := 0;
    If AtOne Then
      DivB := DivBy - 1;
    Repeat
      Inc(DivB);
      If DivBy > 1 Then
        Begin
          If DivB = DivBy Then
            Begin
              DivB := 0;
              Inc(LStart);
              If LimitStart > 0 Then
                Begin
                  If LStart >= LimitStart Then
                    Begin
                      If LimitCountD >= 0 Then
                        inc(rcount);
                      Result.Insert;
                      FieldCopier.Execute;
                      Result.InsertPost;
                    End;
                End
              Else
                Begin
                  If LimitCountD >= 0 Then
                    inc(rcount);
                  Result.Insert;
                  FieldCopier.Execute;
                  Result.InsertPost;
                End;
            End;
        End
      Else
        Begin
          Inc(LStart);
          If LimitStart > 0 Then
            Begin
              If LStart >= LimitStart Then
                Begin
                  If LimitCountD >= 0 Then
                    inc(rcount);
                  Result.Insert;
                  FieldCopier.Execute;
                  Result.InsertPost;
                End;
            End
          Else
            Begin
              If LimitCountD >= 0 Then
                inc(rcount);
              Result.Insert;
              FieldCopier.Execute;
              Result.InsertPost;
            End;
        End;

      Inc(increc);
    Until Not Prior Or (rcount >= LimitCountD);
  End;
Begin
  increc := 0;
  FieldDefList := ExtractFieldDef;
  Try
    Result := FDatabase.CreateTemporaryTableWithoutIndex(AOwner, FieldDefList);
  Finally
    FieldDefList.Free;
  End;

  FieldCopier := TFSFieldCopier.Create;
  Try
    For i := 0 To pred(NormalFieldList.Count) Do
      FieldCopier.Add(Field(i), Result.Field(i));
    FDatabase.StartTransaction([Nil]);
    Try
      If First Then
        If (((LimitCountT = -1) Or (LimitCountT > 0)) And (LimitStart > 0)) Then
          Begin
            Case TopDirection Of
              //DOWN START====================================================================
              tdDown:
                Begin
                  increc := 0;
                  aDown;
                End;
              //DOWN END======================================================================
              //TOP START=====================================================================
              tdTop:
                Begin
                  First;
                  increc := 0;
                  aTop;
                End;
              //TOP END=======================================================================
              //TOPDOWN START=================================================================
              tdTopDown:
                Begin
                  First;
                  increc := 0;
                  aTop;
                  aDown;
                End;
              //TOPDOWN END===================================================================
              //TOPCENTERDOWN START=================================================================
              tdTopCenterDown:
                Begin
                  First;
                  increc := 0;
                  aTop;
                  aCenter;
                  aDown;
                End;
              //TOPCENTERDOWN END===================================================================
              //TOPCENTERDOWN START=================================================================
              tdCenter:
                Begin
                  First;
                  increc := 0;
                  aCenter;
                End;
              //TOPCENTERDOWN END===================================================================

            End; // case
          End;
    Finally
      FDatabase.Commit;
    End;
  Finally
    FieldCopier.Free;
  End;
End;

Function TFSSqlTableProxy.CopyUnique(AOwner: TObject;
  Const CaseSensitive: Boolean): TFSSqlTableProxy;
Var
  i, j, k, aCount: Integer;
  aField: TFSSqlFieldProxy;
  fn, fn1: String;
  FieldCopier: TFSFieldCopier;
  FieldDefList: TfsSqlFieldDefList;
  IsFirst, DoCopy: Boolean;
  Status: TffResult;
  LastValues: TFSVariantList;
  aList: Tlist;
  aTyp: TfsFieldType;
  L, P: Variant;
  {$IFDEF IsNoVariantInt64}
  EVal: Comp;
  {$ENDIF}
Begin
  ClearDistinct;

  If DistinctFieldName.Count > 0 Then
    aCount := FFMinI(DistinctFieldName.Count, fscl_MaxIndexFlds)
  Else
    aCount := FFMinI(NormalFieldList.Count, fscl_MaxIndexFlds);
  Status := 0;
  If aCount > fscl_MaxIndexFlds Then
    Begin
      Status := fserrSortFail;
      If Status <> DBIERR_NONE Then
        Raise EfsException.CreateEx(fsStrResServer, Status, ['[Max sort field (32)]']);
      Exit;
    End;

  // new table
  FieldDefList := ExtractFieldDef;
  Try
    Result := FDatabase.CreateTemporaryTableWithoutIndex(AOwner, FieldDefList);
  Finally
    FieldDefList.Free;
  End;

  For j := 0 To DistinctFieldName.Count - 1 Do
    For k := 0 To Result.FieldCount - 1 Do
      Begin
        fn := Result.Field(k).Name;
        fn1 := DistinctFieldName[j];
        If AnsiCompareText(fn, fn1) = 0 Then
          Begin
            aField := TFSSqlFieldProxy.Create(Self, k, Self.CursorID);
            DistinctFieldList.Add(aField);
          End;
      End;

  If DistinctFieldList.Count > 0 Then
    aList := DistinctFieldList
  Else
    aList := NormalFieldList;

  Status := 0;
  For i := 0 To pred(aList.Count) Do
    Begin
      Case TFSSqlFieldProxy(aList[i]).GetType Of
        fstBlob..fstBlobFile:
          Begin
            Status := fserrBadDistinctField;
            break;
          End;
      End;
    End;
  If Status <> DBIERR_NONE Then
    Begin
      Result.Owner := Nil;
      Result.free;
      Raise EfsException.CreateNoData(fsStrResServer, Status);
    End;

  // sorting
  Status := SortOnAllFields(aList, CaseSensitive);
  If Status <> DBIERR_NONE Then
    Begin
      Result.Owner := Nil;
      Result.free;
      Raise EfsException.CreateNoData(fsStrResServer, Status);
    End;

  {build a map of compatible fields}
  FieldCopier := TFSFieldCopier.Create;
  Try

    For i := 0 To pred(NormalFieldList.Count) Do
      FieldCopier.Add(Field(i), Result.Field(i));

    FDatabase.StartTransaction([Nil]);
    Try
      IsFirst := True;
      LastValues := TFSVariantList.Create(aList.Count);
      Try
        If First Then
          Repeat
            If IsFirst Then
              Begin
                IsFirst := False;
                DoCopy := True;
              End
            Else
              Begin
                DoCopy := False;
                For i := 0 To pred(aList.Count) Do
                  Begin
                    If DistinctFieldList.Count > 0 Then
                      Begin
                        L := DistinctField(i).GetValue(-1);
                        aTyp := DistinctField(i).GetType;
                      End
                    Else
                      Begin
                        L := Field(i).GetValue(-1);
                        aTyp := Field(i).GetType;
                      End;
                    P := LastValues.GetValue(i);
                    {$IFDEF IsNoVariantInt64}
                    Case TVarData(L).Vtype Of
                      vt_e80:
                        Begin
                          Case TVarData(P).Vtype Of
                            VT_E80: If Decimal(L).ext80 <> Decimal(P).ext80 Then
                                Begin
                                  DoCopy := True;
                                  break;
                                End;
                            vt_decimal: If Decimal(L).ext80 <> Decimal(P).lo64 Then
                                Begin
                                  DoCopy := True;
                                  break;
                                End;
                            Else If Decimal(L).ext80 <> P Then
                              Begin
                                DoCopy := True;
                                break;
                              End;
                          End;
                        End;
                      vt_decimal:
                        Begin
                          Case TVarData(P).Vtype Of
                            VT_E80: If Decimal(L).lo64 <> Decimal(P).ext80 Then
                                Begin
                                  DoCopy := True;
                                  break;
                                End;
                            vt_decimal: If Decimal(L).lo64 <> Decimal(P).lo64 Then
                                Begin
                                  DoCopy := True;
                                  break;
                                End;
                            Else
                              Begin
                                Eval := Decimal(L).lo64;
                                If Eval <> P Then
                                  Begin
                                    DoCopy := True;
                                    break;
                                  End;
                              End;
                          End;
                        End;
                      Else
                        Begin
                          Case TVarData(P).Vtype Of
                            VT_E80: If L <> Decimal(P).ext80 Then
                                Begin
                                  DoCopy := True;
                                  break;
                                End;
                            vt_decimal:
                              Begin
                                Eval := Decimal(P).lo64;
                                If L <> Eval Then
                                  Begin
                                    DoCopy := True;
                                    break;
                                  End;
                              End;
                            Else
                              Begin
                                If Not CaseSensitive Then
                                  If (aTyp In [fstShortString..fstWideString,
                                    fstSingleChar, fstSingleWideChar]) Then
                                    Begin
                                      If Not varisnull(L) Then
                                        L := AnsiLowerCase(L);
                                      If Not varisnull(P) Then
                                        P := AnsiLowerCase(P);
                                    End;
                                If L <> P Then
                                  Begin
                                    DoCopy := True;
                                    break;
                                  End;
                              End;
                          End;
                        End;
                    End;
                    {$ELSE}
                    If Not CaseSensitive Then
                      If (aTyp In [fstShortString..fstWideString,
                        fstSingleChar, fstSingleWideChar]) Then
                        Begin
                          If Not varisnull(L) Then
                            L := AnsiLowerCase(L);
                          If Not varisnull(P) Then
                            P := AnsiLowerCase(P);
                        End;
                    If L <> P Then
                      Begin
                        DoCopy := True;
                        break;
                      End;
                    {$ENDIF}
                  End;
              End;
            If DoCopy Then
              Begin
                Result.Insert;
                FieldCopier.Execute;
                Result.InsertPost;
              End;
            For i := 0 To pred(aList.Count) Do
              Begin
                If DistinctFieldList.Count > 0 Then
                  L := DistinctField(i).GetValue(-1)
                Else
                  L := Field(i).GetValue(-1);
                LastValues.SetValue(i, L);
              End;
          Until Not Next;
      Finally
        LastValues.Free;
      End;
    Finally
      FDatabase.Commit;
    End;
  Finally
    FieldCopier.Free;
    ClearDistinct;
  End;
End;

Function TFSSqlTableProxy.HasDuplicates(Const CaseSensitive: Boolean): Boolean; {!!.13}
Var
  i, aCount: Integer;
  LastValues: TFSVariantList;
  IsFirst, Del: Boolean;
  Status: TffResult;
  L, P: Variant;
  {$IFDEF IsNoVariantInt64}
  EVal: Comp;
  {$ENDIF}
Begin
  aCount := FFMinI(NormalFieldList.Count, fscl_MaxIndexFlds);
  Status := 0;
  If NormalFieldList.Count > fscl_MaxIndexFlds Then
    Begin
      Status := fserrSortFail;
      If Status <> DBIERR_NONE Then
        Raise EfsException.CreateEx(fsStrResServer, Status, ['[Max sort field (32)]']);
      Exit;
    End;
  Status := 0;
  For i := 0 To pred(aCount) Do
    Begin
      Case TFSSqlFieldProxy(NormalFieldList[i]).GetType Of
        fstBlob..fstBlobFile:
          Begin
            Status := fserrBadDistinctField;
            break;
          End;
      End;
    End;
  If Status <> DBIERR_NONE Then
    Raise EfsException.CreateNoData(fsStrResServer, Status);
  Status := SortOnAllFields(NormalFieldList, CaseSensitive);
  If Status <> DBIERR_NONE Then
    Raise EfsException.CreateNoData(fsStrResServer, Status);

  FDatabase.StartTransaction([Nil]);
  LastValues := Nil;
  Try
    IsFirst := True;
    LastValues := TFSVariantList.Create(NormalFieldList.Count);
    If First Then
      Repeat
        If IsFirst Then
          IsFirst := False
        Else
          Begin
            Del := True;
            For i := 0 To pred(NormalFieldList.Count) Do
              Begin
                L := Field(i).GetValue(-1);
                P := LastValues.GetValue(i);
                {$IFDEF IsNoVariantInt64}
                Case TVarData(L).Vtype Of
                  vt_e80:
                    Begin
                      Case TVarData(P).Vtype Of
                        VT_E80: If Decimal(L).ext80 <> Decimal(P).ext80 Then
                            Begin
                              Del := False;
                              break;
                            End;
                        vt_decimal: If Decimal(L).ext80 <> Decimal(P).lo64 Then
                            Begin
                              Del := False;
                              break;
                            End;
                        Else If Decimal(L).ext80 <> P Then
                          Begin
                            Del := False;
                            break;
                          End;
                      End;
                    End;
                  vt_decimal:
                    Begin
                      Case TVarData(P).Vtype Of
                        VT_E80: If Decimal(L).lo64 <> Decimal(P).ext80 Then
                            Begin
                              Del := False;
                              break;
                            End;
                        vt_decimal: If Decimal(L).lo64 <> Decimal(P).lo64 Then
                            Begin
                              Del := False;
                              break;
                            End;
                        Else
                          Begin
                            Eval := Decimal(L).lo64;
                            If Eval <> P Then
                              Begin
                                Del := False;
                                break;
                              End;
                          End;
                      End;
                    End;
                  Else
                    Begin
                      Case TVarData(P).Vtype Of
                        VT_E80: If L <> Decimal(P).ext80 Then
                            Begin
                              Del := False;
                              break;
                            End;
                        vt_decimal:
                          Begin
                            Eval := Decimal(P).lo64;
                            If L <> Eval Then
                              Begin
                                Del := False;
                                break;
                              End;
                          End;
                        Else
                          Begin
                            If Not CaseSensitive Then
                              If (Field(i).GetType In [fstShortString..fstWideString,
                                fstSingleChar, fstSingleWideChar]) Then
                                Begin
                                  If Not varisnull(L) Then
                                    L := AnsiLowerCase(L);
                                  If Not varisnull(P) Then
                                    P := AnsiLowerCase(P);
                                End;
                            If L <> P Then
                              Begin
                                Del := False;
                                break;
                              End;
                          End;
                      End;
                    End;
                End;
                {$ELSE}
                If Not CaseSensitive Then
                  If (Field(i).GetType In [fstShortString..fstWideString,
                    fstSingleChar, fstSingleWideChar]) Then
                    Begin
                      If Not varisnull(L) Then
                        L := AnsiLowerCase(L);
                      If Not varisnull(P) Then
                        P := AnsiLowerCase(P);
                    End;
                If L <> P Then
                  Begin
                    Del := False;
                    break;
                  End;
                {$ENDIF}
              End;
            If Del Then
              Begin
                Result := True;
                Exit;
              End;
          End;
        For i := 0 To pred(NormalFieldList.Count) Do
          Begin
            L := Field(i).GetValue(-1);
            LastValues.SetValue(i, L);
          End;
      Until Not Next;
  Finally
    FDatabase.Commit;
    LastValues.Free;
  End;
  Result := False;
End;

Function TFSSqlTableProxy.CopyValidated(AOwner: TObject; Validator: TFSCopyValidator): TFSSqlTableProxy;
Var
  i: Integer;
  FieldCopier: TFSFieldCopier;
  FieldDefList: TfsSqlFieldDefList;
Begin
  FieldDefList := ExtractFieldDef;
  Try
    Result := FDatabase.CreateTemporaryTableWithoutIndex(AOwner, FieldDefList);
  Finally
    FieldDefList.Free;
  End;

  {build a map of compatible fields}
  FieldCopier := TFSFieldCopier.Create;
  Try

    For i := 0 To pred(NormalFieldList.Count) Do
      FieldCopier.Add(Field(i), Result.Field(i));

    FDatabase.StartTransaction([Nil]);
    Try
      If First Then
        Repeat
          If Validator Then
            Begin
              Result.Insert;
              FieldCopier.Execute;
              Result.InsertPost;
            End;
        Until Not Next;
    Finally
      FDatabase.Commit;
    End;
  Finally
    FieldCopier.Free;
  End;
End;

{Begin !!.13}

Function TFSSqlTableProxy.Sort(Const SortListCount: Integer;
  Const SortList: TfsSqlSortArray;
  Const CaseSensitive: Boolean): TffResult;
{End !!.13}
Var
  aOrderByArray: TfsOrderByArray;
  FldList: TffFieldList;
  FieldsAscDesc: TffFieldList;
  FieldsCase, FieldsSize, FieldsFlags, FieldsNullTop: TffFieldList;
  IHList: TffFieldIHList;
  i: Integer;
Begin

  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Assert(SortListCount <= fscl_MaxIndexFlds);
  { A data dictionary contains a sequential access index by default. In order
    to sort the data, we must replace index 0 with the index describing how
    the data is to be sorted. We must leave this index on the cursor. }
  If TfsSrBaseCursor(FCursorID).Dictionary.IndexCount > 0 Then
    TfsSrBaseCursor(FCursorID).Dictionary.RemoveIndex(0);

  { Set up the index for sorting. }
  For i := 0 To pred(SortListCount) Do
    Begin
      Assert(Abs(SortList[i]) > 0);
      FldList[i] := abs(SortList[i]) - 1;
      With TfsSrBaseCursor(FCursorID).Dictionary Do
        If FieldType[FldList[i]] In
          [fstBLOB..fstBlobFile] Then
          FSRaiseException(EfsServerException, fsStrResGeneral,
            fserrBadDistinctField, [FieldName[FldList[i]]]);
      IHList[i] := '';
      If SortList[i] < 0 Then
        aOrderByArray[i] := fsobDescending
      Else
        aOrderByArray[i] := fsobAscending;
      If aOrderByArray[i] = fsobAscending Then
        FieldsAscDesc[i] := 1
      Else
        FieldsAscDesc[i] := 0;
      FieldsCase[i] := Byte(Not CaseSensitive);
      FieldsSize[i] := 0;
      FieldsFlags[i] := 0;
      FieldsNullTop[i] := 1;
    End;
  TfsSrBaseCursor(FCursorID).Dictionary.AddIndex
    ('Sort', '', 0, SortListCount, FldList, FieldsAscDesc, FieldsCase, FieldsSize,
    FieldsFlags, FieldsNullTop, IHList, True);

  TfsSrBaseCursor(FCursorID).Dictionary.BindIndexHelpers;

  Result := TfsSrBaseCursor(FCursorID).SortRecords(FldList, aOrderByArray, SortListCount);
End;

Function TFSSqlTableProxy.OrderSort(Const SortListCount: Integer;
  Const SortList: TfsSqlSortArray;
  Const SortCase: TfsSqlSortArray;
  Const SortSize: TfsSqlSortArray;
  Const SortNull: TfsSqlSortArray): TffResult;
Var
  aOrderByArray: TfsOrderByArray;
  FldList: TffFieldList;
  FieldsAscDesc: TffFieldList;
  FieldsCase, FieldsSize, FieldsFlags, FieldsNullTop: TffFieldList;
  IHList: TffFieldIHList;
  i: Integer;
Begin

  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Assert(SortListCount <= fscl_MaxIndexFlds);
  { A data dictionary contains a sequential access index by default. In order
    to sort the data, we must replace index 0 with the index describing how
    the data is to be sorted. We must leave this index on the cursor. }
  If TfsSrBaseCursor(FCursorID).Dictionary.IndexCount > 0 Then
    TfsSrBaseCursor(FCursorID).Dictionary.RemoveIndex(0);

  { Set up the index for sorting. }
  For i := 0 To pred(SortListCount) Do
    Begin
      Assert(Abs(SortList[i]) > 0);
      FldList[i] := abs(SortList[i]) - 1;
      With TfsSrBaseCursor(FCursorID).Dictionary Do
        If FieldType[FldList[i]] In
          [fstBLOB..fstBlobFile] Then
          FSRaiseException(EfsServerException, fsStrResGeneral,
            fserrBadDistinctField, [FieldName[FldList[i]]]);
      IHList[i] := '';
      If SortList[i] < 0 Then
        aOrderByArray[i] := fsobDescending
      Else
        aOrderByArray[i] := fsobAscending;
      If aOrderByArray[i] = fsobAscending Then
        FieldsAscDesc[i] := 1
      Else
        FieldsAscDesc[i] := 0;
      FieldsCase[i] := SortCase[i];
      FieldsSize[i] := SortSize[i];
      FieldsFlags[i] := 0;
      FieldsNullTop[i] := SortNull[i];
    End;
  TfsSrBaseCursor(FCursorID).Dictionary.AddIndex
    ('Sort', '', 0, SortListCount, FldList, FieldsAscDesc, FieldsCase, FieldsSize,
    FieldsFlags, FieldsNullTop, IHList, True);

  TfsSrBaseCursor(FCursorID).Dictionary.BindIndexHelpers;

  Result := TfsSrBaseCursor(FCursorID).SortRecords(FldList, aOrderByArray, SortListCount);
End;

Function TFSSqlTableProxy.GetCurrentRecordID: tffint64;
Begin
  If NoRecord Then
    ffInitI64(Result)
  Else
    Begin
      Assert(FCursorID <> 0);
      Assert(TObject(FCursorID) Is TfsSrBaseCursor);
      Result := TfsSrBaseCursor(FCursorID).RefNr;
    End;
End;

Function TFSSqlTableProxy.GetRecordByID(ID: TffInt64;
  Const LockType: TfsSrcLockType): TffResult;
Var
  aRefNr: TffInt64;
Begin
  Result := TfsSrBaseCursor(FCursorID).SetToKey(skaEqual, True, 1, 0, @ID);
  If Result = DBIERR_NONE Then
    Result := TfsSrBaseCursor(FCursorID).GetNextRecord(RecordBuffer, LockType, RecFlag, aRefNr);
End;

Procedure TFSSqlTableProxy.Close;
Begin
  Assert(Self <> Nil);
  Assert(TObject(Self) Is TFSSqlTableProxy);
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor,
    Format('%d is not a cursor', [FCursorID]));
  {Begin !!.13}
  With TfsSrBaseCursor(FCursorID) Do
    If CanClose(True) Then
      Free
    Else
      RequestClose;
  {End !!.13}
End;

Procedure TFSSqlTableProxy.ClearDistinct;
Begin
  Assert(Self <> Nil);
  Assert(TObject(Self) Is TFSSqlTableProxy);
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor,
    Format('%d is not a cursor', [FCursorID]));

  While DistinctFieldList.Count > 0 Do
    Begin
      TFSSqlFieldProxy(DistinctFieldList[0]).Free;
      DistinctFieldList.Delete(0);
    End;
End;

Constructor TFSSqlTableProxy.Create(AOwner: TObject;
  ADataBase: TFSSqlDatabaseProxy; ACursorID: TFFCursorID; Const AName, AAlias: String);
Var
  i: Integer;
  Field: TFSSqlFieldProxy;
Begin
  Inherited Create;
  RecFlag := 0;
  Assert(AOwner <> Nil);
  FSelect := Nil;
  FGroup := Nil;
  fOnlineCursor := False;
  IsProcedure := False;
  fIndexID := 0;
  FOwner := AOwner;
  FIndex := -1;
  FDatabase := ADatabase;
  FName := AName;
  FAlias := AAlias; {!!.11}
  FCursorID := ACursorID;
  NormalFieldList := TList.Create;
  DistinctFieldList := TList.Create;
  DistinctFieldName := TStringList.Create;
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);

  For i := 0 To pred(TfsSrBaseCursor(FCursorID).Dictionary.FieldCount) Do
    Begin
      Field := TFSSqlFieldProxy.Create(Self, i, FCursorID);
      NormalFieldList.Add(Field);
    End;
  FRecordLen := TfsSrBaseCursor(FCursorID).Dictionary.RecordLength;
  FFGetMem(RecordBuffer, FRecordLen);
  FFGetMem(KeyBuffer1, FRecordLen);
  FFGetMem(KeyBuffer2, FRecordLen);
  TfsSrBaseCursor(FCursorID).SqlTableProxy := Self;
End;

Destructor TFSSqlTableProxy.Destroy;
Begin
  Assert(Self <> Nil);
  Assert(TObject(Self) Is TFSSqlTableProxy);
  Assert(FOwner = Nil);
  While NormalFieldList.Count > 0 Do
    Begin
      TFSSqlFieldProxy(NormalFieldList[0]).Free;
      NormalFieldList.Delete(0);
    End;
  While DistinctFieldList.Count > 0 Do
    Begin
      TFSSqlFieldProxy(DistinctFieldList[0]).Free;
      DistinctFieldList.Delete(0);
    End;
  NormalFieldList.Free;
  DistinctFieldList.Free;
  DistinctFieldName.free;
  FFFreeMem(RecordBuffer, FRecordLen);
  FFFreeMem(KeyBuffer1, FRecordLen);
  FFFreeMem(KeyBuffer2, FRecordLen);
  If Not LeaveCursorOpen Then
    Try
      Close;
    Except
      On E: Exception Do
        FEngine.LogFmt('Exception when closing TFSSqlTableProxy: %s',
          [E.message]);
    End;
  Inherited;
End;
{Begin !!.11}
{--------}

Function TFSSqlTableProxy.EnsureWritable: TffResult;
Var
  Table: TfsSrcBaseTable;
Begin
  { There cannot be any type of lock on the table (unless its ours and
    is a write lock). }
  Result := DBIERR_NONE;
  Table := TfsSrBaseCursor(FCursorID).Table;
  If Table.ClientLocks.Count > 0 Then
    If Table.ClientLocks.SummaryMode = ffsltExclusive Then
      Begin
        If Not Table.HasClientLock(CursorID) Then
          Begin
            Result := DBIERR_FILELOCKED;
            Exit;
          End;
      End
    Else
      Begin
        Result := DBIERR_FILELOCKED;
        Exit;
      End;
End;
{End !!.11}
{--------}

Function TFSSqlTableProxy.EOF: Boolean;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).Position = cpEOF;
End;
{--------}

Function TFSSqlTableProxy.Field(Index: Integer): TFSSqlFieldProxy;
Begin
  Result := TFSSqlFieldProxy(NormalFieldList[Index]);
End;
{--------}

Function TFSSqlTableProxy.FieldByName(
  Const Name: String): TFSSqlFieldProxy;
Var
  i: Integer;
Begin
  For i := 0 To pred(NormalFieldList.Count) Do
    If AnsiCompareText(TFSSqlFieldProxy(NormalFieldList[i]).Name, Name) = 0 Then
      Begin
        Result := TFSSqlFieldProxy(NormalFieldList[i]);
        Exit;
      End;
  Result := Nil;
End;

Function TFSSqlTableProxy.FieldCount: Integer;
Begin
  Result := NormalFieldList.Count;
End;

Function TFSSqlTableProxy.DistinctField(Index: Integer): TFSSqlFieldProxy;
Begin
  Result := TFSSqlFieldProxy(DistinctFieldList[Index]);
End;
{--------}

Function TFSSqlTableProxy.DistinctFieldByName(
  Const Name: String): TFSSqlFieldProxy;
Var
  i: Integer;
Begin
  For i := 0 To pred(DistinctFieldList.Count) Do
    If AnsiCompareText(TFSSqlFieldProxy(DistinctFieldList[i]).Name, Name) = 0 Then
      Begin
        Result := TFSSqlFieldProxy(DistinctFieldList[i]);
        Exit;
      End;
  Result := Nil;
End;

Function TFSSqlTableProxy.DistinctFieldCount: Integer;
Begin
  Result := DistinctFieldList.Count;
End;

Function TFSSqlTableProxy.First: Boolean;
Var
  aRefNr: TffInt64;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  TfsSrBaseCursor(FCursorID).SetToBegin;
  Result := TfsSrBaseCursor(FCursorID).GetNextRecord(RecordBuffer, ffsltNone, RecFlag, aRefNr) = DBIERR_NONE;
  NoRecord := False;
End;

Function TFSSqlTableProxy.Last: Boolean;
Var
  aRefNr: TffInt64;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  TfsSrBaseCursor(FCursorID).SetToEnd;
  Result := TfsSrBaseCursor(FCursorID).GetPriorRecord(RecordBuffer, ffsltNone, RecFlag, aRefNr) = DBIERR_NONE;
  NoRecord := False;
End;

Function TFSSqlTableProxy.GetSegments: Integer;
Begin
  Result := TfsSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[FIndex + 1].idCount;
End;

Procedure TFSSqlTableProxy.Insert;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  TfsSrBaseCursor(FCursorID).Dictionary.InitRecord(RecordBuffer);
End;

Procedure TFSSqlTableProxy.SetDefaults;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  TfsSrBaseCursor(FCursorID).Dictionary.FUserName := TfsSrBaseCursor(FCursorID).Client.ClientName;
  TfsSrBaseCursor(FCursorID).Dictionary.SetDefaultFieldUpdateValues(RecordBuffer, Nil);
End;

Procedure TFSSqlTableProxy.SetDefault(aField: String);
Var
  i: Integer;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  TfsSrBaseCursor(FCursorID).Dictionary.FUserName := TfsSrBaseCursor(FCursorID).Client.ClientName;
  i := TfsSrBaseCursor(FCursorID).Dictionary.FieldIndex(aField);
  If i >= 0 Then
    TfsSrBaseCursor(FCursorID).Dictionary.SetDefaultFieldUpdateValue(RecordBuffer, i);
End;

Procedure TFSSqlTableProxy.SetNull(aField: String);
Var
  i: Integer;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  TfsSrBaseCursor(FCursorID).Dictionary.FUserName := TfsSrBaseCursor(FCursorID).Client.ClientName;
  i := TfsSrBaseCursor(FCursorID).Dictionary.FieldIndex(aField);
  If i >= 0 Then
    TfsSrBaseCursor(FCursorID).Dictionary.SetRecordFieldNull(i, RecordBuffer, True);
End;

Function TFSSqlTableProxy.InsertPost: TffResult; {!!.11}
Var
  aRefNr: TffInt64;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).InsertRecord(RecordBuffer, {!!.11}
    ffsltExclusive, 0, aRefNr);
  NoRecord := False;
End;

Function TFSSqlTableProxy.InsertPostNoDefaults: TffResult;
Var
  aRefNr: TffInt64;
  Cur: TfsSrBaseCursor;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).InsertRecordNoDefault
    (RecordBuffer, ffsltExclusive, 0, aRefNr);
  NoRecord := False;
  Cur := TfsSrBaseCursor(FCursorID);
  Self.FDataBase.LastAutoInc := Cur.ReadLastAutoInc;
End;

Function TFSSqlTableProxy.NextAutoInc: Int64; // next value
Var
  Cur: TfsSrBaseCursor;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Cur := TfsSrBaseCursor(FCursorID);
  Result := Cur.NextAutoInc;
End;

Function TFSSqlTableProxy.ReadLastAutoInc: Int64; // Last value
Var
  Cur: TfsSrBaseCursor;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Cur := TfsSrBaseCursor(FCursorID);
  Result := Cur.ReadLastAutoInc;
End;

Function TFSSqlTableProxy.IsRecordLocked: boolean;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).IsRecordLocked(ffsltExclusive);
End;

Function TFSSqlTableProxy.IsUndeletedRecord: boolean;
Begin
  Result := (getflags(RecFlag, frUndeletedRecord));
End;

Function TFSSqlTableProxy.IsProtectDeleteRecord: boolean;
Begin
  Result := (getflags(RecFlag, frProtectDeleteRecord));
End;

Function TFSSqlTableProxy.IsProtectUpdateRecord: boolean;
Begin
  Result := (getflags(RecFlag, frProtectUpdateRecord));
End;

Function TFSSqlTableProxy.IsMarkAsBadRecord: boolean;
Begin
  Result := GetFlags(RecFlag, frMarkAsBadRecord);
End;

Function TFSSqlTableProxy.Prior: Boolean;
Var
  DbResult: TffResult;
  aRefNr: TffInt64;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  DbResult := TfsSrBaseCursor(FCursorID).GetPriorRecord(RecordBuffer, ffsltNone, RecFlag, aRefNr);
  Result := DbResult = DBIERR_NONE;
  NoRecord := False;
End;

Function TFSSqlTableProxy.Next: Boolean;
Var
  DbResult: TffResult;
  aRefNr: TffInt64;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  DbResult := TfsSrBaseCursor(FCursorID).GetNextRecord(RecordBuffer, ffsltNone, RecFlag, aRefNr);
  Result := DbResult = DBIERR_NONE;
  NoRecord := False;
End;

Function TFSSqlTableProxy.NextPrior(TD: TTopDirection): Boolean;
Var
  DbResult: TffResult;
  aRefNr: TffInt64;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Case TD Of
    tdDown:
      Begin
        DbResult := TfsSrBaseCursor(FCursorID).GetPriorRecord(RecordBuffer, ffsltNone, RecFlag, aRefNr);
        Result := DbResult = DBIERR_NONE;
      End;
    tdTop:
      Begin
        DbResult := TfsSrBaseCursor(FCursorID).GetNextRecord(RecordBuffer, ffsltNone, RecFlag, aRefNr);
        Result := DbResult = DBIERR_NONE;
      End;
    Else
      Result := False;
  End;
  NoRecord := False;
End;

Procedure TFSSqlTableProxy.SetIndex(KeyNum: Integer);
Begin
  If KeyNum <> FIndex Then
    Begin
      FIndex := KeyNum;
      Assert(FCursorID <> 0);
      Assert(TObject(FCursorID) Is TfsSrBaseCursor);
      TfsSrBaseCursor(FCursorID).SwitchToIndex(KeyNum + 1, False);
    End;
End;

Procedure TFSSqlTableProxy.SetRange(Const StartValues, EndValues: Array Of Variant;
  Const LowCount, HighCount: Integer;
  Const IncludeLowLimit, IncludeHighLimit,
  IndexAsc: Boolean; aPartialLen: Integer = 0);
Var
  LowSegs, HighSegs, i, tl, th: Integer;
  K1, K2: PffByteArray;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);

  LowSegs := FFMinI(GetSegments, LowCount);
  tl := LowSegs;
  HighSegs := FFMinI(GetSegments, HighCount);
  th := HighSegs;

  For i := 0 To pred(tl) Do
    Field(TfsSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[FIndex + 1].
      idFields[i]).SetValue(StartValues[i], -1);

  TfsSrBaseCursor(FCursorID).Table.BuildKeyForRecord(FIndex + 1,
    RecordBuffer, KeyBuffer1, LowSegs, aPartialLen);
  For i := 0 To pred(th) Do
    Field(TfsSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[FIndex + 1].
      idFields[i]).SetValue(EndValues[i], -1);
  TfsSrBaseCursor(FCursorID).Table.BuildKeyForRecord(FIndex + 1,
    RecordBuffer, KeyBuffer2, HighSegs, aPartialLen);
  If LowSegs > 0 Then
    K1 := KeyBuffer1
  Else
    K1 := Nil;
  If HighSegs > 0 Then
    K2 := KeyBuffer2
  Else
    K2 := Nil;
  If IndexAsc Then
    TfsSrBaseCursor(FCursorID).SetRange(True, LowSegs, aPartialLen, K1, IncludeLowLimit,
      HighSegs, aPartialLen, K2, IncludeHighLimit)
  Else
    TfsSrBaseCursor(FCursorID).SetRange(True, HighSegs, aPartialLen, K2, IncludeHighLimit,
      LowSegs, aPartialLen, K1, IncludeLowLimit);
End;

Procedure TFSSqlTableProxy.Iterate(Iterator: TFSSqlTableIterator; Cookie: TffWord32);
Begin
  If First Then
    Repeat
      If Not Iterator(Cookie) Then
        break;
    Until Not Next;
End;

Function TFSSqlTableProxy.FindIndex(Const IndexName: String): Longint;
Var
  i: Integer;
Begin
  Result := -1;
  For i := 0 To pred(TfsSrBaseCursor(FCursorID).Dictionary.IndexCount) Do
    Begin
      If AnsiUpperCase(TfsSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[i].idName) =
        AnsiUpperCase(IndexName) Then
        Begin
          Result := i;
          System.Break;
        End;
    End;
End;

Function TFSSqlTableProxy.IndexesOnField(F: TFSSqlFieldProxy; MustBeCaseInsensitive: Boolean; {!!.10}
  Var IndexRefs: Array Of Integer): Integer;
Var
  i: Integer;
Begin
  Result := 0;
  For i := 0 To pred(TfsSrBaseCursor(FCursorID).Dictionary.IndexCount) Do
    Begin
      If TfsSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[i].idCount > 0 Then
        If (TfsSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[i].idFields[0] = F.Index) And
          // prevent for use var size
        (TfsSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[i].idFieldsSize[0] <= 0) Then
          Begin
            If Not MustBeCaseInsensitive
              Or (Boolean(TfsSrBaseCursor(FCursorID).Dictionary.
              IndexDescriptor[i].idFieldsCase[0])) Then
              Begin
                IndexRefs[Result] := i;
                inc(Result);
              End;
          End;
    End;
End;

Procedure TFSSqlTableProxy.GetIndexProperties(Const Index: Integer;
  Var Unique, IgnoreCase, IndexAsc: Boolean;
  Var IndexFieldCount: Integer;
  Var IndexFields: Array Of Integer);
Var
  i: Integer;
  IdxDescrip: PffIndexDescriptor;
Begin
  IdxDescrip := TfsSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[Index];

  Unique := Not IdxDescrip.idDups;
  If IdxDescrip.idCount <> -1 Then
    Begin
      IgnoreCase := boolean(IdxDescrip.idFieldsCase[0]);
      IndexAsc := Boolean(IdxDescrip.idFieldsAscDesc[0]);
    End
  Else
    Begin
      IgnoreCase := True;
      IndexAsc := True;
    End;
  IndexFieldCount := IdxDescrip.idCount;

  For i := 0 To pred(IndexFieldCount) Do
    IndexFields[i] := IdxDescrip.idFields[i];
End;

Function TFSSqlTableProxy.Delete: TffResult; {!!.11}
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).DeleteRecord(Nil); {!!.11}
End;

Function TFSSqlTableProxy.CurrDelete: TffResult;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).DeleteRecord(RecordBuffer);
End;

Function TFSSqlTableProxy.GetRecordCount: Integer;
Var
  rc: Longword;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  TfsSrBaseCursor(FCursorID).GetRecordCount(rc);
  Result := rc;
End;

Function TFSSqlTableProxy.Update: TffResult; {!!.11}
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).ModifyRecord(RecordBuffer, True, tluDatabase, 0, False, False); {!!.11}
End;

Procedure TFSSqlTableProxy.NullRecord;
Var
  i: Integer;
Begin
  For i := 0 To FieldCount - 1 Do
    Field(i).SetFieldToNull;
  NoRecord := True;
End;

{ TFSSqlFieldProxy }

Constructor TFSSqlFieldProxy.Create(AnOwnerTable: TFSSqlTableProxy; AnIndex: Integer;
  ACursorID: TFFCursorID);
Begin
  Inherited Create;
  FOwnerTable := AnOwnerTable;
  FCursorID := ACursorID;
  FIndex := AnIndex;
  FIsTarget := False;
  FSrcIndex := -1;
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  FieldBufferLength := TfsSrBaseCursor(FCursorID).Dictionary.FieldLength[FIndex];
  FFGetMem(FieldBuffer, FieldBufferLength);
End;

Destructor TFSSqlFieldProxy.Destroy;
Begin
  FFFreeMem(FieldBuffer, FieldBufferLength);
  Inherited;
End;

Function TFSSqlFieldProxy.GetDecimals: Integer;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).Dictionary.FieldDecPl[FIndex];
End;

Function TFSSqlFieldProxy.GetPrecision: Integer;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).Dictionary.FieldUnits[FIndex];
End;

Function TFSSqlFieldProxy.GetBlobLevel: TDataCompLevel;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).Dictionary.fieldbloblevelcomp[FIndex];
End;

Function TFSSqlFieldProxy.GetRound: TRound;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).Dictionary.fieldRound[FIndex];
End;

Function TFSSqlFieldProxy.GetSize: Integer;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).Dictionary.FieldUnits[FIndex];
End;

Function TFSSqlFieldProxy.GetType: TfsFieldType;
Begin
  If Not TypeKnown Then
    Begin
      Assert(FCursorID <> 0);
      Assert(TObject(FCursorID) Is TfsSrBaseCursor);
      FType := TfsSrBaseCursor(FCursorID).Dictionary.FieldType[FIndex];
      If FType = fstAutoInc32 Then
        FType := fstint32
      Else If FType = fstAutoInc64 Then
        FType := fstint64;
      TypeKnown := True;
    End;
  Result := FType;
End;

Procedure TFSSqlFieldProxy.ReadField(Var IsNull: Boolean);
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  {$IFOPT C+}
  Assert(TfsSrBaseCursor(FCursorID).GetRecordField(FIndex,
    FOwnerTable.RecordBuffer, IsNull, FieldBuffer) = DBIERR_NONE);
  {$ELSE}
  TfsSrBaseCursor(FCursorID).GetRecordField(FIndex, FOwnerTable.RecordBuffer,
    IsNull, FieldBuffer);
  {$ENDIF}
End;

{!!.11 new}

Function TFSSqlFieldProxy.GetBlobValue: Variant;
{Rewritten !!.13}
Var
  Offset: Integer;
  BLOBNr: TffInt64;
  Error, Len: Integer;
  BytesRead: TffWord32;
  VPtr: PByte;
  bl: TDataCompLevel;
  TmpStream, ResultStream: TMemoryStream;
  Buffer: pointer;
Begin
  Result := null;
  Offset := TfsSrBaseCursor(FCursorID).Dictionary.FieldOffset[Index];
  BLOBNr := PffInt64(@OwnerTable.RecordBuffer^[Offset])^;
  Len := TfsSrBaseCursor(FCursorID).BLOBGetLength(BLOBNr, Error);
  If Error = DBIERR_NONE Then
    Begin
      If Len = 0 Then
        Result := null
      Else
        Begin
          bl := GetBlobLevel;
          If bl <> blNone Then
            Begin
              TmpStream := TMemoryStream.Create;
              ResultStream := TMemoryStream.Create;
              Try
                TmpStream.position := 0;
                GetMem(buffer, len + 5);
                TfsSrBaseCursor(FCursorID).BLOBRead(BLOBNr, 0, Len, buffer^, BytesRead);
                If BytesRead > 0 Then
                  TmpStream.writebuffer(buffer^, BytesRead);

                If TmpStream.Size > 0 Then
                  Begin
                    TmpStream.position := 0;
                    ResultStream.Position := 0;
                    fsZDecompress(TmpStream, ResultStream);
                    ResultStream.Position := 0;
                    TmpStream.Size := 0;
                    If ResultStream.size > 0 Then
                      Begin
                        Try
                          Result := VarArrayCreate([1, ResultStream.size], VarByte);
                          VPtr := VarArrayLock(Result);
                          move(ResultStream.Memory^, Vptr^, ResultStream.size);
                        Finally
                          VarArrayUnlock(Result);
                        End;
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
              Result := VarArrayCreate([1, Len], VarByte);
              VPtr := VarArrayLock(Result);

              Try
                TfsSrBaseCursor(FCursorID).BLOBRead(BLOBNr, 0, Len, VPtr^, BytesRead);
              Finally
                VarArrayUnlock(Result);
              End;
            End
        End;
    End;
End;

{!!.11 new}

Procedure TFSSqlFieldProxy.SetBlobValue(Const Value: Variant);
{Rewritten !!.13}
Var
  Offset: Integer;
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
          If Stream.Size > 0 Then
            fsZCompress(zcFastest, Stream, aOutput);
          aOutput.Position := 0;
        End;
      blDefault:
        Begin
          aOutput.Position := 0;
          Stream.Position := 0;
          If Stream.Size > 0 Then
            fsZCompress(zcDefault, Stream, aOutput);
          aOutput.Position := 0;
        End;
      blMax:
        Begin
          aOutput.Position := 0;
          Stream.Position := 0;
          If Stream.Size > 0 Then
            fsZCompress(zcMax, Stream, aOutput);
          aOutput.Position := 0;
        End;
    End;
  End;

Begin
  ValueLocked := False;
  Try
    { Obtain the length of the BLOB data & a pointer to the data. }
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

    Offset := TfsSrBaseCursor(FCursorID).Dictionary.FieldOffset[Index];
    BLOBNr := PffInt64(@OwnerTable.RecordBuffer^[Offset])^;
    // kompresja - mo¿e w kliencie w query
    bl := getbloblevel;
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
              Len := TfsSrBaseCursor(FCursorID).BLOBGetLength(BLOBNr, Error);
              If TffWord32(Len) > ValueLen Then
                TfsSrBaseCursor(FCursorID).BLOBTruncate(BLOBNr, ValueLen);
              { If the new value is null then null the field in the record otherwise
                writ the new value over the old value. }
              If ValueLen = 0 Then
                SetFieldToNull
              Else
                { Write the new value over the old value. }
                Begin
                  TfsSrBaseCursor(FCursorID).BLOBWrite(BLOBNr, 0, ValueLen, ResultStream.memory^);
                End;
            End
          Else
            Begin
              { This is a new BLOB. If it is null then set the field in the record to
                null. }
              If ValueLen = 0 Then
                SetFieldToNull
              Else If TfsSrBaseCursor(FCursorID).BLOBAdd(BLOBNr) = DBIERR_NONE Then
                Begin
                  { The BLOB has content & its creation was successful. Write the content
                    to the table. }
                  If TfsSrBaseCursor(FCursorID).BLOBWrite(BLOBNr, 0, ValueLen, ResultStream.memory^) = DBIERR_NONE Then
                    WriteFieldDirect(@BLOBNr);
                  TfsSrBaseCursor(FCursorID).BLOBFree(BLOBNr);
                End;
            End; { if..else }
        Finally
          ResultStream.free;
          tmpStream.free;
          FreeMem(buffer);
        End;
      End
    Else
      Begin
        { If there is already BLOB data, truncate it to the length of the
          new value. }
        If (BLOBNr.iLow <> 0) Or (BLOBNr.iHigh <> 0) Then
          Begin
            Len := TfsSrBaseCursor(FCursorID).BLOBGetLength(BLOBNr, Error);
            If TffWord32(Len) > ValueLen Then
              TfsSrBaseCursor(FCursorID).BLOBTruncate(BLOBNr, ValueLen);
            { If the new value is null then null the field in the record otherwise
              writ the new value over the old value. }
            If ValueLen = 0 Then
              SetFieldToNull
            Else
              { Write the new value over the old value. }
              Begin
                TfsSrBaseCursor(FCursorID).BLOBWrite(BLOBNr, 0, ValueLen, VPtr^);
              End;
          End
        Else
          Begin
            { This is a new BLOB. If it is null then set the field in the record to
              null. }
            If ValueLen = 0 Then
              SetFieldToNull
            Else If TfsSrBaseCursor(FCursorID).BLOBAdd(BLOBNr) = DBIERR_NONE Then
              Begin
                { The BLOB has content & its creation was successful. Write the content
                  to the table. }
                If TfsSrBaseCursor(FCursorID).BLOBWrite(BLOBNr, 0, ValueLen, VPtr^) = DBIERR_NONE Then
                  WriteFieldDirect(@BLOBNr);
                TfsSrBaseCursor(FCursorID).BLOBFree(BLOBNr);
              End;
          End; { if..else }
      End;
  Finally
    If ValueLocked Then
      VarArrayUnlock(Value);
  End;
End;

Function TFSSqlFieldProxy.GetValue(aArrayIndex: Integer): Variant;
Var
  IsNull: Boolean;
  C: Currency;
  E: Extended;
  W: WideString;
  WC: WideChar;
  DT: TDateTime;
  kk: Int64;
  r: TRound;
  decl: Byte;

  Function GetAsVariant: Variant;
  Var
    V: OleVariant;

    Procedure BufferToByteArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);

      Procedure ByteArrayToVariant(ByteArray: Pointer; ArrayLength: Integer);
      Var
        idx: Integer;
        BArr: PffByteArray Absolute ByteArray;
      Begin
        For idx := 0 To ArrayLength - 1 Do
          Result[idx] := BArr[idx];
      End;
    Begin
      VarArray := VarArrayCreate([0, DataSize - 1], varInteger);
      Result := VarArray;
      VarArrayLock(VarArray);
      Try
        ByteArrayToVariant(Data, DataSize);
      Finally
        VarArrayUnlock(VarArray);
      End;
    End;

    Procedure BufferToWordArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);

      Procedure WordArrayToVariant(WordArray: Pointer; ArrayLength: Integer);
      Var
        idx: Integer;
        BArr: PffWordArray Absolute WordArray;
      Begin
        For idx := 0 To ArrayLength - 1 Do
          Result[idx] := BArr[idx];
      End;
    Begin
      VarArray := VarArrayCreate([0, DataSize - 1], varInteger);
      Result := VarArray;
      VarArrayLock(VarArray);
      Try
        WordArrayToVariant(Data, DataSize);
      Finally
        VarArrayUnlock(VarArray);
      End;
    End;

    Procedure BufferToIntArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);
    Var
      PVarData: Pointer;
    Begin
      VarArray := VarArrayCreate([0, DataSize - 1], varInteger);
      PVarData := VarArrayLock(VarArray);
      Try
        Move(Data^, PVarData^, DataSize);
      Finally
        VarArrayUnlock(VarArray);
      End;
    End;

    Procedure BufferToDoubleArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);

      Procedure DoubleArrayToVariant(DoubleArray: Pointer; ArrayLength: Integer);
      Var
        idx: Integer;
        BArr: PffDoubleArray Absolute DoubleArray;
      Begin
        For idx := 0 To ArrayLength - 1 Do
          Result[idx] := BArr[idx];
      End;
    Begin
      VarArray := VarArrayCreate([0, DataSize - 1], varDouble);
      Result := VarArray;
      VarArrayLock(VarArray);
      Try
        DoubleArrayToVariant(Data, DataSize);
      Finally
        VarArrayUnlock(VarArray);
      End;
    End;

  Begin
    If GetType = fstArrayUInt8 Then
      Begin
        BufferToByteArray(@FieldBuffer, Self.GetSize, V);
        Result := V;
      End
    Else If GetType = fstArrayUInt16 Then
      Begin
        BufferToWordArray(@FieldBuffer, Self.GetSize, V);
      End
    Else If GetType = fstArrayInt32 Then
      Begin
        BufferToIntArray(@FieldBuffer, Self.GetSize, V);
        Result := V;
      End
    Else If GetType = fstArrayDouble Then
      Begin
        BufferToDoubleArray(@FieldBuffer, Self.GetSize, V);
      End
    Else
      Result := Null;
  End;

  Function ArrayAsString: String;

    Function ByteArrayToString(ByteArray: Pointer; ArrayLength: Integer): String;
    Var
      idx: Integer;
      BArr: PffByteArray Absolute ByteArray;
    Begin
      Result := '';
      Result := IntToStr(BArr[0]);
      For idx := 1 To ArrayLength - 1 Do
        Result := Result + ',' + IntToStr(BArr[idx]);
    End;

    Function WordArrayToString(WordArray: Pointer; ArrayLength: Integer): String;
    Var
      idx: Integer;
      BArr: PffWordArray Absolute WordArray;
    Begin
      Result := '';
      Result := IntToStr(BArr[0]);
      For idx := 1 To ArrayLength - 1 Do
        Result := Result + ',' + IntToStr(BArr[idx]);
    End;

    Function IntegerArrayToString(IntArray: Pointer; ArrayLength: Integer): String;
    Var
      idx: Integer;
      BArr: PffIntArray Absolute intArray;
    Begin
      Result := '';
      Result := IntToStr(BArr[0]);
      For idx := 1 To ArrayLength - 1 Do
        Result := Result + ',' + IntToStr(BArr[idx]);
    End;

    Function DoubleArrayToString(DoubleArray: Pointer; ArrayLength: Integer; Decimal: Byte): String;
    Var
      idx: Integer;
      BArr: PffDoubleArray Absolute DoubleArray;
      S: String;
      D: Extended;
      c: Char;
    Begin
      Result := '';
      D := BArr[0];
      c := DecimalSeparator;
      DecimalSeparator := '.';
      r := Self.GetRound;
      If r = rNone Then
        r := rMathematical;
      Try
        If decimal > 0 Then
          Begin
            d := RoundExtended(d, decimal, r);
            S := fsFloatToStrF(D, ffFixed, 20, decimal);
          End
        Else
          S := fsFloatToStr(D);
        Result := S;
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
          End;
      Finally
        DecimalSeparator := c;
      End;
    End;

  Begin
    If GetType = fstArrayUInt8 Then
      Result := ByteArrayToString(FieldBuffer, Self.GetSize)
    Else If GetType = fstArrayUInt16 Then
      Result := WordArrayToString(FieldBuffer, Self.GetSize)
    Else If GetType = fstArrayInt32 Then
      Result := IntegerArrayToString(FieldBuffer, Self.GetSize)
    Else If GetType = fstArrayDouble Then
      Result := DoubleArrayToString(FieldBuffer, Self.GetSize, Self.GetDecimals)
    Else
      Result := '';
  End;

  Function GetAValue(Index: Integer): Variant;

    Procedure vIntArray(IntArray: Pointer; ArrayLength: Integer; Var V: Variant);
    Var
      BArr: PffIntArray Absolute IntArray;
    Begin
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        V := BArr[Index]
      Else
        Assert(False);
    End;

    Procedure vDoubleArray(DoubleArray: Pointer; ArrayLength: Integer; Var V: Variant);
    Var
      BArr: PffDoubleArray Absolute DoubleArray;
      r: TRound;
      decl: Integer;

    Begin
      r := getround;
      decl := Self.GetDecimals;
      If r = rNone Then
        r := rMathematical;
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        Begin
          If decl > 0 Then
            V := RoundExtended(BArr[Index], decl, r)
          Else
            V := BArr[Index];
        End
      Else
        Assert(False);
    End;

    Procedure vWordArray(WordArray: Pointer; ArrayLength: Integer; Var V: Variant);
    Var
      BArr: PffWordArray Absolute WordArray;
    Begin
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        V := BArr[Index]
      Else
        Assert(False);
    End;

    Procedure vByteArray(ByteArray: Pointer; ArrayLength: Integer; Var V: Variant);
    Var
      BArr: PffByteArray Absolute ByteArray;
    Begin
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        V := BArr[Index]
      Else
        Assert(False);
    End;
  Begin
    If GetType = fstArrayUInt8 Then
      Begin
        vByteArray(FieldBuffer, GetSize, Result);
      End
    Else If GetType = fstArrayUInt16 Then
      Begin
        vWordArray(FieldBuffer, GetSize, Result);
      End
    Else If GetType = fstArrayInt32 Then
      Begin
        vIntArray(FieldBuffer, GetSize, Result);
      End
    Else If GetType = fstArrayDouble Then
      Begin
        vDoubleArray(FieldBuffer, GetSize, Result);
      End;
  End;
Begin
  ReadField(IsNull);
  If IsNull Then
    Result := Null
  Else
    Case GetType Of
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
          kk := PffWord32(FieldBuffer)^;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := kk;
          {$ELSE}
          Result := kk;
          {$ENDIF}
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
          e := PSingle(FieldBuffer)^;
          r := Self.GetRound;
          decl := Self.GetDecimals;
          If decl > 0 Then
            Begin
              If r = rNone Then
                r := rMathematical;
              e := RoundExtended(E, decl, r);
            End;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).vtype := VT_E80;
          decimal(Result).ext80 := e;
          {$ELSE}
          Result := E;
          {$ENDIF}
        End;

      fstDouble:
        Begin
          e := PDouble(FieldBuffer)^;
          r := Self.GetRound;
          decl := Self.GetDecimals;
          If decl > 0 Then
            Begin
              If r = rNone Then
                r := rMathematical;
              e := RoundExtended(E, decl, r);
            End;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).vtype := VT_E80;
          decimal(Result).ext80 := e;
          {$ELSE}
          Result := E;
          {$ENDIF}
        End;

      fstExtended:
        Begin
          E := PExtended(FieldBuffer)^;
          r := Self.GetRound;
          decl := Self.GetDecimals;
          If decl > 0 Then
            Begin
              If r = rNone Then
                r := rMathematical;
              e := RoundExtended(E, decl, r);
            End;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).vtype := VT_E80;
          decimal(Result).ext80 := E;
          {$ELSE}
          Result := E;
          {$ENDIF}
        End;

      fstInt64, fstAutoinc64, fstRecVersion:
        Begin
          kk := pint64(FieldBuffer)^;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := kk;
          {$ELSE}
          Result := kk;
          {$ENDIF}
        End;

      // fstBinaryDecimals,
      fstCurrency:
        Begin
          C := Pint64(FieldBuffer)^ / 10000.0;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).vtype := VT_E80;
          decimal(Result).ext80 := C;
          {$ELSE}
          Result := c;
          {$ENDIF}
        End;
      fstDate:
        Begin
          dt := StDateToDateTime(PStDate(FieldBuffer)^);
          Result := DT;
        End;
      fstTime:
        Begin
          dt := StTimeToDateTime(PStTime(FieldBuffer)^);
          Result := DT;
        End;
      fstDateTime:
        Begin
          DT := PffDateTime(FieldBuffer)^ - 693594.0;
          fsSetMillisecond(dt, 0);
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
      fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
        If aArrayIndex >= 0 Then
          Result := GetAValue(aArrayIndex)
        Else
          Result := ArrayAsString; //GetAsVariant;
      fstBLOB..fstBLOBGraphic:
        Result := GetBlobValue;
      Else
        Assert(False);
    End;
End;

Procedure TFSSqlFieldProxy.Value(aArrayIndex: Integer; Var Result: Variant);
Var
  IsNull: Boolean;
  C: Currency;
  E: Extended;
  W: WideString;
  WC: WideChar;
  DT: TDateTime;
  kk: Int64;
  r: TRound;
  decl: Byte;

  Function GetAsVariant: Variant;
  Var
    V: OleVariant;

    Procedure BufferToByteArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);

      Procedure ByteArrayToVariant(ByteArray: Pointer; ArrayLength: Integer);
      Var
        idx: Integer;
        BArr: PffByteArray Absolute ByteArray;
      Begin
        For idx := 0 To ArrayLength - 1 Do
          Result[idx] := BArr[idx];
      End;
    Begin
      VarArray := VarArrayCreate([0, DataSize - 1], varInteger);
      Result := VarArray;
      VarArrayLock(VarArray);
      Try
        ByteArrayToVariant(Data, DataSize);
      Finally
        VarArrayUnlock(VarArray);
      End;
    End;

    Procedure BufferToWordArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);

      Procedure WordArrayToVariant(WordArray: Pointer; ArrayLength: Integer);
      Var
        idx: Integer;
        BArr: PffWordArray Absolute WordArray;
      Begin
        For idx := 0 To ArrayLength - 1 Do
          Result[idx] := BArr[idx];
      End;
    Begin
      VarArray := VarArrayCreate([0, DataSize - 1], varInteger);
      Result := VarArray;
      VarArrayLock(VarArray);
      Try
        WordArrayToVariant(Data, DataSize);
      Finally
        VarArrayUnlock(VarArray);
      End;
    End;

    Procedure BufferToIntArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);
    Var
      PVarData: Pointer;
    Begin
      VarArray := VarArrayCreate([0, DataSize - 1], varInteger);
      PVarData := VarArrayLock(VarArray);
      Try
        Move(Data^, PVarData^, DataSize);
      Finally
        VarArrayUnlock(VarArray);
      End;
    End;

    Procedure BufferToDoubleArray(Data: Pointer; DataSize: Integer; Var VarArray: OleVariant);

      Procedure DoubleArrayToVariant(DoubleArray: Pointer; ArrayLength: Integer);
      Var
        idx: Integer;
        BArr: PffDoubleArray Absolute DoubleArray;
      Begin
        For idx := 0 To ArrayLength - 1 Do
          Result[idx] := BArr[idx];
      End;
    Begin
      VarArray := VarArrayCreate([0, DataSize - 1], varDouble);
      Result := VarArray;
      VarArrayLock(VarArray);
      Try
        DoubleArrayToVariant(Data, DataSize);
      Finally
        VarArrayUnlock(VarArray);
      End;
    End;

  Begin
    If GetType = fstArrayUInt8 Then
      Begin
        BufferToByteArray(@FieldBuffer, Self.GetSize, V);
        Result := V;
      End
    Else If GetType = fstArrayUInt16 Then
      Begin
        BufferToWordArray(@FieldBuffer, Self.GetSize, V);
      End
    Else If GetType = fstArrayInt32 Then
      Begin
        BufferToIntArray(@FieldBuffer, Self.GetSize, V);
        Result := V;
      End
    Else If GetType = fstArrayDouble Then
      Begin
        BufferToDoubleArray(@FieldBuffer, Self.GetSize, V);
      End
    Else
      Result := Null;
  End;

  Function ArrayAsString: String;

    Function ByteArrayToString(ByteArray: Pointer; ArrayLength: Integer): String;
    Var
      idx: Integer;
      BArr: PffByteArray Absolute ByteArray;
    Begin
      Result := '';
      Result := IntToStr(BArr[0]);
      For idx := 1 To ArrayLength - 1 Do
        Result := Result + ',' + IntToStr(BArr[idx]);
    End;

    Function WordArrayToString(WordArray: Pointer; ArrayLength: Integer): String;
    Var
      idx: Integer;
      BArr: PffWordArray Absolute WordArray;
    Begin
      Result := '';
      Result := IntToStr(BArr[0]);
      For idx := 1 To ArrayLength - 1 Do
        Result := Result + ',' + IntToStr(BArr[idx]);
    End;

    Function IntegerArrayToString(IntArray: Pointer; ArrayLength: Integer): String;
    Var
      idx: Integer;
      BArr: PffIntArray Absolute intArray;
    Begin
      Result := '';
      Result := IntToStr(BArr[0]);
      For idx := 1 To ArrayLength - 1 Do
        Result := Result + ',' + IntToStr(BArr[idx]);
    End;

    Function DoubleArrayToString(DoubleArray: Pointer; ArrayLength: Integer; Decimal: Byte): String;
    Var
      idx: Integer;
      BArr: PffDoubleArray Absolute DoubleArray;
      S: String;
      D: Extended;
      c: Char;
    Begin
      Result := '';
      D := BArr[0];
      c := DecimalSeparator;
      DecimalSeparator := '.';
      r := Self.GetRound;
      If r = rNone Then
        r := rMathematical;
      Try
        If decimal > 0 Then
          Begin
            d := RoundExtended(d, decimal, r);
            S := fsFloatToStrF(D, ffFixed, 20, decimal);
          End
        Else
          S := fsFloatToStr(D);
        Result := S;
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
          End;
      Finally
        DecimalSeparator := c;
      End;
    End;

  Begin
    If GetType = fstArrayUInt8 Then
      Result := ByteArrayToString(FieldBuffer, Self.GetSize)
    Else If GetType = fstArrayUInt16 Then
      Result := WordArrayToString(FieldBuffer, Self.GetSize)
    Else If GetType = fstArrayInt32 Then
      Result := IntegerArrayToString(FieldBuffer, Self.GetSize)
    Else If GetType = fstArrayDouble Then
      Result := DoubleArrayToString(FieldBuffer, Self.GetSize, Self.GetDecimals)
    Else
      Result := '';
  End;

  Function GetAValue(Index: Integer): Variant;

    Procedure vIntArray(IntArray: Pointer; ArrayLength: Integer; Var V: Variant);
    Var
      BArr: PffIntArray Absolute IntArray;
    Begin
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        V := BArr[Index]
      Else
        Assert(False);
    End;

    Procedure vDoubleArray(DoubleArray: Pointer; ArrayLength: Integer; Var V: Variant);
    Var
      BArr: PffDoubleArray Absolute DoubleArray;
      r: TRound;
      decl: Integer;

    Begin
      r := getround;
      decl := Self.GetDecimals;
      If r = rNone Then
        r := rMathematical;
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        Begin
          If decl > 0 Then
            V := RoundExtended(BArr[Index], decl, r)
          Else
            V := BArr[Index];
        End
      Else
        Assert(False);
    End;

    Procedure vWordArray(WordArray: Pointer; ArrayLength: Integer; Var V: Variant);
    Var
      BArr: PffWordArray Absolute WordArray;
    Begin
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        V := BArr[Index]
      Else
        Assert(False);
    End;

    Procedure vByteArray(ByteArray: Pointer; ArrayLength: Integer; Var V: Variant);
    Var
      BArr: PffByteArray Absolute ByteArray;
    Begin
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        V := BArr[Index]
      Else
        Assert(False);
    End;
  Begin
    If GetType = fstArrayUInt8 Then
      Begin
        vByteArray(FieldBuffer, GetSize, Result);
      End
    Else If GetType = fstArrayUInt16 Then
      Begin
        vWordArray(FieldBuffer, GetSize, Result);
      End
    Else If GetType = fstArrayInt32 Then
      Begin
        vIntArray(FieldBuffer, GetSize, Result);
      End
    Else If GetType = fstArrayDouble Then
      Begin
        vDoubleArray(FieldBuffer, GetSize, Result);
      End;
  End;
Begin
  ReadField(IsNull);
  If IsNull Then
    Result := Null
  Else
    Case GetType Of
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
          kk := PffWord32(FieldBuffer)^;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := kk;
          {$ELSE}
          Result := kk;
          {$ENDIF}
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
          e := PSingle(FieldBuffer)^;
          r := Self.GetRound;
          decl := Self.GetDecimals;
          If decl > 0 Then
            Begin
              If r = rNone Then
                r := rMathematical;
              e := RoundExtended(E, decl, r);
            End;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).vtype := VT_E80;
          decimal(Result).ext80 := e;
          {$ELSE}
          Result := E;
          {$ENDIF}
        End;

      fstDouble:
        Begin
          e := PDouble(FieldBuffer)^;
          r := Self.GetRound;
          decl := Self.GetDecimals;
          If decl > 0 Then
            Begin
              If r = rNone Then
                r := rMathematical;
              e := RoundExtended(E, decl, r);
            End;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).vtype := VT_E80;
          decimal(Result).ext80 := e;
          {$ELSE}
          Result := E;
          {$ENDIF}
        End;

      fstExtended:
        Begin
          E := PExtended(FieldBuffer)^;
          r := Self.GetRound;
          decl := Self.GetDecimals;
          If decl > 0 Then
            Begin
              If r = rNone Then
                r := rMathematical;
              e := RoundExtended(E, decl, r);
            End;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).vtype := VT_E80;
          decimal(Result).ext80 := E;
          {$ELSE}
          Result := E;
          {$ENDIF}
        End;

      fstInt64, fstAutoinc64, fstRecVersion:
        Begin
          kk := pint64(FieldBuffer)^;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).VType := VT_DECIMAL;
          Decimal(Result).lo64 := kk;
          {$ELSE}
          Result := kk;
          {$ENDIF}
        End;

      // fstBinaryDecimals,
      fstCurrency:
        Begin
          C := Pint64(FieldBuffer)^ / 10000.0;
          {$IFDEF IsNoVariantInt64}
          TVarData(Result).vtype := VT_E80;
          decimal(Result).ext80 := C;
          {$ELSE}
          Result := c;
          {$ENDIF}
        End;
      fstDate:
        Begin
          dt := StDateToDateTime(PStDate(FieldBuffer)^);
          Result := DT;
        End;
      fstTime:
        Begin
          dt := StTimeToDateTime(PStTime(FieldBuffer)^);
          Result := DT;
        End;
      fstDateTime:
        Begin
          DT := PffDateTime(FieldBuffer)^ - 693594.0;
          fsSetMillisecond(dt, 0);
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
      fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
        If aArrayIndex >= 0 Then
          Result := GetAValue(aArrayIndex)
        Else
          Result := ArrayAsString; //GetAsVariant;
      fstBLOB..fstBLOBGraphic:
        Result := GetBlobValue;
      Else
        Assert(False);
    End;
End;

Function TFSSqlFieldProxy.IsNull: Boolean;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  {$IFOPT C+}
  Assert(TfsSrBaseCursor(FCursorID).GetRecordField(FIndex, FOwnerTable.RecordBuffer,
    Result, FieldBuffer) = DBIERR_NONE);
  {$ELSE}
  TfsSrBaseCursor(FCursorID).GetRecordField(FIndex, FOwnerTable.RecordBuffer,
    Result, FieldBuffer);
  {$ENDIF}
End;

Function TFSSqlFieldProxy.Name: String;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).Dictionary.FieldName[FIndex];
End;

Function TFSSqlFieldProxy.GetDisplayName: String;
Begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) Is TfsSrBaseCursor);
  Result := TfsSrBaseCursor(FCursorID).Dictionary.FieldDisplay[FIndex];
End;

Procedure TFSSqlFieldProxy.WriteField;
Begin
  TfsSrBaseCursor(FCursorID).Dictionary.SetRecordField(FIndex,
    FOwnerTable.RecordBuffer, FieldBuffer);
End;

Procedure TFSSqlFieldProxy.WriteFieldDirect(Buffer: PffByteArray);
Begin
  TfsSrBaseCursor(FCursorID).Dictionary.SetRecordField(FIndex,
    FOwnerTable.RecordBuffer, Buffer);
End;

{Begin !!.11}

Procedure TFSSqlFieldProxy.SetDefault;
Begin
  TfsSrBaseCursor(FCursorID).Dictionary.FUserName := TfsSrBaseCursor(FCursorID).Client.ClientName;
  TfsSrBaseCursor(FCursorID).Dictionary.SetDefaultFieldUpdateValue
    (FOwnerTable.RecordBuffer, FIndex);
End;
{End !!.11}

Procedure TFSSqlFieldProxy.SetFieldToNull;
Begin
  TfsSrBaseCursor(FCursorID).Dictionary.SetRecordFieldNull(
    FIndex, FOwnerTable.RecordBuffer, True);
End;

Procedure TFSSqlFieldProxy.SetValue(Const Value: Variant; aArrayIndex: Integer);
Var
  S: AnsiString;
  W: WideString;
  FT: TfsFieldType;
  ValueIsNull: Boolean;
  LenW: Word; {!!.11}
  Len, I: Longint; {!!.11}
  C: Currency;
  E: Extended;
  d: Double;
  si: Single;
  l: Longword;
  i64: Int64;
  dt: tdatetime;

  Procedure SetAsString(Const Value: String);

    Procedure StringToIntArray(IntArray: Pointer; ArrayLength: Integer; S: String);
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
          End;
      End;
    Begin
      For idx := 0 To ArrayLength - 1 Do
        BArr[idx] := 0;
      ExtractArray(Trim(S));
    End;

    Procedure StringToDoubleArray(DoubleArray: Pointer; ArrayLength: Integer; S: String);
    Var
      idx, ICode, decl: Integer;
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
        r := getround;
        decl := GetDecimals;
        If r = rNone Then
          r := rMathematical;
        While iPos <= Length(aStrinArray) Do
          Begin
            If idx <= ArrayLength - 1 Then
              Begin
                fsStringToExtended(EArray(aStrinArray, iPos), D, ICode);
                If iCode = 0 Then
                  Begin
                    If decl > 0 Then
                      Begin
                        D := RoundExtended(D, decl, r);
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
          End;
      End;
    Begin
      For idx := 0 To ArrayLength - 1 Do
        BArr[idx] := 0;
      ExtractArray(Trim(S));
    End;

    Procedure StringToWordArray(WordArray: Pointer; ArrayLength: Integer; S: String);
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
          End;
      End;
    Begin
      For idx := 0 To ArrayLength - 1 Do
        BArr[idx] := 0;
      ExtractArray(Trim(S));
    End;

    Procedure StringToByteArray(ByteArray: Pointer; ArrayLength: Integer; S: String);
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
          End;
      End;
    Begin
      For idx := 0 To ArrayLength - 1 Do
        BArr[idx] := 0;
      ExtractArray(Trim(S));
    End;
  Begin
    If GetType = fstArrayUInt8 Then
      StringToByteArray(FieldBuffer, GetSize, Value)
    Else If GetType = fstArrayUInt16 Then
      StringToWordArray(FieldBuffer, GetSize, Value)
    Else If GetType = fstArrayInt32 Then
      StringToIntArray(FieldBuffer, GetSize, Value)
    Else If GetType = fstArrayDouble Then
      StringToDoubleArray(FieldBuffer, GetSize, Value);
  End;

  Procedure SetAValue(Index: Integer; Value: Variant);
  Var
    Rt: Boolean;

    Procedure ToIntArray(IntArray: Pointer; ArrayLength: Integer; Reset: boolean);
    Var
      BArr: PffIntArray Absolute IntArray;
      idx: Integer;
    Begin
      If Value = null Then
        Exit;
      If Reset Then
        For idx := 0 To ArrayLength - 1 Do
          BArr[idx] := 0;
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        Begin
          BArr[Index] := Value;
        End
      Else
        Assert(False);
    End;

    Procedure ToDoubleArray(DoubleArray: Pointer; ArrayLength: Integer; Reset: boolean);
    Var
      BArr: PffDoubleArray Absolute DoubleArray;
      idx, decl: Integer;
      r: TRound;
      e: Extended;
    Begin
      r := getround;
      decl := GetDecimals;
      If r = rNone Then
        r := rMathematical;
      If Value = null Then
        Exit;
      If Reset Then
        For idx := 0 To ArrayLength - 1 Do
          BArr[idx] := 0;
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        Begin
          If decl > 0 Then
            Begin
              e := RoundExtended(Value, decl, r);
              BArr[Index] := e;
            End
          Else
            BArr[Index] := Value;
        End
      Else
        Assert(False);
    End;

    Procedure ToWordArray(WordArray: Pointer; ArrayLength: Integer; Reset: boolean);
    Var
      BArr: PffWordArray Absolute WordArray;
      idx: Integer;
    Begin
      If Value = null Then
        Exit;
      If Reset Then
        For idx := 0 To ArrayLength - 1 Do
          BArr[idx] := 0;
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        Begin
          BArr[Index] := Value;
        End
      Else
        Assert(False);
    End;

    Procedure ToByteArray(ByteArray: Pointer; ArrayLength: Integer; Reset: boolean);
    Var
      BArr: PffByteArray Absolute ByteArray;
      idx: Integer;
    Begin
      If Value = null Then
        Exit;
      If Reset Then
        For idx := 0 To ArrayLength - 1 Do
          BArr[idx] := 0;
      If (Index >= 0) And (Index <= ArrayLength - 1) Then
        Begin
          BArr[Index] := Value;
        End
      Else
        Assert(False);
    End;
  Begin
    If GetType = fstArrayUInt8 Then
      Begin
        rt := (FieldBuffer = Nil) Or (Value = null);
        ToByteArray(FieldBuffer, GetSize, rt);
      End
    Else If GetType = fstArrayUInt16 Then
      Begin
        rt := (FieldBuffer = Nil) Or (Value = null);
        ToWordArray(FieldBuffer, GetSize, rt);
      End
    Else If GetType = fstArrayInt32 Then
      Begin
        rt := (FieldBuffer = Nil) Or (Value = null);
        ToIntArray(FieldBuffer, GetSize, rt);
      End
    Else If GetType = fstArrayDouble Then
      Begin
        rt := (FieldBuffer = Nil) Or (Value = null);
        ToDoubleArray(FieldBuffer, GetSize, rt);
      End;
  End;

Begin
  ValueIsNull := VarIsNull(Value);
  If ValueIsNull Then
    SetFieldToNull
  Else
    Begin
      FT := GetType;
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
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: i := Round(decimal(Value).ext80);
              vt_decimal: i := decimal(Value).lo64;
              Else
                i := Value;
            End;
            {$ELSE}
            i := Value;
            {$ENDIF}
            PByte(FieldBuffer)^ := i;
          End;
        fstUInt16:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: i := Round(decimal(Value).ext80);
              vt_decimal: i := decimal(Value).lo64;
              Else
                i := Value;
            End;
            {$ELSE}
            i := Value;
            {$ENDIF}
            PWord(FieldBuffer)^ := i;
          End;
        fstUInt32:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: d := decimal(Value).ext80;
              vt_decimal: d := decimal(Value).lo64;
              Else
                d := Value;
            End;
            {$ELSE}
            d := Value;
            {$ENDIF}
            l := Round(d);
            PFFWord32(FieldBuffer)^ := l;
          End;
        fstInt8:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: i := Round(decimal(Value).ext80);
              vt_decimal: i := decimal(Value).lo64;
              Else
                i := Value;
            End;
            {$ELSE}
            i := Value;
            {$ENDIF}
            PShortInt(FieldBuffer)^ := i;
          End;
        fstInt16:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: i := Round(decimal(Value).ext80);
              vt_decimal: i := decimal(Value).lo64;
              Else
                i := Value;
            End;
            {$ELSE}
            i := Value;
            {$ENDIF}
            PSmallInt(FieldBuffer)^ := i;
          End;
        fstInt32, fstAutoInc32:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: i := Round(decimal(Value).ext80);
              vt_decimal: i := decimal(Value).lo64;
              Else
                i := Value;
            End;
            {$ELSE}
            i := Value;
            {$ENDIF}
            PLongint(FieldBuffer)^ := i
          End;
        fstSingle:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: si := decimal(Value).ext80;
              vt_decimal: si := decimal(Value).lo64;
              Else
                si := Value;
            End;
            {$ELSE}
            si := Value;
            {$ENDIF}
            PSingle(FieldBuffer)^ := si;
          End;
        fstDouble:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: d := decimal(Value).ext80;
              vt_decimal: d := decimal(Value).lo64;
              Else
                d := Value;
            End;
            {$ELSE}
            d := Value;
            {$ENDIF}
            PDouble(FieldBuffer)^ := d;
          End;
        fstExtended:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: e := decimal(Value).ext80;
              vt_decimal: e := decimal(Value).lo64;
              Else
                e := Value;
            End;
            {$ELSE}
            e := Value;
            {$ENDIF}
            PExtended(FieldBuffer)^ := E;
          End;
        fstInt64, fstAutoinc64, fstRecVersion:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: i64 := Round(decimal(Value).ext80);
              vt_decimal: i64 := decimal(Value).lo64;
              Else
                i64 := Round(Value);
            End;
            {$ELSE}
            i64 := Value;
            {$ENDIF}
            pint64(FieldBuffer)^ := i64;
          End;
        fstCurrency:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: c := decimal(Value).ext80;
              vt_decimal: c := decimal(Value).lo64;
              Else
                c := Value;
            End;
            {$ELSE}
            c := Value;
            {$ENDIF}
            Pint64(FieldBuffer)^ := Round(c * 10000.0);
          End;

        fstDate:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: d := decimal(Value).ext80;
              vt_decimal: d := decimal(Value).lo64;
              Else
                d := Value;
            End;
            {$ELSE}
            d := Value;
            {$ENDIF}
            PStDate(FieldBuffer)^ := DateTimeToStDate(d);
          End;
        fstTime:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: d := decimal(Value).ext80;
              vt_decimal: d := decimal(Value).lo64;
              Else
                d := Value;
            End;
            {$ELSE}
            d := Value;
            {$ENDIF}
            PStTime(FieldBuffer)^ := DateTimeToStTime(d);
          End;
        fstDateTime:
          Begin
            {$IFDEF IsNoVariantInt64}
            Case TVarData(Value).vtype Of
              vt_e80: d := decimal(Value).ext80;
              vt_decimal: d := decimal(Value).lo64;
              Else
                d := Value;
            End;
            {$ELSE}
            d := Value;
            {$ENDIF}
            dt := d;
            fsSetMillisecond(dt, 0);
            PffDateTime(FieldBuffer)^ := dt + 693594;
          End;
        fstShortString:
          Begin
            S := Value;
            FillChar(FieldBuffer^, FieldBufferLength, 0);
            LenW := FFMinI(Length(S), Pred(FieldBufferLength));
            FieldBuffer[0] := LenW;
            If S <> '' Then {!!.12}
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
            SetBLOBValue(Value);
            Exit;
          End;
        fstArrayUInt8, fstArrayUInt16, fstArrayInt32, fstArrayDouble:
          Begin
            If aArrayIndex >= 0 Then
              SetAValue(aArrayIndex, Value)
            Else
              Begin
                S := Value;
                SetAsString(S);
              End;
          End;

        Else
          Assert(False);
      End;
      WriteField;
    End;
End;

Function TFSSqlFieldProxy.QualName: String;
Begin
  Result := FOwnerTable.Name + '.' + Name;
End;

Function TFSSqlFieldProxy.CanUpdate: Boolean;
Begin
  Case GetType Of
    fstBoolean, fstSingleChar, fstSingleWideChar, fstUInt8,
      fstUInt16, fstUInt32, fstInt8, fstInt16,
      fstInt32, fstAutoInc32, fstSingle, fstDouble, //fstBinaryDecimals,
    fstExtended, fstInt64, fstRecVersion, fstAutoinc64, fstCurrency, fstBcd, fstDate,
      fstTime, fstDateTime, fstShortString,
      fstNullString, fstVarNullString,
      fstBLOB..fstBLOBGraphic,
      fstWideString, fstVarWideString, fstArrayDouble, fstArrayUInt8, fstArrayInt32, fstArrayUInt16:
      Result := True;
    Else
      Result := False;
  End;
End;

{!!.11 new}

Procedure fsBMMakeTableS(Const MatchString: ShortString; Var BT: TBTable);
{-Build a Boyer-Moore link table}
Register;
Asm
  push  edi                { Save registers because they will be changed }
  push  esi
  mov   esi, eax           { Move EAX to ESI }
  push  ebx

  xor   eax, eax           { Zero EAX }
  xor   ecx, ecx           { Zero ECX }
  mov   cl, [esi]          { ECX has length of MatchString }
  inc   esi

  mov   ch, cl             { Duplicate CL in CH }
  mov   eax, ecx           { Fill each byte in EAX with length }
  shl   eax, 16
  or    eax, ecx
  mov   edi, edx           { Point to the table }
  mov   ecx, 64            { Fill table bytes with length }
  rep   stosd
  cmp   al, 1              { If length <= 1, we're done }
  jbe   @@MTDone
  xor   ebx, ebx           { Zero EBX }
  mov   cl, al             { Restore CL to length of string }
  dec   ecx

@@MTNext:
  mov   al, [esi]          { Load table with positions of letters }
  mov   bl, al             { that exist in the search string }
  inc   esi
  mov   [edx+ebx], cl
  dec   cl
  jnz   @@MTNext

@@MTDone:
  pop   ebx                { Restore registers }
  pop   esi
  pop   edi
End;

{!!.11 new}

Function BMSearchS(Var Buffer; BufLength: DWord; Const BT: TBTable;
  Const MatchString: ShortString; Var Pos: Cardinal): Boolean; Assembler;
{-Use the Boyer-Moore search method to search a buffer for a string.}
Register;
Var
  BufPtr: Pointer;
Asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and EDI }
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  xor   eax, eax            { Zero EAX }

  mov   dl, [esi]           { Length of MatchString in EDX }
  inc   esi
  and   edx, 0FFh

  cmp   dl, 1               { Check to see if we have a trivial case }
  ja    @@BMSInit           { If Length(MatchString) > 1 do BM search }
  jb    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

  mov   al,[esi]            { If Length(MatchString) = 1 do a REPNE SCASB }
  mov   ebx, edi
  repne scasb
  jne   @@BMSNotFound       { No match during REP SCASB }
  mov   esi, Pos            { Set position in Pos }
  {dec   edi}               { Found, calculate position }
  sub   edi, ebx
  mov   eax, 1              { Set result to True }
  mov   [esi], edi
  jmp   @@BMSDone           { We're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  std                       { Backward string ops }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  repe  cmpsb               { Compare MatchString to buffer }
  je    @@BMSFound          { If equal, string is found }

  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax                 { Pos is one based }
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
End;

{!!.13 new}

Function BMSearchUCS(Var Buffer; BufLength: Cardinal; Const BT: TBTable;
  Const MatchString: ShortString; Var Pos: Cardinal): Boolean; Assembler;
{-Use the Boyer-Moore search method to search a buffer for a string. This
  search is not case sensitive.}
Register;
Var
  BufPtr: Pointer;
Asm
  push  edi                 { Save registers since we will be changing }
  push  esi
  push  ebx

  mov   BufPtr, eax         { Copy Buffer to local variable and ESI }
  mov   edi, eax
  mov   ebx, ecx            { Copy BT ptr to EBX }
  mov   ecx, edx            { Length of buffer to ECX }
  mov   esi, MatchString    { Set ESI to beginning of MatchString }
  xor   eax, eax            { Zero EAX }

  mov   dl, byte ptr [esi]  { Length of MatchString in EDX }
  and   edx, 0FFh           { Clean up EDX }
  inc   esi                 { Set ESI to first character }

  or    dl, dl              { Check to see if we have a trivial case }
  jz    @@BMSNotFound       { If Length(MatchString) = 0 we're done }

@@BMSInit:
  dec   edx                 { Set up for BM Search }
  add   esi, edx            { Set ESI to end of MatchString }
  add   ecx, edi            { Set ECX to end of buffer }
  add   edi, edx            { Set EDI to first check point }
  std                       { Backward string ops }
  mov   dh, [esi]           { Set DH to character we'll be looking for }
  dec   esi                 { Dec ESI in prep for BMSFound loop }
  jmp   @@BMSComp           { Jump to first comparison }

@@BMSNext:
  mov   al, [ebx+eax]       { Look up skip distance from table }
  add   edi, eax            { Skip EDI ahead to next check point }

@@BMSComp:
  cmp   edi, ecx            { Have we reached end of buffer? }
  jae   @@BMSNotFound       { If so, we're done }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  mov   al, [edi]           { Move character from buffer into AL for comparison }
  push  eax                 { Push Char onto stack for CharUpper }
  cld
  call  CharUpper
  std
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  cmp   dh, al              { Compare }
  jne   @@BMSNext           { If not equal, go to next checkpoint }

  push  ecx                 { Save ECX }
  dec   edi
  xor   ecx, ecx            { Zero ECX }
  mov   cl, dl              { Move Length(MatchString) to ECX }
  jecxz @@BMSFound          { If ECX is zero, string is found }

@@StringComp:
  xor   eax, eax
  mov   al, [edi]           { Get char from buffer }
  dec   edi                 { Dec buffer index }

  push  ebx                 { Save registers }
  push  ecx
  push  edx
  push  eax                 { Push Char onto stack for CharUpper }
  cld
  call  CharUpper
  std
  pop   edx                 { Restore registers }
  pop   ecx
  pop   ebx

  mov   ah, al              { Move buffer char to AH }
  mov   al, [esi]           { Get MatchString char }
  dec   esi
  cmp   ah, al              { Compare }
  loope @@StringComp        { OK?  Get next character }
  je    @@BMSFound          { Matched! }

  xor   ah, ah              { Zero AH }
  mov   al, dl              { Move Length(MatchString) to AL }
  sub   al, cl              { Calculate offset that string didn't match }
  add   esi, eax            { Move ESI back to end of MatchString }
  add   edi, eax            { Move EDI to pre-string compare location }
  inc   edi
  mov   al, dh              { Move character back to AL }
  pop   ecx                 { Restore ECX }
  jmp   @@BMSNext           { Do another compare }

@@BMSFound:                 { EDI points to start of match }
  mov   edx, BufPtr         { Move pointer to buffer into EDX }
  mov   esi, Pos
  sub   edi, edx            { Calculate position of match }
  mov   eax, edi
  inc   eax
  inc   eax                 { Pos is one based }
  mov   [esi], eax          { Set Pos to position of match }
  mov   eax, 1              { Set result to True }
  pop   ecx                 { Restore ESP }
  jmp   @@BMSDone

@@BMSNotFound:
  xor   eax, eax            { Set result to False }

@@BMSDone:
  cld                       { Restore direction flag }
  pop   ebx                 { Restore registers }
  pop   esi
  pop   edi
End;

{!!.11 new}
{$IFDEF DCC7OrLater}
{$HINTS OFF}
{$ENDIF}

Function TFSSqlFieldProxy.BLOBBmSearch(Const Table: TBTable; Const SearchPhrase: String;
  IgnoreCase: Boolean): Boolean;
Const
  BufferSize = 4096;
Var
  bl: TDataCompLevel;
  TmpStream, ResultStream: TMemoryStream;
  Offset: Integer;
  BLOBNr: TffInt64;
  Error, Len: Integer;
  BytesRead: TffWord32;
  Pos: Cardinal;
  ChunkSize,
    ChunkOffset: Integer;
  Buffer: Array[0..BufferSize - 1] Of char;
Begin
  Result := False;
  Offset := TfsSrBaseCursor(FCursorID).Dictionary.FieldOffset[Index];
  BLOBNr := PffInt64(@OwnerTable.RecordBuffer^[Offset])^;
  Len := TfsSrBaseCursor(FCursorID).BLOBGetLength(BLOBNr, Error);
  bl := GetBlobLevel;
  If Error = DBIERR_NONE Then
    Begin
      ChunkOffset := 0;
      ChunkSize := BufferSize - length(SearchPhrase);
      If Len > 0 Then
        Begin
          If bl <> blNone Then
            Begin
              TmpStream := TMemoryStream.Create;
              ResultStream := TMemoryStream.Create;
              Try
                TmpStream.position := 0;
                While Len > 0 Do
                  Begin
                    TfsSrBaseCursor(FCursorID).BLOBRead(BLOBNr, ChunkOffset, BufferSize, Buffer, BytesRead);
                    If BytesRead > 0 Then
                      TmpStream.write(buffer, BytesRead);
                    dec(Len, ChunkSize);
                    inc(ChunkOffset, ChunkSize);
                  End;
                If TmpStream.Size > 0 Then
                  Begin
                    TmpStream.position := 0;
                    ResultStream.Position := 0;
                    fsZDecompress(TmpStream, ResultStream);
                    ResultStream.Position := 0;
                    Len := ResultStream.Size;
                    ChunkOffset := 0;
                    ChunkSize := BufferSize - length(SearchPhrase);
                    BytesRead := 0;
                    While Len > 0 Do // if non compress
                      Begin
                        BytesRead := ResultStream.Read(Buffer, BufferSize);
                        If IgnoreCase Then
                          Begin
                            If BMSearchUCS(Buffer, BytesRead, Table, SearchPhrase, Pos) Then
                              Begin
                                Result := True;
                                Exit;
                              End;
                          End
                        Else
                          Begin
                            If BMSearchS(Buffer, BytesRead, Table, SearchPhrase, Pos) Then
                              Begin
                                Result := True;
                                Exit;
                              End;
                          End;
                        dec(Len, ChunkSize);
                        inc(ChunkOffset, ChunkSize);
                      End;

                  End;
              Finally
                TmpStream.free;
                ResultStream.free;
              End;
            End
          Else
            While Len > 0 Do // if non compress
              Begin
                TfsSrBaseCursor(FCursorID).BLOBRead(BLOBNr, ChunkOffset, BufferSize, Buffer, BytesRead);
                {!!.13 begin}
                If IgnoreCase Then
                  Begin
                    If BMSearchUCS(Buffer, BytesRead, Table, SearchPhrase, Pos) Then
                      Begin
                        Result := True;
                        Exit;
                      End;
                  End
                Else
                  Begin
                    If BMSearchS(Buffer, BytesRead, Table, SearchPhrase, Pos) Then
                      Begin
                        Result := True;
                        Exit;
                      End;
                  End;
                {!!.13 end}
                dec(Len, ChunkSize);
                inc(ChunkOffset, ChunkSize);
              End;
        End;
    End;
End;
{$IFDEF DCC7OrLater}
{$HINTS OFF}
{$ENDIF}

{!!.11 new}

Function TFSSqlFieldProxy.BMMatch(Const Table: TBTable; Const SearchPhrase: String;
  IgnoreCase: Boolean): Boolean; {!!.13}
Var
  S: String;
  Pos: Cardinal;
  V: Variant;
Begin
  If IsNull Then
    Result := False
  Else If GetType = fstBLOBMemo Then
    Result := BLOBBmSearch(Table, SearchPhrase, IgnoreCase) {!!.13}
  Else
    Begin
      V := GetValue(-1);
      {$IFDEF IsNoVariantInt64}
      Case TVarData(V).VType Of
        VT_DECIMAL:
          s := IntToStr(Decimal(V).lo64);
        VT_E80:
          s := fsFloatToStr(Decimal(V).ext80);
        Else
          S := GetValue(-1);
      End;
      {$ELSE}
      S := GetValue(-1);
      {$ENDIF}

      Result := False;
      If S <> '' Then
        If IgnoreCase Then
          Begin
            If BMSearchUCS(S[1], length(S), Table, SearchPhrase, Pos) Then
              Result := True;
          End
        Else If BMSearchS(S[1], length(S), Table, SearchPhrase, Pos) Then
          Result := True;
    End;
End;

Type
  TfsHashNodeFriend = Class(TfsHashNode);

  {===TfsNRecordHash========================================================}

Procedure TfsNRecordHash.Add;
Var
  keyPtr: PfsNRecordHashEntry;
  i, Size: Integer;
Begin
  Size := EntrySlots * sizeOf(TffInt64);
  FFGetMem(keyPtr, Size);
  For i := 0 To pred(EntrySlots) Do
    KeyPtr^[i] := FSourceTables[i].GetCurrentRecordID;
  //store size of record in hash entry's value field for destruction
  {$IFOPT C+}
  Assert(fhAddPrim(keyPtr, Pointer(Size)));
  {$ELSE}
  fhAddPrim(keyPtr, Pointer(Size));
  {$ENDIF}
End;
{--------}

Procedure TfsNRecordHash.AddTable(Const SourceTable: TFSSqlTableProxy);
Begin
  FFReallocMem(FSourceTables,
    EntrySlots * sizeof(TFSSqlTableProxy),
    succ(EntrySlots) * sizeof(TFSSqlTableProxy));
  inc(EntrySlots);
  FSourceTables^[EntrySlots - 1] := SourceTable;
End;
{--------}

Constructor TfsNRecordHash.Create;
Begin
  Inherited Create(fsc_Size2099);
End;
{--------}

Destructor TfsNRecordHash.Destroy;
Begin
  If FSourceTables <> Nil Then
    FFFreeMem(FSourceTables, EntrySlots * sizeof(TFSSqlTableProxy));
  Inherited Destroy;
End;
{--------}

Function TfsNRecordHash.fhCompareKey(Const aKey1: Pointer;
  Const aKey2: Pointer): Boolean;
Var
  i: Integer;
Begin
  For i := 0 To pred(EntrySlots) Do
    If FFCmpI64(PfsNRecordHashEntry(aKey1)^[i], PfsNRecordHashEntry(aKey2)^[i]) <> 0 Then
      Begin
        Result := False;
        Exit;
      End;
  Result := True;
End;
{--------}

Procedure TfsNRecordHash.fhFreeKeyPrim(aKey: pointer);
Begin
  FFFreeMem(aKey, EntrySlots * sizeOf(TffInt64));
End;
{--------}

Function TfsNRecordHash.fhGetIndex(Const AKey: Pointer;
  Const ACount: Integer): Integer;
Var
  X: TffInt64;
  I: Integer;
Begin
  X := PfsNRecordHashEntry(aKey)^[0];
  For i := 1 To pred(EntrySlots) Do
    Begin
      X.iLow := X.iLow Xor PfsNRecordHashEntry(aKey)^[i].iLow;
      X.iHigh := X.iHigh Xor PfsNRecordHashEntry(aKey)^[i].iHigh;
    End;
  Result := ffI64ModInt(X, ACount);
End;
{--------}

Function TfsNRecordHash.Exists: Boolean;
Var
  I: Integer;
  Node: TfsHashNode;
  keyPtr: PfsNRecordHashEntry;
Begin

  FFGetMem(keyPtr, EntrySlots * sizeOf(TffInt64));
  Try
    For i := 0 To pred(EntrySlots) Do
      KeyPtr^[i] := FSourceTables[i].GetCurrentRecordID;
    Result := fhFindPrim(KeyPtr, I, Node);
  Finally
    FFFreeMem(keyPtr, EntrySlots * sizeOf(TffInt64));
  End;
End;

Function TfsNRecordHash.fhCreateNode: TfsHashNode;
Begin
  Result := TfsNRecordHashNode.Create;
End;
{--------}

{ TfsNRecordHashNode }

Destructor TfsNRecordHashNode.Destroy;
Begin
  assert(TObject(Self) Is TfsNRecordHashNode);
  assert(fhValue <> Nil);
  Inherited;
End;

Procedure fsCopyField(Const SourceField, TargetField: TFSSqlFieldProxy);
Var
  IsNull: Boolean;
Begin
  Assert(SourceField.GetType = TargetField.GetType);
  SourceField.ReadField(IsNull);
  If Not IsNull Then
    TargetField.WriteFieldDirect(SourceField.FieldBuffer)
  Else
    TargetField.SetFieldToNull;
End;

Procedure CopyBLOBField(Const SourceField,
  TargetField: TFSSqlFieldProxy);
Var
  IsNull: Boolean;
  SrcOffset,
    TgtOffset: Integer;
  aSrcBLOBNr,
    aBLOBNr: TffInt64;
  aLinkTableName: TfsTableName; {!!.11 - New}
Begin
  Assert(SourceField.GetType = TargetField.GetType);
  SourceField.ReadField(IsNull);
  If (Not IsNull) Then
    Begin
      Assert(TObject(SourceField.FCursorID) Is TfsSrBaseCursor);
      SrcOffset := TfsSrBaseCursor(SourceField.FCursorID).Dictionary.FieldOffset[SourceField.Index];
      TgtOffset := TfsSrBaseCursor(TargetField.FCursorID).Dictionary.FieldOffset[TargetField.Index];
      { link the BLOBs }
      { Get the BLOB reference out of the record. }
      aSrcBLOBNr := PffInt64(@SourceField.OwnerTable.RecordBuffer^[SrcOffset])^;

      With TfsSrBaseCursor(TargetField.FCursorID) Do
        Begin {!!.11 - Start}
          { Clear the null flag for the target field. }{!!.10}
          Dictionary.SetRecordFieldNull(TargetField.Index, {!!.10}
            TargetField.OwnerTable.RecordBuffer, {!!.10}
            False); {!!.10}

          { Is aSrcBLOBNr another BLOB Link? }
          If (TfsSrBaseCursor(SourceField.FCursorID).BLOBIsLink(aSrcBLOBNr,
            aLinkTableName,
            aSrcBLOBNr)) Then
            Begin

              { Yes. BLOBIsLink filled in the TableName and updated aSrcBLOBNr. }
              BLOBLinkAdd(aLinkTableName,
                aSrcBLOBNr,
                aBLOBNr);
            End
          Else
            Begin
              { Add a BLOB link. }
              BLOBLinkAdd(TfsSrBaseCursor(SourceField.FCursorID).Table.BaseName,
                aSrcBLOBNr,
                aBLOBNr);
            End;
        End; {!!.11 - End}

      { Update the BLOB reference in the record. }
      PffInt64(@TargetField.OwnerTable.RecordBuffer^[TgtOffset])^ := aBLOBNr;
    End
  Else
    TargetField.SetFieldToNull;
End;

Function fsCompatibleFields(Const SourceField, TargetField: TFSSqlFieldProxy): Boolean;
Begin
  Result := (SourceField.GetType = TargetField.GetType)
    And (SourceField.FieldBufferLength = TargetField.FieldBufferLength);
End;

{ TFSFieldCopier }

Procedure TFSFieldCopier.Add(SourceField, TargetField: TFSSqlFieldProxy);
Begin
  FSourceList.Append(SourceField);
  FTargetList.Append(TargetField);
  If fsCompatibleFields(SourceField, TargetField) Then
    Begin
      FCompatible.Append(Pointer(1));
      Case SourceField.GetType Of
        fstBLOB..fstBLOBGraphic:
          FBlob.Append(Pointer(1));
        Else
          FBlob.Append(Pointer(0));
      End;
    End
  Else
    Begin
      FCompatible.Append(Pointer(0));
      FBlob.Append(Pointer(0));
    End;
End;

Constructor TFSFieldCopier.Create;
Begin
  Inherited Create;
  FSourceList := TfsPointerList.Create;
  FTargetList := TfsPointerList.Create;
  FCompatible := TfsPointerList.Create;
  FBlob := TfsPointerList.Create;
End;

Destructor TFSFieldCopier.Destroy;
Begin
  FSourceList.Free;
  FTargetList.Free;
  FCompatible.Free;
  FBlob.Free;
  Inherited;
End;

Procedure TFSFieldCopier.Execute;
Var
  i: Integer;
Begin
  For i := 0 To pred(FSourceList.Count) Do
    If FCompatible[i] <> Nil Then
      If FBlob[i] <> Nil Then
        CopyBLOBField(
          TFSSqlFieldProxy(FSourceList[i]),
          TFSSqlFieldProxy(FTargetList[i]))
      Else
        fsCopyField(
          TFSSqlFieldProxy(FSourceList[i]),
          TFSSqlFieldProxy(FTargetList[i]))
    Else
      Begin
        TFSSqlFieldProxy(FTargetList[i]).SetValue(
          TFSSqlFieldProxy(FSourceList[i]).GetValue(-1), -1);
      End;
End;

{ TfsSqlFieldDefList }

Procedure TfsSqlFieldDefList.AddField(Const aName, aDisplayName: String;
  aType: TfsFieldType; aUnit, aDec: Integer; aBlobLevelComp: TDataCompLevel; aRound: TRound);
Var
  NewEntry: PFSSqlFieldDefProxyRec;
Begin
  FFGetZeroMem(NewEntry, sizeof(NewEntry^));
  NewEntry.FieldName := aName;
  NewEntry.DisplayName := aDisplayName;
  NewEntry.FieldType := aType;
  NewEntry.BlobLevelComp := aBlobLevelComp;
  NewEntry.Round := aRound;
  If (aType In [fstShortString..fstWideString]) And (aUnit = 0) Then
    NewEntry.FieldUnits := 255
  Else
    NewEntry.FieldUnits := aUnit;
  NewEntry.Decimals := aDec;
  FieldList.Append(NewEntry);
End;

Constructor TfsSqlFieldDefList.Create;
Begin
  Inherited Create;
  FieldList := TfsPointerList.Create;
End;

Destructor TfsSqlFieldDefList.Destroy;
Var
  i: Integer;
  P: PFSSqlFieldDefProxyRec;
Begin
  For i := 0 To pred(FieldList.Count) Do
    Begin
      P := PFSSqlFieldDefProxyRec(FieldList[i]);
      P^.FieldName := '';
      P^.DisplayName := '';
      FFFreeMem(P, sizeof(TFSSqlFieldDefProxyRec));
    End;
  FieldList.Free;
  Inherited;
End;

Function TfsSqlFieldDefList.GetCount: Integer;
Begin
  Result := FieldList.Count;
End;

Function TfsSqlFieldDefList.GetFieldDecimals(Index: Integer): Integer;
Begin
  Case FieldType[Index] Of
    fstSingle..fstExtended, fstCurrency, fstRecVersion, fstBcd, fstAutoInc32, fstAutoInc64:
      Result := PFSSqlFieldDefProxyRec(FieldList[Index])^.Decimals;
    Else
      Result := 0;
  End;
End;

Function TfsSqlFieldDefList.CompareTypes(Other: TfsSqlFieldDefList): Boolean;
Var
  i: Integer;
Begin
  Result := True;
  For i := 0 To FieldList.Count - 1 Do
    Begin
      If ValidPhysicalConversions[PfsSqlFieldDefProxyRec(FieldList[i])^.FieldType, Other.FieldType[i]] Then
        Begin
          PfsSqlFieldDefProxyRec(FieldList[i])^.FieldType := Other.FieldType[i];
          If PfsSqlFieldDefProxyRec(FieldList[i])^.FieldType In [fstArrayUInt16, fstArrayInt32, fstArrayDouble, fstArrayUInt8..fstWideString] Then
            PfsSqlFieldDefProxyRec(FieldList[i])^.FieldUnits :=
              ffMaxI(
              PfsSqlFieldDefProxyRec(FieldList[i])^.FieldUnits,
              Other.FieldUnits[i]);
        End
      Else
        Begin
          Result := False;
          Exit;
        End;
    End;
End;

Function TfsSqlFieldDefList.GetFieldName(Index: Integer): String;
Begin
  Result := PFSSqlFieldDefProxyRec(FieldList[Index])^.FieldName;
End;

Function TfsSqlFieldDefList.GetFieldDisplayName(Index: Integer): String;
Begin
  Result := PFSSqlFieldDefProxyRec(FieldList[Index])^.DisplayName;
End;

Function TfsSqlFieldDefList.GetFieldType(Index: Integer): TfsFieldType;
Begin
  Result := PFSSqlFieldDefProxyRec(FieldList[Index])^.FieldType;
End;

Function TfsSqlFieldDefList.GetFieldUnits(Index: Integer): Integer;
Begin
  Result := PFSSqlFieldDefProxyRec(FieldList[Index])^.FieldUnits;
End;

Function TfsSqlFieldDefList.GetFieldBlob(Index: Integer): TDataCompLevel;
Begin
  Result := PFSSqlFieldDefProxyRec(FieldList[Index])^.BlobLevelComp;
End;

Function TfsSqlFieldDefList.GetFieldRound(Index: Integer): TRound;
Begin
  Result := PFSSqlFieldDefProxyRec(FieldList[Index])^.Round;
End;

End.

