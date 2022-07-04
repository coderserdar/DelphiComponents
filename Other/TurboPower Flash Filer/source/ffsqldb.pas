{*********************************************************}
{* FlashFiler: SQL Engine database interface             *}
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

{$I FFDEFINE.INC}

{$Z+}

unit ffsqldb;

interface
uses
  Windows,
  SysUtils,
  DB,
  Classes,
  ffllbase,
  fflleng,
  ffsrbde,
  ffsreng,
  ffsrlock,
  fflldict,
  ffstdate,
  ffhash,
  ffsrbase,
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  ffsrcur,
  ffsrixhl;

const
  ffcl_MaxSqlSortDepth = 64;
type
  PStDate = ^TStDate;
  PStTime = ^TStTime;
  TBTable  = array[0..255] of Byte;  {Table used by Boyer-Moore search routines} {!!.11}
  PBTable = ^TBTable; {!!.11}

type
  {Type for describing a field for creating temporary tables
   - see CreateTemporaryTable below.}
  PFFSqlFieldDefProxyRec = ^TFFSqlFieldDefProxyRec;
  TFFSqlFieldDefProxyRec = record
    FieldName : string;
    FieldType : TffFieldType;
    FieldUnits : Integer;
    Decimals : Integer;
  end;

  TffSqlFieldDefList = class(TffObject)
  protected
    FieldList: TffPointerList;
    function GetCount: Integer;
    function GetFieldDecimals(Index: Integer): Integer;
    function GetFieldName(Index: Integer): string;
    function GetFieldType(Index: Integer): TffFieldType;
    function GetFieldUnits(Index: Integer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddField(const aName: string; aType: TffFieldType; aUnit: Integer; aDec: Integer);
    property Count: Integer read GetCount;
    property FieldName[Index: Integer]: string read GetFieldName;
    property FieldType[Index: Integer]: TffFieldType read GetFieldType;
    property FieldUnits[Index: Integer]: Integer read GetFieldUnits;
    property FieldDecimals[Index: Integer]: Integer read GetFieldDecimals;
  end;

  TffSqlSortArray = array[0..pred(ffcl_MaxSqlSortDepth)] of Integer;

  TFFSqlTableProxy = class;

  {Interface between SQL engine and a table field definition}
  TFFSqlFieldProxy = class(TffObject)
  private
    procedure SetBlobValue(const Value: Variant);                      {!!.13}
  protected
    FCursorID : TFFCursorID;
    FIndex : Integer;
    FIsTarget : Boolean;
    FOwnerTable : TFFSqlTableProxy;
    FSrcField : TffSqlFieldProxy;
    FSrcIndex : Integer;
    TypeKnown: Boolean;
    FType : TffFieldType;
    procedure ReadField(var IsNull: Boolean);
    procedure WriteField;
    procedure WriteFieldDirect(Buffer: PffByteArray);
    function GetBlobValue: Variant;                                    {!!.11}{!!.13}
    function BLOBBmSearch(const Table: TBTable;
      const SearchPhrase: string;
      IgnoreCase: Boolean                                                     {!!.13}
      ): Boolean; {!!.11}
  public
    FieldBuffer : PffByteArray;
    FieldBufferLength: Integer;
    constructor Create(AnOwnerTable: TFFSqlTableProxy; AnIndex: Integer; ACursorID: TFFCursorID);
    destructor Destroy; override;
    property Index: Integer read FIndex;
    function Name: string;
    function IsNull: Boolean;
    function GetSize: Integer;
    function GetDecimals: Integer;
    function GetType : TffFieldType;
    function GetValue: Variant;
    procedure SetValue(const Value: Variant);
    property IsTarget : Boolean read FIsTarget write FIsTarget;
      { If this is a field in the result set table (i.e., a target field) then
        this property returns True. }
    property OwnerTable:TFFSqlTableProxy read FOwnerTable;
    function QualName: string;
    property SrcField : TffSQLFieldProxy read FSrcField write FSrcField;
      { If this is a target field that refers to a source field then this
        property references the source field. }
    property SrcIndex : Integer read FSrcIndex write FSrcIndex;
      { If this is a target field that refers to a simple expression then
        this property identifies the index of the simple expression in
        protected variable FSX. }
    function CanUpdate: Boolean;
    procedure SetDefault;                                              {!!.11}
    procedure SetFieldToNull;
    function BMMatch(const Table: TBTable; const SearchPhrase: string;
             IgnoreCase: Boolean                                       {!!.13}
             ): Boolean; {!!.11}
  end;

  TFFSqlDatabaseProxy = class;

  TFFCopyValidator = function: Boolean of object;

  TFFSqlTableIterator = function(Cookie: TffWord32): Boolean of object;

  {Interface between SQL engine and a table definition}
  TFFSqlTableProxy = class(TffObject)
  protected
    FCursorID : TFFCursorID;
    FieldList : TList;
    FName : string;
    FAlias: string;
    KeyBuffer1,
    KeyBuffer2,
    RecordBuffer : PffByteArray;
    FDataBase: TFFSqlDatabaseProxy;
    FEngine : TffBaseServerEngine;
    FIndex : Integer;
    FLeaveOpen : Boolean;
    FRecordLen : Longint;
    NoRecord: Boolean;
    FOwner: TObject;
    function SortOnAllFields(const CaseSensitive : Boolean) : TffResult; {!!.13}
  public
    procedure Iterate(Iterator: TFFSqlTableIterator; Cookie: TffWord32);
    constructor Create(AOwner: TObject;
      ADataBase: TFFSqlDatabaseProxy; ACursorID : TFFCursorID; const AName,
        AAlias: string);                                               {!!.11}
    destructor Destroy; override;
    property Name: string read FName;
    property Alias: string read FAlias;                                {!!.11}
    property CursorID : TFFCursorID read FCursorID;
    property LeaveCursorOpen: Boolean read FLeaveOpen write FLeaveOpen;
    function FieldCount: Integer;
    function Field(Index: Integer): TFFSqlFieldProxy;
    function FieldByName(const Name: string): TFFSqlFieldProxy;
    procedure Close;
    function Delete : TffResult;                                       {!!.11}
    function First: Boolean;
    function Next: Boolean;
    function Prior : Boolean;
    procedure SetRange(const StartValues, EndValues: array of Variant;
                       const LowCount, HighCount: Integer;
                       const IncludeLowLimit, IncludeHighLimit,
                             IndexAsc: Boolean);
    function EnsureWritable : TffResult;                               {!!.11}
      { Verify the table may be modified. }                            {!!.11}
    function EOF: Boolean;
    procedure Insert;
    {- create a new record where all fields are initially NULL}
    function Post : TffResult;                                         {!!.11}
    {- actually insert the record.
     - Currently, Insert and Post will only be performed
       on temporary tables created by the SQL statement itself.}
    function Update : TffResult;                                       {!!.11}
    { - update the current record buffer in the table}
    procedure SetIndex(KeyNum: Integer);
    {- switch to specified key, 0..pred(GetNumIndexes) means an actual index;
       -1 means physical order (i.e. use no defined ordering) }
    function GetSegments : Integer;
    {- return number of fields in the currently active index}
    function CopyValidated(AOwner: TObject; Validator: TFFCopyValidator): TFFSqlTableProxy;{!!.10}
    {- return a copy of the table with only records that are valid
       as per the called Validator function}
    function CopySortedOnAllFields(AOwner: TObject): TFFSqlTableProxy;
    function GetCurrentRecordID: Tffint64;
    procedure GetRecordByID(ID: Tffint64;                              {!!.11}
                      const LockType : TffSrLockType);                 {!!.11}
    function IndexesOnField(F : TFFSqlFieldProxy; MustBeCaseInsensitive: Boolean;
      var IndexRefs: array of integer): Integer;
    procedure GetIndexProperties(const Index: Integer;
                                   var Unique, IgnoreCase, IndexAsc: Boolean;
                                   var IndexFieldCount: Integer; var IndexFields: array of integer);
{Begin !!.13}
    function Sort(const SortListCount: Integer;
                  const SortList: TffSqlSortArray;
                  const CaseSensitive : Boolean) : TffResult;
    function CopyUnique(AOwner: TObject;
                  const CaseSensitive : Boolean): TFFSqlTableProxy;
    function HasDuplicates(const CaseSensitive : Boolean): Boolean;
{End !!.13}
    function ExtractFieldDef: TffSqlFieldDefList;
    function GetRecordCount: Integer;
    property Engine : TffBaseServerEngine read FEngine write FEngine;
    procedure NullRecord;
    property Owner: TObject read FOwner write FOwner;
    procedure SetDefaults;
    function PostNoDefaults: TffResult;                                {!!.11}
  end;

  {Interface between SQL engine and the database}
  TFFSqlDatabaseProxy = class(TffObject)
  protected
    FEngine : TFFServerEngine;
    FDatabaseID : TFFDatabaseID;
  public
    property Engine: TFFServerEngine read FEngine;
    constructor Create(Engine : TFFServerEngine; DatabaseID : TFFDatabaseID);
    destructor Destroy; override;
    function TableByName(AOwner: TObject;
                   const S: string;
                   const ExclContentLock : Boolean;
                   const AAlias: string): TFFSqlTableProxy;            {!!.11}
    {- find a table by name. if the table does not exist, NIL
       is returned}

    function CreateTemporaryTableWithIndex(
      AOwner: TObject;
      const FieldDef: TffSqlFieldDefList;
      IndexFields: Integer; IndexColumns: TffSqlSortArray):
        TFFSqlTableProxy;
    {- create a temporary table as per the specified field and
       key segment lists. Return a proxy object, which gives
       access to the (initially empty) table. When the proxy
       object is freed, the tables can (should) be deleted.
       FieldList is a TList containing PFFSqlFieldDefProxyRec
       instances (see above). Each entry describes a field in the
       table. KeyList is a TList containing PFFSqlKeySegmentDefProxyRec
       instances (see above). Each entry describes a key segment}
    function CreateTemporaryTableWithoutIndex(
      AOwner: TObject;
      const FieldDef: TffSqlFieldDefList): TFFSqlTableProxy;

    function StartTransaction(const Tables : array of TffSqlTableProxy) : TffResult;
    procedure Commit;
    procedure AbortTransaction;
    function Alias: string;
  end;

type
  TFFVariantList = class
  protected
    List : TFFPointerList;
  public
    constructor Create(Capacity: Integer);
    destructor Destroy; override;
    function GetValue(Index: Integer): Variant;
    procedure SetValue(Index: Integer; const Value: Variant);
  end;

const
  ffNRHashMaxRecords = MaxInt div sizeof(TffInt64);
  ffMaxSourceTables = MaxInt div sizeof(TFFSqlTableProxy);
type
  TffNRecordHashNode = class(TffHashNode)
    destructor Destroy; override;
  end;

  TffNRecordHashEntry = array[0..pred(ffNRHashMaxRecords)] of TffInt64;
  PffNRecordHashEntry = ^TffNRecordHashEntry;
  TffTableArray = array[0..pred(ffMaxSourceTables)] of TFFSqlTableProxy;
  PffTableArray = ^TffTableArray;

  TffNRecordHash = class(TffBaseHashTable)
  {- a data structure for keeping track of duplicate
     record combinations when doing joins}
    protected
      FSourceTables: PffTableArray;
      EntrySlots : Integer;
      function fhCompareKey(const aKey1 : Pointer;
                            const aKey2 : Pointer) : Boolean; override;

      function fhCreateNode: TffHashNode; override;
      procedure fhFreeKeyPrim(aKey : pointer); override;

      function fhGetIndex(const AKey   : Pointer;
                          const ACount : Integer) : Integer; override;
        {calculate the index, ie hash, of the key}

    public
      constructor Create;
        {$IFDEF DCC4OrLater} reintroduce; {$ENDIF}
      destructor Destroy; override;
      procedure AddTable(const SourceTable: TFFSqlTableProxy);
      procedure Add;
      function Exists: Boolean;
  end;

type
  TFFFieldCopier = class(TffObject)
  protected
    FSourceList, FTargetList, FCompatible, FBlob: TffPointerList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute;
    procedure Add(SourceField, TargetField: TffSqlFieldProxy);
  end;

procedure CopyField(const SourceField, TargetField: TffSqlFieldProxy);
function CompatibleFields(const SourceField, TargetField: TffSqlFieldProxy): Boolean;
procedure BMMakeTableS(const MatchString : ShortString; var BT : TBTable); {!!.11}

implementation

uses
  FFLLExcp,
  FFSrCvex;

{$I FFCONST.INC}

{ TFFSqlDatabaseProxy }

type
  PComp = ^Comp;

procedure TFFSqlDatabaseProxy.AbortTransaction;
begin
  Assert(FEngine <> nil);
  Assert(FEngine is TFFBaseServerEngine);
  FEngine.TransactionRollbackSQL(FDatabaseID, False);
end;

procedure TFFSqlDatabaseProxy.Commit;
begin
  Assert(FEngine <> nil);
  Assert(FEngine is TFFBaseServerEngine);
  FEngine.TransactionCommitSQL(FDatabaseID, False);
end;

constructor TFFSqlDatabaseProxy.Create(Engine: TFFServerEngine;
                                       DatabaseID: TFFDatabaseID);
begin
  inherited Create;
  FEngine := Engine;
  FDatabaseID := DatabaseID;
end;

destructor TffSqlDatabaseProxy.Destroy;
begin
  inherited Destroy;
end;

function TFFSqlDatabaseProxy.CreateTemporaryTableWithIndex(
  AOwner: TObject;
  const FieldDef: TffSqlFieldDefList;
      IndexFields: Integer; IndexColumns: TffSqlSortArray): TFFSqlTableProxy;
var
  Dictionary : TffDataDictionary;
  i: Integer;
  KeySegList : TFFFieldList;
  FldIHList : TFFFieldIHList;
  Cursor : TffSrBaseCursor;

begin
  Dictionary := TffDataDictionary.Create(ffcl_64k);
  try
    for i := 0 to pred(FieldDef.Count) do
      Dictionary.AddField(FieldDef.FieldName[i], '', FieldDef.FieldType[i],
        FieldDef.FieldUnits[i], FieldDef.FieldDecimals[i], False, nil);

    for i := 0 to pred(IndexFields) do begin
      KeySegList[i] := IndexColumns[i];
      FldIHList[i] := '';
    end;

    Dictionary.AddIndex('key0','',0, IndexFields,
      KeySegList, FldIHList, True, True, False);

    Cursor := TffSrCursor.Create(TFFServerEngine(FEngine),
                                 TFFSrDatabase(FDatabaseID),
                                 FFGetRemainingTime);

    Cursor.Build('', Dictionary, omReadWrite, smExclusive,
                 False, True, [fffaTemporary, fffaBLOBChainSafe], 0);

    Cursor.CloseTable := True;
    Result := TFFSqlTableProxy.Create(AOwner, Self, Cursor.CursorID, '', ''); {!!.11}
    Result.Engine := FEngine;

  finally
    Dictionary.Free;
  end;
end;

function TFFSqlDatabaseProxy.CreateTemporaryTableWithoutIndex(AOwner: TObject;
  const FieldDef: TffSqlFieldDefList): TFFSqlTableProxy;
var
  Dictionary : TffDataDictionary;
  i: Integer;
  Cursor : TffSrBaseCursor;
begin
  Dictionary := TffDataDictionary.Create(ffcl_64k);
  try
    for i := 0 to pred(FieldDef.Count) do
      Dictionary.AddField(FieldDef.FieldName[i], '', FieldDef.FieldType[i],
        FieldDef.FieldUnits[i], FieldDef.FieldDecimals[i], False, nil);

    Cursor := TffSrSqlResultSet.Create(TFFServerEngine(FEngine),
                                         TFFSrDatabase(FDatabaseID),
                                         FFGetRemainingTime);

    Cursor.Build('', Dictionary, omReadWrite, smExclusive,
                 False, True, [fffaTemporary, fffaBLOBChainSafe], 0);

    Cursor.CloseTable := True;
    Result := TFFSqlTableProxy.Create(AOwner, Self, Cursor.CursorID, '', '');
    Result.Engine := FEngine;

  finally
    Dictionary.Free;
  end;
end;

function TFFSqlDatabaseProxy.StartTransaction(const Tables : array of TffSqlTableProxy) : TffResult;
var
  CursorIDs : TffPointerList;
  Inx : Integer;
begin
  Assert(FEngine <> nil);
  Assert(FEngine is TFFBaseServerEngine);
  if Tables[0] = nil then begin
    Result := DBIERR_NONE;
    FEngine.TransactionStartSQL(FDatabaseID, False)
  end
  else begin
    { Build the list of cursor IDs. }
    CursorIDs := TffPointerList.Create;
    try
      for Inx := Low(Tables) to High(Tables) do
        CursorIDs.Append(Pointer(Tables[Inx].CursorID));
      Result := FEngine.TransactionStartWith(FDatabaseID, False, CursorIDs);
    finally
      CursorIDs.Free;
    end;
  end;
end;

function TFFSqlDatabaseProxy.TableByName(AOwner: TObject;
                                   const S: string;
                                   const ExclContentLock : Boolean;
                                   const AAlias: string): TFFSqlTableProxy; {!!.11}
var
  Cursor : TffSrBaseCursor;
begin
  Cursor := nil;
  try
    Assert(FEngine <> nil);
    Assert(FEngine is TFFServerEngine);
    Assert(FDatabaseID <> 0);
    Assert(TObject(FDatabaseID) is TFFSrDatabase);


    Cursor := TffSrCursor.Create(TFFServerEngine(FEngine),
                                 TFFSrDatabase(FDatabaseID),
                                 FFGetRemainingTime);
    Cursor.Open(S, '', 0, omReadOnly, smShared, False, ExclContentLock, []);
    Result := TFFSqlTableProxy.Create(AOwner, Self, Cursor.CursorID, S, AAlias);
    Result.Engine := FEngine;

  except
    on E:Exception do begin
      ConvertServerExceptionEx(E, FEngine.EventLog, FEngine.IsReadOnly);
      Cursor.Free;
      Result := nil;
    end;
  end;
end;

function TFFSqlDatabaseProxy.Alias: string;
begin
  Assert(FDatabaseID <> 0);
  Assert(TObject(FDatabaseID) is TFFSrDatabase);
  Result := TFFSrDatabase(FDatabaseID).Alias;
end;

{ TFFVariantList }

constructor TFFVariantList.Create(Capacity: Integer);
var
  I: Integer;
begin
  inherited Create;
  List := TFFPointerList.Create;
  List.Capacity := Capacity;
  List.Count := Capacity;
  for i := 0 to pred(List.Capacity) do
    List[i] := nil;
end;

destructor TFFVariantList.Destroy;
var
  i : Integer;
  P : Pointer;
begin
  for i := 0 to pred(List.Count) do
    if List[i] <> nil then begin
      Finalize(PVariant(List[i])^);
      P := List[i];
      FFFreeMem(P, sizeof(Variant));
  end;
  List.Free;
  inherited;
end;

function TFFVariantList.GetValue(Index: Integer): Variant;
begin
  Assert(List[Index] <> nil);
  Result := PVariant(List[Index])^;
end;

procedure TFFVariantList.SetValue(Index: Integer; const Value: Variant);
var
  PV : PVariant;
begin
  if List[Index] = nil then begin
    FFGetZeroMem(PV, sizeof(Variant));
    List[Index] := PV;
  end;
  PVariant(List[Index])^ := Value;
end;

{ TFFSqlTableProxy }

function TffSqlTableProxy.ExtractFieldDef: TffSqlFieldDefList;
var
  i: Integer;
begin
  Result := TffSqlFieldDefList.Create;
  for i := 0 to pred(FieldList.Count) do
    Result.AddField(Field(i).Name, Field(i).GetType, Field(i).GetSize,
      Field(i).GetDecimals);
end;

function TFFSqlTableProxy.CopySortedOnAllFields(
  AOwner: TObject): TFFSqlTableProxy;
var
  i : Integer;
  FieldDefList : TffSqlFieldDefList;
{$IFOPT C+}
  CopyResult : TffResult;
{$ENDIF}
  IndexColumns: TffSqlSortArray;
begin
  FieldDefList := ExtractFieldDef;
  try
    for i := 0 to pred(FieldList.Count) do
      IndexColumns[i] := i;

    Result := FDatabase.CreateTemporaryTableWithIndex(AOwner, FieldDefList,
      FieldList.Count, IndexColumns);

  finally
    FieldDefList.Free;
  end;

  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Assert(Result.FCursorID <> 0);
  Assert(TObject(Result.FCursorID) is TffSrBaseCursor);

  {$IFOPT C+}
  CopyResult :=
  {$ENDIF}
    TffSrBaseCursor(Result.FCursorID).CopyRecords(
      TffSrBaseCursor(FCursorID), ffbcmCreateLink, nil, 0, 0);

  {$IFOPT C+}
  Assert(CopyResult = DBIERR_NONE);
  {$ENDIF}

  Result.SetIndex(0);
end;

function TFFSqlTableProxy.SortOnAllFields(const CaseSensitive : Boolean) : TffResult; {!!.13}
var
  aCount : Integer;
  i : Integer;
  KeyArray : TffSqlSortArray;
begin
  aCount := FFMinI(FieldList.Count, ffcl_MaxIndexFlds);
  for i := 0 to pred(aCount) do begin
    KeyArray[i] := TFFSqlFieldProxy(FieldList[i]).Index + 1;
    {KeyArray values are +1 to allow for specifying descending sorting on column 0
     (by negating)}
  end;

  Result := Sort(aCount, KeyArray, CaseSensitive);                     {!!.13}
end;

{Begin !!.13}
function TFFSqlTableProxy.CopyUnique(AOwner: TObject;
                               const CaseSensitive : Boolean): TFFSqlTableProxy;
{End !!.13}
var
  i : Integer;
  FieldCopier : TFFFieldCopier;
  FieldDefList : TffSqlFieldDefList;
  IsFirst, DoCopy: Boolean;
  Status : TffResult;
  LastValues : TffVariantList;
begin
  Status := SortOnAllFields(CaseSensitive);                            {!!.13}
  if Status <> DBIERR_NONE then
    raise EffException.CreateNoData(ffStrResServer, Status);

  FieldDefList := ExtractFieldDef;
  try
    Result := FDatabase.CreateTemporaryTableWithoutIndex(AOwner, FieldDefList);
  finally
    FieldDefList.Free;
  end;

  {build a map of compatible fields}
  FieldCopier := TFFFieldCopier.Create;
  try

    for i := 0 to pred(FieldList.Count) do
      FieldCopier.Add(Field(i), Result.Field(i));

    FDatabase.StartTransaction([nil]);
    try
      IsFirst := True;
      LastValues := TffVariantList.Create(FieldList.Count);
      try
        if First then
          repeat
            if IsFirst then begin
              IsFirst := False;
              DoCopy := True;
            end else begin
              DoCopy := False;
              for i := 0 to pred(FieldList.Count) do
                if Field(i).GetValue <> LastValues.GetValue(i) then begin
                  DoCopy := True;
                  break;
                end;
            end;
            if DoCopy then begin
              Result.Insert;
              FieldCopier.Execute;
              Result.Post;
            end;
            for i := 0 to pred(FieldList.Count) do
              LastValues.SetValue(i, Field(i).GetValue);
          until not Next;
      finally
        LastValues.Free;
      end;
    finally
      FDatabase.Commit;
    end;
  finally
    FieldCopier.Free;
  end;
end;

function TFFSqlTableProxy.HasDuplicates(const CaseSensitive : Boolean): Boolean; {!!.13}
var
  i : Integer;
  LastValues : TffVariantList;
  IsFirst, Del : Boolean;
  Status : TffResult;
begin
  Status := SortOnAllFields(CaseSensitive);                            {!!.13}
  if Status <> DBIERR_NONE then
    raise EffException.CreateNoData(ffStrResServer, Status);

  FDatabase.StartTransaction([nil]);
  LastValues := nil;
  try
    IsFirst := True;
    LastValues := TffVariantList.Create(FieldList.Count);
    if First then
      repeat
        if IsFirst then
          IsFirst := False
        else begin
          Del := True;
          for i := 0 to pred(FieldList.Count) do
            if Field(i).GetValue <> LastValues.GetValue(i) then begin
              Del := False;
              break;
            end;
          if Del then begin
            Result := True;
            exit;
          end;
        end;
        for i := 0 to pred(FieldList.Count) do
          LastValues.SetValue(i, Field(i).GetValue);
      until not Next;
  finally
    FDatabase.Commit;
    LastValues.Free;
  end;
  Result := False;
end;

function TFFSqlTableProxy.CopyValidated(AOwner: TObject; Validator: TFFCopyValidator): TFFSqlTableProxy;
var
  i : Integer;
  FieldCopier : TFFFieldCopier;
  FieldDefList : TffSqlFieldDefList;
begin
  FieldDefList := ExtractFieldDef;
  try
    Result := FDatabase.CreateTemporaryTableWithoutIndex(AOwner, FieldDefList);
  finally
    FieldDefList.Free;
  end;

  {build a map of compatible fields}
  FieldCopier := TFFFieldCopier.Create;
  try

    for i := 0 to pred(FieldList.Count) do
      FieldCopier.Add(Field(i), Result.Field(i));

    FDatabase.StartTransaction([nil]);
    try
      if First then
        repeat
          if Validator then begin
            Result.Insert;
            FieldCopier.Execute;
            Result.Post;
          end;
        until not Next;
    finally
      FDatabase.Commit;
    end;
  finally
    FieldCopier.Free;
  end;
end;

{Begin !!.13}
function TFFSqlTableProxy.Sort(const SortListCount: Integer;
                               const SortList: TffSqlSortArray;
                               const CaseSensitive : Boolean) : TffResult;
{End !!.13}
var
  aOrderByArray : TffOrderByArray;
  FldList : TffFieldList;
  IHList : TffFieldIHList;
  i : Integer;
begin

  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Assert(SortListCount <= ffcl_MaxIndexFlds);
  { A data dictionary contains a sequential access index by default. In order
    to sort the data, we must replace index 0 with the index describing how
    the data is to be sorted. We must leave this index on the cursor. }
  if TffSrBaseCursor(FCursorID).Dictionary.IndexCount > 0 then
    TffSrBaseCursor(FCursorID).Dictionary.RemoveIndex(0);

  { Set up the index for sorting. }
  for i := 0 to pred(SortListCount) do begin
    Assert(Abs(SortList[i]) > 0);
    FldList[i] := abs(SortList[i]) - 1;
    with TffSrBaseCursor(FCursorID).Dictionary do
      if FieldType[FldList[i]] in
         [fftByteArray, fftBLOB, fftBLOBMemo, fftBLOBFmtMemo, fftBLOBOLEObj,
          fftBLOBGraphic, fftBLOBDBSOLEObj, fftBLOBTypedBin, fftBLOBFile] then
        FFRaiseException(EffServerException, ffStrResGeneral,
                         fferrBadDistinctField, [FieldName[FldList[i]]]);
    IHList[i] := '';
    if SortList[i] < 0 then
      aOrderByArray[i] := ffobDescending
    else
      aOrderByArray[i] := ffobAscending;
  end;
  TffSrBaseCursor(FCursorID).Dictionary.AddIndex
    ('Sort', '', 0, SortListCount, FldList, IHList, True, True,        {!!.13}
     not CaseSensitive);                                               {!!.13}

  TffSrBaseCursor(FCursorID).Dictionary.BindIndexHelpers;

  Result :=
    TffSrBaseCursor(FCursorID).SortRecords(FldList, aOrderByArray, SortListCount);
end;

function TFFSqlTableProxy.GetCurrentRecordID: tffint64;
begin
  if NoRecord then
    ffInitI64(Result)
  else begin
    Assert(FCursorID <> 0);
    Assert(TObject(FCursorID) is TffSrBaseCursor);
    Result := TffSrBaseCursor(FCursorID).RefNr;
  end;
end;

procedure TFFSqlTableProxy.GetRecordByID(ID: TffInt64;                 {!!.11}
                                   const LockType : TffSrLockType);    {!!.11}
begin
  TffSrBaseCursor(FCursorID).SetToKey(skaEqual, True, 1, 0, @ID);
  TffSrBaseCursor(FCursorID).GetNextRecord(RecordBuffer, LockType);    {!!.11}
end;

procedure TFFSqlTableProxy.Close;
begin
  Assert(Self <> nil);
  Assert(TObject(Self) is TFFSqlTableProxy);
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor,
         Format('%d is not a cursor', [FCursorID]));
{Begin !!.13}
  with TffSrBaseCursor(FCursorID) do
    if CanClose(True) then
      Free
    else
      RequestClose;
{End !!.13}
end;

constructor TFFSqlTableProxy.Create(AOwner: TObject;
  ADataBase: TFFSqlDatabaseProxy; ACursorID: TFFCursorID; const AName, AAlias: string);
var
  i : Integer;
  Field : TFFSqlFieldProxy;
begin
  inherited Create;
  Assert(AOwner <> nil);
  FOwner := AOwner;
  FIndex := -1;
  FDatabase := ADatabase;
  FName := AName;
  FAlias := AAlias;                                                    {!!.11}
  FCursorID := ACursorID;
  FieldList := TList.Create;
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  for i := 0 to pred(TffSrBaseCursor(FCursorID).Dictionary.FieldCount) do begin
    Field := TFFSqlFieldProxy.Create(Self, i, FCursorID);
    FieldList.Add(Field);
  end;
  FRecordLen := TffSrBaseCursor(FCursorID).Dictionary.RecordLength;
  FFGetMem(RecordBuffer, FRecordLen);
  FFGetMem(KeyBuffer1, FRecordLen);
  FFGetMem(KeyBuffer2, FRecordLen);
end;

destructor TFFSqlTableProxy.Destroy;
begin
  Assert(Self <> nil);
  Assert(TObject(Self) is TFFSqlTableProxy);
  Assert(FOwner = nil);
  while FieldList.Count > 0 do begin
    TFFSqlFieldProxy(FieldList[0]).Free;
    FieldList.Delete(0);
  end;
  FieldList.Free;
  FFFreeMem(RecordBuffer, FRecordLen);
  FFFreeMem(KeyBuffer1, FRecordLen);
  FFFreeMem(KeyBuffer2, FRecordLen);
  if not LeaveCursorOpen then
    try
      Close;
    except
      on E:Exception do
        FEngine.LogFmt('Exception when closing TffSqlTableProxy: %s',
                       [E.message]);
    end;
  inherited;
end;
{Begin !!.11}
{--------}
function TffSqlTableProxy.EnsureWritable : TffResult;
var
  Table : TffSrBaseTable;
begin
  { There cannot be any type of lock on the table (unless its ours and
    is a write lock). }
  Result := DBIERR_NONE;
  Table := TffSrBaseCursor(FCursorID).Table;
  if Table.ClientLocks.Count > 0 then
    if Table.ClientLocks.SummaryMode = ffsltExclusive then begin
      if not Table.HasClientLock(CursorID) then begin
        Result := DBIERR_FILELOCKED;
        Exit;
      end;
    end
    else begin
      Result := DBIERR_FILELOCKED;
      Exit;
    end;
end;
{End !!.11}
{--------}
function TFFSqlTableProxy.EOF: Boolean;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Result := TffSrBaseCursor(FCursorID).Position = cpEOF;
end;
{--------}
function TFFSqlTableProxy.Field(Index: Integer): TFFSqlFieldProxy;
begin
  Result := TFFSqlFieldProxy(FieldList[index]);
end;
{--------}
function TFFSqlTableProxy.FieldByName(
  const Name: string): TFFSqlFieldProxy;
var
  i : Integer;
begin
  for i := 0 to pred(FieldList.Count) do
    if AnsiCompareText(TFFSqlFieldProxy(FieldList[i]).Name, Name) = 0 then begin
      Result := TFFSqlFieldProxy(FieldList[i]);
      exit;
    end;
  Result := nil;
end;

function TFFSqlTableProxy.FieldCount: Integer;
begin
  Result := FieldList.Count;
end;

function TFFSqlTableProxy.First: Boolean;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  TffSrBaseCursor(FCursorID).SetToBegin;
  Result := TffSrBaseCursor(FCursorID).GetNextRecord(RecordBuffer, ffsltNone) = DBIERR_NONE;
  NoRecord := False;
end;

function TFFSqlTableProxy.GetSegments: Integer;
begin
  Result := TffSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[FIndex + 1].idCount;
end;

procedure TFFSqlTableProxy.Insert;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  TffSrBaseCursor(FCursorID).Dictionary.InitRecord(RecordBuffer);
end;

procedure TFFSqlTableProxy.SetDefaults;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  TffSrBaseCursor(FCursorID).Dictionary.SetDefaultFieldValues(RecordBuffer);
end;

function TFFSqlTableProxy.Next: Boolean;
var
  DbResult : TffResult;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  DbResult := TffSrBaseCursor(FCursorID).GetNextRecord(RecordBuffer, ffsltNone);
  Result := DbResult = DBIERR_NONE;
  NoRecord := False;
end;

function TFFSqlTableProxy.Post : TffResult;                            {!!.11}
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Result := TffSrBaseCursor(FCursorID).InsertRecord(RecordBuffer,      {!!.11}
                                                    ffsltExclusive);   {!!.11}
  NoRecord := False;
end;

function TFFSqlTableProxy.PostNoDefaults: TffResult;
{Rewritten !!.11}
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Result := TffSrBaseCursor(FCursorID).InsertRecordNoDefault
                                         (RecordBuffer, ffsltExclusive);
  NoRecord := False;
end;

function TFFSqlTableProxy.Prior: Boolean;
var
  DbResult : TffResult;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  DbResult := TffSrBaseCursor(FCursorID).GetPriorRecord(RecordBuffer, ffsltNone);
  Result := DbResult = DBIERR_NONE;
  NoRecord := False;
end;

procedure TFFSqlTableProxy.SetIndex(KeyNum: Integer);
begin
  if KeyNum <> FIndex then begin
    FIndex := KeyNum;
    Assert(FCursorID <> 0);
    Assert(TObject(FCursorID) is TffSrBaseCursor);
    TffSrBaseCursor(FCursorID).SwitchToIndex(KeyNum + 1, False);
  end;
end;

procedure TFFSqlTableProxy.SetRange(const StartValues, EndValues: array of Variant;
                                    const LowCount, HighCount : Integer;
                                    const IncludeLowLimit, IncludeHighLimit,
                                          IndexAsc : Boolean);
var
  LowSegs, HighSegs, i : Integer;
  K1, K2 : PffByteArray;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  LowSegs := FFMinI(GetSegments, LowCount);
  HighSegs := FFMinI(GetSegments, HighCount);
  for i := 0 to pred(LowSegs) do
    Field(TffSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[FIndex + 1].
      idFields[i]).SetValue(StartValues[i]);
  TffSrBaseCursor(FCursorID).Table.BuildKeyForRecord(FIndex + 1,
    RecordBuffer, KeyBuffer1, LowSegs, 0);
  for i := 0 to pred(HighSegs) do
    Field(TffSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[FIndex + 1].
      idFields[i]).SetValue(EndValues[i]);
  TffSrBaseCursor(FCursorID).Table.BuildKeyForRecord(FIndex + 1,
    RecordBuffer, KeyBuffer2, HighSegs, 0);
  if LowSegs > 0 then
    K1 := KeyBuffer1
  else
    K1 := nil;
  if HighSegs > 0 then
    K2 := KeyBuffer2
  else
    K2 := nil;
  if IndexAsc then
    TffSrBaseCursor(FCursorID).SetRange(True, LowSegs, 0, K1, IncludeLowLimit,
                                        HighSegs, 0, K2, IncludeHighLimit)
  else
    TffSrBaseCursor(FCursorID).SetRange(True, HighSegs, 0, K2, IncludeHighLimit,
                                        LowSegs, 0, K1, IncludeLowLimit);
end;

procedure TFFSqlTableProxy.Iterate(Iterator: TFFSqlTableIterator; Cookie: TffWord32);
begin
  if First then
    repeat
      if not Iterator(Cookie) then
        break;
    until not Next;
end;

function TFFSqlTableProxy.IndexesOnField(F: TFFSqlFieldProxy; MustBeCaseInsensitive: Boolean; {!!.10}
      var IndexRefs: array of integer): Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to pred(TffSrBaseCursor(FCursorID).Dictionary.IndexCount) do begin
    if TffSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[i].idCount > 0 then
      if TffSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[i].idFields[0] =
        F.Index then begin
        if not MustBeCaseInsensitive                                                          {!!.10}
        or (TffSrBaseCursor(FCursorID).Dictionary.
          IndexDescriptor[i].idNoCase) then begin
            IndexRefs[Result] := i;
            inc(Result);
          end;
      end;
  end;
end;

procedure TFFSqlTableProxy.GetIndexProperties(const Index: Integer;
                                 var Unique, IgnoreCase, IndexAsc: Boolean;
                                 var IndexFieldCount: Integer;
                                 var IndexFields: array of integer);
var
  i : Integer;
  IdxDescrip : PffIndexDescriptor;
begin
  IdxDescrip := TffSrBaseCursor(FCursorID).Dictionary.IndexDescriptor[Index];
  Unique := not IdxDescrip.idDups;
  IgnoreCase := IdxDescrip.idNoCase;
  IndexFieldCount := IdxDescrip.idCount;
  IndexAsc := IdxDescrip.idAscend;
  for i := 0 to pred(IndexFieldCount) do
    IndexFields[i] := IdxDescrip.idFields[i];
end;

function TFFSqlTableProxy.Delete : TffResult;                          {!!.11}
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Result := TffSrBaseCursor(FCursorID).DeleteRecord(nil);              {!!.11}
end;

function TFFSqlTableProxy.GetRecordCount: Integer;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  TffSrBaseCursor(FCursorID).GetRecordCount(Result);
end;

function TFFSqlTableProxy.Update : TffResult;                          {!!.11}
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Result := TffSrBaseCursor(FCursorID).ModifyRecord(RecordBuffer, true); {!!.11}
end;

procedure TFFSqlTableProxy.NullRecord;
var
  i: Integer;
begin
  for i := 0 to FieldCount - 1 do
    Field(i).SetFieldToNull;
  NoRecord := True;
end;

{ TFFSqlFieldProxy }

constructor TFFSqlFieldProxy.Create(AnOwnerTable: TFFSqlTableProxy; AnIndex: Integer;
  ACursorID: TFFCursorID);
begin
  inherited Create;
  FOwnerTable := AnOwnerTable;
  FCursorID := ACursorID;
  FIndex := AnIndex;
  FIsTarget := False;
  FSrcIndex := -1;
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  FieldBufferLength := TffSrBaseCursor(FCursorID).Dictionary.FieldLength[FIndex];
  FFGetMem(FieldBuffer, FieldBufferLength);
end;

destructor TFFSqlFieldProxy.Destroy;
begin
  FFFreeMem(FieldBuffer, FieldBufferLength);
  inherited;
end;

function TFFSqlFieldProxy.GetDecimals: Integer;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Result := TffSrBaseCursor(FCursorID).Dictionary.FieldDecPl[FIndex];
end;

function TFFSqlFieldProxy.GetSize: Integer;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Result := TffSrBaseCursor(FCursorID).Dictionary.FieldUnits[FIndex];
end;

function TFFSqlFieldProxy.GetType: TffFieldType;
begin
  if not TypeKnown then begin
    Assert(FCursorID <> 0);
    Assert(TObject(FCursorID) is TffSrBaseCursor);
    FType := TffSrBaseCursor(FCursorID).Dictionary.FieldType[FIndex];
    {!!.13
    if FType = fftAutoInc then
      FType := fftWord32;
    }
    TypeKnown := True;
  end;
  Result := FType;
end;

procedure TFFSqlFieldProxy.ReadField(var IsNull: Boolean);
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  {$IFOPT C+}
  Assert(TffSrBaseCursor(FCursorID).GetRecordField(FIndex,
    FOwnerTable.RecordBuffer, IsNull, FieldBuffer) = DBIERR_NONE);
  {$ELSE}
  TffSrBaseCursor(FCursorID).GetRecordField(FIndex, FOwnerTable.RecordBuffer,
    IsNull, FieldBuffer);
  {$ENDIF}
end;

{!!.11 new}
function TffSqlFieldProxy.GetBlobValue: Variant;
{Rewritten !!.13}
var
  Offset        : Integer;
  BLOBNr        : TffInt64;
  Error, Len    : Integer;
  BytesRead     : TffWord32;
  VPtr          : PByte;
begin
  Offset := TffSrBaseCursor(FCursorID).Dictionary.FieldOffset[Index];
  BLOBNr := PffInt64(@OwnerTable.RecordBuffer^[Offset])^;
  Len := TffSrBaseCursor(FCursorID).BLOBGetLength(BLOBNr, Error);
  if Error = DBIERR_NONE then begin
    if Len = 0 then
      Result := null
    else begin
      Result := VarArrayCreate([1, Len], VarByte);
      VPtr := VarArrayLock(Result);
      try
        TffSrBaseCursor(FCursorID).BLOBRead(BLOBNr, 0, Len, VPtr^, BytesRead);
      finally
        VarArrayUnlock(Result);
      end;
    end;
  end;
end;

{!!.11 new}
procedure TffSqlFieldProxy.SetBlobValue(const Value: Variant);
{Rewritten !!.13}
var
  Offset   : Integer;
  BLOBNr   : TffInt64;
  Error,
  Len      : Longint;
  ValueLen : TffWord32;
  ValueLocked : Boolean;
  VPtr     : PAnsiChar;
  VStr     : string;
begin
  ValueLocked := False;
  try
    { Obtain the length of the BLOB data & a pointer to the data. }
    if TVarData(Value).VType and VarTypeMask = varByte then begin
      ValueLen := VarArrayHighBound(Value, 1);
      VPtr := VarArrayLock(Value);
      ValueLocked := True;
    end
    else begin
      VStr := VarToStr(Value);
      ValueLen := Length(VStr);
      VPtr := PAnsiChar(VStr);
    end;

    Offset := TffSrBaseCursor(FCursorID).Dictionary.FieldOffset[Index];
    BLOBNr := PffInt64(@OwnerTable.RecordBuffer^[Offset])^;

    { If there is already BLOB data, truncate it to the length of the
      new value. }
    if (BLOBNr.iLow <> 0) or (BLOBNr.iHigh <> 0) then begin
      Len := TffSrBaseCursor(FCursorID).BLOBGetLength(BLOBNr, Error);
      if TffWord32(Len) > ValueLen then
        TffSrBaseCursor(FCursorID).BLOBTruncate(BLOBNr, ValueLen);
      { If the new value is null then null the field in the record otherwise
        writ the new value over the old value. }
      if ValueLen = 0 then
        SetFieldToNull
      else
        { Write the new value over the old value. }
        TffSrBaseCursor(FCursorID).BLOBWrite(BLOBNr, 0, ValueLen, VPtr^);
    end
    else begin
      { This is a new BLOB. If it is null then set the field in the record to
        null. }
      if ValueLen = 0 then
        SetFieldToNull
      else if TffSrBaseCursor(FCursorID).BLOBAdd(BLOBNr) = DBIERR_NONE then begin
        { The BLOB has content & its creation was successful. Write the content
          to the table. }
        if TffSrBaseCursor(FCursorID).BLOBWrite(BLOBNr, 0, ValueLen, VPtr^) = DBIERR_NONE then
          WriteFieldDirect(@BLOBNr);
        TffSrBaseCursor(FCursorID).BLOBFree(BLOBNr);
      end;
    end;  { if..else }
  finally
    if ValueLocked then
      VarArrayUnlock(Value);
  end;
end;

function TFFSqlFieldProxy.GetValue: Variant;
var
  IsNull : Boolean;
  D : double;
  W : WideString;
  WC : WideChar;
  DT : TDateTime;
begin
  ReadField(IsNull);
  if IsNull then
    Result := Null
  else case GetType of
    fftBoolean :
      Result := Boolean(FieldBuffer^[0]);
    fftChar :
      Result := Char(FieldBuffer^[0]);
    fftWideChar :
      begin
        WC := PWideChar(FieldBuffer)^;
        W := WC;
        Result := W;
      end;
    fftByte :
      Result := PByte(FieldBuffer)^;
    fftWord16 :
      Result := PWord(FieldBuffer)^;
    fftWord32 :
      begin
        D := PffWord32(FieldBuffer)^;
        Result := D;
      end;
    fftInt8 :
      Result := PShortInt(FieldBuffer)^;
    fftInt16 :
      Result := PSmallInt(FieldBuffer)^;
    fftInt32 :
      Result := PInteger(FieldBuffer)^;
    fftAutoInc :
      begin
        D := PffWord32(FieldBuffer)^;
        Result := D;
      end;
    fftSingle :
      Result := PSingle(FieldBuffer)^;
    fftDouble :
      Result := PDouble(FieldBuffer)^;
    fftExtended :
      Result := PExtended(FieldBuffer)^;
    fftComp :
      Result := PComp(FieldBuffer)^;
    fftCurrency :
      Result := PCurrency(FieldBuffer)^;
    fftStDate :
      Result := StDateToDateTime(PStDate(FieldBuffer)^);
    fftStTime :
      Result := StTimeToDateTime(PStTime(FieldBuffer)^);
    fftDateTime :
      begin
        DT := PffDateTime(FieldBuffer)^ - 693594.0;
        Result := DT;
      end;
    fftShortString :
      Result := PShortString(FieldBuffer)^;
    fftShortAnsiStr :
      Result := PShortString(FieldBuffer)^;
    fftNullString :
      Result := StrPas(PChar(FieldBuffer));
    fftNullAnsiStr :
      Result := String(PChar(FieldBuffer));
    fftWideString :
      Result := WideString(PWideChar(FieldBuffer));
    fftBLOB..fftBLOBFile :                                             {!!.11}
      Result := GetBlobValue;                                          {!!.11}{!!.13}
    else
      Assert(False);
    end;
end;

function TFFSqlFieldProxy.IsNull: Boolean;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  {$IFOPT C+}
  Assert(TffSrBaseCursor(FCursorID).GetRecordField(FIndex, FOwnerTable.RecordBuffer,
    Result, FieldBuffer) = DBIERR_NONE);
  {$ELSE}
  TffSrBaseCursor(FCursorID).GetRecordField(FIndex, FOwnerTable.RecordBuffer,
    Result, FieldBuffer);
  {$ENDIF}
end;

function TFFSqlFieldProxy.Name: string;
begin
  Assert(FCursorID <> 0);
  Assert(TObject(FCursorID) is TffSrBaseCursor);
  Result := TffSrBaseCursor(FCursorID).Dictionary.FieldName[FIndex];
end;

procedure TFFSqlFieldProxy.WriteField;
begin
  TffSrBaseCursor(FCursorID).Dictionary.SetRecordField(FIndex,
    FOwnerTable.RecordBuffer, FieldBuffer);
end;

procedure TFFSqlFieldProxy.WriteFieldDirect(Buffer: PffByteArray);
begin
  TffSrBaseCursor(FCursorID).Dictionary.SetRecordField(FIndex,
    FOwnerTable.RecordBuffer, Buffer);
end;

{Begin !!.11}
procedure TffSqlFieldProxy.SetDefault;
begin
  TffSrBaseCursor(FCursorID).Dictionary.SetDefaultFieldValue
    (FOwnerTable.RecordBuffer, FIndex);
end;
{End !!.11}

procedure TFFSqlFieldProxy.SetFieldToNull;
begin
  TffSrBaseCursor(FCursorID).Dictionary.SetRecordFieldNull(
    FIndex, FOwnerTable.RecordBuffer, True);
end;

procedure TFFSqlFieldProxy.SetValue(const Value: Variant);
var
  S : string;
  W : WideString;
  FT : TffFieldType;
  ValueIsNull: Boolean;
  LenW : Word;                                                         {!!.11}
  Len : Integer;                                                       {!!.11}
begin
  ValueIsNull := VarIsNull(Value);
  if ValueIsNull then
    SetFieldToNull
  else begin
    FT := GetType;
    case FT of
    fftBoolean :
      Boolean(FieldBuffer^[0]) := Value;
    fftChar :
      begin
        S := Value;
        char(FieldBuffer^[0]) := S[1];
      end;
    fftWideChar :
      begin
        W := Value;
        PWideChar(FieldBuffer)^ := W[1];
      end;
    fftByte :
      PByte(FieldBuffer)^ := Value;
    fftWord16 :
      PWord(FieldBuffer)^ := Value;
    fftWord32 :
      PFFWord32(FieldBuffer)^ := Value;
    fftInt8 :
      PShortInt(FieldBuffer)^ := Value;
    fftInt16 :
      PSmallInt(FieldBuffer)^ := Value;
    fftInt32 :
      PInteger(FieldBuffer)^ := Value;
    fftAutoInc :
      PFFWord32(FieldBuffer)^ := Value;
    fftSingle :
      PSingle(FieldBuffer)^ := Value;
    fftDouble :
      PDouble(FieldBuffer)^ := Value;
    fftExtended :
      PExtended(FieldBuffer)^ := Value;
    fftComp :
      PComp(FieldBuffer)^ := Value;
    fftCurrency :
      PCurrency(FieldBuffer)^ := Value;
    fftStDate :
      PStDate(FieldBuffer)^ := DateTimeToStDate(Value);
    fftStTime :
      PStTime(FieldBuffer)^ := DateTimeToStTime(Value);
    fftDateTime :
      PffDateTime(FieldBuffer)^ := Value + 693594;
{Begin !!.11}
    fftShortString, fftShortAnsiStr :
      begin
        S := Value;
        FillChar(FieldBuffer^, FieldBufferLength, 0);
        LenW := FFMinI(Length(S), Pred(FieldBufferLength));
        FieldBuffer[0] := LenW;
        if S <> '' then                                                {!!.12}
          Move(S[1], FieldBuffer[1], LenW);
      end;
    fftNullString, fftNullAnsiStr :
      begin
        S := Value;
        FillChar(FieldBuffer^, FieldBufferLength, 0);
        Len := FFMinI(Length(S), Pred(FieldBufferLength));
        if S <> '' then                                                {!!.12}
          Move(S[1], FieldBuffer^, Len);
      end;
    fftWideString :
      begin
        W := Value;
        FillChar(FieldBuffer^, FieldBufferLength, 0);
        if W <> '' then                                                {!!.12}
          Move(W[1], FieldBuffer^,
             FFMinI(Length(W) * 2, FieldBufferLength - 2));
      end;
    fftBLOB..fftBLOBTypedBin :
{Begin !!.13}
      begin
        SetBLOBValue(Value);
        Exit;
      end;
{End !!.13}
{End !!.11}
    else
      Assert(False);
    end;
    WriteField;
  end;
end;

function TFFSqlFieldProxy.QualName: string;
begin
  Result := FOwnerTable.Name + '.' + Name;
end;

function TFFSqlFieldProxy.CanUpdate: Boolean;
begin
  case GetType of
  fftBoolean, fftChar, fftWideChar, fftByte,
  fftWord16, fftWord32, fftInt8, fftInt16,
  fftInt32, fftAutoInc, fftSingle, fftDouble,
  fftExtended, fftComp, fftCurrency, fftStDate,
  fftStTime, fftDateTime, fftShortString,
  fftShortAnsiStr, fftNullString, fftNullAnsiStr,
  fftBLOB..fftBLOBTypedBin,                                            {!!.11}
  fftWideString :
    Result := True;
  else
    Result := False;
  end;
end;

{!!.11 new}
procedure BMMakeTableS(const MatchString : ShortString; var BT : TBTable);
  {-Build a Boyer-Moore link table}
register;
asm
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
end;

{!!.11 new}
function BMSearchS(var Buffer; BufLength : DWord; const BT : TBTable;
  const MatchString : ShortString ; var Pos : Cardinal) : Boolean; assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string.}
register;
var
  BufPtr : Pointer;
asm
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
end;

{!!.13 new}
function BMSearchUCS(var Buffer; BufLength : Cardinal; const BT : TBTable;
  const MatchString : ShortString ; var Pos : Cardinal) : Boolean; assembler;
  {-Use the Boyer-Moore search method to search a buffer for a string. This
    search is not case sensitive.}
register;
var
  BufPtr : Pointer;
asm
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
end;

{!!.11 new}
function TFFSqlFieldProxy.BLOBBmSearch(const Table: TBTable; const SearchPhrase: string;
         IgnoreCase: Boolean): Boolean;
const
  BufferSize = 4096;
var
  Offset        : Integer;
  BLOBNr        : TffInt64;
  Error, Len    : Integer;
  BytesRead     : TffWord32;
  Pos           : Cardinal;
  ChunkSize,
  ChunkOffset   : Integer;
  Buffer        : array[0..BufferSize-1] of char;
begin
  Result := False;
  Offset := TffSrBaseCursor(FCursorID).Dictionary.FieldOffset[Index];
  BLOBNr := PffInt64(@OwnerTable.RecordBuffer^[Offset])^;
  Len := TffSrBaseCursor(FCursorID).BLOBGetLength(BLOBNr, Error);
  if Error = DBIERR_NONE then begin
    ChunkOffset := 0;
    ChunkSize := BufferSize - length(SearchPhrase);
    while Len > 0 do begin
      TffSrBaseCursor(FCursorID).BLOBRead(BLOBNr, ChunkOffset, BufferSize, Buffer, BytesRead);
      {!!.13 begin}
      if IgnoreCase then begin
        if BMSearchUCS(Buffer, BytesRead, Table, SearchPhrase, Pos) then begin
          Result := True;
          exit;
        end;
      end else begin
        if BMSearchS(Buffer, BytesRead, Table, SearchPhrase, Pos) then begin
          Result := True;
          exit;
        end;
      end;
      {!!.13 end}
      dec(Len, ChunkSize);
      inc(ChunkOffset, ChunkSize);
    end;
  end;
end;

{!!.11 new}
function TFFSqlFieldProxy.BMMatch(const Table: TBTable; const SearchPhrase: string;
                                        IgnoreCase: Boolean): Boolean; {!!.13}
var
  S: string;
  Pos: Cardinal;
begin
  if IsNull then
    Result := False
  else if GetType = fftBLOBMemo then
    Result := BLOBBmSearch(Table, SearchPhrase, IgnoreCase)            {!!.13}
  else begin
    S := GetValue;
    {!!.13 begin
    Result := (S <> '') and BMSearchS(S[1], length(S), Table, SearchPhrase, Pos);
    }
    Result := False;
    if S <> '' then
      if IgnoreCase then begin
        if BMSearchUCS(S[1], length(S), Table, SearchPhrase, Pos) then
          Result := True;
      end else
        if BMSearchS(S[1], length(S), Table, SearchPhrase, Pos) then
          Result := True;
    {!!.13 end}
  end;
end;

type
  TffHashNodeFriend = class(TffHashNode);

{===TffNRecordHash========================================================}
procedure TffNRecordHash.Add;
var
  keyPtr : PffNRecordHashEntry;
  i, Size : Integer;
begin
  Size := EntrySlots * sizeOf(TffInt64);
  FFGetMem(keyPtr, Size);
  for i := 0 to pred(EntrySlots) do
    KeyPtr^[i] := FSourceTables[i].GetCurrentRecordID;
  //store size of record in hash entry's value field for destruction
  {$IFOPT C+}
  Assert(fhAddPrim(keyPtr, Pointer(Size)));
  {$ELSE}
  fhAddPrim(keyPtr, Pointer(Size));
  {$ENDIF}
end;
{--------}
procedure TffNRecordHash.AddTable(const SourceTable: TFFSqlTableProxy);
begin
  FFReallocMem(FSourceTables,
    EntrySlots * sizeof(TFFSqlTableProxy),
    succ(EntrySlots) * sizeof(TFFSqlTableProxy));
  inc(EntrySlots);
  FSourceTables^[EntrySlots - 1] := SourceTable;
end;
{--------}
constructor TffNRecordHash.Create;
begin
  inherited Create(ffc_Size2099);
end;
{--------}
destructor TffNRecordHash.Destroy;
begin
  if FSourceTables <> nil then
    FFFreeMem(FSourceTables, EntrySlots * sizeof(TFFSqlTableProxy));
  inherited Destroy;
end;
{--------}
function TffNRecordHash.fhCompareKey(const aKey1 : Pointer;
                                const aKey2 : Pointer) : Boolean;
var
  i : Integer;
begin
  for i := 0 to pred(EntrySlots) do
    if FFCmpI64(PffNRecordHashEntry(aKey1)^[i], PffNRecordHashEntry(aKey2)^[i]) <> 0 then begin
      Result := False;
      exit;
    end;
  Result := True;
end;
{--------}
procedure TffNRecordHash.fhFreeKeyPrim(aKey : pointer);
begin
  FFFreeMem(aKey, EntrySlots * sizeOf(TffInt64));
end;
{--------}
function TffNRecordHash.fhGetIndex(const AKey   : Pointer;
                              const ACount : Integer): Integer;
var
  X : TffInt64;
  I : Integer;
begin
  X := PffNRecordHashEntry(aKey)^[0];
  for i := 1 to pred(EntrySlots) do begin
    X.iLow := X.iLow xor PffNRecordHashEntry(aKey)^[i].iLow;
    X.iHigh := X.iHigh xor PffNRecordHashEntry(aKey)^[i].iHigh;
  end;
  Result := ffI64ModInt(X, ACount);
end;
{--------}
function TffNRecordHash.Exists: Boolean;
var
  I  : integer;
  Node : TffHashNode;
  keyPtr : PffNRecordHashEntry;
begin

  FFGetMem(keyPtr, EntrySlots * sizeOf(TffInt64));
  try
    for i := 0 to pred(EntrySlots) do
      KeyPtr^[i] := FSourceTables[i].GetCurrentRecordID;
    Result := fhFindPrim(KeyPtr, I, Node);
  finally
    FFFreeMem(keyPtr, EntrySlots * sizeOf(TffInt64));
  end;
end;

function TffNRecordHash.fhCreateNode: TffHashNode;
begin
  Result := TffNRecordHashNode.Create;
end;
{--------}


{ TffNRecordHashNode }

destructor TffNRecordHashNode.Destroy;
begin
  assert(TObject(Self) is TffNRecordHashNode);
  assert(fhValue <> nil);
  inherited;
end;

procedure CopyField(const SourceField, TargetField: TffSqlFieldProxy);
var
  IsNull: Boolean;
begin
  Assert(SourceField.GetType = TargetField.GetType);
  SourceField.ReadField(IsNull);
  if not IsNull then
    TargetField.WriteFieldDirect(SourceField.FieldBuffer)
  else
    TargetField.SetFieldToNull;
end;

procedure CopyBLOBField(const SourceField,
                              TargetField : TffSqlFieldProxy);
var
  IsNull         : Boolean;
  SrcOffset,
  TgtOffset      : Integer;
  aSrcBLOBNr,
  aBLOBNr        : TffInt64;
  aLinkTableName : TffTableName;                                       {!!.11 - New}
begin
  Assert(SourceField.GetType = TargetField.GetType);
  SourceField.ReadField(IsNull);
  if (not IsNull) then begin
    Assert(TObject(SourceField.FCursorID) is TffSrBaseCursor);
    SrcOffset := TffSrBaseCursor(SourceField.FCursorID).Dictionary.FieldOffset[SourceField.Index];
    TgtOffset := TffSrBaseCursor(TargetField.FCursorID).Dictionary.FieldOffset[TargetField.Index];
    { link the BLOBs }
    { Get the BLOB reference out of the record. }
    aSrcBLOBNr := PffInt64(@SourceField.OwnerTable.RecordBuffer^[SrcOffset])^;

    with TffSrBaseCursor(TargetField.FCursorID) do begin               {!!.11 - Start}
      { Clear the null flag for the target field. }                     {!!.10}
      Dictionary.SetRecordFieldNull(TargetField.Index,                  {!!.10}
                                    TargetField.OwnerTable.RecordBuffer,{!!.10}
                                    False);                             {!!.10}

      { Is aSrcBLOBNr another BLOB Link? }
      if (TffSrBaseCursor(SourceField.FCursorID).BLOBIsLink(aSrcBLOBNr,
                                                            aLinkTableName,
                                                            aSrcBLOBNr)) then begin

        { Yes. BLOBIsLink filled in the TableName and updated aSrcBLOBNr. }
        BLOBLinkAdd(aLinkTableName,
                    aSrcBLOBNr,
                    aBLOBNr);
      end else begin
        { Add a BLOB link. }
        BLOBLinkAdd(TffSrBaseCursor(SourceField.FCursorID).Table.BaseName,
                    aSrcBLOBNr,
                    aBLOBNr);
      end;
    end;                                                               {!!.11 - End}

    { Update the BLOB reference in the record. }
    PffInt64(@TargetField.OwnerTable.RecordBuffer^[TgtOffset])^ := aBLOBNr;
  end else
    TargetField.SetFieldToNull;
end;

function CompatibleFields(const SourceField, TargetField: TffSqlFieldProxy): Boolean;
begin
  Result := (SourceField.GetType = TargetField.GetType)
    and (SourceField.FieldBufferLength = TargetField.FieldBufferLength);
end;

{ TFFFieldCopier }

procedure TFFFieldCopier.Add(SourceField, TargetField: TffSqlFieldProxy);
begin
  FSourceList.Append(SourceField);
  FTargetList.Append(TargetField);
  if CompatibleFields(SourceField, TargetField) then begin
    FCompatible.Append(Pointer(1));
    case SourceField.GetType of
    fftBLOB..fftBLOBFile :
      FBlob.Append(Pointer(1));
    else
      FBlob.Append(Pointer(0));
    end;
  end else begin
    FCompatible.Append(Pointer(0));
    FBlob.Append(Pointer(0));
  end;
end;

constructor TFFFieldCopier.Create;
begin
  inherited Create;
  FSourceList := TffPointerList.Create;
  FTargetList := TffPointerList.Create;
  FCompatible := TffPointerList.Create;
  FBlob := TffPointerList.Create;
end;

destructor TFFFieldCopier.Destroy;
begin
  FSourceList.Free;
  FTargetList.Free;
  FCompatible.Free;
  FBlob.Free;
  inherited;
end;

procedure TFFFieldCopier.Execute;
var
  i : Integer;
begin
  for i := 0 to pred(FSourceList.Count) do
    if FCompatible[i] <> nil then
      if FBlob[i] <> nil then
        CopyBLOBField(
          TffSqlFieldProxy(FSourceList[i]),
          TffSqlFieldProxy(FTargetList[i]))
      else
        CopyField(
          TffSqlFieldProxy(FSourceList[i]),
          TffSqlFieldProxy(FTargetList[i]))
    else
      TffSqlFieldProxy(FTargetList[i]).SetValue(
        TffSqlFieldProxy(FSourceList[i]).GetValue);
end;

{ TffSqlFieldDefList }

procedure TffSqlFieldDefList.AddField(const aName: string;
  aType: TffFieldType; aUnit, aDec: Integer);
var
  NewEntry : PFFSqlFieldDefProxyRec;
begin
  FFGetZeroMem(NewEntry, sizeof(NewEntry^));
  NewEntry.FieldName := aName;
  NewEntry.FieldType := aType;
{Begin !!.13}
  { If this field is of type string and the units are set to zero then this
    is probably a scalar function that is being applied to a BLOB. Set the # of
    units to 255. The value 255 sounds good because the actual size may vary &
    we cannot predict what it will be. }
  if (aType in [fftShortString..fftWideString]) and (aUnit = 0) then
    NewEntry.FieldUnits := 255
  else
    NewEntry.FieldUnits := aUnit;
{End !!.13}
  NewEntry.Decimals := aDec;
  FieldList.Append(NewEntry);
end;

constructor TffSqlFieldDefList.Create;
begin
  inherited Create;
  FieldList := TffPointerList.Create;
end;

destructor TffSqlFieldDefList.Destroy;
var
  i: Integer;
  P : PFFSqlFieldDefProxyRec;
begin
  for i := 0 to pred(FieldList.Count) do begin
    P := PFFSqlFieldDefProxyRec(FieldList[i]);
    P^.FieldName := '';
    FFFreeMem(P, sizeof(TFFSqlFieldDefProxyRec));
  end;
  FieldList.Free;
  inherited;
end;

function TffSqlFieldDefList.GetCount: Integer;
begin
  Result := FieldList.Count;
end;

function TffSqlFieldDefList.GetFieldDecimals(Index: Integer): Integer;
begin
  case FieldType[Index] of
  fftSingle..fftExtended, fftCurrency :
    Result := PFFSqlFieldDefProxyRec(FieldList[Index])^.Decimals;
  else
    Result := 0;
  end;
end;

function TffSqlFieldDefList.GetFieldName(Index: Integer): string;
begin
  Result := PFFSqlFieldDefProxyRec(FieldList[Index])^.FieldName;
end;

function TffSqlFieldDefList.GetFieldType(Index: Integer): TffFieldType;
begin
  Result := PFFSqlFieldDefProxyRec(FieldList[Index])^.FieldType;
end;

function TffSqlFieldDefList.GetFieldUnits(Index: Integer): Integer;
begin
  case FieldType[Index] of
  fftChar,
  fftWideChar :
    Result := 1;
  fftAutoInc :
    Result := 10;
  fftByteArray..fftWideString :
    Result := PFFSqlFieldDefProxyRec(FieldList[Index])^.FieldUnits;
  else
    Result := 0;
  end;
end;

end.

