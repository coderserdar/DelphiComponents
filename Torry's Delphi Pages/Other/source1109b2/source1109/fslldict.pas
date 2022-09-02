{NOTES:
   1. Have verification as optional--IFDEF'd out}

{$I fsdefine.inc}

Unit fslldict;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  Dialogs,
  FsConst,
  fsllbase,
  fsindexhelper,
  fssrmgr,
  fssrbase,
  fsllexcp,
  fsstdate,
  fsfunInterp;

{---Data dictionary class---}
Type

  PfsFieldDescriptorArray = ^TfsFieldDescriptorArray;
  TfsFieldDescriptorArray = Array[Word] Of PffFieldDescriptor;

  PfsIndexDescriptorArray = ^TfsIndexDescriptorArray;
  TfsIndexDescriptorArray = Array[0..Pred(fscl_MaxIndexes)] Of PffIndexDescriptor;

  PfsIndexHelperArray = ^TfsIndexHelperArray;
  TfsIndexHelperArray = Array[0..Pred(fscl_MaxIndexes),
    0..Pred(fscl_MaxIndexFlds)] Of TfsSrIndexHelper;

  TfsTriBool = (fftbUnknown, fftbTrue, fftbFalse); {!!.03}

  TFSInfoDict = Class(TPersistent)
  Protected {private}
    FBLOBFileNumber: Integer; {file number for BLOBs}
    FFieldCapacity: Longint; {the number of fields the FieldDescriptor
    array has been sized to hold }
    FFldCount: Integer; {count of fields--duplicate for speed}
    FHasBLOBs: TfsTriBool; {True if table contains any BLOB fields} {!!.03}
    FIndexCapacity: Longint; {the number of indices the IndexDescriptor
    array has been sized to hold }
    FInxCount: Integer; {count of indexes--duplicate for speed}
    FFileCount: Integer; {count of files--duplicate for speed}
    FBaseName: TfsTableName; {the base name for the table}
    FLogRecLen: Longint; {logical rec length--dupe for speed}
    FIsEncrypted: Boolean; {true is files are encrypted}
    FEngineDeleteType: TRecoveryEngine; // for delete record
    fVersionRecord: TVersionRecord;
    ddFileList: TList; {list of files}
    ddDefFldList: TList; {list of field numbers that have defaults}

    ddReadOnly: Boolean; {true if the dictionary cannot be updated}
    ddTableType: TTableType;
    fcascadedelete: TStringList;
    fcascadeupdate: TStringList;
    fcascadedeletenull: TStringList;
    fcascadeupdatenull: TStringList;
    fcascadeinsert: TStringList;
    frestrictdelete: TStringList;
    frestrictupdate: TStringList;
    fNoActionUpdate: TStringList;

    fTrigersBeforeDelete: TStringList;
    fTrigersAfterDelete: TStringList;
    fTrigersBeforeUpdate: TStringList;
    fTrigersAfterUpdate: TStringList;
    fTrigersBeforeInsert: TStringList;
    fTrigersAfterInsert: TStringList;

    fTrigersBeforeDeleteOperation: TStringList;
    fTrigersAfterDeleteOperation: TStringList;
    fTrigersBeforeUpdateOperation: TStringList;
    fTrigersAfterUpdateOperation: TStringList;
    fTrigersBeforeInsertOperation: TStringList;
    fTrigersAfterInsertOperation: TStringList;

    fIsAnyEmptyAsNull: Boolean;
    fIsAnyRound: Boolean;
    fIsAnyDefault: boolean;
    fIsRecVersion: boolean;

    Procedure AnsiStringWriter(Const aString: String; aWriter: TWriter);
    Procedure ddExpandFieldArray(Const minCapacity: Longint);
    Procedure ddExpandIndexArray(Const minCapacity: Longint);
    Function GetBaseRecordLength: Longint;
    Function GetBlockSize: Longint;
    Function GetBookmarkSize(aIndexID: Integer): Integer;
    Function GetDefaultFldCount: Integer;
    Function GetFieldDecPl(aField: Integer): Longint;
    Function GetFieldDisplay(aField: Integer): TffDictItemDesc;
    Function GetFieldEmptyAsNull(aField: Integer): Boolean;
    Function GetFieldDefaultUpdate(aField: Integer): TDefaultUpdate;
    Function GetFieldDescription(aField: Integer): TffDictDescription;
    Function GetFieldLength(aField: Integer): Longint;
    Function GetFieldName(aField: Integer): TffDictItemName;
    Function GetFieldOffset(aField: Integer): Longint;
    Function GetFieldRequired(aField: Integer): boolean;
    Function GetFieldType(aField: Integer): TfsFieldType;
    Function GetFieldBlobLevelComp(aField: Integer): TDataCompLevel;
    Function GetFieldRound(aField: Integer): TRound;
    Function GetFieldUnits(aField: Integer): Longint;
    Function GetFieldVCheck(aField: Integer): PffVCheckDescriptor;
    Function GetFileBlockSize(aFile: Integer): Longint;
    Function GetFileDesc(aFile: Integer): TffDictItemDesc;
    Function GetFileDescriptor(aFile: Integer): PffFileDescriptor;
    Function GetFileExt(aFile: Integer): TffExtension;
    Function GetFileNameExt(aFile: Integer): TffFileNameExt;
    Function GetFileType(aFile: Integer): TffFileType;
    Function GetHasBLOBs: Boolean; {!!.03}
    Function GetIndexAllowDups(aIndexID: Integer): boolean;
    Function GetIndexAscend(aIndexID: Integer): boolean;
    Function GetIndexDesc(aIndexID: Integer): TffDictItemDesc;
    Function GetIndexFileNumber(aIndexID: Integer): Longint;
    Function GetIndexKeyLength(aIndexID: Integer): Longint;
    Function GetIndexName(aIndexID: Integer): TffDictItemName;
    Function GetIndexNoCase(aIndexID: Integer): Boolean;
    Function GetIndexType(aIndexID: Integer): TfsIndexType;
    Function GetRecordLength: Longint;
    Procedure CheckForDefault(aVCheckDesc: PffVCheckDescriptor;
      aFieldDesc: PffFieldDescriptor);
    Procedure SetBlockSize(BS: Longint);
    Procedure SetIsEncrypted(IE: Boolean);
    Procedure SetEngineDeleteType(Value: TRecoveryEngine);
    Procedure SetVersionRecord(Value: TVersionRecord);
    Procedure SetTableType(TableType: TTableType);
  Protected
    Procedure OldReadFromStream(SignStream: Byte; Reader: TReader; S: TStream);
    Procedure ClearPrim(InclFileZero: boolean);
    Function CreateFieldDesc(Const aIdent: TffDictItemName;
      Const aDesc: TffDictItemDesc;
      aType: TfsFieldType;
      aUnits: Integer;
      aDecPl: Integer;
      aReqFld: Boolean;
      Const aValCheck: PffVCheckDescriptor;
      aBlobLevelComp: TDataCompLevel;
      aDescription: TffDictDescription;
      aRound: TRound;
      aEmptyAsNull: boolean;
      aDefaultUpdate: TDefaultUpdate)
      : PffFieldDescriptor;
    Function CreateFileDesc(Const aDesc: TffDictItemDesc;
      Const aExtension: TffExtension;
      aBlockSize: Longint;
      aType: TffFileType): PffFileDescriptor;
    Function CreateIndexDesc(Const aIdent: TffDictItemName;
      Const aDesc: TffDictItemDesc;
      aFile: Integer;
      aFldCount: Integer;
      Const aFldList: TffFieldList;
      Const aFieldsAscDesc: TffFieldList;
      Const aFieldsCase: TffFieldList;
      Const aFieldsSize: TffFieldList;
      Const aFieldsFlags: TffFieldList;
      Const aFieldsNullTop: TffFieldList;
      Const aFldIHList: TffFieldIHList;
      aAllowDups: Boolean): PffIndexDescriptor;
    Function CreateUserIndexDesc(Const aIdent: TffDictItemName;
      Const aDesc: TffDictItemDesc;
      aFile: Integer;
      aKeyLength: Integer;
      aAllowDups: Boolean): PffIndexDescriptor;

  Public
    FieldDescriptor: PfsFieldDescriptorArray;
    { Array of field information for the fields in this dictionary.
      Declared as a public array for speed reasons. }

    IndexDescriptor: PfsIndexDescriptorArray;
    { Array of index information for the indexes in this dictionary.
      Declared as a public array for speed reasons. }

    IndexHelpers: PfsIndexHelperArray;
    { Index helper objects for composite indices
      declared public (instead of private + public propert)
      for speed reasons}

    Class Function NewInstance: TObject; Override;
    Procedure FreeInstance; Override;

  Public
    FUserName: TffName;
    VersionStream: Integer;
    Constructor Create(aBlockSize: Longint);
    {-Create the instance, aBlockSize is the eventual block size
      of the data file component of the table}
    Destructor Destroy; Override;
    {-Destroy the instance}

    Function AddFile(Const aDesc: TffDictItemDesc;
      Const aExtension: TffExtension;
      aBlockSize: Longint;
      aFileType: TffFileType): Integer;
    {-Add a file to the data dictionary (the actual file name will
      be the base table name plus aExtension); result is the index
      of the newly-added file in the file list}
    Procedure AddIndex(Const aIdent: TffDictItemName;
      Const aDesc: TffDictItemDesc;
      aFile: Integer;
      aFldCount: Integer;
      Const aFldList: TffFieldList;
      Const aFieldsAscDesc: TffFieldList;
      Const aFieldsCase: TffFieldList;
      Const aFieldsSize: TffFieldList;
      Const aFieldsFlags: TffFieldList;
      Const aFieldsNullTop: TffFieldList;
      Const aFldIHList: TffFieldIHList;
      aAllowDups: boolean);
    {-Add an extended index to the data dictionary}
    Procedure AddUserIndex(Const aIdent: TffDictItemName;
      Const aDesc: TffDictItemDesc;
      aFile: Integer;
      aKeyLength: Integer;
      aAllowDups: boolean);
    {-Add a user defined index to the dictionary}
    Procedure AddField(Const aIdent: TffDictItemName;
      Const aDesc: TffDictItemDesc;
      aType: TfsFieldType;
      aUnits: Integer;
      aDecPl: Integer;
      aReqFld: Boolean;
      Const aValCheck: PffVCheckDescriptor;
      aBlobLevelComp: TDataCompLevel;
      aDescription: TffDictDescription;
      aRound: TRound;
      aEmptyAsNull: boolean;
      aDefaultUpdate: TDefaultUpdate);
    {-Append a field to the end of the data dictionary's field list}
    Procedure Assign(Source: TPersistent); Override;
    {-Assign a data dictionary's data}
    Procedure BindIndexHelpers;
    {-Binds the TfsSrIndexHelper objects to the dictionary}
    Procedure CheckValid;
    {-Raise an exception if the dictionary is invalid}
    Procedure Clear;
    {-Delete all field/index data from the data dictionary}
    Procedure ExtractKey(aIndexID: Integer;
      aData: PffByteArray;
      aKey: PffByteArray);
    {-Given a record buffer and an index number, extract the key
      for that index from the record}
    Function GetFieldFromName(Const aFieldName: TffDictItemName): Integer;
    {-Return the field number for a given field name, or -1 if not
      found}
    Function GetIndexFromName(Const aIndexName: TffDictItemName): Integer;
    {-Return the index number for a given index name, or -1 if not
      found}
    Function HasAutoIncField(Var aField: Integer): boolean;
    {-Return true and the index of the first autoinc field in the
      dictionary}
    Procedure InsertField(AtIndex: Integer;
      Const aIdent: TffDictItemName;
      Const aDesc: TffDictItemDesc;
      aType: TfsFieldType;
      aUnits: Integer;
      aDecPl: Integer;
      aReqFld: Boolean;
      Const aValCheck: PffVCheckDescriptor;
      aBlobLevelComp: TDataCompLevel;
      aDescription: TffDictDescription;
      aRound: TRound;
      aEmptyAsNull: Boolean;
      aDefaultUpdate: TDefaultUpdate);
    {-Insert a field into the data dictionary's field list}
    Function IsIndexDescValid(Const aIndexDesc: TffIndexDescriptor): boolean;
    {-Return true if the given index descriptor defines a valid index}
    Procedure RemoveField(aField: Longint);
    {-Remove a field from the data dictionary's field list}
    Procedure RemoveFile(aFile: Longint);
    {-Remove a file from the data dictionary; if index file, the
      relevant indexes are also removed}
    Procedure RemoveIndex(aIndex: Longint);
    {-Remove an index from the data dictionary's index list}

  {===Validity check routines===}
    Procedure SetValidityCheck(aField: Integer;
      Var aExists: boolean;
      Const aVCheck: TffVCheckDescriptor);
    {-Set a field's validity check record}
    Function FieldIndex(aFieldName: String): Integer;
    Function HasSameFields(aSrcDict: TFSInfoDict;
      Var aBLOBFields: TfsPointerList): boolean;
    {-Use this method to verify a dictionary has the same field types,
      sizes, and ordering as a source dictionary. Returns True if the
      field information matches otherwise returns False. Note that the
      fields may have different names. If the record contains any
      BLOB fields, the number of each BLOB field is stored in output
      parameter aBLOBFields. }

    Function HasSameFieldsEx(aSrcDict: TFSInfoDict;
      aFields: PffLongintArray;
      aNumFields: Integer;
      Var aBLOBFields: TfsPointerList): boolean;
    {-Use this method to verify a dictionary has the same field types,
      sizes, and ordering as the specified fields within a source
      dictionary. Returns True if the field information matches otherwise
      returns False. Note that the fields may have different names. If the
      record contains any BLOB fields, the number of each BLOB field is
      stored in output parameter aBLOBFields. }

  {===record utility routines===}
    Function CheckRequiredRecordFields(aData: PffByteArray): boolean;
    {-Given a record buffer, checks that all required fields are
      non-null}
    Procedure GetRecordField(aField: Integer;
      aData: PffByteArray;
      Var aIsNull: boolean;
      aValue: pointer);
    {-Given a record buffer, read the required field; aIsNull is
      set to true if the field is null (no data is written to
      aValue)}
    Procedure InitRecord(aData: PffByteArray);
    {-Given a record buffer, initialize it so that all fields are
      null}
    Function IsRecordFieldNull(aField: Integer;
      aData: PffByteArray): boolean;
    {-Given a record buffer, return true if the field is null}
    Procedure SetRecordField(aField: Integer;
      aData: PffByteArray;
      aValue: pointer);
    {-Given a record buffer, write the required field from the
      buffer pointed to by aValue; if aValue is nil, the field is
      set to null}
    Procedure SetRecordFieldNull(aField: Integer;
      aData: PffByteArray;
      aIsNull: boolean);
    {-Given a record buffer, set the required field to null or
      non-null. Set the field in the record to binary zeros.}

    Procedure SetBaseName(Const BN: TfsTableName);
    {-Set the internal table base name - used for error messages}

{Begin !!.11}
    Procedure SetDefaultFieldUpdateValue(aData: PffByteArray;
      Const aField: Integer);
    { If the field has a default value, this method sets the field to that
      value. }
{End !!.11}

    Procedure SetDefaultFieldUpdateValues(aData, oldData: PffByteArray);
    {-Set any null fields to their default field, if the field
      has a default value}

    Property BLOBFileNumber: Integer
      Read FBLOBFileNumber;
    {-The file number of the file that holds the BLOBs}
    Property BlockSize: Longint
      Read GetBlockSize Write SetBlockSize;
    {-The block size of the table to which this dictionary refers;
      equals FileBlockSize[0] the block size of the base file}
    Property BookmarkSize[aIndexID: Integer]: Integer
    Read GetBookmarkSize;
    {-The length of a bookmark for the given index}
    Property DefaultFieldCount: Integer
      Read GetDefaultFldCount;
    {-Number of fields with default values}
    Property IsEncrypted: boolean
      Read FIsEncrypted Write SetIsEncrypted;
    Property EngineDeleteType: TRecoveryEngine Read FEngineDeleteType Write SetEngineDeleteType;
    Property TableType: TTableType Read ddTableType Write setTableType Default ttdata;
    Property VersionRecord: TVersionRecord Read fVersionRecord Write SetVersionRecord;
    //Property TypeEncrypt:
    {-Whether the files comprising the table are encrypted}
    Property FieldCount: Integer
      Read FFldCount;
    {-The number of fields in the data dictionary}
    Property FieldDecPl[aField: Integer]: Longint
    Read GetFieldDecPl;
    {-The decimal places value for a given field in the data dictionary}
    Property FieldDisplay[aField: Integer]: TffDictItemDesc
    Read GetFieldDisplay;
    Property FieldDescription[aField: Integer]: TffDictDescription
    Read GetFieldDescription;
    {-The description of a given field in the data dictionary}
    Property FieldLength[aField: Integer]: Longint
    Read GetFieldLength;
    {-The length in bytes of a given field in the data dictionary}
    Property FieldName[aField: Integer]: TffDictItemName
    Read GetFieldName;
    {-The name of a given field in the data dictionary}
    Property FieldOffset[aField: Integer]: Longint
    Read GetFieldOffset;
    {-The offset of a given field in the record in the data dictionary}
    Property FieldRequired[aField: Integer]: boolean
    Read GetFieldRequired;
    {-Whether the field is required or not}
    Property FieldType[aField: Integer]: TfsFieldType
    Read GetFieldType;
    {-The type of a given field in the data dictionary}
    Property FieldBlobLevelComp[aField: Integer]: TDataCompLevel
    Read GetFieldBlobLevelComp;

    Property FieldRound[aField: Integer]: TRound Read GetFieldRound;
    Property FieldEmptyAsNull[aField: Integer]: Boolean Read GetFieldEmptyAsNull;
    Property FieldDefaultUpdate[aField: Integer]: TDefaultUpdate Read GetFieldDefaultUpdate;
    Property FieldUnits[aField: Integer]: Longint
    Read GetFieldUnits;
    {-The units value for a given field in the data dictionary}
    Property FieldVCheck[aField: Integer]: PffVCheckDescriptor
    Read GetFieldVCheck;
    {-The validity check info for a given field}

    Property FileBlockSize[aFile: Integer]: Longint
    Read GetFileBlockSize;
    {-The block size of a given file in the data dictionary}
    Property FileCount: Integer
      Read FFileCount;
    {-The number of files in the data dictionary}
    Property FileDesc[aFile: Integer]: TffDictItemDesc
    Read GetFileDesc;
    {-The description of a given file in the data dictionary}
    Property FileDescriptor[aFile: Integer]: PffFileDescriptor
    Read GetFileDescriptor;
    {-The descriptor of a given file in the data dictionary}
    Property FileExt[aFile: Integer]: TffExtension
    Read GetFileExt;
    {-The extension of a given file in the data dictionary}
    Property DiskFileName[aFile: Integer]: TffFileNameExt
    Read GetFileNameExt;
    {-The disk name of a given file in the data dictionary}
    Property FileType[aFile: Integer]: TffFileType
    Read GetFileType;
    {-The type of file: data, index or BLOB}
    Property HasBLOBFields: Boolean {!!.03}
    Read GetHasBLOBs; {!!.03}
    {-Returns True if the table contains any BLOB fields. }{!!.03}
    Property IndexAllowDups[aIndexID: Integer]: boolean
    Read GetIndexAllowDups;
    {-Whether the given index allows duplicate keys}
    Property IndexIsAscending[aIndexID: Integer]: boolean
    Read GetIndexAscend;
    {-Whether the given index has keys in ascending order}
    Property IndexIsCaseInsensitive[aIndexID: Integer]: boolean
    Read GetIndexNoCase;
    {-Whether the given index has keys in ascending order}
    Property IndexCount: Integer
      Read FInxCount;
    {-The number of indexes in the data dictionary}
    Property IndexDesc[aIndexID: Integer]: TffDictItemDesc
    Read GetIndexDesc;
    {-The description of a given index in the data dictionary}
    Property IndexFileNumber[aIndexID: Integer]: Longint
    Read GetIndexFileNumber;
    {-The descriptor of a given index in the data dictionary}
    Property IndexKeyLength[aIndexID: Integer]: Longint
    Read GetIndexKeyLength;
    {-The key length for the given index}
    Property IndexName[aIndexID: Integer]: TffDictItemName
    Read GetIndexName;
    {-The name of a given field in the data dictionary}
    Property IndexType[aIndexID: Integer]: TfsIndexType
    Read GetIndexType;
    {-The type of the given index}

    Property RecordLength: Longint
      Read GetRecordLength;
    {-The length of the physical record for the data dictionary.  Includes
      trailing byte array to identify null fields. }
    Property LogicalRecordLength: Longint
      Read GetBaseRecordLength;
    {-The length of the logical record for the data dictionary (ie
      just the total size of the fields. }

    Property CascadeDelete: TStringList Read fcascadedelete Write fcascadedelete;
    Property CascadeUpdate: TStringList Read fcascadeupdate Write fcascadeupdate;
    Property CascadeDeleteNull: TStringList Read fcascadedeleteNull Write fcascadedeleteNull;
    Property CascadeUpdateNull: TStringList Read fcascadeupdateNull Write fcascadeupdateNull;
    Property CascadeInsert: TStringList Read fcascadeinsert Write fcascadeinsert;
    Property RestrictDelete: TStringList Read frestrictdelete Write frestrictdelete;
    Property RestrictUpdate: TStringList Read frestrictupdate Write frestrictupdate;
    Property NoActionUpdate: TStringList Read fNoActionUpdate Write fNoActionUpdate;

    Property TrigersBeforeDelete: TStringList Read fTrigersBeforeDelete Write fTrigersBeforeDelete;
    Property TrigersAfterDelete: TStringList Read fTrigersAfterDelete Write fTrigersAfterDelete;
    Property TrigersBeforeUpdate: TStringList Read fTrigersBeforeUpdate Write fTrigersBeforeUpdate;
    Property TrigersAfterUpdate: TStringList Read fTrigersAfterUpdate Write fTrigersAfterUpdate;
    Property TrigersBeforeInsert: TStringList Read fTrigersBeforeInsert Write fTrigersBeforeInsert;
    Property TrigersAfterInsert: TStringList Read fTrigersAfterInsert Write fTrigersAfterInsert;

    Property TrigersBeforeDeleteOperation: TStringList Read fTrigersBeforeDeleteOperation Write fTrigersBeforeDeleteOperation;
    Property TrigersAfterDeleteOperation: TStringList Read fTrigersAfterDeleteOperation Write fTrigersAfterDeleteOperation;
    Property TrigersBeforeUpdateOperation: TStringList Read fTrigersBeforeUpdateOperation Write fTrigersBeforeUpdateOperation;
    Property TrigersAfterUpdateOperation: TStringList Read fTrigersAfterUpdateOperation Write fTrigersAfterUpdateOperation;
    Property TrigersBeforeInsertOperation: TStringList Read fTrigersBeforeInsertOperation Write fTrigersBeforeInsertOperation;
    Property TrigersAfterInsertOperation: TStringList Read fTrigersAfterInsertOperation Write fTrigersAfterInsertOperation;

    Property IsAnyEmptyAsNull: Boolean Read fIsAnyEmptyAsNull Write fIsAnyEmptyAsNull;
    Property IsAnyRound: Boolean Read fIsAnyRound Write fIsAnyRound;
    Property IsAnyDefault: Boolean Read fIsAnyDefault Write fIsAnyDefault;
    Property IsRecVersion: Boolean Read fIsRecVersion Write fIsRecVersion;

    Procedure ReadFromStream(S: TStream);
    Procedure WriteToStream(S: TStream);

  End;

  {===Key manipulation routines===}{moved here from FFTBBASE}
Procedure FSInitKey(aKey: PffByteArray;
  aKeyLen: Integer;
  aKeyFldCount: Integer);
Function FSIsKeyFieldNull(aKey: PffByteArray;
  aKeyLen: Integer;
  aKeyFldCount: Integer;
  aKeyFld: Integer): boolean;
Procedure FSSetKeyFieldNonNull(aKey: PffByteArray;
  aKeyLen: Integer;
  aKeyFldCount: Integer;
  aKeyFld: Integer);

Implementation
Uses fsutil;

Const
  fscl_InitialFieldCapacity = 10;
  { Number of fields dictionary can hold upon creation.  The dictionary
    will expand its capacity as necessary. }
  fscl_InitialIndexCapacity = 5;
  { Number of indices dictionary can hold upon creation. The dictionary
    will expand its capacity as necessary. }

{===TFSInfoDict================================================}

Constructor TFSInfoDict.Create(aBlockSize: Longint);
Var
  NewFileDesc: PffFileDescriptor;
  NewInxDesc: PffIndexDescriptor;
  SeqAccessName: TffShStr;
Begin
  Inherited Create;
  FHasBLOBs := fftbUnknown; {!!.03}
  {verify the block size}
  If Not FFVerifyBlockSize(aBlockSize) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrBadBlockSize,
      [aBlockSize]);
  {create the file list}
  ddFileList := TList.Create;
  {add the first file name (for the data/data dict file)}
  NewFileDesc := CreateFileDesc(fsStrResGeneral[fsscMainTableFileDesc],
    fsc_ExtForData, aBlockSize, ftBaseFile);
  Try
    NewFileDesc^.fdNumber := 0;
    ddFileList.Add(pointer(NewFileDesc));
    FFileCount := 1;
  Except
    FFFreeMem(NewFileDesc, sizeof(TffFileDescriptor));
    Raise;
  End; {try..except}

  ddDefFldList := TList.Create;

  {create the field list}
  FFieldCapacity := fscl_InitialFieldCapacity;
  FFGetMem(FieldDescriptor, SizeOf(PffFieldDescriptor) * FFieldCapacity);
  {create the index list, add index 0: this is the sequential access
   index}

  FIndexCapacity := fscl_InitialIndexCapacity;
  FFGetMem(IndexDescriptor, SizeOf(PffIndexDescriptor) * FIndexCapacity);
  SeqAccessName := fsStrResGeneral[fsscSeqAccessIndexName];
  NewInxDesc := CreateUserIndexDesc(SeqAccessName, SeqAccessName, 0,
    sizeof(TffInt64), False);
  Try
    NewInxDesc^.idNumber := 0;
    IndexDescriptor^[0] := NewInxDesc;
    FInxCount := 1;
  Except
    FFFreeMem(NewInxDesc, sizeof(TffIndexDescriptor));
    Raise;
  End; {try..except}

  FFGetMem(IndexHelpers,
    SizeOf(TfsSrIndexHelper) * fscl_MaxIndexFlds * FIndexCapacity);

  fCascadeDelete := TStringList.Create;
  fCascadeUpdate := TStringList.Create;
  fCascadeDeletenull := TStringList.Create;
  fCascadeUpdatenull := TStringList.Create;
  fCascadeInsert := TStringList.Create;
  fRestrictDelete := TStringList.Create;
  fRestrictUpdate := TStringList.Create;
  fNoActionUpdate := TStringList.Create;

  fTrigersBeforeDelete := TStringList.Create;
  fTrigersAfterDelete := TStringList.Create;
  fTrigersBeforeUpdate := TStringList.Create;
  fTrigersAfterUpdate := TStringList.Create;
  fTrigersBeforeInsert := TStringList.Create;
  fTrigersAfterInsert := TStringList.Create;

  fTrigersBeforeDeleteOperation := TStringList.Create;
  fTrigersAfterDeleteOperation := TStringList.Create;
  fTrigersBeforeUpdateOperation := TStringList.Create;
  fTrigersAfterUpdateOperation := TStringList.Create;
  fTrigersBeforeInsertOperation := TStringList.Create;
  fTrigersAfterInsertOperation := TStringList.Create;
  fIsAnyRound := False;
  fIsAnyEmptyAsNull := False;
  FUserName := '';
  fIsAnyDefault := False;
  fIsRecVersion := False;
  fEngineDeleteType := edtNotUndelete;
  fVersionRecord := trNotUseVersion;
End;
{--------}

Destructor TFSInfoDict.Destroy;
Var
  Index: Integer;
  P: pointer;
  Pfd: PffFieldDescriptor Absolute P; {!!.01}
Begin
  If assigned(IndexHelpers) Then
    FFFreeMem(IndexHelpers,
      FIndexCapacity * fscl_MaxIndexFlds * SizeOf(TfsSrIndexHelper));

  ClearPrim(True);

  For Index := pred(FInxCount) Downto 0 Do
    Begin
      P := IndexDescriptor^[Index];
      FFFreeMem(P, sizeof(TffIndexDescriptor));
    End;
  FFFreeMem(IndexDescriptor, SizeOf(PffIndexDescriptor) * FIndexCapacity);

  For Index := pred(FFldCount) Downto 0 Do
    Begin
      P := FieldDescriptor^[Index];
      If Pfd^.fdVCheck <> Nil Then {!!.01}
        FFFreeMem(Pfd^.fdVCheck, sizeof(TffVCheckDescriptor)); {!!.01}
      FFFreeMem(P, SizeOf(PffFieldDescriptor) * FFieldCapacity);
    End;
  FFFreeMem(FieldDescriptor, SizeOf(PffFieldDescriptor) * FFieldCapacity);

  For Index := (ddFileList.Count - 1) Downto 0 Do
    Begin
      P := PffFileDescriptor(ddFileList[Index]);
      FFFreeMem(P, sizeOf(TffFileDescriptor));
      ddFileList.Delete(Index);
    End;

  ddFileList.Free;
  ddDefFldList.Free;

  fCascadeDelete.Free;
  fCascadeUpdate.Free;
  fCascadeDeletenull.Free;
  fCascadeUpdatenull.Free;
  fCascadeInsert.Free;
  fRestrictDelete.Free;
  fRestrictUpdate.Free;
  fNoActionUpdate.free;

  fTrigersBeforeDelete.Free;
  fTrigersAfterDelete.Free;
  fTrigersBeforeUpdate.Free;
  fTrigersAfterUpdate.Free;
  fTrigersBeforeInsert.Free;
  fTrigersAfterInsert.Free;

  fTrigersBeforeDeleteOperation.Free;
  fTrigersAfterDeleteOperation.Free;
  fTrigersBeforeUpdateOperation.Free;
  fTrigersAfterUpdateOperation.Free;
  fTrigersBeforeInsertOperation.Free;
  fTrigersAfterInsertOperation.Free;

  Inherited Destroy;
End;
{--------}

Class Function TFSInfoDict.NewInstance: TObject;
Begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
End;
{--------}

Procedure TFSInfoDict.FreeInstance;
Var
  Temp: pointer;
Begin
  Temp := Self;
  FFFreeMem(Temp, InstanceSize);
End;
{--------}

Function TFSInfoDict.AddFile(Const aDesc: TffDictItemDesc;
  Const aExtension: TffExtension;
  aBlockSize: Longint;
  aFileType: TffFileType): Integer;
Var
  NewDesc: PffFileDescriptor;
  i: Integer;
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  {verify the extension}
  If Not FFVerifyExtension(aExtension) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrBadExtension, [FBaseName, aExtension]);
  {verify the block size}
  If Not FFVerifyBlockSize(aBlockSize) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrBadBlockSize, [aBlockSize]);
  {if a base file type, check to see whether file 0 has been added
   already}
  If (aFileType = ftBaseFile) Then
    If (FFileCount > 0) Then
      FSRaiseException(EfsException, fsStrResGeneral, fserrDataFileDefd, [FBaseName]);
  {check to see whether the extension has been used already}
  For i := 0 To pred(FFileCount) Do
    If (PffFileDescriptor(ddFileList[i])^.fdExtension = aExtension) Then
      FSRaiseException(EfsException, fsStrResGeneral, fserrDupExtension, [FBaseName, aExtension]);
  {if a BLOB file type check to see whether we have one already; we
   can ignore file 0: it's the base file (ie data & dictionary)}
  If (aFileType = ftBLOBFile) Then
    If (BLOBFileNumber <> 0) Then
      FSRaiseException(EfsException, fsStrResGeneral, fserrBLOBFileDefd, [FBaseName]);
  {add a new file descriptor}
  NewDesc := CreateFileDesc(aDesc, aExtension, aBlockSize, aFileType);
  Try
    Result := FFileCount;
    NewDesc^.fdNumber := FFileCount;
    If (aFileType = ftBLOBFile) Then
      FBLOBFileNumber := FFileCount;
    ddFileList.Add(pointer(NewDesc));
    inc(FFileCount);
  Except
    FFFreeMem(NewDesc, sizeof(TffFileDescriptor));
    Raise;
  End; {try..except}
End;
{--------}

Procedure TFSInfoDict.AddIndex(Const aIdent: TffDictItemName;
  Const aDesc: TffDictItemDesc;
  aFile: Integer;
  aFldCount: Integer;
  Const aFldList: TffFieldList;
  Const aFieldsAscDesc: TffFieldList;
  Const aFieldsCase: TffFieldList;
  Const aFieldsSize: TffFieldList;
  Const aFieldsFlags: TffFieldList;
  Const aFieldsNullTop: TffFieldList;
  Const aFldIHList: TffFieldIHList;
  aAllowDups: boolean);
Var
  NewDesc: PffIndexDescriptor;
  i: Integer;
Begin
  {check for a duplicate index name}
  If (GetIndexFromName(aIdent) <> -1) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDupIndexName,
      [FBaseName, aIdent]);
  {check the file number}
  If (0 > aFile) Or (aFile >= FFileCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrBadFileNumber,
      [FBaseName, aFile]);
  {check all field numbers in field list}
  For i := 0 To pred(aFldCount) Do
    If (aFldList[i] < 0) Or (aFldList[i] >= FFldCount) Then
      FSRaiseException(EfsException, fsStrResGeneral, fserrBadFieldRef,
        [FBaseName, aFldList[i]]);
  {create the new index}
  NewDesc := CreateIndexDesc(aIdent, aDesc, aFile, aFldCount, aFldList, aFieldsAscDesc, aFieldsCase,
    aFieldsSize, aFieldsFlags, aFieldsNullTop, aFldIHList, aAllowDups);
  Try
    NewDesc^.idNumber := FInxCount;
    IndexDescriptor^[FInxCount] := NewDesc;
    inc(FInxCount);
    { Have we reached our index capacity? }
    If FInxCount = FIndexCapacity Then
      ddExpandIndexArray(0);
  Except
    FFFreeMem(NewDesc, sizeof(TffIndexDescriptor));
    Raise;
  End; {try..except}
End;
{--------}

Procedure TFSInfoDict.AddUserIndex(Const aIdent: TffDictItemName;
  Const aDesc: TffDictItemDesc;
  aFile: Integer;
  aKeyLength: Integer;
  aAllowDups: boolean);
Var
  NewDesc: PffIndexDescriptor;
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  {check the file number}
  If (0 > aFile) Or (aFile >= FFileCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrBadFileNumber, [FBaseName, aFile]);
  {check the key length}
  If Not FFVerifyKeyLength(aKeyLength) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrKeyTooLong, [aKeyLength]);
  {create the new index}
  NewDesc := CreateUserIndexDesc(aIdent, aDesc, aFile, aKeyLength, aAllowDups);
  Try
    NewDesc^.idNumber := FInxCount;
    IndexDescriptor^[FInxCount] := NewDesc;
    inc(FInxCount);
    { Have we reached our index capacity? }
    If FInxCount = FIndexCapacity Then
      ddExpandIndexArray(0);
  Except
    FFFreeMem(NewDesc, sizeof(TffIndexDescriptor));
    Raise;
  End; {try..except}
End;
{--------}

Procedure TFSInfoDict.AddField(Const aIdent: TffDictItemName;
  Const aDesc: TffDictItemDesc; // caption
  aType: TfsFieldType;
  aUnits: Integer;
  aDecPl: Integer;
  aReqFld: Boolean;
  Const aValCheck: PffVCheckDescriptor;
  aBlobLevelComp: TDataCompLevel;
  aDescription: TffDictDescription;
  aRound: TRound;
  aEmptyAsNull: boolean;
  aDefaultUpdate: TDefaultUpdate);
Var
  NewDesc: PffFieldDescriptor;
  TempDesc: PffFieldDescriptor;
  aaIdent: TffDictItemName;
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  aaIdent := Trim(aIdent);
  {check for a duplicate field name and empty}
  If aaIdent = '' Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrBadBaseName, [FBaseName, aaIdent]);
  If (GetFieldFromName(aaIdent) <> -1) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDupFieldName, [FBaseName, aaIdent]);
  {create it}
  NewDesc := CreateFieldDesc(aaIdent, aDesc, aType, aUnits, aDecPl, aReqFld, aValCheck, aBlobLevelComp,
    aDescription, aRound, aEmptyAsNull, aDefaultUpdate);
  Try
    NewDesc^.fdNumber := FFldCount;
    If (FFldCount > 0) Then
      Begin
        TempDesc := FieldDescriptor^[pred(FFldCount)];
        With TempDesc^ Do
          NewDesc^.fdOffset := fdOffset + fdLength;
      End;
    FieldDescriptor^[FFldCount] := NewDesc;
    inc(FFldCount);
    { Have we reached our field capacity? }
    If FFldCount = FFieldCapacity Then
      { Yes, expand our field array. }
      ddExpandFieldArray(0);
    With NewDesc^ Do
      FLogRecLen := fdOffset + fdLength;
    FHasBLOBs := fftbUnknown; {!!.03}
  Except
    FFFreeMem(NewDesc, sizeof(TffFieldDescriptor));
    Raise;
  End; {try..except}
End;
{--------}

Function fsReadString(Stream: TReader): AnsiString;
Var
  s: AnsiString;
  n: Integer;
Begin
  Result := '';
  Stream.Read(n, 4);
  If n > 0 Then
    Begin
      SetLength(s, n);
      Stream.Read(s[1], n);
      Result := s;
    End;
End;

Procedure fsWriteString(Stream: TWriter; s: AnsiString);
Var
  n: Integer;
Begin
  n := Length(s);
  Stream.Write(n, 4);
  If n > 0 Then
    Stream.Write(s[1], n);
End;

Procedure fsReadMemo(Stream: TReader; l: TStrings);
Var
  s: String;
  b: Byte;
  n: Longint;
Begin
  Stream.Read(n, 4);
  If l <> Nil Then
    Begin
      l.Clear;
      If n > 0 Then
        Repeat
          Stream.Read(n, 4);
          SetLength(s, n);
          Stream.Read(s[1], n);
          l.Add(s);
          Stream.Read(b, 1);
        Until b = 0
      Else If n <> -1 Then
        Stream.Read(b, 1);
    End;
End;

Procedure fsWriteMemo(Stream: TWriter; l: TStrings);
Var
  s: String;
  i: Integer;
  n: Longint;
  b: Byte;
Begin
  n := -1;
  If l <> Nil Then
    Begin
      n := l.Count;
      Stream.Write(n, 4);
      For i := 0 To l.Count - 1 Do
        Begin
          s := l[i];
          n := Length(s);
          Stream.Write(n, 4);
          Stream.Write(s[1], n);
          b := 13;
          If i <> l.Count - 1 Then
            Stream.Write(b, 1);
        End;
      b := 0;
      Stream.Write(b, 1);
    End
  Else
    Stream.Write(n, 4);
End;

{--------}{!!.05 - End Added}

Procedure TFSInfoDict.Assign(Source: TPersistent);
Var
  item: Integer;
  SelfFldDesc: PffFieldDescriptor;
  SrcDict: TFSInfoDict Absolute Source;
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  {Source must be one of us}
  If Not (Source Is TFSInfoDict) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrNotADict, [FBaseName]);
  {firstly clear our own lists (remove the base file item as well)}
  ClearPrim(True);
  {copy over the encrypted mode}
  Self.FIsEncrypted := TFSInfoDict(Source).IsEncrypted;
  Self.ddTableType := TFSInfoDict(Source).ddTableType;
  Self.EngineDeleteType := TfsInfoDict(Source).fEngineDeleteType;
  Self.VersionRecord := TfsInfoDict(Source).VersionRecord;
  fCascadeDelete.Assign(TFSInfoDict(Source).fCascadeDelete);
  fCascadeUpdate.Assign(TFSInfoDict(Source).fCascadeUpdate);
  fCascadeDeleteNull.Assign(TFSInfoDict(Source).fCascadeDeleteNull);
  fCascadeUpdateNull.Assign(TFSInfoDict(Source).fCascadeUpdateNull);
  fCascadeInsert.Assign(TFSInfoDict(Source).fCascadeInsert);
  fRestrictDelete.Assign(TFSInfoDict(Source).fRestrictDelete);
  fRestrictUpdate.Assign(TFSInfoDict(Source).fRestrictUpdate);

  fTrigersBeforeDelete.Assign(TFSInfoDict(Source).fTrigersBeforeDelete);
  fTrigersAfterDelete.Assign(TFSInfoDict(Source).fTrigersAfterDelete);
  fTrigersBeforeUpdate.Assign(TFSInfoDict(Source).fTrigersBeforeUpdate);
  fTrigersAfterUpdate.Assign(TFSInfoDict(Source).fTrigersAfterUpdate);
  fTrigersBeforeInsert.Assign(TFSInfoDict(Source).fTrigersBeforeInsert);
  fTrigersAfterInsert.Assign(TFSInfoDict(Source).fTrigersAfterInsert);
  fTrigersBeforeDeleteOperation.Assign(TFSInfoDict(Source).fTrigersBeforeDeleteOperation);
  fTrigersAfterDeleteOperation.Assign(TFSInfoDict(Source).fTrigersAfterDeleteOperation);
  fTrigersBeforeUpdateOperation.Assign(TFSInfoDict(Source).fTrigersBeforeUpdateOperation);
  fTrigersAfterUpdateOperation.Assign(TFSInfoDict(Source).fTrigersAfterUpdateOperation);
  fTrigersBeforeInsertOperation.Assign(TFSInfoDict(Source).fTrigersBeforeInsertOperation);
  fTrigersAfterInsertOperation.Assign(TFSInfoDict(Source).fTrigersAfterInsertOperation);

  { Now duplicate the items in the Source's lists. }
  Try
    { The file list first; do include index 0. }
    For item := 0 To pred(SrcDict.FFileCount) Do
      With PffFileDescriptor(SrcDict.ddFileList[item])^ Do
        Self.AddFile(fdDesc, fdExtension, fdBlockSize, fdType);

    { The field list next. }
    FHasBLOBs := fftbUnknown; {!!.03}
    For item := 0 To pred(SrcDict.FFldCount) Do
      With SrcDict.FieldDescriptor^[Item]^ Do
        Begin
          If Assigned(fdVCheck) Then
            Self.AddField(fdName, fdDesc, fdType, fdUnits, fdDecPl, fdRequired,
              fdVCheck, fdBlobLevelComp, fdDescription, fdRound, fdEmptyAsNull, fdDefaultUpdate)
          Else
            Begin
              //        FFGetZeroMem(CheckVal, sizeof(TffVCheckDescriptor));
              Self.AddField(fdName, fdDesc, fdType, fdUnits, fdDecPl, fdRequired,
                Nil, fdBlobLevelComp, fdDescription, fdRound, fdEmptyAsNull, fdDefaultUpdate)
            End;
          If assigned(fdVCheck) Then
            Begin
              SelfFldDesc := Self.FieldDescriptor^[item];
              If SelfFldDesc^.fdVCheck = Nil Then {!!.06}
                FFGetMem(SelfFldDesc^.fdVCheck, sizeOf(TffVCheckDescriptor));
              Move(fdVCheck^, SelfFldDesc^.fdVCheck^, sizeof(fdVCheck^));
            End;
        End;

    { The index list next; skip index 0. }
    For item := 1 To pred(SrcDict.FInxCount) Do
      With SrcDict.IndexDescriptor^[item]^ Do
        If (idCount <> -1) Then
          Self.AddIndex(idName, idDesc, idFile, idCount,
            idFields, idFieldsAscDesc, idFieldsCase, idFieldsSize, idFieldsFlags, idFieldsNullTop, idFieldIHlprs, idDups)
        Else
          Self.AddUserIndex(idName, idDesc, idFile, idKeyLen, idDups)
  Except
    ClearPrim(True);
    Raise;
  End; {try..except}
End;
{--------}

Procedure TFSInfoDict.BindIndexHelpers;
Var
  i, j: Integer;
Begin
  For i := 0 To pred(IndexCount) Do
    With IndexDescriptor^[i]^ Do
      If idCount >= 0 Then
        Begin
          For j := 0 To Pred(idCount) Do
            IndexHelpers[i, j] :=
              TfsSrIndexHelper.FindHelper(idFieldIHlprs[j], GetFieldType(idFields[j]));
        End;
End;
{--------}

Function TFSInfoDict.CheckRequiredRecordFields(aData: PffByteArray): Boolean;
Var
  FieldInx: Integer;
  BS: PffByteArray;
Begin
  {note: it's probably faster to find all the null fields and then
         check their required status, rather than the other way round
         (getting a field descriptor requires a whole lot more calls
         than checking a bit) but it does depend on a lotta factors.}
  Result := False;
  If (aData = Nil) Then
    Exit;
  BS := PffByteArray(@aData^[FLogRecLen]);
  For FieldInx := 0 To pred(FFldCount) Do
    Begin
      If FFIsBitSet(BS, FieldInx) Then
        If FieldDescriptor^[FieldInx]^.fdRequired Then
          Exit;
    End;
  Result := True;
End;
{--------}

Procedure TFSInfoDict.CheckValid;
Var
  item: Integer;
  i: Integer;
  Fld: PffFieldDescriptor;
  Indx: PffIndexDescriptor;
  PlusInfo: Integer;
Begin
  If EngineDeleteType In [edtUndeleteIfPossibleNotBlob, edtUndeleteIfPossibleAndBlob] Then
    PlusInfo := SizeOf(Byte) + sizeof(TffInt64) // extra info for delete
  Else If EngineDeleteType In [edtUndeleteFull] Then
    PlusInfo := SizeOf(Byte) + sizeof(TffInt64) + sizeof(TffInt64) // extra info for delete
  Else
    PlusInfo := SizeOf(Byte);
  If VersionRecord = trUseVersion Then
    PlusInfo := PlusInfo + Sizeof(Int64);
  //PlusInfo := PlusInfo + Sizeof(TfsExtraRecInfo);
  If (FFldCount <= 0) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrNoFields, [FBaseName]);
  If (RecordLength > (BlockSize - fsc_BlockHeaderSizeData - sizeof(Longint) - PlusInfo)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrRecTooLong, [FBaseName]);
  If (IndexCount > fscl_MaxIndexes) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrMaxIndexes, [FBaseName]);
  {check all field numbers in all indexes, recalc key lengths}
  If (FInxCount > 1) Then
    For item := 1 To pred(FInxCount) Do
      With IndexDescriptor^[item]^ Do
        If (idCount <> -1) Then
          Begin
            If (idCount = 0) Then
              FSRaiseException(EfsException, fsStrResGeneral, fserrNoFieldsInKey, [FBaseName]);
            idKeyLen := 0;
            For i := 0 To pred(idCount) Do
              Begin
                If (idFields[i] < 0) Or (idFields[i] >= FFldCount) Then
                  FSRaiseException(EfsException, fsStrResGeneral, fserrBadFieldRef, [FBaseName, idFields[i]]);
                If (idFieldsSize[i] > 0) And (FieldDescriptor^[idFields[i]]^.fdType In [fstShortString,
                  fstVarNullString, fstNullString]) Then
                  Begin
                    If idFieldsSize[i] + 1 > FieldDescriptor^[idFields[i]]^.fdLength Then
                      FSRaiseException(EfsException, fsStrResGeneral, fserrLenMismatch, [FBaseName, idFields[i]]);
                    inc(idKeyLen, idFieldsSize[i] + 1);
                  End
                Else If (idFieldsSize[i] > 0) And (FieldDescriptor^[idFields[i]]^.fdType In [fstWideString, fstVarWideString]) Then
                  Begin
                    If (idFieldsSize[i] + 1) * sizeof(WideChar) > FieldDescriptor^[idFields[i]]^.fdLength Then
                      FSRaiseException(EfsException, fsStrResGeneral, fserrLenMismatch, [FBaseName, idFields[i]]);
                    inc(idKeyLen, (idFieldsSize[i] + 1) * sizeof(WideChar));
                  End
                Else
                  inc(idKeyLen, FieldDescriptor^[idFields[i]]^.fdLength);
              End;
            inc(idKeyLen, (idCount + 7) Div 8);
          End;
  {field names must be unique and not empty}
  For item := 0 To pred(FFldCount) Do
    Begin
      Fld := FieldDescriptor^[item];
      If Fld^.fdName = '' Then
        FSRaiseException(EfsException, fsStrResGeneral, fserrBadBaseName, [FBaseName, Fld^.fdName]);
      If (GetFieldFromName(Fld^.fdName) <> item) Then
        FSRaiseException(EfsException, fsStrResGeneral, fserrDupFieldName, [FBaseName, Fld^.fdName]);
    End;
  {index names must be unique}
  If (FInxCount > 1) Then
    For item := 1 To pred(FInxCount) Do
      Begin
        Indx := IndexDescriptor^[item];
        If (GetIndexFromName(Indx^.idName) <> item) Then
          FSRaiseException(EfsException, fsStrResGeneral, fserrDupIndexName, [FBaseName, Indx^.idName]);
      End;
End;
{--------}

Procedure TFSInfoDict.Clear;
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  ClearPrim(False);
End;
{--------}

Procedure TFSInfoDict.ClearPrim(InclFileZero: boolean);
Var
  item: Integer;
  BaseFileDesc: PffFileDescriptor;
  TmpIndexDesc: PffIndexDescriptor;
  FldDesc: PffFieldDescriptor;
Begin
  {clear the entire file list EXCEPT item zero}
  For item := 1 To pred(FFileCount) Do
    Begin
      BaseFileDesc := PffFileDescriptor(ddFileList[item]);
      FFFreeMem(BaseFileDesc, sizeof(TffFileDescriptor));
    End;
  {decide what to do about item zero: save it or dispose of it}
  If InclFileZero And (FFileCount > 0) Then
    Begin
      BaseFileDesc := PffFileDescriptor(ddFileList[0]);
      FFFreeMem(BaseFileDesc, sizeof(TffFileDescriptor));
      ddFileList.Clear;
      FFileCount := 0;
    End
  Else {don't dispose of file 0}
    Begin
      BaseFileDesc := PffFileDescriptor(ddFileList[0]);
      ddFileList.Clear;
      ddFileList.Add(pointer(BaseFileDesc));
      FFileCount := 1;
    End;
  {clear the entire field list}
  For item := 0 To pred(FFldCount) Do
    Begin
      FldDesc := FieldDescriptor^[item];
      If (FldDesc^.fdVCheck <> Nil) Then
        FFFreeMem(FldDesc^.fdVCheck, sizeOf(TffVCheckDescriptor));
      FFFreeMem(FldDesc, sizeOf(TffFieldDescriptor));
    End;
  FFldCount := 0;
  FLogRecLen := 0;
  {clear the entire index list EXCEPT for the first item}
  For item := 1 To pred(FInxCount) Do
    Begin
      TmpIndexDesc := IndexDescriptor^[item];
      FFFreeMem(TmpIndexDesc, sizeOf(TffIndexDescriptor));
      IndexDescriptor^[item] := Nil;
    End;
  FInxCount := 1;

  {clear out any old default field values}{!!.03}
  ddDefFldList.Clear; {!!.03}
  FHasBLOBs := fftbUnknown; {!!.03}
End;
{--------}

Function TFSInfoDict.CreateFieldDesc(Const aIdent: TffDictItemName;
  Const aDesc: TffDictItemDesc;
  aType: TfsFieldType;
  aUnits: Integer;
  aDecPl: Integer;
  aReqFld: Boolean;
  Const aValCheck: PffVCheckDescriptor;
  aBlobLevelComp: TDataCompLevel;
  aDescription: TffDictDescription;
  aRound: TRound;
  aEmptyAsNull: boolean;
  aDefaultUpdate: TDefaultUpdate)
  : PffFieldDescriptor;
Var
  FT, i: Integer;
Begin
  If (aType In [fstAutoInc32, fstAutoInc64]) Then
    aReqFld := False;
  FFGetZeroMem(Result, sizeof(TffFieldDescriptor));
  With Result^ Do
    Begin
      fdName := aIdent;
      fdDesc := aDesc;
      fdDescription := aDescription;
      fdType := aType;
      fdRequired := aReqFld;
      fdReadOnly := False;
      fdVisible := True;
      fdRound := rNone;
      fdxxx1 := 0;
      fdIsCalculated := False;
      fdEmptyAsNull := aEmptyAsNull;
      fdDefaultUpdate := aDefaultUpdate;
      For i := 0 To 31 Do
        fdReserved.fdResStr[i] := #0;
      For i := 0 To 11 Do
        fdReserved.fdResByt[i] := 0;

      Case aType Of
        fstBoolean:
          Begin
            fdUnits := 0;
            fdDecPl := 0;
            fdLength := sizeof(Boolean);
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstSingleChar:
          Begin
            fdUnits := 1;
            fdDecPl := 0;
            fdLength := sizeof(AnsiChar);
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstSingleWideChar:
          Begin
            fdUnits := 1;
            fdDecPl := 0;
            fdLength := sizeof(WideChar);
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstUInt8:
          Begin
            If (aUnits < 0) Or (aUnits > 3) Then
              fdUnits := 3
            Else
              fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(Byte);
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstUInt16:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(TffWord16);
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstUInt32:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(TffWord32);
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstInt8:
          Begin
            If (aUnits < 0) Or (aUnits > 3) Then
              fdUnits := 3
            Else
              fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(Shortint);
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstInt16:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(Smallint);
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstInt32:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(Longint);
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstAutoInc32:
          Begin
            fdUnits := 11;
            fdDecPl := 0;
            fdLength := sizeof(Longint);
            fdBlobLevelComp := blNone;
          End;
        fstSingle:
          Begin
            fdUnits := aUnits;
            fdDecPl := aDecPl;
            fdLength := sizeof(Single);
            fdRound := aRound;
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstDouble:
          Begin
            fdUnits := aUnits;
            fdDecPl := aDecPl;
            fdLength := sizeof(Double);
            fdRound := aRound;
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstExtended:
          Begin
            fdUnits := aUnits;
            fdDecPl := aDecPl;
            fdLength := 10;
            fdRound := aRound;
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstInt64:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(Int64);
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstRecVersion:
          Begin
            fdUnits := aUnits;
            fdDecPl := aDecPl;
            fdLength := sizeof(Int64);
            fdBlobLevelComp := blNone;
          End;
        fstAutoInc64:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(Int64);
            fdBlobLevelComp := blNone;
          End;
        fstCurrency:
          Begin
            fdUnits := aUnits;
            fdDecPl := aDecPl;
            fdLength := 8; //sizeof(Comp);
            fdRound := aRound;
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstBcd:
          Begin
            fdUnits := aUnits;
            fdDecPl := aDecPl;
            fdLength := 32; //sizeof(TfsBcd);
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstDate:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(Longint);
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstTime:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(Longint);
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstDateTime:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := sizeof(Double);
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstBLOB..fstBLOBGraphic:
          Begin
            fdUnits := 0;
            fdDecPl := 0;
            fdLength := sizeof(TffInt64);
            fdBlobLevelComp := aBlobLevelComp;
          End;
        fstArrayUInt8:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := aUnits;
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstArrayUInt16:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := aUnits * 2;
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstArrayInt32:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := aUnits * 4;
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstArrayDouble:
          Begin
            fdUnits := aUnits;
            fdDecPl := aDecPl;
            fdLength := aUnits * 8;
            fdBlobLevelComp := blNone;
            CheckForDefault(aValCheck, Result);
          End;
        fstShortString, fstNullString, fstVarNullString:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := (aUnits + 1) * sizeof(AnsiChar);
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        fstWideString, fstVarWideString:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := (aUnits + 1) * sizeof(WideChar);
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End;
        {fstUnicode:
          Begin
            fdUnits := aUnits;
            fdDecPl := 0;
            fdLength := (aUnits + 1) * sizeof(WideChar);
            CheckForDefault(aValCheck, Result);
            fdBlobLevelComp := blNone;
          End; }
        Else
          FT := ord(aType);
          FSRaiseException(EfsException, fsStrResGeneral, fserrBadFieldType, [FT]);
      End; {case}
    End;
End;
{--------}

Function TFSInfoDict.CreateFileDesc(Const aDesc: TffDictItemDesc;
  Const aExtension: TffExtension;
  aBlockSize: Longint;
  aType: TffFileType)
  : PffFileDescriptor;
Begin
  FFGetZeroMem(Result, sizeof(TffFileDescriptor));
  With Result^ Do
    Begin
      fdDesc := aDesc;
      fdExtension := aExtension;
      fdBlockSize := aBlockSize;
      fdType := aType;
    End;
End;
{--------}

Function TFSInfoDict.CreateIndexDesc(Const aIdent: TffDictItemName;
  Const aDesc: TffDictItemDesc;
  aFile: Integer;
  aFldCount: Integer;
  Const aFldList: TffFieldList;
  Const aFieldsAscDesc: TffFieldList;
  Const aFieldsCase: TffFieldList;
  Const aFieldsSize: TffFieldList;
  Const aFieldsFlags: TffFieldList;
  Const aFieldsNullTop: TffFieldList;
  Const aFldIHList: TffFieldIHList;
  aAllowDups: boolean)
  : PffIndexDescriptor;
Var
  i: Integer;
Begin
  FFGetZeroMem(Result, sizeof(TffIndexDescriptor));
  With Result^ Do
    Begin
      idName := aIdent;
      idDesc := aDesc;
      idFile := aFile;
      idCount := aFldCount;
      idDups := aAllowDups;
      idKeyLen := 0;
      idIndexType := itComposite;
      For i := 0 To pred(aFldCount) Do
        Begin
          idFields[i] := aFldList[i];
          idFieldsAscDesc[i] := aFieldsAscDesc[i];
          idFieldsCase[i] := aFieldsCase[i];
          idFieldsSize[i] := aFieldsSize[i];
          idFieldsFlags[i] := aFieldsFlags[i];
          idFieldsNullTop[i] := aFieldsNullTop[i];
          If (idFieldsSize[i] > 0) And (FieldDescriptor^[aFldList[i]]^.fdType In [fstShortString,
            fstVarNullString, fstNullString]) Then
            inc(idKeyLen, idFieldsSize[i] + 1)
          Else If (idFieldsSize[i] > 0) And (FieldDescriptor^[aFldList[i]]^.fdType In [fstWideString, fstvarWideString]) Then
            inc(idKeyLen, (idFieldsSize[i] + 1) * sizeof(WideChar))
          Else
            inc(idKeyLen, FieldDescriptor^[aFldList[i]]^.fdLength);
        End;
      For i := 0 To pred(aFldCount) Do
        idFieldIHlprs[i] := aFldIHList[i];
      inc(idKeyLen, {the key length itself}
        (aFldCount + 7) Div 8); {the bit array for nulls}
    End;
End;
{--------}

Function TFSInfoDict.CreateUserIndexDesc(Const aIdent: TffDictItemName;
  Const aDesc: TffDictItemDesc;
  aFile: Integer;
  aKeyLength: Integer;
  aAllowDups: boolean)
  : PffIndexDescriptor;
Begin
  FFGetZeroMem(Result, sizeof(TffIndexDescriptor));
  With Result^ Do
    Begin
      idName := aIdent;
      idFile := aFile;
      idDups := aAllowDups;
      idCount := -1;
      idKeyLen := aKeyLength;
      idIndexType := itSysRef;
    End;
End;
{--------}

Procedure TFSInfoDict.AnsiStringWriter(Const aString: String; aWriter: TWriter);
Var
  TempInt: Integer;
  iString: String;
Begin
  iString := Trim(aString);
  TempInt := Integer(vaString);
  aWriter.Write(TempInt, SizeOf(vaString));
  TempInt := Length(iString);
  aWriter.Write(TempInt, SizeOf(Byte));

  If (TempInt > 0) Then
    aWriter.Write(iString[1], TempInt);
End;

Procedure TFSInfoDict.ddExpandFieldArray(Const minCapacity: Longint);
Var
  OldCapacity: Longint;
Begin
  OldCapacity := FFieldCapacity;
  {Begin !!.02}
  If minCapacity = 0 Then
    inc(FFieldCapacity, fscl_InitialFieldCapacity * 2)
  Else If FFieldCapacity = minCapacity Then
    Exit
  Else
    FFieldCapacity := minCapacity;
  {End !!.02}
  FFReallocMem(FieldDescriptor, SizeOf(PffFieldDescriptor) * OldCapacity,
    SizeOf(PffFieldDescriptor) * FFieldCapacity);
End;
{--------}

Procedure TFSInfoDict.ddExpandIndexArray(Const minCapacity: Longint);
Var
  OldCapacity: Longint;
Begin
  OldCapacity := FIndexCapacity;
  {Begin !!.02}
  If minCapacity = 0 Then
    inc(FIndexCapacity, fscl_InitialIndexCapacity * 2)
  Else If FIndexCapacity = minCapacity Then
    Exit
  Else
    FIndexCapacity := minCapacity;
  {End !!.02}
  FFReallocMem(IndexDescriptor, SizeOf(PffIndexDescriptor) * OldCapacity,
    SizeOf(PffIndexDescriptor) * FIndexCapacity);
  FFReallocMem(IndexHelpers,
    SizeOf(TfsSrIndexHelper) * fscl_MaxIndexFlds * OldCapacity,
    SizeOf(TfsSrIndexHelper) * fscl_MaxIndexFlds * FIndexCapacity);
End;
{--------}

Procedure TFSInfoDict.ExtractKey(aIndexID: Integer;
  aData: PffByteArray;
  aKey: PffByteArray);
Var
  KeyOffset: Integer;
  FieldNumber: Integer;
Begin
  KeyOffset := 0;
  With IndexDescriptor^[aIndexID]^ Do
    Begin
      {clear the entire key - sets all fields to null as well}
      FSInitKey(aKey, idKeyLen, idCount);
      {now build it}
      For FieldNumber := 0 To pred(idCount) Do
        Begin
          With FieldDescriptor^[idFields[FieldNumber]]^ Do
            Begin
              If Not IsRecordFieldNull(idFields[FieldNumber], aData) Then
                Begin
                  Move(aData^[fdOffset], aKey^[KeyOffset], fdLength);
                  FSSetKeyFieldNonNull(aKey, idKeyLen, idCount, FieldNumber);
                End;
              inc(KeyOffset, fdLength);
            End;
        End;
    End;
End;
{--------}

Function TFSInfoDict.GetBaseRecordLength: Longint;
Begin
  { A record must be at last fscl_MinRecordLength bytes in length.  This
    is because we need that many bytes in order to store the next deleted
    record when the record becomes part of the deleted record chain. }
  Result := FFMaxL(FLogRecLen, fscl_MinRecordLength);
End;
{--------}

Function TFSInfoDict.GetBlockSize: Longint;
Begin
  If (FFileCount > 0) Then
    Result := PffFileDescriptor(ddFileList.Items[0])^.fdBlockSize
  Else
    Result := 4096;
End;
{--------}

Function TFSInfoDict.GetBookmarkSize(aIndexID: Integer): Integer;
Begin
  If Not ((0 <= aIndexID) And (aIndexID < FInxCount)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aIndexID]);
  Result := fscl_FixedBookmarkSize + IndexDescriptor^[aIndexID]^.idKeyLen;
End;
{--------}

Function TFSInfoDict.GetFieldDecPl(aField: Integer): Longint;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdDecPl;
End;
{--------}

Function TFSInfoDict.GetFieldDisplay(aField: Integer): TffDictItemDesc;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdDesc;
End;

Function TFSInfoDict.GetFieldEmptyAsNull(aField: Integer): Boolean;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdEmptyAsNull;
End;

Function TFSInfoDict.GetFieldDefaultUpdate(aField: Integer): TDefaultUpdate;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdDefaultUpdate;
End;

Function TFSInfoDict.GetFieldDescription(aField: Integer): TffDictDescription;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdDescription;
End;
{--------}

Function TFSInfoDict.GetFieldFromName(Const aFieldName: TffDictItemName): Integer;
Begin
  For Result := 0 To pred(FFldCount) Do
    If (FFCmpShStrUC(aFieldName, FieldDescriptor^[Result]^.fdName, 255) = 0) Then
      Exit;
  Result := -1;
End;
{--------}

Function TFSInfoDict.GetFieldLength(aField: Integer): Longint;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdLength;
End;
{--------}

Function TFSInfoDict.GetFieldName(aField: Integer): TffDictItemName;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdName;
End;
{--------}

Function TFSInfoDict.GetFieldOffset(aField: Integer): Longint;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdOffset;
End;
{--------}

Function TFSInfoDict.GetFieldRequired(aField: Integer): boolean;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdRequired;
End;
{--------}

Function TFSInfoDict.GetFieldType(aField: Integer): TfsFieldType;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdType;
End;
{--------}

Function TFSInfoDict.GetFieldBlobLevelComp(aField: Integer): TDataCompLevel;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdBlobLevelComp;
End;

Function TFSInfoDict.GetFieldRound(aField: Integer): TRound;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdRound;
End;

Function TFSInfoDict.GetFieldUnits(aField: Integer): Longint;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdUnits;
End;
{--------}

Function TFSInfoDict.GetFieldVCheck(aField: Integer): PffVCheckDescriptor;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdVCheck;
End;
{--------}

Function TFSInfoDict.GetFileBlockSize(aFile: Integer): Longint;
Begin
  If (aFile < 0) Or (aFile >= FFileCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile])^.fdBlockSize;
End;
{--------}

Function TFSInfoDict.GetFileDesc(aFile: Integer): TffDictItemDesc;
Begin
  If (aFile < 0) Or (aFile >= FFileCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile])^.fdDesc;
End;
{--------}

Function TFSInfoDict.GetFileDescriptor(aFile: Integer): PffFileDescriptor;
Begin
  If (aFile < 0) Or (aFile >= FFileCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile]);
End;
{--------}

Function TFSInfoDict.GetFileExt(aFile: Integer): TffExtension;
Begin
  If (aFile < 0) Or (aFile >= FFileCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile])^.fdExtension;
End;
{--------}

Function TFSInfoDict.GetFileNameExt(aFile: Integer): TffFileNameExt;
Var
  Temp: PffFileDescriptor;
Begin
  If (aFile < 0) Or (aFile >= FFileCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aFile]);
  Temp := PffFileDescriptor(ddFileList.Items[aFile]);
  Result := FFMakeFileNameExt(FBaseName, Temp^.fdExtension);
End;
{--------}

Function TFSInfoDict.GetFileType(aFile: Integer): TffFileType;
Begin
  If (aFile < 0) Or (aFile >= FFileCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile])^.fdType;
End;
{Begin !!.03}
{--------}

Function TFSInfoDict.GetHasBLOBs: Boolean;
Var
  Index: Integer;
  P: PffFieldDescriptor;
Begin
  If FHasBLOBs = fftbUnknown Then
    Begin
      FHasBLOBs := fftbFalse;
      For Index := 0 To Pred(FFldCount) Do
        Begin
          P := FieldDescriptor^[Index];
          If P^.fdType In [fstBLOB..fstBLOBGraphic] Then
            Begin
              FHasBLOBs := fftbTrue;
              Break;
            End; { if }
        End; { for }
    End; { if }
  Result := (FHasBLOBs = fftbTrue);
End;
{End !!.03}
{--------}

Function TFSInfoDict.GetIndexAllowDups(aIndexID: Integer): boolean;
Begin
  If Not ((0 <= aIndexID) And (aIndexID < FInxCount)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idDups;
End;
{--------}

Function TFSInfoDict.GetIndexAscend(aIndexID: Integer): boolean;
Begin
  If Not ((0 <= aIndexID) And (aIndexID < FInxCount)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aIndexID]);
  Result := Boolean(IndexDescriptor^[aIndexID]^.idFieldsAscDesc[0]);
  If IndexDescriptor^[aIndexID]^.idIndexType In [itSysRef, itSysVersion] Then Result := True;
End;
{--------}

Function TFSInfoDict.GetIndexDesc(aIndexID: Integer): TffDictItemDesc;
Begin
  If Not ((0 <= aIndexID) And (aIndexID < FInxCount)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idDesc;
End;
{--------}

Function TFSInfoDict.GetIndexFileNumber(aIndexID: Integer): Longint;
Begin
  If Not ((0 <= aIndexID) And (aIndexID < FInxCount)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexId]^.idFile;
End;
{--------}

Function TFSInfoDict.GetIndexFromName(Const aIndexName: TffDictItemName): Integer;
Begin
  For Result := 0 To pred(FInxCount) Do
    If (FFCmpShStrUC(aIndexName,
      indexDescriptor^[Result]^.idName,
      255) = 0) Then
      Exit;
  Result := -1;
End;
{--------}

Function TFSInfoDict.GetIndexKeyLength(aIndexID: Integer): Longint;
Begin
  If Not ((0 <= aIndexID) And (aIndexID < FInxCount)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idKeyLen;
End;
{--------}

Function TFSInfoDict.GetIndexName(aIndexID: Integer): TffDictItemName;
Begin
  If Not ((0 <= aIndexID) And (aIndexID < FInxCount)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idName;
End;
{--------}

Function TFSInfoDict.GetIndexNoCase(aIndexID: Integer): boolean;
Begin
  If Not ((0 <= aIndexID) And (aIndexID < FInxCount)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aIndexID]);
  Result := Boolean(IndexDescriptor^[aIndexID]^.idFieldsCase[0]);
  If IndexDescriptor^[aIndexID]^.idIndexType In [itSysRef, itSysVersion] Then Result := True;
End;
{--------}

Function TFSInfoDict.GetIndexType(aIndexID: Integer): TfsIndexType;
Begin
  If Not ((0 <= aIndexID) And (aIndexID < FInxCount)) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idIndexType;
End;
{--------}

Procedure TFSInfoDict.GetRecordField(aField: Integer;
  aData: PffByteArray;
  Var aIsNull: boolean;
  aValue: pointer);
Begin
  aIsNull := IsRecordFieldNull(aField, aData);
  If (Not aIsNull) And (aValue <> Nil) Then
    With FieldDescriptor^[aField]^ Do
      Move(aData^[fdOffset], aValue^, fdLength);
End;
{--------}

Function TFSInfoDict.GetRecordLength: Longint;
Begin
  Result := GetBaseRecordLength + {the fields themselves}
  ((FFldCount + 7) Div 8); {the bit array for nulls}
End;
{--------}

Function TFSInfoDict.HasAutoIncField(Var aField: Integer): boolean;
Begin
  Result := True;
  aField := 0;
  While (aField < FFldCount) Do
    Begin
      If FieldDescriptor^[aField]^.fdType In [fstAutoInc32, fstAutoInc64] Then
        Exit;
      inc(aField);
    End;
  Result := False;
End;
{--------}

Function TFSInfoDict.HasSameFields(aSrcDict: TFSInfoDict;
  Var aBLOBFields: TfsPointerList): boolean;
Var
  anIndex: Integer;
Begin
  Result := False;
  If FieldCount <> aSrcDict.FieldCount Then
    Exit;
  aBLOBFields.Empty;

  For anIndex := 0 To pred(FieldCount) Do
    Begin
      { Must have same field type, length, decimal places, & units. }
      Result := (FieldLength[anIndex] = aSrcDict.FieldLength[anIndex]) And
        (FieldType[anIndex] = aSrcDict.FieldType[anIndex]) And
        (FieldDecPl[anIndex] = aSrcDict.FieldDecPl[anIndex]) And
        (FieldUnits[anIndex] = aSrcDict.FieldUnits[anIndex]) And
        (FieldBlobLevelComp[anIndex] = aSrcDict.FieldBlobLevelComp[anIndex]);
      If (Not Result) Then
        Exit;
      If FieldType[anIndex] In [fstBLOB..fstBLOBGraphic] Then
        aBLOBFields.Append(Pointer(anIndex));
    End;
End;

Function TFSInfoDict.FieldIndex(aFieldName: String): Integer;
Var
  anIndex: Integer;
Begin
  Result := -1;
  For anIndex := 0 To pred(FieldCount) Do
    Begin
      If Trim(AnsiUpperCase(aFieldName)) = Trim(AnsiUpperCase(FieldName[anIndex])) Then
        Begin
          Result := anIndex;
          Exit;
        End;
    End;
End;
{--------}

Function TFSInfoDict.HasSameFieldsEx(aSrcDict: TFSInfoDict;
  aFields: PffLongintArray;
  aNumFields: Integer;
  Var aBLOBFields: TfsPointerList): boolean;
Var
  anIndex, aSrcIndex: Integer;
Begin
  Result := False;
  If FieldCount <> aNumFields Then
    Exit;
  aBLOBFields.Empty;

  For anIndex := 0 To pred(aNumFields) Do
    Begin
      aSrcIndex := aFields^[anIndex];
      { Must have same field type, length, decimal places, & units. }
      Result := (FieldLength[anIndex] = aSrcDict.FieldLength[aSrcIndex]) And
        (FieldType[anIndex] = aSrcDict.FieldType[aSrcIndex]) And
        (FieldDecPl[anIndex] = aSrcDict.FieldDecPl[aSrcIndex]) And
        (FieldUnits[anIndex] = aSrcDict.FieldUnits[aSrcIndex]) And
        (FieldBlobLevelComp[anIndex] = aSrcDict.FieldBlobLevelComp[anIndex]);
      If (Not Result) Then
        Exit;
      If FieldType[anIndex] In [fstBLOB..fstBLOBGraphic] Then
        aBLOBFields.Append(Pointer(anIndex));
    End;
End;
{--------}

Procedure TFSInfoDict.CheckForDefault(aVCheckDesc: PffVCheckDescriptor;
  aFieldDesc: PffFieldDescriptor);
Var
  CheckVal: PffVCheckDescriptor;
Begin
  If Assigned(aVCheckDesc) And aVCheckDesc^.vdHasDefVal Then
    Begin
      If (Not Assigned(aFieldDesc^.fdVCheck)) Then
        Begin
          FFGetZeroMem(CheckVal, sizeof(TffVCheckDescriptor));
          aFieldDesc^.fdVCheck := CheckVal;
        End;
      aFieldDesc^.fdVCheck^.vdHasDefVal := True;
      aFieldDesc^.fdVCheck^.vdDefVal := aVCheckDesc.vdDefVal;
      aFieldDesc^.fdVCheck^.vdres := 0;
      aFieldDesc^.fdVCheck^.vdres1 := 0;
    End;
End;
{--------}

Function TFSInfoDict.GetDefaultFldCount: Integer;
Begin
  ddDefFldList.Pack;
  Result := ddDefFldList.Count;
End;
{--------}

Procedure TFSInfoDict.InitRecord(aData: PffByteArray);
Begin
  If (aData <> Nil) And (FFldCount > 0) Then
    Begin
      FillChar(aData^, FLogRecLen + ((FFldCount + 7) Div 8), 0);
      FFSetAllBits(PffByteArray(@aData^[LogicalRecordLength]), FFldCount); {!!.02}
    End;
End;
{--------}

Procedure TFSInfoDict.InsertField(AtIndex: Integer;
  Const aIdent: TffDictItemName;
  Const aDesc: TffDictItemDesc;
  aType: TfsFieldType;
  aUnits: Integer;
  aDecPl: Integer;
  aReqFld: Boolean;
  Const aValCheck: PffVCheckDescriptor;
  aBlobLevelComp: TDataCompLevel;
  aDescription: TffDictDescription;
  aRound: TRound;
  aEmptyAsNull: Boolean;
  aDefaultUpdate: TDefaultUpdate);
Var
  NewDesc: PffFieldDescriptor;
  TempDesc: PffFieldDescriptor;
  NewOffset: Integer;
  Inx: Integer;
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  {check for a duplicate field name}
  If (GetFieldFromName(aIdent) <> -1) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDupFieldName, [FBaseName, aIdent]);
  {create it}
  If (0 <= AtIndex) And (AtIndex < FFldCount) Then
    Begin
      FHasBLOBs := fftbUnknown; {!!03}
      NewDesc := CreateFieldDesc(aIdent, aDesc, aType, aUnits, aDecPl, aReqFld, aValCheck,
        aBlobLevelComp, aDescription, aRound, aEmptyAsNull, aDefaultUpdate);
      Try
        NewDesc^.fdNumber := AtIndex;
        If (AtIndex > 0) Then
          Begin
            TempDesc := FieldDescriptor^[pred(AtIndex)];
            With TempDesc^ Do
              NewDesc^.fdOffset := fdOffset + fdLength;
          End;
        { Shift existing fields up. }
        For Inx := pred(FFldCount) Downto AtIndex Do
          FieldDescriptor^[succ(Inx)] := FieldDescriptor^[Inx];
        FieldDescriptor^[AtIndex] := NewDesc;
        inc(FFldCount);
        { Have we reached our field capacity? }
        If FFldCount = FFieldCapacity Then
          { Yes, expand our field array. }
          ddExpandFieldArray(0);
        {patch up all successive descriptors}
        With NewDesc^ Do
          NewOffset := fdOffset + fdLength;
        For Inx := succ(AtIndex) To pred(FFldCount) Do
          Begin
            TempDesc := FieldDescriptor^[Inx];
            With TempDesc^ Do
              Begin
                fdNumber := Inx;
                fdOffset := NewOffset;
                inc(NewOffset, fdLength);
              End;
          End;
        FLogRecLen := NewOffset;
      Except
        FFFreeMem(NewDesc, sizeof(TffFieldDescriptor));
        Raise;
      End; {try..except}
    End;
End;
{--------}

Function TFSInfoDict.IsIndexDescValid(Const aIndexDesc: TffIndexDescriptor): boolean;
Var
  i: Integer;
  KeyLen: Integer;
Begin
  Result := False;
  With aIndexDesc Do
    Begin
      If (idName = '') Then
        Exit;
      If (0 > idFile) Or (idFile >= FFileCount) Then
        Exit;
      If (idCount = -1) Then
        Begin {user-defined index}
          If (idKeyLen <= 0) Then
            Exit;
        End
      Else
        Begin {composite index}
          If (idCount = 0) Then
            Exit;
          KeyLen := 0;
          For i := 0 To pred(idCount) Do
            Begin
              If (idFields[i] < 0) Or (idFields[i] >= FFldCount) Then
                Exit;
              If (idFieldsSize[i] > 0) And (FieldDescriptor^[idfields[i]]^.fdType In [fstShortString,
                fstVarNullString, fstNullString]) Then
                inc(KeyLen, idFieldsSize[i] + 1)
              Else If (idFieldsSize[i] > 0) And (FieldDescriptor^[idfields[i]]^.fdType In [fstWideString, fstVarWideString]) Then
                inc(KeyLen, (idFieldsSize[i] + 1) * sizeof(WideChar))
              Else
                inc(KeyLen, FieldDescriptor^[idfields[i]]^.fdLength);
            End;
          inc(KeyLen, (idCount + 7) Div 8);
          If (KeyLen > fscl_MaxKeyLength) Then
            Exit;
        End;
    End;
  Result := True;
End;
{--------}

Function TFSInfoDict.IsRecordFieldNull(aField: Integer;
  aData: PffByteArray): boolean;
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds,
      [FBaseName, aField]);
  Result := (aData = Nil) Or
    FFIsBitSet(PffByteArray(@aData^[FLogRecLen]), aField);
End;
{--------}

Procedure TFSInfoDict.WriteToStream(S: TStream);
Var
  Writer: TWriter;
  i, j, k: Integer;
  FileDesc: PffFileDescriptor;
  FldDesc: PffFieldDescriptor;
  InxDesc: PffIndexDescriptor;
  s1: String;
  Locale: Integer;
  fsSignStream, b: Byte;
Begin
  CheckValid;
  Writer := TWriter.Create(S, 4096);
  Try
    With Writer Do
      Begin
        b := 0;
        k := 0;
        fsSignStream := fsSignSignatueStream;
        Write(fsSignStream, 1);
        s1 := '$FSDICTIONARY$';
        fsWriteString(Writer, s1);
        s1 := '';
        WriteInteger(fsVersionNumber);
        Write(b, 1);
        Write(Byte(FIsEncrypted), 1); // 0 none 1 std .....
        Write(k, 4);
        WriteInteger(FFileCount);
        For i := 0 To pred(FFileCount) Do
          Begin
            FileDesc := PffFileDescriptor(ddFileList[i]);
            With FileDesc^ Do
              Begin
                fsWriteString(Writer, fdDesc); //TffDictItemDesc
                fsWriteString(Writer, fdExtension);
                WriteInteger(fdBlockSize);
                WriteInteger(ord(fdType));
              End;
          End;
        WriteInteger(FFldCount);
        WriteInteger(ord(ddTableType));
        Write(fEngineDeleteType, 1);
        Write(fVersionRecord, 1);

        For i := 0 To pred(FFldCount) Do
          Begin
            FldDesc := FieldDescriptor^[i];
            With FldDesc^ Do
              Begin
                fsWriteString(Writer, fdName);
                fsWriteString(Writer, fdDesc);
                fsWriteString(Writer, fdDescription);
                fsWriteString(Writer, fdDisplayMask);
                fsWriteString(Writer, fdEditMask);
                WriteInteger(fdUnits);
                WriteInteger(fdDecPl);
                WriteInteger(fdOffset);
                WriteInteger(fdLength);
                WriteInteger(ord(fdType));
                Write(fdRequired, 2);
                Write(fdReadOnly, 2);
                Write(fdVisible, 2);
                WriteInteger(ord(fdBlobLevelComp));
                Write(fdRound, 1);
                Write(fdIsCalculated, 2);
                Write(fdEmptyAsNull, 2);
                Write(b, 1);
                Try
                  fdOEMCodePage := 0; //GetOEMCP
                  fdCodePage := GetACP; // ansi codepage
                Except
                  fdCodePage := 0; // is default system
                  fdOEMCodePage := 0;
                End;
                Try
                  fdLocale := GetUserDefaultLCID;
                Except
                  fdLocale := 0;
                End;
                Write(fdCodePage, 4);
                Write(fdOemCodePage, 4);
                Write(fdLocale, 4);
                Write(fdDefaultUpdate, 1);
                fsWriteString(Writer, s1);
                fsWriteString(Writer, s1);
                fsWriteString(Writer, s1);
                Write(fdReserved, sizeof(TfsReserved));
                WriteBoolean(fdVCheck <> Nil);
                If (fdVCheck <> Nil) Then
                  Begin
                    With fdVCheck^ Do
                      Begin
                        fsWriteString(Writer, vdPicture);
                        Write(vdHasMinVal, 2);
                        Write(vdHasMaxVal, 2);
                        Write(vdHasDefVal, 2);
                        WriteInteger(0);
                        WriteInteger(0);
                        If vdHasMinVal Then
                          Write(vdMinVal, sizeof(vdMinVal));
                        If vdHasMaxVal Then
                          Write(vdMaxVal, sizeof(vdMaxVal));
                        If vdHasDefVal Then
                          Write(vdDefVal, sizeof(vdDefVal));
                      End;
                  End;
              End;
          End;
        WriteInteger(FLogRecLen);
        WriteInteger(FInxCount);
        {note we don't write index 0 to the stream}
        For i := 1 To pred(FInxCount) Do
          Begin
            InxDesc := IndexDescriptor^[i];
            With InxDesc^ Do
              Begin
                fsWriteString(Writer, idName);
                fsWriteString(Writer, idDesc);
                Write(IdIndexType, SizeOf(TfsIndexType));
                fsWriteString(Writer, s1);
                fsWriteString(Writer, s1);
                fsWriteString(Writer, s1);
                fsWriteString(Writer, s1);
                WriteInteger(idFile);
                WriteInteger(idKeyLen);
                WriteInteger(idCount);
                Write(b, 1);
                Write(b, 1);
                If (idCount <> -1) Then
                  For j := 0 To pred(idCount) Do
                    Begin
                      WriteInteger(idFields[j]);
                      WriteInteger(idFieldsAscDesc[j]);
                      WriteInteger(idFieldsSize[j]);
                      WriteInteger(idFieldsCase[j]);
                      WriteInteger(idFieldsFlags[j]);
                      WriteInteger(idFieldsNullTop[j]);
                      Try
                        Locale := GetACP; // ansi codepage
                      Except
                        Locale := 0;
                      End;
                      WriteInteger(Locale);
                      Locale := 0; //oemcodepage
                      WriteInteger(Locale);
                      Try
                        Locale := GetUserDefaultLCID; //LOCALE_SYSTEM_DEFAULT;
                      Except
                        Locale := 0;
                      End;
                      WriteInteger(Locale);
                      Write(b, 1);
                      Write(b, 1);
                      fsWriteString(Writer, s1);
                      fsWriteString(Writer, s1);
                      fsWriteString(Writer, s1);
                      fsWriteString(Writer, s1);
                      b := 0;
                      If Length(idFieldIHlprs[j]) > 0 Then
                        b := 1;
                      Write(b, 1);
                      If b = 1 Then
                        fsWriteString(Writer, idFieldIHlprs[j]);
                    End;

                Write(idDups, 2);
                // for expression and user index dll
                idAscend := False;
                idNoCase := False;
                Write(idAscend, 2);
                Write(idNoCase, 2);
              End;
          End;
      End;
  Finally
    Writer.Free;
    tmpVersion := fsVersionNumber;
  End; {try..finally}
End;

Function aReadString(Stream: TReader): String;
Var
  L: Integer;
  ReadValue: TValueType;

  Procedure ReadError(Ident: String);
  Begin
    Raise EReadError.Create(Ident);
  End;

  Procedure PropValueError;
  Begin
    ReadError('Invalid Property Value');
  End;
Begin
  L := 0;
  Stream.Read(ReadValue, SizeOf(TValueType));
  Case ReadValue Of
    vaString:
      Stream.Read(L, SizeOf(Byte));
    vaLString:
      Stream.Read(L, SizeOf(Integer));
    Else
      PropValueError;
  End;
  SetString(Result, PChar(Nil), L);
  Stream.Read(Pointer(Result)^, L);
  Result := Trim(Result);
End;

Procedure TFSInfoDict.OldReadFromStream(SignStream: Byte; Reader: TReader; S: TStream);
Var
  i, j, k: Integer;
  FileDesc: PffFileDescriptor;
  FldDesc: PffFieldDescriptor;
  InxDesc: PffIndexDescriptor;
  HasVCheck: Boolean;
  w: Word;
  s1: String;
  ires: Integer;
  bb: Boolean;
  br: Array[0..4] Of Byte;
Begin
  With Reader Do
    Begin
      FBLOBFileNumber := 0;
      FIsEncrypted := TValueType(SignStream) = vaTrue; //9
      FFileCount := ReadInteger;
      Try
        For i := 0 To pred(FFileCount) Do
          Begin
            FFGetZeroMem(FileDesc, sizeof(TffFileDescriptor));
            With FileDesc^ Do
              Begin
                fdNumber := i;
                fdDesc := aReadString(Reader);
                fdExtension := aReadString(Reader);
                fdBlockSize := ReadInteger;
                fdType := TffFileType(ReadInteger);
                If (fdType = ftBLOBFile) Then
                  FBLOBFileNumber := i;
              End;
            ddFileList.Add(pointer(FileDesc));
            FileDesc := Nil;
          End;
      Except
        If Assigned(FileDesc) Then
          FFFreeMem(FileDesc, sizeOf(TffFileDescriptor));
        Raise;
      End; {try..except}
      FFldCount := ReadInteger;
      ddTableType := TTableType(ReadInteger);
      VersionStream := ReadInteger; //VersionStream, fsVersionNumber
      s1 := aReadString(Reader); //res
      w := ReadInteger;
      w := ReadInteger;
      If VersionStream >= 1027 Then
        k := ReadInteger;
      If VersionStream >= 1028 Then
        k := ReadInteger;
      If VersionStream >= 1038 Then
        Begin
          Read(k, 4);
          Read(k, 4);
        End;
      If VersionStream >= 1040 Then
        Read(k, 4);
      If VersionStream >= 1060 Then
        Read(fEngineDeleteType, 1);
      If VersionStream >= 1062 Then
        Read(fVersionRecord, 1);

      ddExpandFieldArray(FFldCount + 1);
      Try
        For i := 0 To pred(FFldCount) Do
          Begin
            FFGetZeroMem(FldDesc, sizeof(TffFieldDescriptor));
            With FldDesc^ Do
              Begin
                fdNumber := i;
                fdName := aReadString(Reader);
                If fdName = '' Then fdName := 'EmptyName' + IntToStr(i);
                fdDesc := aReadString(Reader);
                fdDescription := aReadString(Reader);
                If VersionStream >= 1037 Then
                  Begin
                    fdDisplayMask := aReadString(Reader);
                    fdEditMask := aReadString(Reader);
                  End;
                fdUnits := ReadInteger;
                fdDecPl := ReadInteger;
                fdOffset := ReadInteger;
                fdLength := ReadInteger;
                fdType := TfsFieldType(ReadInteger);
                If (VersionStream = 0) Or (VersionStream <= 1026) Then
                  Begin
                    Case fdType Of
                      fstBLOB: fdType := fstBLOB;
                      fstBLOBMemo: fdType := fstBLOBMemo;
                      fstBLOBGraphic: fdType := fstBLOBMemo;
                      fstClob: fdType := fstBLOB;
                      fstWideClob: fdType := fstBLOBGraphic;
                      fstInterval: fdType := fstBLOB;
                      fstReserved1: fdType := fstBLOB;
                      fstReserved2: fdType := fstBLOB;
                    End;
                  End;

                If (VersionStream = 0) Or (VersionStream <= 1077) Then
                  Begin
                    Case fdType Of
                      fstVarNullString: fdType := fstShortString;
                      fstVarWideString: fdType := fstNullString;
                    End;
                  End;

                fdRequired := ReadBoolean;
                fdReadOnly := ReadBoolean;
                fdVisible := ReadBoolean;
                fdBlobLevelComp := TDataCompLevel(ReadInteger);
                read(fdRound, 1);
                If fdRound <> rNone Then IsAnyRound := True;
                read(fdIsCalculated, 2);
                read(fdxxx1, 2);
                read(fdEmptyAsNull, 2);
                If VersionStream < fsVersionNumber Then
                  fdEmptyAsNull := False;
                If fdEmptyAsNull Then IsAnyEmptyAsNull := True;
                read(fdCodePage, 4);
                read(fdOemCodePage, 4);
                read(fdLocale, 4);
                Read(fdDefaultUpdate, 1);
                If fdDefaultUpdate In [duIFNULL, duALWAYS] Then fIsAnyDefault := True;
                If fdtype = fstRecVersion Then fIsRecVersion := True;
                If VersionStream <= 1045 Then
                  Begin
                    fdDefaultUpdate := duNormal;
                    Try
                      fdOEMCodePage := 0; //GetOEMCP
                      fdCodePage := GetACP; // ansi codepage
                    Except
                      fdCodePage := 0; // is default system
                      fdOEMCodePage := 0;
                    End;
                    Try
                      fdLocale := GetUserDefaultLangID;
                    Except
                      fdLocale := 0;
                    End;
                  End;
                If VersionStream >= 1052 Then
                  Begin
                    s1 := fsReadString(reader);
                    s1 := fsReadString(reader);
                    s1 := fsReadString(reader);
                  End;
                Read(fdReserved, SizeOf(TfsReserved));
                HasVCheck := ReadBoolean;
                If HasVCheck Then
                  Begin
                    FFGetZeroMem(fdVCheck, sizeof(TffVCheckDescriptor));
                    With fdVCheck^ Do
                      Begin
                        vdPicture := aReadString(Reader);
                        vdHasMinVal := ReadBoolean;
                        vdHasMaxVal := ReadBoolean;
                        vdHasDefVal := ReadBoolean;
                        vdres := Byte(ReadInteger);
                        vdres1 := Byte(ReadInteger);
                        {if the field has a default value, we add the field
                         number to ddDefFldList}
                        If vdHasDefVal Then
                          Begin
                            ddDefFldList.Add(Pointer(i));
                          End;
                        If vdHasMinVal Then
                          Begin
                            Read(vdMinVal, sizeof(vdMinVal));
                          End;
                        If vdHasMaxVal Then
                          Begin
                            Read(vdMaxVal, sizeof(vdMaxVal));
                          End;
                        If vdHasDefVal Then
                          Begin
                            Read(vdDefVal, sizeof(vdDefVal));
                          End;
                      End;
                  End;
              End;
            FieldDescriptor^[i] := FldDesc;
            FldDesc := Nil;
          End;
      Except
        If Assigned(FldDesc) Then
          FFFreeMem(FldDesc, sizeOf(TffFieldDescriptor));
        Raise;
      End; {try..except}
      FLogRecLen := ReadInteger;
      FInxCount := ReadInteger;
      ddExpandIndexArray(FInxCount + 1);
      Try
        {note that index 0 is never stored on a stream}
        For i := 1 To pred(FInxCount) Do
          Begin
            FFGetZeroMem(InxDesc, sizeof(TffIndexDescriptor));
            With InxDesc^ Do
              Begin
                idNumber := i;
                idName := aReadString(Reader);
                idDesc := aReadString(Reader);
                idFile := ReadInteger;
                idKeyLen := ReadInteger;
                idCount := ReadInteger;
                If idCount = -1 Then
                  IdIndexType := itSysRef
                Else
                  IdIndexType := itcomposite;
                If (idCount <> -1) Then
                  For j := 0 To pred(idCount) Do
                    Begin
                      idFields[j] := ReadInteger;
                      idFieldsAscDesc[j] := ReadInteger;
                      If VersionStream >= 1052 Then
                        idFieldsSize[j] := ReadInteger
                      Else
                        idFieldsSize[j] := 0; // if > 0 then use
                      idFieldsCase[j] := 1;
                      idFieldsNullTop[j] := 1;
                      If NextValue = vaString Then
                        idFieldIHlprs[j] := aReadString(Reader)
                      Else
                        idFieldIHlprs[j] := '';
                    End;
                idDups := ReadBoolean;
                idAscend := ReadBoolean; //Old Ascend
                idNoCase := ReadBoolean; //Old NoCase
                idFieldsCase[0] := Byte(idNoCase);
                If VersionStream >= 1046 Then
                  Begin
                    ires := ReadInteger;
                    ires := ReadInteger;
                    ires := ReadInteger;
                    read(br, sizeof(br));
                  End;
                If VersionStream >= 1051 Then
                  s1 := fsReadString(Reader);
                If VersionStream >= 1064 Then
                  Begin
                    s1 := fsReadString(Reader);
                    s1 := fsReadString(Reader);
                  End;
                If VersionStream >= 1052 Then
                  Begin
                    s1 := fsReadString(Reader);
                    bb := ReadBoolean;
                  End;
                If VersionStream >= 1054 Then
                  Read(ires, 1);
              End;
            IndexDescriptor^[i] := InxDesc;
            InxDesc := Nil;
          End;
      Except
        If Assigned(InxDesc) Then
          FFFreeMem(InxDesc, sizeOf(TffIndexDescriptor));
        Raise;
      End; {try..except}
    End;
End;

Procedure TFSInfoDict.ReadFromStream(S: TStream);
Var
  Reader: TReader;
  i, j, k: Integer;
  FileDesc: PffFileDescriptor;
  FldDesc: PffFieldDescriptor;
  InxDesc: PffIndexDescriptor;
  HasVCheck: Boolean;
  s1: String;
  Locale: Integer;
  fsSignStream, b: Byte;
Begin
  ClearPrim(True);
  IsAnyEmptyAsNull := False;
  IsAnyRound := False;
  fIsAnyDefault := False;
  fIsRecVersion := False;
  Reader := TReader.Create(S, 4096);
  Try
    With Reader Do
      Begin
        FBLOBFileNumber := 0;
        Read(fsSignStream, 1);
        If fsSignStream <> fsSignSignatueStream Then // old stream
          OldReadFromStream(fsSignStream, Reader, S)
        Else
          Begin // new
            s1 := fsReadString(Reader); // dict
            VersionStream := ReadInteger;
            read(b, 1);
            Read(fsSignStream, 1);
            Read(k, 4);
            FIsEncrypted := boolean(fsSignStream);
            FFileCount := ReadInteger;
            Try
              For i := 0 To pred(FFileCount) Do
                Begin
                  FFGetZeroMem(FileDesc, sizeof(TffFileDescriptor));
                  With FileDesc^ Do
                    Begin
                      fdNumber := i;
                      fdDesc := fsReadString(Reader);
                      fdExtension := fsReadString(Reader);
                      fdBlockSize := ReadInteger;
                      fdType := TffFileType(ReadInteger);
                      If (fdType = ftBLOBFile) Then
                        FBLOBFileNumber := i;
                    End;
                  ddFileList.Add(pointer(FileDesc));
                  FileDesc := Nil;
                End;
            Except
              If Assigned(FileDesc) Then
                FFFreeMem(FileDesc, sizeOf(TffFileDescriptor));
              Raise;
            End; {try..except}
            FFldCount := ReadInteger;
            ddTableType := TTableType(ReadInteger);
            Read(fEngineDeleteType, 1);
            Read(fVersionRecord, 1);

            ddExpandFieldArray(FFldCount + 1);
            Try
              For i := 0 To pred(FFldCount) Do
                Begin
                  FFGetZeroMem(FldDesc, sizeof(TffFieldDescriptor));
                  With FldDesc^ Do
                    Begin
                      fdNumber := i;
                      fdName := fsReadString(Reader);
                      If fdName = '' Then fdName := 'EmptyName' + IntToStr(i);
                      fdDesc := fsReadString(Reader);
                      fdDescription := fsReadString(Reader);
                      fdDisplayMask := fsReadString(Reader);
                      fdEditMask := fsReadString(Reader);
                      fdUnits := ReadInteger;
                      fdDecPl := ReadInteger;
                      fdOffset := ReadInteger;
                      fdLength := ReadInteger;
                      fdType := TfsFieldType(ReadInteger);
                      read(fdRequired, 2);
                      read(fdReadOnly, 2);
                      read(fdVisible, 2);
                      fdBlobLevelComp := TDataCompLevel(ReadInteger);
                      read(fdRound, 1);
                      If fdRound <> rNone Then IsAnyRound := True;
                      read(fdIsCalculated, 2);
                      read(fdEmptyAsNull, 2);
                      read(b, 1);
                      If fdEmptyAsNull Then IsAnyEmptyAsNull := True;
                      read(fdCodePage, 4);
                      read(fdOemCodePage, 4);
                      read(fdLocale, 4);
                      Read(fdDefaultUpdate, 1);
                      If fdDefaultUpdate In [duIFNULL, duALWAYS] Then fIsAnyDefault := True;
                      If fdtype = fstRecVersion Then fIsRecVersion := True;
                      Try
                        fdOEMCodePage := 0; //GetOEMCP
                        fdCodePage := GetACP; // ansi codepage
                      Except
                        fdCodePage := 0; // is default system
                        fdOEMCodePage := 0;
                      End;
                      Try
                        fdLocale := GetUserDefaultLangID;
                      Except
                        fdLocale := 0;
                      End;
                      s1 := fsReadString(reader);
                      s1 := fsReadString(reader);
                      s1 := fsReadString(reader);
                      Read(fdReserved, SizeOf(TfsReserved));
                      HasVCheck := ReadBoolean;
                      If HasVCheck Then
                        Begin
                          FFGetZeroMem(fdVCheck, sizeof(TffVCheckDescriptor));
                          With fdVCheck^ Do
                            Begin
                              vdPicture := fsReadString(Reader);
                              Read(vdHasMinVal, 2);
                              Read(vdHasMaxVal, 2);
                              Read(vdHasDefVal, 2);
                              vdres := Byte(ReadInteger);
                              vdres1 := Byte(ReadInteger);
                              {if the field has a default value, we add the field
                               number to ddDefFldList}
                              If vdHasDefVal Then
                                ddDefFldList.Add(Pointer(i));
                              If vdHasMinVal Then
                                Read(vdMinVal, sizeof(vdMinVal));
                              If vdHasMaxVal Then
                                Read(vdMaxVal, sizeof(vdMaxVal));
                              If vdHasDefVal Then
                                Read(vdDefVal, sizeof(vdDefVal));
                            End;
                        End;
                    End;
                  FieldDescriptor^[i] := FldDesc;
                  FldDesc := Nil;
                End;
            Except
              If Assigned(FldDesc) Then
                FFFreeMem(FldDesc, sizeOf(TffFieldDescriptor));
              Raise;
            End; {try..except}
            FLogRecLen := ReadInteger;
            FInxCount := ReadInteger;
            ddExpandIndexArray(FInxCount + 1);
            Try
              {note that index 0 is never stored on a stream}
              For i := 1 To pred(FInxCount) Do
                Begin
                  FFGetZeroMem(InxDesc, sizeof(TffIndexDescriptor));
                  With InxDesc^ Do
                    Begin
                      idNumber := i;
                      idName := fsReadString(Reader);
                      idDesc := fsReadString(Reader);
                      Read(IdIndexType, SizeOf(TfsIndexType));
                      s1 := fsReadString(Reader);
                      s1 := fsReadString(Reader);
                      s1 := fsReadString(Reader);
                      s1 := fsReadString(Reader);
                      idFile := ReadInteger;
                      idKeyLen := ReadInteger;
                      idCount := ReadInteger;
                      read(b, 1);
                      read(b, 1);
                      If (idCount <> -1) Then
                        For j := 0 To pred(idCount) Do
                          Begin
                            idFields[j] := ReadInteger;
                            idFieldsAscDesc[j] := ReadInteger;
                            idFieldsSize[j] := ReadInteger; // if > 0 then use
                            idFieldsCase[j] := ReadInteger;
                            idFieldsFlags[j] := ReadInteger;
                            idFieldsNullTop[j] := ReadInteger;
                            Locale := ReadInteger; // codepage
                            Locale := ReadInteger; // oemcodepage
                            Locale := ReadInteger; // langid
                            Read(b, 1);
                            read(b, 1);
                            s1 := fsReadString(Reader);
                            s1 := fsReadString(Reader);
                            s1 := fsReadString(Reader);
                            s1 := fsReadString(Reader);
                            read(b, 1);
                            If b = 1 Then
                              idFieldIHlprs[j] := fsReadString(Reader)
                            Else
                              idFieldIHlprs[j] := '';
                          End;
                      Read(idDups, 2);
                      Read(idAscend, 2); //Old Ascend
                      Read(idNoCase, 2); //Old NoCase
                    End;
                  IndexDescriptor^[i] := InxDesc;
                  InxDesc := Nil;
                End;
            Except
              If Assigned(InxDesc) Then
                FFFreeMem(InxDesc, sizeOf(TffIndexDescriptor));
              Raise;
            End; {try..except}
          End;
      End;
  Finally
    Reader.Free;
    tmpVersion := fsVersionNumber;
  End; {try..finally}
End;

{--------}

Procedure TFSInfoDict.RemoveField(aField: Longint);
Var
  TempDesc: PffFieldDescriptor;
  NewOffset: Integer;
  Inx, {!!.13}
  FldInx: Integer; {!!.13}
  InxDesc: PffIndexDescriptor; {!!.13}
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  If (0 <= aField) And (aField < FFldCount) Then
    Begin
      {Begin !!.13}
          { Verify the field is not being used by an index. }
      For Inx := Pred(IndexCount) Downto 0 Do
        Begin
          InxDesc := IndexDescriptor[Inx];
          For FldInx := 0 To Pred(InxDesc^.idCount) Do
            If InxDesc^.idFields[FldInx] = aField Then
              FSRaiseException(EfsException, fsStrResGeneral, fserrFileInUse,
                [aField]);
        End;
      {End !!.13}
      FHasBLOBs := fftbUnknown; {!!.03}
      TempDesc := FieldDescriptor^[aField];
      NewOffset := TempDesc^.fdOffset;
      FFFreeMem(TempDesc, sizeOf(TffFieldDescriptor));
      { Shift fields down to cover the empty space. }
      For Inx := aField To (FFldCount - 2) Do
        FieldDescriptor^[Inx] := FieldDescriptor^[succ(Inx)]; {!!.01}
      dec(FFldCount);
      {patch up all successive descriptors}
      For Inx := aField To pred(FFldCount) Do
        Begin
          TempDesc := FieldDescriptor^[Inx];
          With TempDesc^ Do
            Begin
              fdNumber := Inx;
              fdOffset := NewOffset;
              inc(NewOffset, fdLength);
            End;
        End;
      FLogRecLen := NewOffset;
    End;
End;
{--------}

Procedure TFSInfoDict.RemoveFile(aFile: Longint);
Var
  TempDesc: PffFileDescriptor;
  Inx: Integer;
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  {can't remove entry 0: it's the base file}
  If (aFile = 0) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrBaseFile, [FBaseName]);
  {remove the entry}
  If (0 < aFile) And (aFile < FFileCount) Then
    Begin
      TempDesc := PffFileDescriptor(ddFileList.Items[aFile]);
      {if the BLOB file is being removed from the dictionary then reset
       the BLOB file number field}
      If (TempDesc^.fdType = ftBLOBFile) Then
        FBLOBFileNumber := 0;
      {Begin!!.13}
          { If an index file is being removed from the dictionary then make sure
            it is not referenced by an index. }
      If (TempDesc^.fdType = ftIndexFile) Then
        Begin
          For Inx := pred(FInxCount) Downto 0 Do
            If (IndexDescriptor^[Inx]^.idFile = aFile) Then
              FSRaiseException(EfsException, fsStrResGeneral, fserrFileInUse,
                [aFile]);
          { Fixup index descriptors referencing files with higher file numbers. }
          For Inx := Pred(IndexCount) Downto 0 Do
            If (IndexDescriptor^[Inx]^.idFile > aFile) Then
              Dec(IndexDescriptor^[Inx]^.idFile);
        End; { if }
      {End !!.13}

      FFFreeMem(TempDesc, sizeOf(TffFileDescriptor));
      ddFileList.Delete(aFile);
      dec(FFileCount);
      {patch up all successive descriptors}
      For Inx := aFile To pred(FFileCount) Do
        Begin
          TempDesc := PffFileDescriptor(ddFileList[Inx]);
          TempDesc^.fdNumber := Inx;
        End;
    End;
End;
{--------}

Procedure TFSInfoDict.RemoveIndex(aIndex: Longint);
Var
  TempDesc: PffIndexDescriptor;
  Inx: Integer;
Begin
  (*
  {can't be done in readonly mode}
  if ddReadOnly then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  *)
  {remove the entry}
  If (0 <= aIndex) And (aIndex < FInxCount) Then
    Begin
      TempDesc := IndexDescriptor^[aIndex];
      FFFreeMem(TempDesc, sizeOf(TffIndexDescriptor));
      {Begin !!.02}
          { Shift the descriptors above the deleted index down to fill in
            the gap. }
      For Inx := aIndex To (FInxCount - 2) Do
        Begin
          IndexDescriptor^[Inx] := IndexDescriptor^[succ(Inx)];
          IndexDescriptor^[Inx]^.idNumber := Inx;
        End;
      dec(FInxCount);
    End;
  {End !!.02}
End;
{--------}

Procedure TFSInfoDict.SetBaseName(Const BN: TfsTableName);
Begin
  FBaseName := BN;
End;
{--------}

Procedure TFSInfoDict.SetBlockSize(BS: Longint);
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  If (BS <> BlockSize) And FFVerifyBlockSize(BS) Then
    If (BS > BlockSize) Or
      (RecordLength <= (BS - fsc_BlockHeaderSizeData - sizeof(Longint))) Then
      Begin
        If (FFileCount > 0) Then
          PffFileDescriptor(ddFileList.Items[0])^.fdBlockSize := BS;
      End;
End;
{Begin !!.11}
{--------}

Function WeekNo(ADate: TDateTime): Integer;
Var
  AYear, AMonth, ADay: Word;
  //TheYear : Word;      // week, to which the week belongs
  AWeekDay: Word; // Day of week for 1. Jan
  ANumDays: Integer; // Days since 1. Jan
  AFirstDayOfYear: TDateTime; // Date of 1. Jn
Begin
  Try
    DecodeDate(ADate, AYear, AMonth, ADay);
    //TheYear := AYear;
    AFirstDayOfYear := EncodeDate(AYear, 1, 1);
    AWeekDay := SysUtils.DayOfWeek(AFirstDayOfYear);
    ANumDays := Trunc(Int(ADate) - AFirstDayOfYear) + (7 - SysUtils.DayOfWeek(ADate - 1)) +
      (7 * Ord(AWeekDay In [2..5]));
    Result := ANumDays Div 7;
    If Result = 0 Then
      Begin
        If (SysUtils.DayOfWeek(EncodeDate(AYear - 1, 1, 1)) > 5) Or
          (SysUtils.DayOfWeek(EncodeDate(AYear - 1, 12, 31)) < 5) Then
          Result := 52
        Else
          Result := 53;
        //TheYear := AYear - 1;
      End
    Else If Result = 53 Then
      If (AWeekDay > 5) Or (SysUtils.DayOfWeek(EncodeDate(AYear, 12, 31)) < 5) Then
        Begin
          Result := 1;
          //TheYear := AYear + 1;
        End;
  Except
    Result := 0;
  End;
End;

Function WeekNo1(ADate: TDateTime): Integer;
Var
  DayOne: TDateTime;
  WDay, WMonth, WYear: Word;
  Nr: Integer;
Begin
  DecodeDate(ADate, WYear, WMonth, WDay);

  DayOne := EncodeDate(WYear, 1, 1); { januari first }

  Nr := (Trunc(ADate - DayOne) Div 7) + 1; { weeknr }
  If Nr > 52 Then
    Nr := 1; { correction for week 53 }
  WeekNo1 := Nr;
End;

Function OnlyYear(ADate: TDateTime): Integer;
Var
  WDay, WMonth, WYear: Word;
Begin
  DecodeDate(ADate, WYear, WMonth, WDay);
  Result := WYear;
End;

Function OnlyMonth(ADate: TDateTime): Integer;
Var
  WDay, WMonth, WYear: Word;
Begin
  DecodeDate(ADate, WYear, WMonth, WDay);
  Result := WMonth;
End;

Function OnlyDay(ADate: TDateTime): Integer;
Var
  WDay, WMonth, WYear: Word;
Begin
  DecodeDate(ADate, WYear, WMonth, WDay);
  Result := WDay;
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
              BArr[idx] := 0;
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

Procedure TFSInfoDict.SetDefaultFieldUpdateValue(aData: PffByteArray; Const aField: Integer);
Var
  i: Integer;
  BS: PffByteArray;
  CurrField: PffByteArray;
  HasDefault: Boolean;
  S, S1: AnsiString;
  D: Double;
  T: Longint;
  V: TffVCheckValue;
  TempDT: TDateTime;
  TempInt16: Smallint;
  TempInt64: Int64; {!!.13}
  TempInt: Longint;
  TempExtend: Extended;
  TempCurrency: Currency;
  TempSingle: Single;
  TempDouble: Double;
  StS: ShortString;
  dt: TfsDefaultType;
  intres: Integer;
  ByteArrayBuffer: Pointer;

  Function MapBDEDataToFF(PhySize: Integer; aSource: pointer; aDest: pointer): boolean;
  Begin
    // If Not IGNORE_ANSIOEM Then
      // AnsiToOEM(aSource, aSource);
    StrLCopy(aDest, aSource, pred(PhySize));
  End;

  Procedure CDefault;
  Var
    j: Integer;
    p: pointer;
  Begin
    S := '';
    j := 0;
    FillChar(V, SizeOf(V), 0);
    If FieldDescriptor^[aField]^.fdType In [fstWideString, fstVarWideString] Then
      Begin
        Try
          i := 0;
          While ((char(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal[i])) +
            (char(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal[succ(i)]))) <> #0#0 Do
            inc(i);
          If i > 0 Then
            Begin
              ffgetmem(p, i);
              Try
                fillchar(p^, i, 0);
                Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, p^, i);
                s := WideCharLenToString(PWideChar(p), (i Div 2) + 1);
                S := Trim(s);
              Finally
                fffreemem(p, i);
              End;
            End;
        Except
          s := '';
        End;
        dt := fsCheckDefaultType(S);
      End
    Else
      Begin
        While FieldDescriptor^[aField]^.fdVCheck^.vdDefVal[j] <> 0 Do
          Begin
            inc(j);
            If FieldDescriptor^[aField]^.fdType In [fstNullString, fstVarNullString {, fstWideString , fstUnicode}] Then
              s := s + chr(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal[j - 1])
            Else
              s := s + chr(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal[j]);
          End;
        SetLength(S, j);
        S := Trim(s);
        dt := fsCheckDefaultType(S);
      End;

    If s <> '' Then
      Begin
        Case FieldDescriptor^[aField]^.fdType Of
          fstTime, fstDate, fstDateTime:
            Begin
              If dt In [dtNow] Then
                Begin
                  Case FieldDescriptor^[aField]^.fdType Of
                    fstDateTime:
                      Begin
                        TempDT := Now;
                        fsSetMillisecond(TempDT, 0);
                        D := TempDT + 693594;
                        move(D, CurrField^, FieldDescriptor^[aField]^.fdLength);
                      End;
                    fstDate:
                      Begin
                        T := DateTimeToStDate(Now);
                        move(T, CurrField^, FieldDescriptor^[aField]^.fdLength);
                      End;
                    fstTime:
                      Begin
                        T := DateTimeToStTime(Now);
                        move(T, CurrField^, FieldDescriptor^[aField]^.fdLength);
                      End;
                  End;
                End
              Else
                Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                  FieldDescriptor^[aField]^.fdLength);
            End;

          fstUInt16, fstInt16:
            Begin
              Case dt Of
                dtYear:
                  Begin
                    TempInt := OnlyYear(Now);
                    TempInt16 := TempInt;
                    Move(TempInt16, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempInt16 := OnlyMonth(Now);
                    Move(TempInt16, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempInt16 := OnlyDay(Now);
                    Move(TempInt16, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempInt16 := WeekNo(Now);
                    Move(TempInt16, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[aField]^.fdLength);
              End;
            End;

          fstInt32, fstUInt32:
            Begin
              Case dt Of
                dtYear:
                  Begin
                    TempInt := OnlyYear(Now);
                    Move(TempInt, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempInt := OnlyMonth(Now);
                    Move(TempInt, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempInt := OnlyDay(Now);
                    Move(TempInt, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempInt := WeekNo(Now);
                    Move(TempInt, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[aField]^.fdLength);
              End;
            End;
          fstInt64:
            Begin
              Case dt Of
                dtYear:
                  Begin
                    TempInt64 := OnlyYear(Now);
                    Move(TempInt64, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempInt64 := OnlyMonth(Now);
                    Move(TempInt64, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempInt64 := OnlyDay(Now);
                    Move(TempInt64, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempInt64 := WeekNo(Now);
                    Move(TempInt64, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[aField]^.fdLength);
              End;
            End;
          fstSingle:
            Begin
              Case dt Of
                dtYear:
                  Begin
                    TempSingle := OnlyYear(Now);
                    Move(TempSingle, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempSingle := OnlyMonth(Now);
                    Move(TempSingle, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempSingle := OnlyDay(Now);
                    Move(TempSingle, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempSingle := WeekNo(Now);
                    Move(TempSingle, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Begin
                    Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                      FieldDescriptor^[aField]^.fdLength);
                    TempSingle := pSingle(CurrField)^;
                    TempSingle := RoundExtended(TempSingle,
                      FieldDescriptor^[aField]^.fdDecPl, FieldDescriptor^[aField]^.fdRound);
                    Move(TempSingle, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
              End;
            End;
          fstDouble:
            Begin
              Case dt Of
                dtNow:
                  Begin
                    TempDouble := Now;
                    TempDouble := TempDouble + 693594;
                    TempDouble := RoundExtended(TempDouble,
                      FieldDescriptor^[aField]^.fdDecPl, FieldDescriptor^[aField]^.fdRound);
                    move(TempDouble, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtYear:
                  Begin
                    TempDouble := OnlyYear(Now);
                    Move(TempDouble, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempDouble := OnlyMonth(Now);
                    Move(TempDouble, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempDouble := OnlyDay(Now);
                    Move(TempDouble, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempDouble := WeekNo(Now);
                    Move(TempDouble, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Begin
                    Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                      FieldDescriptor^[aField]^.fdLength);
                    TempDouble := pDouble(CurrField)^;
                    TempDouble := RoundExtended(TempDouble,
                      FieldDescriptor^[aField]^.fdDecPl, FieldDescriptor^[aField]^.fdRound);
                    Move(TempDouble, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
              End;
            End;
          fstExtended:
            Begin
              Case dt Of
                dtNow:
                  Begin
                    TempExtend := Now;
                    TempExtend := TempExtend + 693594;
                    TempExtend := RoundExtended(TempExtend,
                      FieldDescriptor^[aField]^.fdDecPl, FieldDescriptor^[aField]^.fdRound);
                    move(TempExtend, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtYear:
                  Begin
                    TempExtend := OnlyYear(Now);
                    Move(TempExtend, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempExtend := OnlyMonth(Now);
                    Move(TempExtend, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempExtend := OnlyDay(Now);
                    Move(TempExtend, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempExtend := WeekNo(Now);
                    Move(TempExtend, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Begin
                    Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                      FieldDescriptor^[aField]^.fdLength);
                    TempExtend := pExtended(CurrField)^;
                    TempExtend := RoundExtended(TempExtend,
                      FieldDescriptor^[aField]^.fdDecPl, FieldDescriptor^[aField]^.fdRound);
                    Move(TempExtend, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
              End;
            End;
          fstCurrency:
            Begin
              Case dt Of
                dtNow:
                  Begin
                    TempDouble := Now;
                    TempDouble := TempDouble + 693594;
                    TempDouble := RoundExtended(TempDouble,
                      FieldDescriptor^[aField]^.fdDecPl, FieldDescriptor^[aField]^.fdRound);
                    TempCurrency := TempDouble;
                    move(TempCurrency, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtYear:
                  Begin
                    TempCurrency := OnlyYear(Now);
                    Move(TempCurrency, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempCurrency := OnlyMonth(Now);
                    Move(TempCurrency, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempCurrency := OnlyDay(Now);
                    Move(TempCurrency, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempCurrency := WeekNo(Now);
                    Move(TempCurrency, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Begin
                    Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                      FieldDescriptor^[aField]^.fdLength);
                    TempCurrency := pCurrency(CurrField)^;
                    TempCurrency := RoundExtended(TempCurrency,
                      FieldDescriptor^[aField]^.fdDecPl, FieldDescriptor^[aField]^.fdRound);
                    move(TempCurrency, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
              End;
            End;
          fstArrayUInt8:
            Begin
              GetMem(ByteArrayBuffer, FieldDescriptor^[aField]^.fdLength);
              Try
                If s <> '' Then
                  Begin
                    StringToByteArray(ByteArrayBuffer, FieldDescriptor^[aField]^.fdLength, s, intres);
                    If intres > 0 Then
                      Move(ByteArrayBuffer^, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
              Finally
                FreeMem(ByteArrayBuffer);
              End;
            End;
          fstArrayUInt16:
            Begin
              GetMem(ByteArrayBuffer, FieldDescriptor^[aField]^.fdLength);
              Try
                If s <> '' Then
                  Begin
                    StringToWordArray(ByteArrayBuffer, FieldDescriptor^[aField]^.fdUnits, s, intres);
                    If intres > 0 Then
                      Move(ByteArrayBuffer^, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
              Finally
                FreeMem(ByteArrayBuffer);
              End;
            End;
          fstArrayInt32:
            Begin
              GetMem(ByteArrayBuffer, FieldDescriptor^[aField]^.fdLength);
              Try
                If s <> '' Then
                  Begin
                    StringToIntArray(ByteArrayBuffer, FieldDescriptor^[aField]^.fdUnits, s, intres);
                    If intres > 0 Then
                      Move(ByteArrayBuffer^, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
              Finally
                FreeMem(ByteArrayBuffer);
              End;
            End;
          fstArrayDouble:
            Begin
              GetMem(ByteArrayBuffer, FieldDescriptor^[aField]^.fdLength);
              Try
                If s <> '' Then
                  Begin
                    StringToDoubleArray(ByteArrayBuffer, FieldDescriptor^[aField]^.fdUnits, FieldDescriptor^[aField]^.fdDecPl,
                      FieldDescriptor^[aField]^.fdRound, s, intres);
                    If intres > 0 Then
                      Move(ByteArrayBuffer^, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
              Finally
                FreeMem(ByteArrayBuffer);
              End;
            End;
          fstShortString:
            Begin
              Case dt Of
                dtUser:
                  Begin
                    If FUserName <> '' Then
                      Move(FUserName, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtYear:
                  Begin
                    StS := IntToStr(OnlyYear(Now));
                    Move(StS, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    StS := IntToStr(OnlyMonth(Now));
                    Move(StS, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    StS := IntToStr(OnlyDay(Now));
                    Move(StS, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    StS := IntToStr(WeekNo(Now));
                    Move(StS, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtNow:
                  Begin
                    StS := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
                    Move(StS, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[aField]^.fdLength);
              End;
            End;
          fstWideString, fstVarWideString {, fstUnicode}:
            Begin
              Case dt Of
                dtUser:
                  Begin
                    If FUserName <> '' Then
                      Begin
                        StS := FUserName;
                        StringToWideChar(StS, @v, (Length(StS) * 2));
                        Move(v, CurrField^, FieldDescriptor^[aField]^.fdLength);
                      End;
                  End;
                dtYear:
                  Begin
                    StS := IntToStr(OnlyYear(Now));
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    StS := IntToStr(OnlyMonth(Now));
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    StS := IntToStr(OnlyDay(Now));
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    StS := IntToStr(WeekNo(Now));
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtNow:
                  Begin
                    StS := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Begin
                    Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                      FieldDescriptor^[afield]^.fdLength);
                  End;
              End;
            End;
          fstNullString, fstVarNullString:
            Begin
              Case dt Of
                dtUser:
                  Begin
                    If FUserName <> '' Then
                      Begin
                        MapBDEDataToFF(succ(Length(FUserName)), @FUserName[1], @V);
                        Move(V, CurrField^, FieldDescriptor^[aField]^.fdLength);
                      End;
                  End;
                dtYear:
                  Begin
                    S1 := IntToStr(OnlyYear(Now));
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    S1 := IntToStr(OnlyMonth(Now));
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtDay:
                  Begin
                    S1 := IntToStr(OnlyDay(Now));
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    S1 := IntToStr(WeekNo(Now));
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                dtNow:
                  Begin
                    S1 := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[aField]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[aField]^.fdLength);
              End;
            End;

          Else // jeœli inne typy
            Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
              FieldDescriptor^[aField]^.fdLength);
        End; // dla case
      End
    Else // jeœli brak default
      Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal, CurrField^,
        FieldDescriptor^[aField]^.fdLength);
  End;
Begin
  If (aData = Nil) Then
    Exit;
  BS := PffByteArray(@aData^[LogicalRecordLength]);
  HasDefault := False;
  For i := 0 To Pred(ddDefFldList.Count) Do
    Begin
      HasDefault := (Integer(ddDefFldList[i]) = aField);
      If HasDefault Then
        Begin
          If FieldDescriptor^[aField]^.fdVCheck <> Nil Then
            Begin
              If (FieldDescriptor^[aField]^.fdVCheck^.vdHasDefVal And
                FFIsBitSet(BS, aField)) Or (FieldDescriptor^[aField]^.fdDefaultUpdate = duALWAYS) Then
                Begin
                  CurrField := PffByteArray(@aData^[FieldDescriptor^[aField]^.fdOffset]);
                  CDefault;
                  FFClearBit(BS, aField);
                End;
            End;
          break;
        End; { if }
    End; { for }
  If Not HasDefault Then
    SetRecordFieldNull(aField, aData, True);
End;
{End !!.11}
{--------}

Procedure TFSInfoDict.SetDefaultFieldUpdateValues(aData, oldData: PffByteArray);
Var
  DefFldNo: Integer;
  i: Integer;
  BS: PffByteArray;
  CurrField: PffByteArray;
  S, S1, fldName: AnsiString;
  StS: ShortString;
  D: Double;
  T: Longint;
  V: TffVCheckValue;
  TempDT: TDateTime;
  TempInt16: Smallint;
  TempInt64: Int64; {!!.13}
  TempInt: Longint;
  TempExtend: Extended;
  TempCurrency: Currency;
  TempSingle: Single;
  TempDouble: Double;
  dt: TfsDefaultType;
  intres: Integer;
  ByteArrayBuffer: Pointer;

  Function MapBDEDataToFF(PhySize: Integer; aSource: pointer; aDest: pointer): boolean;
  Begin
    // If Not IGNORE_ANSIOEM Then
      // AnsiToOEM(aSource, aSource);
    StrLCopy(aDest, aSource, pred(PhySize));
  End;

  Procedure CDefault;
  Var
    j: Integer;
    p: pointer;
  Begin
    S := '';
    j := 0;
    FillChar(V, SizeOf(V), 0);
    If FieldDescriptor^[DefFldNo]^.fdType In [fstWideString, fstVarWideString] Then
      Begin
        Try
          i := 0;
          While ((char(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal[i])) +
            (char(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal[succ(i)]))) <> #0#0 Do
            inc(i);
          If i > 0 Then
            Begin
              ffgetmem(p, i);
              Try
                fillchar(p^, i, 0);
                Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, p^, i);
                s := WideCharLenToString(PWideChar(p), (i Div 2) + 1);
                S := Trim(s);
              Finally
                fffreemem(p, i);
              End;
            End;
        Except
          s := '';
        End;
        dt := fsCheckDefaultType(S);
      End
    Else
      Begin
        While FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal[j] <> 0 Do
          Begin
            inc(j);
            If FieldDescriptor^[DefFldNo]^.fdType In [fstNullString, fstVarNullString {, fstWideString , fstUnicode}] Then
              s := s + chr(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal[j - 1])
            Else
              s := s + chr(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal[j]);
          End;
        SetLength(S, j);
        S := Trim(s);
        dt := fsCheckDefaultType(S);
      End;

    If s <> '' Then
      Begin
        Case FieldDescriptor^[DefFldNo]^.fdType Of
          fstTime, fstDate, fstDateTime:
            Begin
              If dt In [dtNow] Then
                Begin
                  Case FieldDescriptor^[DefFldNo]^.fdType Of
                    fstDateTime:
                      Begin
                        TempDT := Now;
                        fsSetMillisecond(TempDT, 0);
                        D := TempDT + 693594;
                        move(D, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                      End;
                    fstDate:
                      Begin
                        T := DateTimeToStDate(Now);
                        move(T, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                      End;
                    fstTime:
                      Begin
                        T := DateTimeToStTime(Now);
                        move(T, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                      End;
                  End;
                End
              Else
                Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                  FieldDescriptor^[DefFldNo]^.fdLength);
            End;

          fstUInt16, fstInt16:
            Begin
              Case dt Of
                dtYear:
                  Begin
                    TempInt := OnlyYear(Now);
                    TempInt16 := TempInt;
                    Move(TempInt16, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempInt16 := OnlyMonth(Now);
                    Move(TempInt16, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempInt16 := OnlyDay(Now);
                    Move(TempInt16, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempInt16 := WeekNo(Now);
                    Move(TempInt16, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[DefFldNo]^.fdLength);
              End;
            End;

          fstInt32, fstUInt32:
            Begin
              Case dt Of
                dtYear:
                  Begin
                    TempInt := OnlyYear(Now);
                    Move(TempInt, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempInt := OnlyMonth(Now);
                    Move(TempInt, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempInt := OnlyDay(Now);
                    Move(TempInt, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempInt := WeekNo(Now);
                    Move(TempInt, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[DefFldNo]^.fdLength);
              End;
            End;
          fstInt64:
            Begin
              Case dt Of
                dtYear:
                  Begin
                    TempInt64 := OnlyYear(Now);
                    Move(TempInt64, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempInt64 := OnlyMonth(Now);
                    Move(TempInt64, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempInt64 := OnlyDay(Now);
                    Move(TempInt64, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempInt64 := WeekNo(Now);
                    Move(TempInt64, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[DefFldNo]^.fdLength);
              End;
            End;
          fstSingle:
            Begin
              Case dt Of
                dtYear:
                  Begin
                    TempSingle := OnlyYear(Now);
                    Move(TempSingle, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempSingle := OnlyMonth(Now);
                    Move(TempSingle, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempSingle := OnlyDay(Now);
                    Move(TempSingle, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempSingle := WeekNo(Now);
                    Move(TempSingle, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Begin
                    Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                      FieldDescriptor^[DefFldNo]^.fdLength);
                    TempSingle := pSingle(CurrField)^;
                    TempSingle := RoundExtended(TempSingle,
                      FieldDescriptor^[DefFldNo]^.fdDecPl, FieldDescriptor^[DefFldNo]^.fdRound);
                    Move(TempSingle, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
              End;
            End;
          fstDouble:
            Begin
              Case dt Of
                dtNow:
                  Begin
                    TempDouble := Now;
                    TempDouble := TempDouble + 693594;
                    TempDouble := RoundExtended(TempDouble,
                      FieldDescriptor^[DefFldNo]^.fdDecPl, FieldDescriptor^[DefFldNo]^.fdRound);
                    move(TempDouble, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtYear:
                  Begin
                    TempDouble := OnlyYear(Now);
                    Move(TempDouble, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempDouble := OnlyMonth(Now);
                    Move(TempDouble, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempDouble := OnlyDay(Now);
                    Move(TempDouble, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempDouble := WeekNo(Now);
                    Move(TempDouble, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Begin
                    Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                      FieldDescriptor^[DefFldNo]^.fdLength);
                    TempDouble := pDouble(CurrField)^;
                    TempDouble := RoundExtended(TempDouble,
                      FieldDescriptor^[DefFldNo]^.fdDecPl, FieldDescriptor^[DefFldNo]^.fdRound);
                    Move(TempDouble, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
              End;
            End;
          fstExtended:
            Begin
              Case dt Of
                dtNow:
                  Begin
                    TempExtend := Now;
                    TempExtend := TempExtend + 693594;
                    TempExtend := RoundExtended(TempExtend,
                      FieldDescriptor^[DefFldNo]^.fdDecPl, FieldDescriptor^[DefFldNo]^.fdRound);
                    move(TempExtend, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtYear:
                  Begin
                    TempExtend := OnlyYear(Now);
                    Move(TempExtend, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempExtend := OnlyMonth(Now);
                    Move(TempExtend, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempExtend := OnlyDay(Now);
                    Move(TempExtend, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempExtend := WeekNo(Now);
                    Move(TempExtend, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Begin
                    Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                      FieldDescriptor^[DefFldNo]^.fdLength);
                    TempExtend := pExtended(CurrField)^;
                    TempExtend := RoundExtended(TempExtend,
                      FieldDescriptor^[DefFldNo]^.fdDecPl, FieldDescriptor^[DefFldNo]^.fdRound);
                    Move(TempExtend, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
              End;
            End;
          fstCurrency:
            Begin
              Case dt Of
                dtNow:
                  Begin
                    TempDouble := Now;
                    TempDouble := TempDouble + 693594;
                    TempDouble := RoundExtended(TempDouble,
                      FieldDescriptor^[DefFldNo]^.fdDecPl, FieldDescriptor^[DefFldNo]^.fdRound);
                    TempCurrency := TempDouble;
                    move(TempCurrency, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtYear:
                  Begin
                    TempCurrency := OnlyYear(Now);
                    Move(TempCurrency, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    TempCurrency := OnlyMonth(Now);
                    Move(TempCurrency, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    TempCurrency := OnlyDay(Now);
                    Move(TempCurrency, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    TempCurrency := WeekNo(Now);
                    Move(TempCurrency, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Begin
                    Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                      FieldDescriptor^[DefFldNo]^.fdLength);
                    TempCurrency := pCurrency(CurrField)^;
                    TempCurrency := RoundExtended(TempCurrency,
                      FieldDescriptor^[DefFldNo]^.fdDecPl, FieldDescriptor^[DefFldNo]^.fdRound);
                    move(TempCurrency, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
              End;
            End;
          fstArrayUInt8:
            Begin
              GetMem(ByteArrayBuffer, FieldDescriptor^[DefFldNo]^.fdLength);
              Try
                If s <> '' Then
                  Begin
                    StringToByteArray(ByteArrayBuffer, FieldDescriptor^[DefFldNo]^.fdLength, s, intres);
                    If intres > 0 Then
                      Move(ByteArrayBuffer^, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
              Finally
                FreeMem(ByteArrayBuffer);
              End;
            End;
          fstArrayUInt16:
            Begin
              ffGetMem(ByteArrayBuffer, FieldDescriptor^[DefFldNo]^.fdLength);
              Try
                If s <> '' Then
                  Begin
                    StringToWordArray(ByteArrayBuffer, FieldDescriptor^[DefFldNo]^.fdUnits, s, intres);
                    If intres > 0 Then
                      Move(ByteArrayBuffer^, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
              Finally
                ffFreeMem(ByteArrayBuffer, FieldDescriptor^[DefFldNo]^.fdLength);
              End;
            End;
          fstArrayInt32:
            Begin
              GetMem(ByteArrayBuffer, FieldDescriptor^[DefFldNo]^.fdLength);
              Try
                If s <> '' Then
                  Begin
                    StringToIntArray(ByteArrayBuffer, FieldDescriptor^[DefFldNo]^.fdUnits, s, intres);
                    If intres > 0 Then
                      Move(ByteArrayBuffer^, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
              Finally
                FreeMem(ByteArrayBuffer);
              End;
            End;
          fstArrayDouble:
            Begin
              GetMem(ByteArrayBuffer, FieldDescriptor^[DefFldNo]^.fdLength);
              Try
                If s <> '' Then
                  Begin
                    StringToDoubleArray(ByteArrayBuffer, FieldDescriptor^[DefFldNo]^.fdUnits, FieldDescriptor^[DefFldNo]^.fdDecPl,
                      FieldDescriptor^[DefFldNo]^.fdRound, s, intres);
                    If intres > 0 Then
                      Move(ByteArrayBuffer^, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
              Finally
                FreeMem(ByteArrayBuffer);
              End;
            End;
          fstShortString:
            Begin
              Case dt Of
                dtUser:
                  Begin
                    If FUserName <> '' Then
                      Move(FUserName, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtYear:
                  Begin
                    StS := IntToStr(OnlyYear(Now));
                    Move(StS, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    StS := IntToStr(OnlyMonth(Now));
                    Move(StS, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    StS := IntToStr(OnlyDay(Now));
                    Move(StS, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    StS := IntToStr(WeekNo(Now));
                    Move(StS, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtNow:
                  Begin
                    StS := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
                    Move(StS, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[DefFldNo]^.fdLength);
              End;
            End;
          fstWideString, fstVarWideString {, fstUnicode}:
            Begin
              Case dt Of
                dtUser:
                  Begin
                    If FUserName <> '' Then
                      Begin
                        Sts := FUserName;
                        StringToWideChar(StS, @v, (Length(StS) * 2));
                        Move(v, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                      End;
                  End;
                dtYear:
                  Begin
                    StS := IntToStr(OnlyYear(Now));
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    StS := IntToStr(OnlyMonth(Now));
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    StS := IntToStr(OnlyDay(Now));
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    StS := IntToStr(WeekNo(Now));
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtNow:
                  Begin
                    StS := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
                    StringToWideChar(StS, @v, (Length(StS) * 2));
                    Move(v, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[DefFldNo]^.fdLength);
              End;
            End;
          fstNullString, fstVarNullString:
            Begin
              Case dt Of
                dtUser:
                  Begin
                    If FUserName <> '' Then
                      Begin
                        MapBDEDataToFF(succ(Length(FUserName)), @FUserName[1], @V);
                        Move(V, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                      End;
                  End;
                dtYear:
                  Begin
                    S1 := IntToStr(OnlyYear(Now));
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtMonth:
                  Begin
                    S1 := IntToStr(OnlyMonth(Now));
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtDay:
                  Begin
                    S1 := IntToStr(OnlyDay(Now));
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtWeekNo:
                  Begin
                    S1 := IntToStr(WeekNo(Now));
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                dtNow:
                  Begin
                    S1 := FormatDateTime('yyyy-mm-dd hh:nn:ss', now);
                    MapBDEDataToFF(succ(Length(S1)), @S1[1], @V);
                    Move(V, CurrField^, FieldDescriptor^[DefFldNo]^.fdLength);
                  End;
                Else
                  Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
                    FieldDescriptor^[DefFldNo]^.fdLength);
              End;
            End;

          Else // jeœli inne typy
            Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
              FieldDescriptor^[DefFldNo]^.fdLength);
        End; // dla case
      End
    Else // jeœli brak default
      Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal, CurrField^,
        FieldDescriptor^[DefFldNo]^.fdLength);
  End;
Begin
  If (aData = Nil) Then
    Exit;
  BS := PffByteArray(@aData^[LogicalRecordLength]); {!!.06}
  For i := 0 To pred(ddDefFldList.Count) Do
    Begin
      {if the field is nil and it has a default value, we're going to
       add the default value for the field}
      DefFldNo := Integer(ddDefFldList[i]);
      If FieldDescriptor^[DefFldNo]^.fdVCheck <> Nil Then
        Begin
          If (FieldDescriptor^[DefFldNo]^.fdVCheck^.vdHasDefVal
            And FFIsBitSet(BS, DefFldNo)) Or (FieldDescriptor^[DefFldNo]^.fdDefaultUpdate = duALWAYS) Then
            Begin
              fldName := AnsiUpperCase(FieldDescriptor^[DefFldNo]^.fdName);
              CurrField := PffByteArray(@aData^[FieldDescriptor^[DefFldNo]^.fdOffset]);
              CDefault;
              FFClearBit(BS, DefFldNo);
            End;
        End;
    End;
End;
{--------}

Procedure TFSInfoDict.SetIsEncrypted(IE: Boolean);
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  FIsEncrypted := IE;
End;

Procedure TFSInfoDict.SetEngineDeleteType(Value: TRecoveryEngine);
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  FEngineDeleteType := Value;
End;

Procedure TFSInfoDict.SetVersionRecord(Value: TVersionRecord);
Begin
  {can't be done in readonly mode}
  If ddReadOnly Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrDictReadOnly, [FBaseName]);
  fVersionRecord := Value;
End;

Procedure TFSInfoDict.SetTableType(TableType: TTableType);
Begin
  ddTableType := TableType;
End;
{--------}

Procedure TFSInfoDict.SetRecordField(aField: Integer;
  aData: PffByteArray;
  aValue: pointer);
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  With FieldDescriptor^[aField]^ Do
    Begin
      If (aValue = Nil) Then
        Begin
          FFSetBit(PffByteArray(@aData^[FLogRecLen]), aField);
          FillChar(aData^[fdOffset], fdLength, 0);
        End
      Else
        Begin
          FFClearBit(PffByteArray(@aData^[FLogRecLen]), aField);
          Move(aValue^, aData^[fdOffset], fdLength);
        End;
    End;
End;
{--------}

Procedure TFSInfoDict.SetRecordFieldNull(aField: Integer;
  aData: PffByteArray;
  aIsNull: boolean);
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  With FieldDescriptor^[aField]^ Do
    Begin
      If aIsNull Then
        FFSetBit(PffByteArray(@aData^[FLogRecLen]), aField)
      Else
        FFClearBit(PffByteArray(@aData^[FLogRecLen]), aField);
      FillChar(aData^[fdOffset], fdLength, 0);
    End;
End;
{--------}

Procedure TFSInfoDict.SetValidityCheck(aField: Integer;
  Var aExists: boolean;
  Const aVCheck: TffVCheckDescriptor);
Begin
  If (aField < 0) Or (aField >= FFldCount) Then
    FSRaiseException(EfsException, fsStrResGeneral, fserrOutOfBounds, [FBaseName, aField]);
  With FieldDescriptor^[aField]^ Do
    Begin
      If aExists Then
        Begin
          If (fdVCheck = Nil) Then
            FFGetZeroMem(fdVCheck, sizeOf(TffVCheckDescriptor));
          If (@aVCheck <> fdVCheck) Then
            Move(aVCheck, fdVCheck^, sizeof(fdVCheck))
        End
      Else {aExists is false}
        Begin
          If (fdVCheck <> Nil) Then
            FFFreeMem(fdVCheck, sizeOf(TffVCheckDescriptor));
        End;
    End;
End;
{--------}

{====================================================================}

{moved from FFTBBASE}
{===Composite Key manipulation routines==============================}

Procedure FSInitKey(aKey: PffByteArray;
  aKeyLen: Integer;
  aKeyFldCount: Integer);
Begin
  If (aKey <> Nil) Then
    Begin
      FillChar(aKey^, aKeyLen, 0);
      If (aKeyFldCount <= 8) Then
        FFSetAllBits(PffByteArray(@aKey^[aKeyLen - 1]), aKeyFldCount)
      Else
        FFSetAllBits(PffByteArray(@aKey^[aKeyLen - 2]), aKeyFldCount);
    End;
End;
{--------}

Function FSIsKeyFieldNull(aKey: PffByteArray;
  aKeyLen: Integer;
  aKeyFldCount: Integer;
  aKeyFld: Integer): boolean;
Begin
  If (aKey = Nil) Then
    Result := True
  Else
    Begin
      If (aKeyFldCount <= 8) Then
        Result := FFIsBitSet(PffByteArray(@aKey^[aKeyLen - 1]), aKeyFld)
      Else
        Result := FFIsBitSet(PffByteArray(@aKey^[aKeyLen - 2]), aKeyFld);
    End;
End;
{--------}

Procedure FSSetKeyFieldNonNull(aKey: PffByteArray;
  aKeyLen: Integer;
  aKeyFldCount: Integer;
  aKeyFld: Integer);
Begin
  If (aKey <> Nil) Then
    Begin
      If (aKeyFldCount <= 8) Then
        FFClearBit(PffByteArray(@aKey^[aKeyLen - 1]), aKeyFld)
      Else
        FFClearBit(PffByteArray(@aKey^[aKeyLen - 2]), aKeyFld);
    End;
End;
{====================================================================}
End.

