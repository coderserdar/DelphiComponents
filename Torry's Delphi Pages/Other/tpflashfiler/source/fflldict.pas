{NOTES:
   1. Have verification as optional--IFDEF'd out}

{*********************************************************}
{* FlashFiler: Table data dictionary                     *}
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

{$I ffdefine.inc}

unit fflldict;

interface

uses
  Windows,
  SysUtils,
  Classes,
  FFConst,
  ffllbase,
  ffsrixhl,
  ffsrmgr,
  ffllexcp;


{---Data dictionary class---}
type

  PffFieldDescriptorArray = ^TffFieldDescriptorArray;
  TffFieldDescriptorArray = array[Word] of PffFieldDescriptor;

  PffIndexDescriptorArray = ^TffIndexDescriptorArray;
  TffIndexDescriptorArray = array[0..Pred(ffcl_MaxIndexes)] of PffIndexDescriptor;

  PffIndexHelperArray = ^TffIndexHelperArray;
  TffIndexHelperArray = array[0..Pred(ffcl_MaxIndexes),
                              0..Pred(ffcl_MaxIndexFlds)] of TffSrIndexHelper;

  TffTriBool = (fftbUnknown, fftbTrue, fftbFalse);                     {!!.03}

  TffDataDictionary = class(TPersistent)
    protected {private}
      FBLOBFileNumber : Integer;     {file number for BLOBs}
      FFieldCapacity  : Longint;     {the number of fields the FieldDescriptor
                                      array has been sized to hold }
      FFldCount       : Integer;     {count of fields--duplicate for speed}
      FHasBLOBs       : TffTriBool;  {True if table contains any BLOB fields} {!!.03}
      FIndexCapacity  : Longint;     {the number of indices the IndexDescriptor
                                      array has been sized to hold }
      FInxCount       : Integer;     {count of indexes--duplicate for speed}
      FFileCount      : Integer;     {count of files--duplicate for speed}
      FBaseName       : TffTableName;{the base name for the table}
      FLogRecLen      : Longint;     {logical rec length--dupe for speed}
      FIsEncrypted    : Boolean;     {true is files are encrypted}

      ddFileList   : TList;   {list of files}
      ddDefFldList : TList;   {list of field numbers that have defaults}

      ddReadOnly   : Boolean; {true if the dictionary cannot be updated}

      procedure AnsiStringWriter(const aString : string;               {!!.05}
                                       aWriter : TWriter);             {!!.05}
      { This method is used to bypass D6's TWriter.WriteString's logic
        for writing strings with extended charcters as UTF8 strings.
        Since D3-D5 and C3-C5 don't recognize the UTF8 string type, it
        causes an error when TReader.ReadString tries to read the
        streams created by D6 using the UTF8 string type.}
      procedure ddExpandFieldArray(const minCapacity : Longint);
      procedure ddExpandIndexArray(const minCapacity : Longint);
      function GetBaseRecordLength : Longint;
      function GetBlockSize : Longint;
      function GetBookmarkSize(aIndexID : Integer) : Integer;
      function GetDefaultFldCount : Integer;
      function GetFieldDecPl(aField : Integer) : Longint;
      function GetFieldDesc(aField : Integer) : TffDictItemDesc;
      function GetFieldLength(aField : Integer) : Longint;
      function GetFieldName(aField : integer) : TffDictItemName;
      function GetFieldOffset(aField : integer) : Longint;
      function GetFieldRequired(aField : integer) : boolean;
      function GetFieldType(aField : integer) : TffFieldType;
      function GetFieldUnits(aField : integer) : Longint;
      function GetFieldVCheck(aField : integer) : PffVCheckDescriptor;
      function GetFileBlockSize(aFile : integer) : Longint;
      function GetFileDesc(aFile : integer) : TffDictItemDesc;
      function GetFileDescriptor(aFile : integer) : PffFileDescriptor;
      function GetFileExt(aFile : integer) : TffExtension;
      function GetFileNameExt(aFile : integer) : TffFileNameExt;
      function GetFileType(aFile : integer) : TffFileType;
      function GetHasBLOBs : Boolean;                                  {!!.03}
      function GetIndexAllowDups(aIndexID : integer) : boolean;
      function GetIndexAscend(aIndexID : integer) : boolean;
      function GetIndexDesc(aIndexID : integer) : TffDictItemDesc;
      function GetIndexFileNumber(aIndexID : integer) : Longint;
      function GetIndexKeyLength(aIndexID : integer) : Longint;
      function GetIndexName(aIndexID : integer) : TffDictItemName;
      function GetIndexNoCase(aIndexID : Integer) : Boolean;
      function GetIndexType(aIndexID : Integer) : TffIndexType;
      function GetRecordLength : Longint;
      procedure CheckForDefault(aVCheckDesc : PffVCheckDescriptor;
                                aFieldDesc  : PffFieldDescriptor);
      procedure SetBlockSize(BS : Longint);
      procedure SetIsEncrypted(IE : Boolean);
    protected
      procedure ClearPrim(InclFileZero : boolean);
      function CreateFieldDesc(const aIdent    : TffDictItemName;
                               const aDesc     : TffDictItemDesc;
                                     aType     : TffFieldType;
                                     aUnits    : Integer;
                                     aDecPl    : Integer;
                                     aReqFld   : Boolean;
                               const aValCheck : PffVCheckDescriptor)
                                               : PffFieldDescriptor;
      function CreateFileDesc(const aDesc      : TffDictItemDesc;
                              const aExtension : TffExtension;
                                    aBlockSize : Longint;
                                    aType      : TffFileType) : PffFileDescriptor;
      function CreateIndexDesc(const aIdent      : TffDictItemName;
                               const aDesc       : TffDictItemDesc;
                                     aFile       : Integer;
                                     aFldCount   : Integer;
                               const aFldList    : TffFieldList;
                               const aFldIHList  : TffFieldIHList;
                                     aAllowDups  : Boolean;
                                     aAscend     : Boolean;
                                     aNoCase     : Boolean) : PffIndexDescriptor;
      function CreateUserIndexDesc(const aIdent     : TffDictItemName;
                                   const aDesc      : TffDictItemDesc;
                                         aFile      : Integer;
                                         aKeyLength : Integer;
                                         aAllowDups : Boolean;
                                         aAscend    : Boolean;
                                         aNoCase    : Boolean) : PffIndexDescriptor;

    public
      FieldDescriptor : PffFieldDescriptorArray;
        { Array of field information for the fields in this dictionary.
          Declared as a public array for speed reasons. }

      IndexDescriptor : PffIndexDescriptorArray;
        { Array of index information for the indexes in this dictionary.
          Declared as a public array for speed reasons. }

      IndexHelpers: PffIndexHelperArray;
        { Index helper objects for composite indices
          declared public (instead of private + public propert)
          for speed reasons}

      class function NewInstance: TObject; override;
      procedure FreeInstance; override;

    public
      constructor Create(aBlockSize : Longint);
        {-Create the instance, aBlockSize is the eventual block size
          of the data file component of the table}
      destructor Destroy; override;
        {-Destroy the instance}

      function AddFile(const aDesc      : TffDictItemDesc;
                       const aExtension : TffExtension;
                             aBlockSize : Longint;
                             aFileType  : TffFileType) : integer;
        {-Add a file to the data dictionary (the actual file name will
          be the base table name plus aExtension); result is the index
          of the newly-added file in the file list}
      procedure AddIndex(const aIdent      : TffDictItemName;
                         const aDesc       : TffDictItemDesc;
                               aFile       : integer;
                               aFldCount   : integer;
                         const aFldList    : TffFieldList;
                         const aFldIHList  : TffFieldIHList;
                               aAllowDups  : boolean;
                               aAscend     : boolean;
                               aCaseInsens : boolean);
        {-Add an extended index to the data dictionary}
      procedure AddUserIndex(const aIdent     : TffDictItemName;
                             const aDesc      : TffDictItemDesc;
                                   aFile      : integer;
                                   aKeyLength : integer;
                                   aAllowDups : boolean;
                                   aAscend    : boolean;
                                   aCaseInsens: boolean);
        {-Add a user defined index to the dictionary}
      procedure AddField(const aIdent    : TffDictItemName;
                         const aDesc     : TffDictItemDesc;
                               aType     : TffFieldType;
                               aUnits    : Integer;
                               aDecPl    : Integer;
                               aReqFld   : Boolean;
                         const aValCheck : PffVCheckDescriptor);
        {-Append a field to the end of the data dictionary's field list}
      procedure Assign(Source: TPersistent); override;
        {-Assign a data dictionary's data}
      procedure BindIndexHelpers;                                     
        {-Binds the TffSrIndexHelper objects to the dictionary}
      procedure CheckValid;
        {-Raise an exception if the dictionary is invalid}
      procedure Clear;
        {-Delete all field/index data from the data dictionary}
      procedure ExtractKey(aIndexID : integer;
                           aData    : PffByteArray;
                           aKey     : PffByteArray);
        {-Given a record buffer and an index number, extract the key
          for that index from the record}
      function GetFieldFromName(const aFieldName : TffDictItemName) : integer;
        {-Return the field number for a given field name, or -1 if not
          found}
      function GetIndexFromName(const aIndexName : TffDictItemName) : integer;
        {-Return the index number for a given index name, or -1 if not
          found}
      function HasAutoIncField(var aField : integer) : boolean;
        {-Return true and the index of the first autoinc field in the
          dictionary}
      procedure InsertField(AtIndex   : Integer;
                      const aIdent    : TffDictItemName;
                      const aDesc     : TffDictItemDesc;
                            aType     : TffFieldType;
                            aUnits    : Integer;
                            aDecPl    : Integer;
                            aReqFld   : Boolean;
                      const aValCheck : PffVCheckDescriptor);
        {-Insert a field into the data dictionary's field list}
      function IsIndexDescValid(const aIndexDesc : TffIndexDescriptor) : boolean;
        {-Return true if the given index descriptor defines a valid index}
      procedure RemoveField(aField : Longint);
        {-Remove a field from the data dictionary's field list}
      procedure RemoveFile(aFile : Longint);
        {-Remove a file from the data dictionary; if index file, the
          relevant indexes are also removed}
      procedure RemoveIndex(aIndex : Longint);
        {-Remove an index from the data dictionary's index list}

      {===Validity check routines===}
      procedure SetValidityCheck(aField  : integer;
                             var aExists : boolean;
                           const aVCheck : TffVCheckDescriptor);
        {-Set a field's validity check record}

      function HasSameFields(aSrcDict : TffDataDictionary;
                         var aBLOBFields : TffPointerList) : boolean;
        {-Use this method to verify a dictionary has the same field types,
          sizes, and ordering as a source dictionary. Returns True if the
          field information matches otherwise returns False. Note that the
          fields may have different names. If the record contains any
          BLOB fields, the number of each BLOB field is stored in output
          parameter aBLOBFields. }

      function HasSameFieldsEx(aSrcDict : TffDataDictionary;
                               aFields : PffLongintArray;
                               aNumFields : integer;
                           var aBLOBFields : TffPointerList) : boolean;
        {-Use this method to verify a dictionary has the same field types,
          sizes, and ordering as the specified fields within a source
          dictionary. Returns True if the field information matches otherwise
          returns False. Note that the fields may have different names. If the
          record contains any BLOB fields, the number of each BLOB field is
          stored in output parameter aBLOBFields. }

      {===record utility routines===}
      function CheckRequiredRecordFields(aData  : PffByteArray) : boolean;
        {-Given a record buffer, checks that all required fields are
          non-null}
      procedure GetRecordField(aField : integer;
                               aData  : PffByteArray;
                           var aIsNull: boolean;
                               aValue : pointer);
        {-Given a record buffer, read the required field; aIsNull is
          set to true if the field is null (no data is written to
          aValue)}
      procedure InitRecord(aData : PffByteArray);
        {-Given a record buffer, initialize it so that all fields are
          null}
      function IsRecordFieldNull(aField : integer;
                                 aData  : PffByteArray) : boolean;
        {-Given a record buffer, return true if the field is null}
      procedure SetRecordField(aField : integer;
                               aData  : PffByteArray;
                               aValue : pointer);
        {-Given a record buffer, write the required field from the
          buffer pointed to by aValue; if aValue is nil, the field is
          set to null}
      procedure SetRecordFieldNull(aField  : integer;
                                   aData   : PffByteArray;
                                   aIsNull : boolean);
        {-Given a record buffer, set the required field to null or
          non-null. Set the field in the record to binary zeros.}

      procedure SetBaseName(const BN : TffTableName);
        {-Set the internal table base name - used for error messages}

{Begin !!.11}
      procedure SetDefaultFieldValue(aData : PffByteArray;
                               const aField : Integer);
        { If the field has a default value, this method sets the field to that
          value. }
{End !!.11}
          
      procedure SetDefaultFieldValues(aData : PffByteArray);
        {-Set any null fields to their default field, if the field
          has a default value}

      property BLOBFileNumber : integer
         read FBLOBFileNumber;
        {-The file number of the file that holds the BLOBs}
      property BlockSize : Longint
         read GetBlockSize write SetBlockSize;
        {-The block size of the table to which this dictionary refers;
          equals FileBlockSize[0] the block size of the base file}
      property BookmarkSize [aIndexID : integer] : integer
         read GetBookmarkSize;
        {-The length of a bookmark for the given index}
      property DefaultFieldCount : Integer
         read GetDefaultFldCount;
        {-Number of fields with default values}
      property IsEncrypted : boolean
         read FIsEncrypted write SetIsEncrypted;
        {-Whether the files comprising the table are encrypted}

      property FieldCount : integer
         read FFldCount;
        {-The number of fields in the data dictionary}
      property FieldDecPl [aField : integer] : Longint
         read GetFieldDecPl;
        {-The decimal places value for a given field in the data dictionary}
      property FieldDesc [aField : integer] : TffDictItemDesc
         read GetFieldDesc;
        {-The description of a given field in the data dictionary}
      property FieldLength [aField : integer] : Longint
         read GetFieldLength;
        {-The length in bytes of a given field in the data dictionary}
      property FieldName [aField : integer] : TffDictItemName
         read GetFieldName;
        {-The name of a given field in the data dictionary}
      property FieldOffset [aField : integer] : Longint
         read GetFieldOffset;
        {-The offset of a given field in the record in the data dictionary}
      property FieldRequired [aField : integer] : boolean
         read GetFieldRequired;
        {-Whether the field is required or not}
      property FieldType [aField : integer] : TffFieldType
         read GetFieldType;
        {-The type of a given field in the data dictionary}
      property FieldUnits [aField : integer] : Longint
         read GetFieldUnits;
        {-The units value for a given field in the data dictionary}
      property FieldVCheck [aField : integer] : PffVCheckDescriptor
         read GetFieldVCheck;
        {-The validity check info for a given field}

      property FileBlockSize [aFile : integer] : Longint
         read GetFileBlockSize;
        {-The block size of a given file in the data dictionary}
      property FileCount : integer
         read FFileCount;
        {-The number of files in the data dictionary}
      property FileDesc [aFile : integer] : TffDictItemDesc
         read GetFileDesc;
        {-The description of a given file in the data dictionary}
      property FileDescriptor [aFile : integer] : PffFileDescriptor
         read GetFileDescriptor;
        {-The descriptor of a given file in the data dictionary}
      property FileExt [aFile : integer] : TffExtension
         read GetFileExt;
        {-The extension of a given file in the data dictionary}
      property DiskFileName [aFile : integer] : TffFileNameExt
         read GetFileNameExt;
        {-The disk name of a given file in the data dictionary}
      property FileType [aFile : integer] : TffFileType
         read GetFileType;
        {-The type of file: data, index or BLOB}
      property HasBLOBFields : Boolean                                 {!!.03}
        read GetHasBLOBs;                                              {!!.03}
        {-Returns True if the table contains any BLOB fields. }        {!!.03}
      property IndexAllowDups [aIndexID : integer] : boolean
         read GetIndexAllowDups;
        {-Whether the given index allows duplicate keys}
      property IndexIsAscending [aIndexID : integer] : boolean
         read GetIndexAscend;
        {-Whether the given index has keys in ascending order}
      property IndexIsCaseInsensitive [aIndexID : integer] : boolean
         read GetIndexNoCase;
        {-Whether the given index has keys in ascending order}
      property IndexCount : integer
         read FInxCount;
        {-The number of indexes in the data dictionary}
      property IndexDesc [aIndexID : integer] : TffDictItemDesc
         read GetIndexDesc;
        {-The description of a given index in the data dictionary}
      property IndexFileNumber [aIndexID : integer] : Longint
         read GetIndexFileNumber;
        {-The descriptor of a given index in the data dictionary}
      property IndexKeyLength [aIndexID : integer] : Longint
         read GetIndexKeyLength;
        {-The key length for the given index}
      property IndexName [aIndexID : integer] : TffDictItemName
         read GetIndexName;
        {-The name of a given field in the data dictionary}
      property IndexType [aIndexID : integer] : TffIndexType
         read GetIndexType;
        {-The type of the given index}

      property RecordLength : Longint
         read GetRecordLength;
        {-The length of the physical record for the data dictionary.  Includes
          trailing byte array to identify null fields. }
      property LogicalRecordLength : Longint
         read GetBaseRecordLength;
        {-The length of the logical record for the data dictionary (ie
          just the total size of the fields. }

      procedure ReadFromStream(S : TStream);
      procedure WriteToStream(S : TStream);

  end;

{===Key manipulation routines===}     {moved here from FFTBBASE}
procedure FFInitKey(aKey         : PffByteArray;
                    aKeyLen      : integer;
                    aKeyFldCount : integer);
function FFIsKeyFieldNull(aKey         : PffByteArray;
                          aKeyLen      : integer;
                          aKeyFldCount : integer;
                          aKeyFld      : integer) : boolean;
procedure FFSetKeyFieldNonNull(aKey         : PffByteArray;
                               aKeyLen      : integer;
                               aKeyFldCount : integer;
                               aKeyFld      : integer);

implementation

const
  ffcl_InitialFieldCapacity = 10;
    { Number of fields dictionary can hold upon creation.  The dictionary
      will expand its capacity as necessary. }
  ffcl_InitialIndexCapacity = 5;
    { Number of indices dictionary can hold upon creation. The dictionary
      will expand its capacity as necessary. }

{===TffDataDictionary================================================}
constructor TffDataDictionary.Create(aBlockSize : Longint);
var
  NewFileDesc   : PffFileDescriptor;
  NewInxDesc    : PffIndexDescriptor;
  SeqAccessName : TffShStr;
begin
  inherited Create;
  FHasBLOBs := fftbUnknown;                                            {!!.03}
  {verify the block size}
  if not FFVerifyBlockSize(aBlockSize) then
    FFRaiseException(EffException, ffStrResGeneral, fferrBadBlockSize,
                     [aBlockSize]);
  {create the file list}
  ddFileList := TList.Create;
  {add the first file name (for the data/data dict file)}
  NewFileDesc := CreateFileDesc(ffStrResGeneral[ffscMainTableFileDesc],
                                ffc_ExtForData, aBlockSize, ftBaseFile);
  try
    NewFileDesc^.fdNumber := 0;
    ddFileList.Add(pointer(NewFileDesc));
    FFileCount := 1;
  except
    FFFreeMem(NewFileDesc,sizeof(TffFileDescriptor));
    raise;
  end;{try..except}

  ddDefFldList := TList.Create;

  {create the field list}
  FFieldCapacity := ffcl_InitialFieldCapacity;
  FFGetMem(FieldDescriptor, SizeOf(PffFieldDescriptor) * FFieldCapacity);
  {create the index list, add index 0: this is the sequential access
   index}

  FIndexCapacity := ffcl_InitialIndexCapacity;
  FFGetMem(IndexDescriptor, SizeOf(PffIndexDescriptor) * FIndexCapacity);
  SeqAccessName := ffStrResGeneral[ffscSeqAccessIndexName];
  NewInxDesc := CreateUserIndexDesc(SeqAccessName, SeqAccessName, 0,
                                    sizeof(TffInt64), false, true, true);
  try
    NewInxDesc^.idNumber := 0;
    IndexDescriptor^[0] := NewInxDesc;
    FInxCount := 1;
  except
    FFFreeMem(NewInxDesc,sizeof(TffIndexDescriptor));
    raise;
  end;{try..except}

  FFGetMem(IndexHelpers,
           SizeOf(TffSrIndexHelper) * ffcl_MaxIndexFlds * FIndexCapacity);
end;
{--------}
destructor TffDataDictionary.Destroy;
var
  index : integer;
  P : pointer;
  Pfd : PffFieldDescriptor absolute P;                                 {!!.01}
begin

  if assigned(IndexHelpers) then
    FFFreeMem(IndexHelpers,
              FIndexCapacity * ffcl_MaxIndexFlds * SizeOf(TffSrIndexHelper));

  ClearPrim(true);

  for Index := pred(FInxCount) downto 0 do begin
    P := IndexDescriptor^[index];
    FFFreeMem(P, sizeof(TffIndexDescriptor));
  end;
  FFFreeMem(IndexDescriptor, SizeOf(PffIndexDescriptor) * FIndexCapacity);

  for Index := pred(FFldCount) downto 0 do begin
    P := FieldDescriptor^[index];
    if Pfd^.fdVCheck <> nil then                                       {!!.01}
      FFFreeMem(Pfd^.fdVCheck, sizeof(TffVCheckDescriptor));           {!!.01}
    FFFreeMem(P, SizeOf(PffFieldDescriptor) * FFieldCapacity);
  end;
  FFFreeMem(FieldDescriptor, SizeOf(PffFieldDescriptor) * FFieldCapacity);

  for index := (ddFileList.count - 1) downto 0 do begin
    P := PffFileDescriptor(ddFileList[index]);
    FFFreeMem(P, sizeOf(TffFileDescriptor));
    ddFileList.delete(index);
  end;

  ddFileList.Free;
  ddDefFldList.Free;
  inherited Destroy;
end;
{--------}
class function TffDataDictionary.NewInstance: TObject;
begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
end;
{--------}
procedure TffDataDictionary.FreeInstance;
var
  Temp : pointer;
begin
  Temp := Self;
  FFFreeMem(Temp, InstanceSize);
end;
{--------}
function TffDataDictionary.AddFile(const aDesc      : TffDictItemDesc;
                                   const aExtension : TffExtension;
                                         aBlockSize : Longint;
                                         aFileType  : TffFileType) : integer;
var
  NewDesc : PffFileDescriptor;
  i       : integer;
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  {verify the extension}
  if not FFVerifyExtension(aExtension) then
    FFRaiseException(EffException, ffStrResGeneral, fferrBadExtension, [FBaseName, aExtension]);
  {verify the block size}
  if not FFVerifyBlockSize(aBlockSize) then
    FFRaiseException(EffException, ffStrResGeneral, fferrBadBlockSize, [aBlockSize]);
  {if a base file type, check to see whether file 0 has been added
   already}
  if (aFileType = ftBaseFile) then
    if (FFileCount > 0) then
      FFRaiseException(EffException, ffStrResGeneral, fferrDataFileDefd, [FBaseName]);
  {check to see whether the extension has been used already}
  for i := 0 to pred(FFileCount) do
    if (PffFileDescriptor(ddFileList[i])^.fdExtension = aExtension) then
      FFRaiseException(EffException, ffStrResGeneral, fferrDupExtension, [FBaseName, aExtension]);
  {if a BLOB file type check to see whether we have one already; we
   can ignore file 0: it's the base file (ie data & dictionary)}
  if (aFileType = ftBLOBFile) then
    if (BLOBFileNumber <> 0) then
      FFRaiseException(EffException, ffStrResGeneral, fferrBLOBFileDefd, [FBaseName]);
  {add a new file descriptor}
  NewDesc := CreateFileDesc(aDesc, aExtension, aBlockSize, aFileType);
  try
    Result := FFileCount;
    NewDesc^.fdNumber := FFileCount;
    if (aFileType = ftBLOBFile) then
      FBLOBFileNumber := FFileCount;
    ddFileList.Add(pointer(NewDesc));
    inc(FFileCount);
  except
    FFFreeMem(NewDesc,sizeof(TffFileDescriptor));
    raise;
  end;{try..except}
end;
{--------}
procedure TffDataDictionary.AddIndex(const aIdent     : TffDictItemName;
                                     const aDesc      : TffDictItemDesc;
                                           aFile      : integer;
                                           aFldCount  : integer;
                                     const aFldList   : TffFieldList;
                                     const aFldIHList : TffFieldIHList;
                                           aAllowDups : boolean;
                                           aAscend    : boolean;
                                           aCaseInsens: boolean);
var
  NewDesc : PffIndexDescriptor;
  i       : integer;
begin
  {check for a duplicate index name}
  if (GetIndexFromName(aIdent) <> -1) then
    FFRaiseException(EffException, ffStrResGeneral, fferrDupIndexName,
      [FBaseName, aIdent]);
  {check the file number}
  if (0 > aFile) or (aFile >= FFileCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrBadFileNumber,
      [FBaseName, aFile]);
  {check all field numbers in field list}
  for i := 0 to pred(aFldCount) do
    if (aFldList[i] < 0) or (aFldList[i] >= FFldCount) then
      FFRaiseException(EffException, ffStrResGeneral, fferrBadFieldRef,
        [FBaseName, aFldList[i]]);
  {create the new index}
  NewDesc := CreateIndexDesc(aIdent, aDesc, aFile, aFldCount, aFldList,
                             aFldIHList, aAllowDups, aAscend, aCaseInsens);
  try
    NewDesc^.idNumber := FInxCount;
    IndexDescriptor^[FInxCount] := NewDesc;
    inc(FInxCount);
    { Have we reached our index capacity? }
    if FInxCount = FIndexCapacity then
      ddExpandIndexArray(0);
  except
    FFFreeMem(NewDesc,sizeof(TffIndexDescriptor));
    raise;
  end;{try..except}
end;
{--------}
procedure TffDataDictionary.AddUserIndex(const aIdent     : TffDictItemName;
                                         const aDesc      : TffDictItemDesc;
                                               aFile      : integer;
                                               aKeyLength : integer;
                                               aAllowDups : boolean;
                                               aAscend    : boolean;
                                               aCaseInsens: boolean);
var
  NewDesc : PffIndexDescriptor;
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  {check the file number}
  if (0 > aFile) or (aFile >= FFileCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrBadFileNumber, [FBaseName, aFile]);
  {check the key length}
  if not FFVerifyKeyLength(aKeyLength) then
    FFRaiseException(EffException, ffStrResGeneral, fferrKeyTooLong, [aKeyLength]);
  {create the new index}
  NewDesc := CreateUserIndexDesc(aIdent, aDesc, aFile, aKeyLength, aAllowDups, aAscend, aCaseInsens);
  try
    NewDesc^.idNumber := FInxCount;
    IndexDescriptor^[FInxCount] := NewDesc;
    inc(FInxCount);
    { Have we reached our index capacity? }
    if FInxCount = FIndexCapacity then
      ddExpandIndexArray(0);
  except
    FFFreeMem(NewDesc,sizeof(TffIndexDescriptor));
    raise;
  end;{try..except}
end;
{--------}
procedure TffDataDictionary.AddField(const aIdent    : TffDictItemName;
                                     const aDesc     : TffDictItemDesc;
                                           aType     : TffFieldType;
                                           aUnits    : Integer;
                                           aDecPl    : Integer;
                                           aReqFld   : Boolean;
                                     const aValCheck : PffVCheckDescriptor);
var
  NewDesc  : PffFieldDescriptor;
  TempDesc : PffFieldDescriptor;
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  {check for a duplicate field name}
  if (GetFieldFromName(aIdent) <> -1) then
    FFRaiseException(EffException, ffStrResGeneral, fferrDupFieldName, [FBaseName, aIdent]);
  {create it}
  NewDesc := CreateFieldDesc(aIdent, aDesc, aType, aUnits, aDecPl, aReqFld, aValCheck);
  try
    NewDesc^.fdNumber := FFldCount;
    if (FFldCount > 0) then begin
      TempDesc := FieldDescriptor^[pred(FFldCount)];
      with TempDesc^ do
        NewDesc^.fdOffset := fdOffset + fdLength;
    end;
    FieldDescriptor^[FFldCount] := NewDesc;
    inc(FFldCount);
    { Have we reached our field capacity? }
    if FFldCount = FFieldCapacity then
      { Yes, expand our field array. }
      ddExpandFieldArray(0);
    with NewDesc^ do
      FLogRecLen := fdOffset + fdLength;
    FHasBLOBs := fftbUnknown;                                          {!!.03}
  except
    FFFreeMem(NewDesc,sizeof(TffFieldDescriptor));
    raise;
  end;{try..except}
end;
{--------}
procedure TffDataDictionary.AnsiStringWriter(const aString : string;   {!!.05 - Added}
                                                   aWriter : TWriter);
var
  TempInt : Integer;
begin
  TempInt := Integer(vaString);
  aWriter.Write(TempInt, SizeOf(vaString));

  TempInt := Length(aString);
  aWriter.Write(TempInt, SizeOf(Byte));

  if (TempInt > 0) then
    aWriter.Write(aString[1], TempInt);
end;
{--------}                                                             {!!.05 - End Added}
procedure TffDataDictionary.Assign(Source: TPersistent);
var
//  CheckVal    : PffVCheckDescriptor;                                 {!!.01}
  item        : integer;
  SelfFldDesc : PffFieldDescriptor;
  SrcDict     : TffDataDictionary absolute Source;
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  {Source must be one of us}
  if not (Source is TffDataDictionary) then
    FFRaiseException(EffException, ffStrResGeneral, fferrNotADict, [FBaseName]);
  {firstly clear our own lists (remove the base file item as well)}
  ClearPrim(true);
  {copy over the encrypted mode}
  Self.FIsEncrypted := TffDataDictionary(Source).IsEncrypted;
  { Now duplicate the items in the Source's lists. }
  try
    { The file list first; do include index 0. }
    for item := 0 to pred(SrcDict.FFileCount) do
      with PffFileDescriptor(SrcDict.ddFileList[item])^ do
        Self.AddFile(fdDesc, fdExtension, fdBlockSize, fdType);

    { The field list next. }
    FHasBLOBs := fftbUnknown;                                          {!!.03}
    for item := 0 to pred(SrcDict.FFldCount) do
      with SrcDict.FieldDescriptor^[Item]^ do begin
        if Assigned(fdVCheck) then
          Self.AddField(fdName, fdDesc, fdType, fdUnits, fdDecPl, fdRequired,
                        fdVCheck)
        else begin
//        FFGetZeroMem(CheckVal, sizeof(TffVCheckDescriptor));       {Deleted !!.01}
          Self.AddField(fdName, fdDesc, fdType, fdUnits, fdDecPl, fdRequired,
                        nil)                                         {!!.01} 
        end;
        if assigned(fdVCheck) then begin
          SelfFldDesc := Self.FieldDescriptor^[item];
          if SelfFldDesc^.fdVCheck = nil then                             {!!.06}
            FFGetMem(SelfFldDesc^.fdVCheck, sizeOf(TffVCheckDescriptor)); {!!.06}
          Move(fdVCheck^, SelfFldDesc^.fdVCheck^, sizeof(fdVCheck^));
        end;
      end;

    { The index list next; skip index 0. }
    for item := 1 to pred(SrcDict.FInxCount) do
      with SrcDict.IndexDescriptor^[item]^ do
        if (idCount <> -1) then
          Self.AddIndex(idName, idDesc, idFile, idCount,
                        idFields, idFieldIHlprs, idDups, idAscend, idNoCase)
        else
          Self.AddUserIndex(idName, idDesc, idFile, idKeyLen, idDups, idAscend, idNoCase)
  except
    ClearPrim(true);
    raise;
  end;{try..except}
end;
{--------}
procedure TffDataDictionary.BindIndexHelpers;
var
  i,j : Integer;
begin
  for i:= 0 to pred(IndexCount) do
    with IndexDescriptor^[i]^do
      if idCount>=0 then begin
        for j:= 0 to Pred(idCount) do
          IndexHelpers[i,j] :=
            TffSrIndexHelper.FindHelper(idFieldIHlprs[j],GetFieldType(idFields[j]));
      end;
end;
{--------}
function TffDataDictionary.CheckRequiredRecordFields(aData  : PffByteArray) : Boolean;
var
  FieldInx : integer;
  BS       : PffByteArray;
begin
  {note: it's probably faster to find all the null fields and then
         check their required status, rather than the other way round
         (getting a field descriptor requires a whole lot more calls
         than checking a bit) but it does depend on a lotta factors.}
  Result := false;
  if (aData = nil) then
    Exit;
  BS := PffByteArray(@aData^[FLogRecLen]);
  for FieldInx := 0 to pred(FFldCount) do begin
    if FFIsBitSet(BS, FieldInx) then
      if FieldDescriptor^[FieldInx]^.fdRequired then
        Exit;
  end;
  Result := true;
end;
{--------}
procedure TffDataDictionary.CheckValid;
var
  item : integer;
  i    : integer;
  Fld  : PffFieldDescriptor;
  Indx : PffIndexDescriptor;
begin
  if (FFldCount <= 0) then
    FFRaiseException(EffException, ffStrResGeneral, fferrNoFields, [FBaseName]);
  if (RecordLength > (BlockSize - ffc_BlockHeaderSizeData - sizeof(Longint))) then
    FFRaiseException(EffException, ffStrResGeneral, fferrRecTooLong, [FBaseName]);
  if (IndexCount > ffcl_MaxIndexes) then
    FFRaiseException(EffException, ffStrResGeneral, fferrMaxIndexes, [FBaseName]);
  {check all field numbers in all indexes, recalc key lengths}
  if (FInxCount > 1) then
    for item := 1 to pred(FInxCount) do
      with IndexDescriptor^[item]^ do
        if (idCount <> -1) then begin
          if (idCount = 0) then
            FFRaiseException(EffException, ffStrResGeneral, fferrNoFieldsInKey, [FBaseName]);
          idKeyLen := 0;
          for i := 0 to pred(idCount) do begin
            if (idFields[i] < 0) or (idFields[i] >= FFldCount) then
              FFRaiseException(EffException, ffStrResGeneral, fferrBadFieldRef, [FBaseName, idFields[i]]);
            inc(idKeyLen, FieldDescriptor^[idFields[i]]^.fdLength);
          end;
          inc(idKeyLen, (idCount + 7) div 8);
        end;
  {field names must be unique}
  for item := 0 to pred(FFldCount) do begin
    Fld := FieldDescriptor^[item];
    if (GetFieldFromName(Fld^.fdName) <> item) then
      FFRaiseException(EffException, ffStrResGeneral, fferrDupFieldName, [FBaseName, Fld^.fdName]);
  end;
  {index names must be unique}
  if (FInxCount > 1) then
    for item := 1 to pred(FInxCount) do begin
      Indx := IndexDescriptor^[item];
      if (GetIndexFromName(Indx^.idName) <> item) then
        FFRaiseException(EffException, ffStrResGeneral, fferrDupIndexName, [FBaseName, Indx^.idName]);
    end;
end;
{--------}
procedure TffDataDictionary.Clear;
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  ClearPrim(false);
end;
{--------}
procedure TffDataDictionary.ClearPrim(InclFileZero : boolean);
var
  item         : integer;
  BaseFileDesc : PffFileDescriptor;
  TmpIndexDesc : PffIndexDescriptor;
  FldDesc      : PffFieldDescriptor;
begin
  {clear the entire file list EXCEPT item zero}
  for item := 1 to pred(FFileCount) do begin
    BaseFileDesc := PffFileDescriptor(ddFileList[item]);
    FFFreeMem(BaseFileDesc, sizeof(TffFileDescriptor));
  end;
  {decide what to do about item zero: save it or dispose of it}
  if InclFileZero and (FFileCount > 0) then begin
    BaseFileDesc := PffFileDescriptor(ddFileList[0]);
    FFFreeMem(BaseFileDesc, sizeof(TffFileDescriptor));
    ddFileList.Clear;
    FFileCount := 0;
  end
  else {don't dispose of file 0} begin
    BaseFileDesc := PffFileDescriptor(ddFileList[0]);
    ddFileList.Clear;
    ddFileList.Add(pointer(BaseFileDesc));
    FFileCount := 1;
  end;
  {clear the entire field list}
  for item := 0 to pred(FFldCount) do begin
    FldDesc := FieldDescriptor^[item];
    if (FldDesc^.fdVCheck <> nil) then
      FFFreeMem(FldDesc^.fdVCheck, sizeOf(TffVCheckDescriptor));
    FFFreeMem(FldDesc, sizeOf(TffFieldDescriptor));
  end;
  FFldCount := 0;
  FLogRecLen := 0;
  {clear the entire index list EXCEPT for the first item}
  for item := 1 to pred(FInxCount) do begin
    TmpIndexDesc := IndexDescriptor^[item];
    FFFreeMem(TmpIndexDesc, sizeOf(TffIndexDescriptor));
    IndexDescriptor^[item] := nil;
  end;
  FInxCount := 1;

  {clear out any old default field values}                             {!!.03}
  ddDefFldList.Clear;                                                  {!!.03}
  FHasBLOBs := fftbUnknown;                                            {!!.03}
end;
{--------}
function TffDataDictionary.CreateFieldDesc(const aIdent    : TffDictItemName;
                                           const aDesc     : TffDictItemDesc;
                                                 aType     : TffFieldType;
                                                 aUnits    : Integer;
                                                 aDecPl    : Integer;
                                                 aReqFld   : Boolean;
                                           const aValCheck : PffVCheckDescriptor)
                                                           : PffFieldDescriptor;
var
  FT : Integer;
begin
  if (aType = fftAutoInc) then
    aReqFld := false;
  FFGetZeroMem(Result, sizeof(TffFieldDescriptor));
  with Result^ do begin
    fdName := aIdent;
    fdDesc := aDesc;
    fdType := aType;
    fdRequired := aReqFld;
    case aType of
      fftBoolean :
        begin
          fdUnits := 0;
          fdDecPl := 0;
          fdLength := sizeof(Boolean);
          CheckForDefault(aValCheck, Result);
        end;
      fftChar :
        begin
          fdUnits := 1;
          fdDecPl := 0;
          fdLength := sizeof(AnsiChar);
          CheckForDefault(aValCheck, Result);
        end;
      fftWideChar :
        begin
          fdUnits := 1;
          fdDecPl := 0;
          fdLength := sizeof(WideChar);
          CheckForDefault(aValCheck, Result);
        end;
      fftByte :
        begin
          if (aUnits < 0) or (aUnits > 3) then
            fdUnits := 3
          else
            fdUnits := aUnits;
          fdDecPl := 0;
          fdLength := sizeof(byte);
          CheckForDefault(aValCheck, Result);
        end;
      fftWord16 :
        begin
          if (aUnits < 0) or (aUnits > 5) then
            fdUnits := 5
          else
            fdUnits := aUnits;
          fdDecPl := 0;
          fdLength := sizeof(TffWord16);
          CheckForDefault(aValCheck, Result);
        end;
      fftWord32 :
        begin
          if (aUnits < 0) or (aUnits > 10) then
            fdUnits := 10
          else
            fdUnits := aUnits;
          fdDecPl := 0;
          fdLength := sizeof(TffWord32);
          CheckForDefault(aValCheck, Result);
        end;
      fftInt8 :
        begin
          if (aUnits < 0) or (aUnits > 3) then
            fdUnits := 3
          else
            fdUnits := aUnits;
          fdDecPl := 0;
          fdLength := sizeof(shortint);
          CheckForDefault(aValCheck, Result);
        end;
      fftInt16 :
        begin
          if (aUnits < 0) or (aUnits > 5) then
            fdUnits := 5
          else
            fdUnits := aUnits;
          fdDecPl := 0;
          fdLength := sizeof(smallint);
          CheckForDefault(aValCheck, Result);
        end;
      fftInt32 :
        begin
          if (aUnits < 0) or (aUnits > 10) then
            fdUnits := 10
          else
            fdUnits := aUnits;
          fdDecPl := 0;
          fdLength := sizeof(Longint);
          CheckForDefault(aValCheck, Result);
        end;
      fftAutoInc :
        begin
          fdUnits := 10;
          fdDecPl := 0;
          fdLength := sizeof(Longint);
        end;
      fftSingle :
        begin
          fdUnits := aUnits;
          fdDecPl := aDecPl;
          fdLength := sizeof(single);
          CheckForDefault(aValCheck, Result);
        end;
      fftDouble :
        begin
          fdUnits := aUnits;
          fdDecPl := aDecPl;
          fdLength := sizeof(double);
          CheckForDefault(aValCheck, Result);
        end;
      fftExtended :
        begin
          fdUnits := aUnits;
          fdDecPl := aDecPl;
          fdLength := sizeof(extended);
          CheckForDefault(aValCheck, Result);
        end;
      fftComp :
        begin
          fdUnits := aUnits;
          fdDecPl := aDecPl;
          fdLength := sizeof(comp);
          CheckForDefault(aValCheck, Result);
        end;
      fftCurrency :
        begin
          fdUnits := aUnits;
          fdDecPl := aDecPl;
          fdLength := sizeof(comp);
          CheckForDefault(aValCheck, Result);
        end;
      fftStDate :
        begin
          fdUnits := 0;
          fdDecPl := 0;
          fdLength := sizeof(Longint);
          CheckForDefault(aValCheck, Result);
        end;
      fftStTime :
        begin
          fdUnits := 0;
          fdDecPl := 0;
          fdLength := sizeof(Longint);
          CheckForDefault(aValCheck, Result);
        end;
      fftDateTime :
        begin
          fdUnits := 0;
          fdDecPl := 0;
          fdLength := sizeof(double);
          CheckForDefault(aValCheck, Result);
        end;
      fftBLOB,
      fftBLOBMemo,
      fftBLOBFmtMemo,
      fftBLOBOLEObj,
      fftBLOBGraphic,
      fftBLOBDBSOLEObj,
      fftBLOBTypedBin,
      fftBLOBFile :
        begin
          fdUnits := 0;
          fdDecPl := 0;
          fdLength := sizeof(TffInt64);
        end;
      fftByteArray :
        begin
          fdUnits := aUnits;
          fdDecPl := 0;
          fdLength := aUnits;
        end;
      fftShortString, fftShortAnsiStr, fftNullString, fftNullAnsiStr  :
        begin
          fdUnits := aUnits;
          fdDecPl := 0;
          fdLength := (aUnits + 1) * sizeof(AnsiChar);
          CheckForDefault(aValCheck, Result);
        end;
      fftWideString :
        begin
          fdUnits := aUnits;
          fdDecPl := 0;
          fdLength := (aUnits + 1) * sizeof(WideChar);
          CheckForDefault(aValCheck, Result);
        end;
    else
      FT := ord(aType);
      FFRaiseException(EffException, ffStrResGeneral, fferrBadFieldType, [FT]);
    end;{case}
  end;
end;
{--------}
function TffDataDictionary.CreateFileDesc(const aDesc      : TffDictItemDesc;
                                          const aExtension : TffExtension;
                                                aBlockSize : Longint;
                                                aType      : TffFileType)
                                                           : PffFileDescriptor;
begin
  FFGetZeroMem(Result, sizeof(TffFileDescriptor));
  with Result^ do
    begin
      fdDesc := aDesc;
      fdExtension := aExtension;
      fdBlockSize := aBlockSize;
      fdType := aType;
    end;
end;
{--------}
function TffDataDictionary.CreateIndexDesc(const aIdent     : TffDictItemName;
                                           const aDesc      : TffDictItemDesc;
                                                 aFile      : integer;
                                                 aFldCount  : integer;
                                           const aFldList   : TffFieldList;
                                           const aFldIHList : TffFieldIHList;
                                                 aAllowDups : boolean;
                                                 aAscend    : boolean;
                                                 aNoCase    : boolean)
                                                            : PffIndexDescriptor;
var
  i : integer;
begin
  FFGetZeroMem(Result, sizeof(TffIndexDescriptor));
  with Result^ do begin
    idName := aIdent;
    idDesc := aDesc;
    idFile := aFile;
    idCount := aFldCount;
    idDups := aAllowDups;
    idKeyLen := 0;
    for i := 0 to pred(aFldCount) do begin
      idFields[i] := aFldList[i];
      inc(idKeyLen, FieldDescriptor^[aFldList[i]]^.fdLength);
    end;
    for i := 0 to pred(aFldCount) do
      idFieldIHlprs[i] := aFldIHList[i];
    inc(idKeyLen,                   {the key length itself}
        (aFldCount + 7) div 8);     {the bit array for nulls}
    idAscend := aAscend;
    idNoCase := aNoCase;
  end;
end;
{--------}
function TffDataDictionary.CreateUserIndexDesc(const aIdent     : TffDictItemName;
                                               const aDesc      : TffDictItemDesc;
                                                     aFile      : integer;
                                                     aKeyLength : integer;
                                                     aAllowDups : boolean;
                                                     aAscend    : boolean;
                                                     aNoCase    : boolean)
                                                                : PffIndexDescriptor;
begin
  FFGetZeroMem(Result, sizeof(TffIndexDescriptor));
  with Result^ do begin
    idName := aIdent;
    idFile := aFile;
    idDups := aAllowDups;
    idCount := -1;
    idKeyLen := aKeyLength;
    idAscend := aAscend;
    idNoCase := aNoCase;
  end;
end;
{--------}
procedure TffDataDictionary.ddExpandFieldArray(const minCapacity : Longint);
var
  OldCapacity : Longint;
begin
  OldCapacity := FFieldCapacity;
{Begin !!.02}
  if minCapacity = 0 then
    inc(FFieldCapacity, ffcl_InitialFieldCapacity * 2)
  else if FFieldCapacity = minCapacity then
    Exit
  else
    FFieldCapacity := minCapacity;
{End !!.02}
  FFReallocMem(FieldDescriptor, SizeOf(PffFieldDescriptor) * OldCapacity,
               SizeOf(PffFieldDescriptor) * FFieldCapacity);
end;
{--------}
procedure TffDataDictionary.ddExpandIndexArray(const minCapacity : Longint);
var
  OldCapacity : Longint;
begin
  OldCapacity := FIndexCapacity;
{Begin !!.02}
  if minCapacity = 0 then
    inc(FIndexCapacity, ffcl_InitialIndexCapacity * 2)
  else if FIndexCapacity = minCapacity then
    Exit
  else
    FIndexCapacity := minCapacity;
{End !!.02}
  FFReallocMem(IndexDescriptor, SizeOf(PffIndexDescriptor) * OldCapacity,
               SizeOf(PffIndexDescriptor) * FIndexCapacity);
  FFReallocMem(IndexHelpers,
               SizeOf(TffSrIndexHelper) * ffcl_MaxIndexFlds * OldCapacity,
               SizeOf(TffSrIndexHelper) * ffcl_MaxIndexFlds * FIndexCapacity);
end;
{--------}
procedure TffDataDictionary.ExtractKey(aIndexID : integer;
                                       aData    : PffByteArray;
                                       aKey     : PffByteArray);
var
  KeyOffset   : integer;
  FieldNumber : integer;
begin
  KeyOffset := 0;
  with IndexDescriptor^[aIndexID]^ do begin
    {clear the entire key - sets all fields to null as well}
    FFInitKey(aKey, idKeyLen, idCount);
    {now build it}
    for FieldNumber := 0 to pred(idCount) do begin
      with FieldDescriptor^[idFields[FieldNumber]]^ do begin
        if not IsRecordFieldNull(idFields[FieldNumber], aData) then begin
          Move(aData^[fdOffset], aKey^[KeyOffset], fdLength);
          FFSetKeyFieldNonNull(aKey, idKeyLen, idCount, FieldNumber);
        end;
        inc(KeyOffset, fdLength);
      end;
    end;
  end;
end;
{--------}
function TffDataDictionary.GetBaseRecordLength : Longint;
begin
  { A record must be at last ffcl_MinRecordLength bytes in length.  This
    is because we need that many bytes in order to store the next deleted
    record when the record becomes part of the deleted record chain. }
  Result := FFMaxL(FLogRecLen, ffcl_MinRecordLength);
end;
{--------}
function TffDataDictionary.GetBlockSize : Longint;
begin
  if (FFileCount > 0) then
    Result := PffFileDescriptor(ddFileList.Items[0])^.fdBlockSize
  else
    Result := 4096;
end;
{--------}
function TffDataDictionary.GetBookmarkSize(aIndexID : integer) : integer;
begin
  if not ((0 <= aIndexID) and (aIndexID < FInxCount)) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aIndexID]);
  Result := ffcl_FixedBookmarkSize + IndexDescriptor^[aIndexID]^.idKeyLen;
end;
{--------}
function TffDataDictionary.GetFieldDecPl(aField : integer) : Longint;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdDecPl;
end;
{--------}
function TffDataDictionary.GetFieldDesc(aField : integer) : TffDictItemDesc;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdDesc;
end;
{--------}
function TffDataDictionary.GetFieldFromName(const aFieldName : TffDictItemName) : integer;
begin
  for Result := 0 to pred(FFldCount) do
    if (FFCmpShStrUC(aFieldName,
                     FieldDescriptor^[Result]^.fdName,
                     255) = 0) then
      Exit;
  Result := -1;
end;
{--------}
function TffDataDictionary.GetFieldLength(aField : integer) : Longint;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdLength;
end;
{--------}
function TffDataDictionary.GetFieldName(aField : integer) : TffDictItemName;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdName;
end;
{--------}
function TffDataDictionary.GetFieldOffset(aField : integer) : Longint;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdOffset;
end;
{--------}
function TffDataDictionary.GetFieldRequired(aField : integer) : boolean;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdRequired;
end;
{--------}
function TffDataDictionary.GetFieldType(aField : integer) : TffFieldType;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdType;
end;
{--------}
function TffDataDictionary.GetFieldUnits(aField : integer) : Longint;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdUnits;
end;
{--------}
function TffDataDictionary.GetFieldVCheck(aField : integer) : PffVCheckDescriptor;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  Result := FieldDescriptor^[aField]^.fdVCheck;
end;
{--------}
function TffDataDictionary.GetFileBlockSize(aFile : integer) : Longint;
begin
  if (aFile < 0) or (aFile >= FFileCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile])^.fdBlockSize;
end;
{--------}
function TffDataDictionary.GetFileDesc(aFile : integer) : TffDictItemDesc;
begin
  if (aFile < 0) or (aFile >= FFileCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile])^.fdDesc;
end;
{--------}
function TffDataDictionary.GetFileDescriptor(aFile : integer) : PffFileDescriptor;
begin
  if (aFile < 0) or (aFile >= FFileCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile]);
end;
{--------}
function TffDataDictionary.GetFileExt(aFile : integer) : TffExtension;
begin
  if (aFile < 0) or (aFile >= FFileCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile])^.fdExtension;
end;
{--------}
function TffDataDictionary.GetFileNameExt(aFile : integer) : TffFileNameExt;
var
  Temp : PffFileDescriptor;
begin
  if (aFile < 0) or (aFile >= FFileCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aFile]);
  Temp := PffFileDescriptor(ddFileList.Items[aFile]);
  Result := FFMakeFileNameExt(FBaseName, Temp^.fdExtension);
end;
{--------}
function TffDataDictionary.GetFileType(aFile : integer) : TffFileType;
begin
  if (aFile < 0) or (aFile >= FFileCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aFile]);
  Result := PffFileDescriptor(ddFileList.Items[aFile])^.fdType;
end;
{Begin !!.03}
{--------}
function TffDataDictionary.GetHasBLOBs : Boolean;
var
  Index : Integer;
  P : PffFieldDescriptor;
begin
  if FHasBLOBs = fftbUnknown then begin
    FHasBLOBs := fftbFalse;
    for Index := 0 to Pred(FFldCount) do begin
      P := FieldDescriptor^[index];
      if P^.fdType in [fftBLOB..fftBLOBFile] then begin
        FHasBLOBs := fftbTrue;
        Break;
      end;  { if }
    end;  { for }
  end;  { if }
  Result := (FHasBLOBs = fftbTrue);
end;
{End !!.03}
{--------}
function TffDataDictionary.GetIndexAllowDups(aIndexID : integer) : boolean;
begin
  if not ((0 <= aIndexID) and (aIndexID < FInxCount)) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idDups;
end;
{--------}
function TffDataDictionary.GetIndexAscend(aIndexID : integer) : boolean;
begin
  if not ((0 <= aIndexID) and (aIndexID < FInxCount)) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idAscend;
end;
{--------}
function TffDataDictionary.GetIndexDesc(aIndexID : integer) : TffDictItemDesc;
begin
  if not ((0 <= aIndexID) and (aIndexID < FInxCount)) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idDesc;
end;
{--------}
function TffDataDictionary.GetIndexFileNumber(aIndexID : integer) : Longint;
begin
  if not ((0 <= aIndexID) and (aIndexID < FInxCount)) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexId]^.idFile;
end;
{--------}
function TffDataDictionary.GetIndexFromName(const aIndexName : TffDictItemName) : integer;
begin
  for Result := 0 to pred(FInxCount) do
    if (FFCmpShStrUC(aIndexName,
                     indexDescriptor^[Result]^.idName,
                     255) = 0) then
      Exit;
  Result := -1;
end;
{--------}
function TffDataDictionary.GetIndexKeyLength(aIndexID : integer) : Longint;
begin
  if not ((0 <= aIndexID) and (aIndexID < FInxCount)) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idKeyLen;
end;
{--------}
function TffDataDictionary.GetIndexName(aIndexID : integer) : TffDictItemName;
begin
  if not ((0 <= aIndexID) and (aIndexID < FInxCount)) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idName;
end;
{--------}
function TffDataDictionary.GetIndexNoCase(aIndexID : integer) : boolean;
begin
  if not ((0 <= aIndexID) and (aIndexID < FInxCount)) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aIndexID]);
  Result := IndexDescriptor^[aIndexID]^.idNoCase;
end;
{--------}
function TffDataDictionary.GetIndexType(aIndexID : integer) : TffIndexType;
begin
  if not ((0 <= aIndexID) and (aIndexID < FInxCount)) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aIndexID]);
  Result := TffIndexType(IndexDescriptor^[aIndexID]^.idCount = -1);
end;
{--------}
procedure TffDataDictionary.GetRecordField(aField : integer;
                                           aData  : PffByteArray;
                                       var aIsNull: boolean;
                                           aValue : pointer);
begin
  aIsNull := IsRecordFieldNull(aField, aData);
  if (not aIsNull) and (aValue <> nil) then
    with FieldDescriptor^[aField]^ do
      Move(aData^[fdOffset], aValue^, fdLength);
end;
{--------}
function TffDataDictionary.GetRecordLength : Longint;
begin
  Result := GetBaseRecordLength +        {the fields themselves}
            ((FFldCount + 7) div 8);     {the bit array for nulls}
end;
{--------}
function TffDataDictionary.HasAutoIncField(var aField : integer) : boolean;
begin
  Result := true;
  aField := 0;
  while (aField < FFldCount) do begin
    if FieldDescriptor^[aField]^.fdType = fftAutoInc then
      Exit;
    inc(aField);
  end;
  Result := false;
end;
{--------}
function TffDataDictionary.HasSameFields(aSrcDict : TffDataDictionary;
                                     var aBLOBFields : TffPointerList) : boolean;
var
 anIndex : integer;
begin
  Result := False;
  if FieldCount <> aSrcDict.FieldCount then
    Exit;
  aBLOBFields.Empty;

  for anIndex := 0 to pred(FieldCount) do begin
    { Must have same field type, length, decimal places, & units. }
    Result := (FieldLength[anIndex] = aSrcDict.FieldLength[anIndex]) and
              (FieldType[anIndex] = aSrcDict.FieldType[anIndex]) and
              (FieldDecPl[anIndex] = aSrcDict.FieldDecPl[anIndex]) and
              (FieldUnits[anIndex] = aSrcDict.FieldUnits[anIndex]);
    if (not Result) then
      Exit;
    if FieldType[anIndex] in [fftBLOB..fftBLOBFile] then
      aBLOBFields.Append(Pointer(anIndex));
  end;
end;
{--------}
function TffDataDictionary.HasSameFieldsEx(aSrcDict : TffDataDictionary;
                                           aFields : PffLongintArray;
                                           aNumFields : integer;
                                       var aBLOBFields : TffPointerList) : boolean;
var
 anIndex, aSrcIndex : integer;
begin
  Result := False;
  if FieldCount <> aNumFields then
    Exit;
  aBLOBFields.Empty;

  for anIndex := 0 to pred(aNumFields) do begin
    aSrcIndex := aFields^[anIndex];
    { Must have same field type, length, decimal places, & units. }
    Result := (FieldLength[anIndex] = aSrcDict.FieldLength[aSrcIndex]) and
              (FieldType[anIndex] = aSrcDict.FieldType[aSrcIndex]) and
              (FieldDecPl[anIndex] = aSrcDict.FieldDecPl[aSrcIndex]) and
              (FieldUnits[anIndex] = aSrcDict.FieldUnits[aSrcIndex]);
    if (not Result) then
      Exit;
    if FieldType[anIndex] in [fftBLOB..fftBLOBFile] then
      aBLOBFields.Append(Pointer(anIndex));
  end;
end;
{--------}
procedure TffDataDictionary.CheckForDefault(aVCheckDesc : PffVCheckDescriptor;
                                            aFieldDesc  : PffFieldDescriptor);
var
  CheckVal : PffVCheckDescriptor;
begin
  if Assigned(aVCheckDesc) and aVCheckDesc^.vdHasDefVal then begin
    if (not Assigned(aFieldDesc^.fdVCheck)) then begin
      FFGetZeroMem(CheckVal, sizeof(TffVCheckDescriptor));
      aFieldDesc^.fdVCheck := CheckVal;
    end;
    aFieldDesc^.fdVCheck^.vdHasDefVal := True;
    aFieldDesc^.fdVCheck^.vdDefVal := aVCheckDesc.vdDefVal;
  end;
end;
{--------}
function TffDataDictionary.GetDefaultFldCount: Integer;
begin
  ddDefFldList.Pack;
  Result := ddDefFldList.Count;
end;
{--------}
procedure TffDataDictionary.InitRecord(aData : PffByteArray);
begin
  if (aData <> nil) and (FFldCount > 0) then begin
    FillChar(aData^, FLogRecLen + ((FFldCount + 7) div 8), 0);
    FFSetAllBits(PffByteArray(@aData^[LogicalRecordLength]), FFldCount); {!!.02}
  end;
end;
{--------}
procedure TffDataDictionary.InsertField(AtIndex   : Integer;
                                  const aIdent    : TffDictItemName;
                                  const aDesc     : TffDictItemDesc;
                                        aType     : TffFieldType;
                                        aUnits    : Integer;
                                        aDecPl    : Integer;
                                        aReqFld   : Boolean;
                                  const aValCheck : PffVCheckDescriptor);
var
  NewDesc  : PffFieldDescriptor;
  TempDesc : PffFieldDescriptor;
  NewOffset: integer;
  Inx      : integer;
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  {check for a duplicate field name}
  if (GetFieldFromName(aIdent) <> -1) then
    FFRaiseException(EffException, ffStrResGeneral, fferrDupFieldName, [FBaseName, aIdent]);
  {create it}
  if (0 <= AtIndex) and (AtIndex < FFldCount) then begin
    FHasBLOBs := fftbUnknown;                                          {!!03}
    NewDesc := CreateFieldDesc(aIdent, aDesc, aType, aUnits, aDecPl, aReqFld, aValCheck);
    try
      NewDesc^.fdNumber := AtIndex;
      if (AtIndex > 0) then begin
        TempDesc := FieldDescriptor^[pred(AtIndex)];
        with TempDesc^ do
          NewDesc^.fdOffset := fdOffset + fdLength;
      end;
      { Shift existing fields up. }
      for Inx := pred(FFldCount) downto AtIndex do
        FieldDescriptor^[succ(Inx)] := FieldDescriptor^[Inx];
      FieldDescriptor^[AtIndex] := NewDesc;
      inc(FFldCount);
      { Have we reached our field capacity? }
      if FFldCount = FFieldCapacity then
        { Yes, expand our field array. }
        ddExpandFieldArray(0);
      {patch up all successive descriptors}
      with NewDesc^ do
        NewOffset := fdOffset + fdLength;
      for Inx := succ(AtIndex) to pred(FFldCount) do begin
        TempDesc := FieldDescriptor^[Inx];
        with TempDesc^ do
          begin
            fdNumber := Inx;
            fdOffset := NewOffset;
            inc(NewOffset, fdLength);
          end;
      end;
      FLogRecLen := NewOffset;
    except
      FFFreeMem(NewDesc,sizeof(TffFieldDescriptor));
      raise;
    end;{try..except}
  end;
end;
{--------}
function TffDataDictionary.IsIndexDescValid(const aIndexDesc : TffIndexDescriptor) : boolean;
var
  i      : integer;
  KeyLen : integer;
begin
  Result := false;
  with aIndexDesc do begin
    if (idName = '') then
      Exit;
    if (0 > idFile) or (idFile >= FFileCount) then
      Exit;
    if (idCount = -1) then begin {user-defined index}
      if (idKeyLen <= 0) then
        Exit;
    end
    else begin {composite index}
      if (idCount = 0) then
        Exit;
      KeyLen := 0;
      for i := 0 to pred(idCount) do begin
        if (idFields[i] < 0) or (idFields[i] >= FFldCount) then
          Exit;
        inc(KeyLen, FieldDescriptor^[idfields[i]]^.fdLength);
      end;
      inc(KeyLen, (idCount + 7) div 8);
      if (KeyLen > ffcl_MaxKeyLength) then
        Exit;
    end;
  end;
  Result := true;
end;
{--------}
function TffDataDictionary.IsRecordFieldNull(aField : integer;
                                             aData  : PffByteArray) : boolean;
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds,
                     [FBaseName, aField]);
  Result := (aData = nil) or
            FFIsBitSet(PffByteArray(@aData^[FLogRecLen]), aField);
end;
{--------}
procedure TffDataDictionary.ReadFromStream(S : TStream);
var
  Reader    : TReader;
  i, j      : Integer;
  FileDesc  : PffFileDescriptor;
  FldDesc   : PffFieldDescriptor;
  InxDesc   : PffIndexDescriptor;
  HasVCheck : Boolean;
begin
  ClearPrim(true);
  Reader := TReader.Create(S, 4096);
  try
    with Reader do begin
      FBLOBFileNumber := 0;
      FIsEncrypted := ReadBoolean;
      FFileCount := ReadInteger;
      try
        for i := 0 to pred(FFileCount) do begin
          FFGetZeroMem(FileDesc, sizeof(TffFileDescriptor));
          with FileDesc^ do begin
            fdNumber := i;
            fdDesc := ReadString;
            fdExtension := ReadString;
            fdBlockSize := ReadInteger;
            fdType := TffFileType(ReadInteger);
            if (fdType = ftBLOBFile) then
              FBLOBFileNumber := i;
          end;
          ddFileList.Add(pointer(FileDesc));
          FileDesc := nil;
        end;
      except
        if Assigned(FileDesc) then
          FFFreeMem(FileDesc, sizeOf(TffFileDescriptor));
        raise;
      end;{try..except}
      FFldCount := ReadInteger;
      ddExpandFieldArray(FFldCount + 1);
      try
        for i := 0 to pred(FFldCount) do begin
          FFGetZeroMem(FldDesc, sizeof(TffFieldDescriptor));
          with FldDesc^ do begin
            fdNumber := i;
            fdName := ReadString;
            fdDesc := ReadString;
            fdUnits := ReadInteger;
            fdDecPl := ReadInteger;
            fdOffset := ReadInteger;
            fdLength := ReadInteger;
            fdType := TffFieldType(ReadInteger);
            fdRequired := ReadBoolean;
            HasVCheck := ReadBoolean;
            if HasVCheck then begin
              FFGetZeroMem(fdVCheck, sizeof(TffVCheckDescriptor));
              with fdVCheck^ do begin
                vdPicture := ReadString;
                vdHasMinVal := ReadBoolean;
                vdHasMaxVal := ReadBoolean;
                vdHasDefVal := ReadBoolean;
                {if the field has a default value, we add the field
                 number to ddDefFldList}
                if vdHasDefVal then begin
                  ddDefFldList.Add(Pointer(i));
                end;
                if vdHasMinVal then
                  Read(vdMinVal, fdLength);
                if vdHasMaxVal then
                  Read(vdMaxVal, fdLength);
                if vdHasDefVal then
                  Read(vdDefVal, fdLength);
              end;
            end;
          end;
          FieldDescriptor^[i] := FldDesc;
          FldDesc := nil;
        end;
      except
        if Assigned(FldDesc) then
          FFFreeMem(FldDesc, sizeOf(TffFieldDescriptor));
        raise;
      end;{try..except}
      FLogRecLen := ReadInteger;
      FInxCount := ReadInteger;
      ddExpandIndexArray(FInxCount + 1);
      try
        {note that index 0 is never stored on a stream}
        for i := 1 to pred(FInxCount) do begin
          FFGetZeroMem(InxDesc, sizeof(TffIndexDescriptor));
          with InxDesc^ do begin
            idNumber := i;
            idName := ReadString;
            idDesc := ReadString;
            idFile := ReadInteger;
            idKeyLen := ReadInteger;
            idCount := ReadInteger;
            if (idCount <> -1) then
              for j := 0 to pred(idCount) do begin
                idFields[j] := ReadInteger;
                if NextValue=vaString then
                  idFieldIHlprs[j] := ReadString
                else
                  idFieldIHlprs[j] := '';
              end;
            idDups := ReadBoolean;
            idAscend := ReadBoolean;
            idNoCase := ReadBoolean;
          end;
          IndexDescriptor^[i] := InxDesc;
          InxDesc := nil;
        end;
      except
        if Assigned(InxDesc) then
          FFFreeMem(InxDesc, sizeOf(TffIndexDescriptor));
        raise;
      end;{try..except}
    end;
  finally
    Reader.Free;
  end;{try..finally}
end;
{--------}
procedure TffDataDictionary.RemoveField(aField : Longint);
var
  TempDesc   : PffFieldDescriptor;
  NewOffset  : Integer;
  Inx,                                                                 {!!.13}
  FldInx     : Integer;                                                {!!.13}
  InxDesc    : PffIndexDescriptor;                                     {!!.13}
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  if (0 <= aField) and (aField < FFldCount) then begin
{Begin !!.13}
    { Verify the field is not being used by an index. }
    for Inx := Pred(IndexCount) downto 0 do begin
      InxDesc := IndexDescriptor[Inx];
      for FldInx := 0 to Pred(InxDesc^.idCount) do
        if InxDesc^.idFields[FldInx] = aField then
          FFRaiseException(EffException, ffStrResGeneral, fferrFileInUse,
                           [aField]);
    end;
{End !!.13}
    FHasBLOBs := fftbUnknown;                                          {!!.03}
    TempDesc := FieldDescriptor^[aField];
    NewOffset := TempDesc^.fdOffset;
    FFFreeMem(TempDesc, sizeOf(TffFieldDescriptor));
    { Shift fields down to cover the empty space. }
    for Inx := aField to (FFldCount - 2) do
      FieldDescriptor^[Inx] := FieldDescriptor^[succ(Inx)];            {!!.01}
    dec(FFldCount);
    {patch up all successive descriptors}
    for Inx := aField to pred(FFldCount) do begin
      TempDesc := FieldDescriptor^[Inx];
      with TempDesc^ do begin
        fdNumber := Inx;
        fdOffset := NewOffset;
        inc(NewOffset, fdLength);
      end;
    end;
    FLogRecLen := NewOffset;
  end;
end;
{--------}
procedure TffDataDictionary.RemoveFile(aFile : Longint);
var
  TempDesc   : PffFileDescriptor;
  Inx        : integer;
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  {can't remove entry 0: it's the base file}
  if (aFile = 0) then
    FFRaiseException(EffException, ffStrResGeneral, fferrBaseFile, [FBaseName]);
  {remove the entry}
  if (0 < aFile) and (aFile < FFileCount) then begin
    TempDesc := PffFileDescriptor(ddFileList.Items[aFile]);
    {if the BLOB file is being removed from the dictionary then reset
     the BLOB file number field}
    if (TempDesc^.fdType = ftBLOBFile) then
      FBLOBFileNumber := 0;
{Begin!!.13}
    { If an index file is being removed from the dictionary then make sure
      it is not referenced by an index. }
    if (TempDesc^.fdType = ftIndexFile) then begin
      for Inx := pred(FInxCount) downto 0 do
        if (IndexDescriptor^[Inx]^.idFile = aFile) then
          FFRaiseException(EffException, ffStrResGeneral, fferrFileInUse,
                           [aFile]);
      { Fixup index descriptors referencing files with higher file numbers. }
      for Inx := Pred(IndexCount) downto 0 do
        if (IndexDescriptor^[Inx]^.idFile > aFile) then
          Dec(IndexDescriptor^[Inx]^.idFile);
    end;  { if }
{End !!.13}

    FFFreeMem(TempDesc, sizeOf(TffFileDescriptor));
    ddFileList.Delete(aFile);
    dec(FFileCount);
    {patch up all successive descriptors}
    for Inx := aFile to pred(FFileCount) do begin
      TempDesc := PffFileDescriptor(ddFileList[Inx]);
      TempDesc^.fdNumber := Inx;
    end;
  end;
end;
{--------}
procedure TffDataDictionary.RemoveIndex(aIndex : Longint);
var
  TempDesc   : PffIndexDescriptor;
  Inx        : integer;
begin
  (*
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  *)
  {remove the entry}
  if (0 <= aIndex) and (aIndex < FInxCount) then begin
    TempDesc := IndexDescriptor^[aIndex];
    FFFreeMem(TempDesc, sizeOf(TffIndexDescriptor));
{Begin !!.02}
    { Shift the descriptors above the deleted index down to fill in
      the gap. }
    for Inx := aIndex to (FInxCount - 2) do begin
      IndexDescriptor^[Inx] := IndexDescriptor^[succ(Inx)];
      IndexDescriptor^[Inx]^.idNumber := Inx;
    end;
    dec(FInxCount);
  end;
{End !!.02}
end;
{--------}
procedure TffDataDictionary.SetBaseName(const BN : TffTableName);
begin
  FBaseName := BN;
end;
{--------}
procedure TffDataDictionary.SetBlockSize(BS : Longint);
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  if (BS <> BlockSize) and FFVerifyBlockSize(BS) then
    if (BS > BlockSize) or
       (RecordLength <= (BS - ffc_BlockHeaderSizeData - sizeof(Longint))) then begin
      if (FFileCount > 0) then
        PffFileDescriptor(ddFileList.Items[0])^.fdBlockSize := BS;
    end;
end;
{Begin !!.11}
{--------}
procedure TffDataDictionary.SetDefaultFieldValue(aData : PffByteArray;
                                           const aField : Integer);
var
  i         : Integer;
  BS        : PffByteArray;
  CurrField : PffByteArray;
  HasDefault : Boolean;
begin
  if (aData = nil) then
    Exit;
  BS := PffByteArray(@aData^[LogicalRecordLength]);
  HasDefault := False;
  for i := 0 to Pred(ddDefFldList.Count) do begin
    HasDefault := (Integer(ddDefFldList[i]) = aField);
    if HasDefault then begin
      { If the field is nil and it has a default value, we're going to
        add the default value for the field. }
      if FieldDescriptor^[aField]^.fdVCheck <> nil then
        if FFIsBitSet(BS, aField) and
           FieldDescriptor^[aField]^.fdVCheck^.vdHasDefVal then begin
          CurrField := PffByteArray(@aData^[FieldDescriptor^[aField]^.fdOffset]);
          Move(FieldDescriptor^[aField]^.fdVCheck^.vdDefVal,
               CurrField^,
               FieldDescriptor^[afield]^.fdLength);
          FFClearBit(BS, aField);
        end;  { if }
      break;
    end;  { if }
  end;  { for }
  if not HasDefault then
    SetRecordFieldNull(aField, aData, True);
end;
{End !!.11}
{--------}
procedure TffDataDictionary.SetDefaultFieldValues(aData : PffByteArray);
var
  DefFldNo  : Integer;
  i         : Integer;
  BS        : PffByteArray;
  CurrField : PffByteArray;
begin
  if (aData = nil) then
    Exit;
  BS := PffByteArray(@aData^[LogicalRecordLength]);                    {!!.06}
  for i := 0 to pred(ddDefFldList.Count) do begin
    {if the field is nil and it has a default value, we're going to
     add the default value for the field}
    DefFldNo := Integer(ddDefFldList[i]);
    if FieldDescriptor^[DefFldNo]^.fdVCheck <> nil then
      if FFIsBitSet(BS, DefFldNo) and
         FieldDescriptor^[DefFldNo]^.fdVCheck^.vdHasDefVal then begin
        CurrField := PffByteArray(@aData^[FieldDescriptor^[DefFldNo]^.fdOffset]);
        Move(FieldDescriptor^[DefFldNo]^.fdVCheck^.vdDefVal,
             CurrField^,
             FieldDescriptor^[DefFldNo]^.fdLength);
        FFClearBit(BS, DefFldNo);
      end;
  end;
end;
{--------}
procedure TffDataDictionary.SetIsEncrypted(IE : Boolean);
begin
  {can't be done in readonly mode}
  if ddReadOnly then
    FFRaiseException(EffException, ffStrResGeneral, fferrDictReadOnly, [FBaseName]);
  FIsEncrypted := IE;
end;
{--------}
procedure TffDataDictionary.SetRecordField(aField : integer;
                                           aData  : PffByteArray;
                                           aValue : pointer);
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  with FieldDescriptor^[aField]^ do begin
    if (aValue = nil) then begin
      FFSetBit(PffByteArray(@aData^[FLogRecLen]), aField);
      FillChar(aData^[fdOffset], fdLength, 0);
    end
    else begin
      FFClearBit(PffByteArray(@aData^[FLogRecLen]), aField);
      Move(aValue^, aData^[fdOffset], fdLength);
    end;
  end;
end;
{--------}
procedure TffDataDictionary.SetRecordFieldNull(aField  : integer;
                                               aData   : PffByteArray;
                                               aIsNull : boolean);
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  with FieldDescriptor^[aField]^ do begin
    if aIsNull then
      FFSetBit(PffByteArray(@aData^[FLogRecLen]), aField)
    else
      FFClearBit(PffByteArray(@aData^[FLogRecLen]), aField);
    FillChar(aData^[fdOffset], fdLength, 0);
  end;
end;
{--------}
procedure TffDataDictionary.SetValidityCheck(aField  : integer;
                                         var aExists : boolean;
                                       const aVCheck : TffVCheckDescriptor);
begin
  if (aField < 0) or (aField >= FFldCount) then
    FFRaiseException(EffException, ffStrResGeneral, fferrOutOfBounds, [FBaseName, aField]);
  with FieldDescriptor^[aField]^ do begin
    if aExists then begin
      if (fdVCheck = nil) then
        FFGetZeroMem(fdVCheck, sizeOf(TffVCheckDescriptor));
      if (@aVCheck <> fdVCheck) then
        Move(aVCheck, fdVCheck^, sizeof(fdVCheck))
    end
    else {aExists is false} begin
      if (fdVCheck <> nil) then
        FFFreeMem(fdVCheck, sizeOf(TffVCheckDescriptor));
    end;
  end;
end;
{--------}
procedure TffDataDictionary.WriteToStream(S : TStream);
var
  Writer   : TWriter;
  i, j     : Integer;
  FileDesc : PffFileDescriptor;
  FldDesc  : PffFieldDescriptor;
  InxDesc  : PffIndexDescriptor;
begin
  CheckValid;
  Writer := TWriter.Create(S, 4096);
  try
    with Writer do begin
      WriteBoolean(FIsEncrypted);
      WriteInteger(FFileCount);
      for i := 0 to pred(FFileCount) do begin
        FileDesc := PffFileDescriptor(ddFileList[i]);
        with FileDesc^ do begin
          AnsiStringWriter(fdDesc, Writer);                            {!!.05}
          AnsiStringWriter(fdExtension, Writer);
          WriteInteger(fdBlockSize);
          WriteInteger(ord(fdType));
        end;
      end;
      WriteInteger(FFldCount);
      for i := 0 to pred(FFldCount) do begin
        FldDesc := FieldDescriptor^[i];
        with FldDesc^ do begin
          AnsiStringWriter(fdName, Writer);                            {!!.05}
          AnsiStringWriter(fdDesc, Writer);                            {!!.05}
          WriteInteger(fdUnits);
          WriteInteger(fdDecPl);
          WriteInteger(fdOffset);
          WriteInteger(fdLength);
          WriteInteger(ord(fdType));
          WriteBoolean(fdRequired);
          WriteBoolean(fdVCheck <> nil);
          if (fdVCheck <> nil) then begin
            with fdVCheck^ do begin
              AnsiStringWriter(vdPicture, Writer);                     {!!.05}
              WriteBoolean(vdHasMinVal);
              WriteBoolean(vdHasMaxVal);
              WriteBoolean(vdHasDefVal);
              if vdHasMinVal then
                Write(vdMinVal, fdLength);
              if vdHasMaxVal then
                Write(vdMaxVal, fdLength);
              if vdHasDefVal then
                Write(vdDefVal, fdLength);
            end;
          end;
        end;
      end;
      WriteInteger(FLogRecLen);
      WriteInteger(FInxCount);
      {note we don't write index 0 to the stream}
      for i := 1 to pred(FInxCount) do begin
        InxDesc := IndexDescriptor^[i];
        with InxDesc^ do begin
          AnsiStringWriter(idName, Writer);                            {!!.05}
          AnsiStringWriter(idDesc, Writer);                            {!!.05}
          WriteInteger(idFile);
          WriteInteger(idKeyLen);
          WriteInteger(idCount);
          if (idCount <> -1) then
            for j := 0 to pred(idCount) do begin
              WriteInteger(idFields[j]);
              if Length(idFieldIHlprs[j]) > 0 then
                AnsiStringWriter(idFieldIHlprs[j], Writer);            {!!.05}
            end;
          WriteBoolean(idDups);
          WriteBoolean(idAscend);
          WriteBoolean(idNoCase);
        end;
      end;
    end;
  finally
    Writer.Free;
  end;{try..finally}
end;
{====================================================================}

                                           {moved from FFTBBASE}
{===Composite Key manipulation routines==============================}
procedure FFInitKey(aKey         : PffByteArray;
                    aKeyLen      : integer;
                    aKeyFldCount : integer);
begin
  if (aKey <> nil) then begin
    FillChar(aKey^, aKeyLen, 0);
    if (aKeyFldCount <= 8) then
      FFSetAllBits(PffByteArray(@aKey^[aKeyLen-1]), aKeyFldCount)
    else
      FFSetAllBits(PffByteArray(@aKey^[aKeyLen-2]), aKeyFldCount);
  end;
end;
{--------}
function FFIsKeyFieldNull(aKey         : PffByteArray;
                          aKeyLen      : integer;
                          aKeyFldCount : integer;
                          aKeyFld      : integer) : boolean;
begin
  if (aKey = nil) then
    Result := true
  else begin
    if (aKeyFldCount <= 8) then
      Result := FFIsBitSet(PffByteArray(@aKey^[aKeyLen-1]), aKeyFld)
    else
      Result := FFIsBitSet(PffByteArray(@aKey^[aKeyLen-2]), aKeyFld);
  end;
end;
{--------}
procedure FFSetKeyFieldNonNull(aKey         : PffByteArray;
                               aKeyLen      : integer;
                               aKeyFldCount : integer;
                               aKeyFld      : integer);
begin
  if (aKey <> nil) then begin
    if (aKeyFldCount <= 8) then
      FFClearBit(PffByteArray(@aKey^[aKeyLen-1]), aKeyFld)
    else
      FFClearBit(PffByteArray(@aKey^[aKeyLen-2]), aKeyFld);
  end;
end;
{====================================================================}
end.
