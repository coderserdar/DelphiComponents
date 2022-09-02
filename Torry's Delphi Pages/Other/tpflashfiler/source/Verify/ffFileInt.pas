{*********************************************************}
{* FlashFiler: FF 2 file interface definition            *}
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

unit ffFileInt;

interface

uses
  Dialogs,
  Classes,
  FFLLBase,
  FFSrBase,
  FFTBDict;

type
  TffBlockType = (btUnknown, btFileHeader, btIndexHeader, btData,
                  btIndex, btBLOB, btStream, btFree);

{===Interface declarations===========================================}

  ICommonBlock = interface;   { forward declaration }
  TffFileInterface = class;   { forward declaration }
  TffGeneralFileInfo = class; { forward declaration }

  { Event declarations }

  TffGetInfoEvent = procedure(var Info : TffGeneralFileInfo) of object;
    { This event is raised by a block when it needs to
      obtain information about the file containing the block. }

  TffReportErrorEvent = procedure(Block : ICommonBlock;
                            const ErrCode : Integer;
                            const ErrorStr : string) of object;
    { This event is raised when an error is encountered during verification
      of a block. It may be raised during both verification & repair. ErrCode
      is the type of error encountered (see unit FFREPCNST for specific error
      codes) and ErrorStr is an informative string describing the error. }

  TffReportFixEvent = procedure(Block : ICommonBlock;
                          const ErrCode : Integer;
                          const RepairStr : string) of object;
    { This event is raised when an error in a block is repaired. ErrCode is
      the type of error encountered (see unit FFREPCNST for specific error
      codes) and RepairStr is an informative string describing how the
      error was fixed. }

  TffReportRebuildProgressEvent = procedure(FileInterface : TffFileInterface;
                                            Position, Maximum : Integer) of object;
    { This event should be raised by the file interface while it is packing
      or reindexing the table. }

  ICommonBlock = interface
  ['{D23CBB0D-375D-4125-9FE6-E543B651B665}']
    { Common interface to a file block. Other interfaces specific to block
      types are defined below. }

    procedure BeginUpdate;
      { Call this method prior to updating a file block. }
    procedure EndUpdate;
      { Call this method to commit changes to a file block. }

    function GetBlockNum : TffWord32;
    function GetBlockType : TffBlockType;
    function GetLSN : TffWord32;
    function GetNextBlock : TffWord32;
    function GetOnGetInfo : TffGetInfoEvent;
    function GetOnReportError : TffReportErrorEvent;
    function GetOnReportFix : TffReportFixEvent;
    function GetRawData : PffBlock;
    function GetSignature : Longint;
    function GetThisBlock : TffWord32;

    { Property access }
    function GetPropertyCell(const Row, Column : Integer) : string;
    function GetPropertyColCaption(const Index : Integer) : string;
    function GetPropertyColCount : Integer;
    function GetPropertyColWidth(const Index : Integer) : Integer;
    function GetPropertyRowCount : Integer;

    { Data access }
    function GetDataCell(const Row, Column : Integer) : string;
    function GetDataColCaption(const Index : Integer) : string;
    function GetDataColCount : Integer;
    function GetDataColWidth(const Index : Integer) : Integer;
    function GetDataRowCount : Integer;

    function MapBlockTypeToStr(const BlockType : TffBlockType) : string;
    function MapFlagsToStr(const Flags : Byte) : string;
    function MapSigToStr(const Signature : Longint) : string;

    procedure SetLSN(const Value : TffWord32);
    procedure SetNextBlock(const Value : TffWord32);
    procedure SetOnGetInfo(Value : TffGetInfoEvent);
    procedure SetOnReportError(Value : TffReportErrorEvent);
    procedure SetOnReportFix(Value : TffReportFixEvent);
    procedure SetSignature(const Value : Longint);
    procedure SetThisBlock(const Value : TffWord32);

    procedure Repair;
      { Call this method to have a block verify itself & repair any flaws it
        can repair on its own. }

    procedure Verify;
      { Call this method to have a block verify itself. }

    { Properties }
    property BlockNum : TffWord32
      read GetBlockNum;

    property BlockType : TffBlockType
      read GetBlockType;

    property LSN : TffWord32
      read GetLSN write SetLSN;

    property NextBlock : TffWord32
      read GetNextBlock write SetNextBlock;

    property OnGetInfo : TffGetInfoEvent
      read GetOnGetInfo write SetOnGetInfo;
      { This event is raised when a block needs to obtain information about its
        parent file. }

    property OnReportError : TffReportErrorEvent
      read GetOnReportError write SetOnReportError;
      { This event is raised when an error is detected in the block. It may
        be raised during both verification & repair. }

    property OnReportFix : TffReportFixEvent
      read GetOnReportFix write SetOnReportFix;
      { This event is raised when an error is fixed. It is raised only during
        the repair of a file. }

    property RawData : PffBlock
      read GetRawData;

    property Signature : Longint
      read GetSignature write SetSignature;

    property ThisBlock : TffWord32
      read GetThisBlock write SetThisBlock;

    { Property access }
    property PropertyCell[const Row, Column : Integer] : string
      read GetPropertyCell;
      { Returns the contents of the specified cell in the property view for
        this block. The Row and Column values are zero-based. }

    property PropertyColCaption[const Index : Integer] : string
      read GetPropertyColCaption;
      { Returns the suggested caption for the specified column. The Index
        parameter is zero-based. }

    property PropertyColCount : Integer
      read GetPropertyColCount;
      { The number of columns in the property view for this block. }

    property PropertyColWidth[const Index : Integer] : Integer
      read GetPropertyColWidth;
      { Returns the suggested width for the specified column. The Index
        parameter is zero-based. }

    property PropertyRowCount : Integer
      read GetPropertyRowCount;
      { The number of property rows in the view for this block. }

    { Data access }
    property DataCell[const Row, Column : Integer] : string
      read GetDataCell;
      { Returns the contents of the specified cell in the data view for this
        block. The Row and Column values are zero-based. }

    property DataColCaption[const Index : Integer] : string
      read GetDataColCaption;
      { Returns the suggested caption for the specified column. The Index
        parameter is zero-based. }

    property DataColCount : Integer
      read GetDataColCount;
      { The number of columns in the data view for this block. }

    property DataColWidth[const Index : Integer] : Integer
      read GetDataColWidth;
      { Returns the suggested width for the specified column. The Index
        parameter is zero-based. }

    property DataRowCount : Integer
      read GetDataRowCount;
      { The number of data rows in the view for this block. }

  end;

  IFileHeaderBlock = interface(ICommonBlock)
  ['{51157301-A9FA-4CBB-90A7-8FA30E8C17B9}']
    function GetAvailBlocks : Longint;
    function GetBLOBCount : TffWord32;
    function GetBlockSize : Longint;
    function GetDataDictBlockNum : TffWord32;
    function GetDeletedBLOBHead : TffInt64;
    function GetDeletedBLOBTail : TffInt64;
    function GetDeletedRecordCount : Longint;
    function GetEncrypted : Longint;
    function GetEstimatedUsedBlocks : TffWord32;
    function GetFFVersion : Longint;
    function GetFieldCount : Longint;
    function GetFirstDataBlock : TffWord32;
    function GetFirstDeletedRecord : TffInt64;
    function GetFirstFreeBlock : TffWord32;
    function GetHasSequentialIndex : Longint;
    function GetIndexCount : Longint;
    function GetIndexHeaderBlockNum : TffWord32;
    function GetLastAutoIncValue : TffWord32;
    function GetLastDataBlock : TffWord32;
    function GetLog2BlockSize : TffWord32;
    function GetRecLenPlusTrailer : Longint;
    function GetRecordCount : Longint;
    function GetRecordLength : Longint;
    function GetRecordsPerBlock : Longint;
    function GetUsedBlocks : TffWord32;

    procedure SetFirstDataBlock(const Value : TffWord32);
    procedure SetFirstFreeBlock(const Value : TffWord32);
    procedure SetHasSequentialIndex(const Value : Longint);
    procedure SetLastDataBlock(const Value : TffWord32);
    procedure SetLog2BlockSize(const Value : TffWord32);
    procedure SetUsedBlocks(const Value : TffWord32);

    property AvailBlocks : Longint
      read GetAvailBlocks;
      { The number of free blocks in the file. }

    property BLOBCount : TffWord32
      read GetBLOBCount;
      { The number of BLOBs in the table. }

    property BlockSize : Longint
      read GetBlockSize;
      { Size of blocks in bytes (e.g., 4k, 8k, 16k, 32k, 64k) }

    property DataDictBlockNum : TffWord32
      read GetDataDictBlockNum;
      { The block number of the data dictionary. If there is no data
        dictionary then this property returns the value zero. }

    property DeletedBLOBHead : TffInt64
      read GetDeletedBLOBHead;
      { The file-relative offset of the first segment in the deleted BLOB
        chain. }

    property DeletedBLOBTail : TffInt64
      read GetDeletedBLOBTail;
      { The file-relative offset of the last segment in the deleted BLOB
        chain. }

    property DeletedRecordCount : Longint
      read GetDeletedRecordCount;
      { The number of deleted records in the table. }

    property Encrypted : Longint
      read GetEncrypted;
      { 0 = not encrypted, 1 = encrypted }

    property EstimatedUsedBlocks : TffWord32
      read GetEstimatedUsedBlocks;
      { For cases where the UsedBlocks counter is invalid, use this property
        to estimate the number of used blocks in the file. }

    property FFVersion : Longint
      read GetFFVersion;
      { The version of FlashFiler with which this table was created. }

    property FieldCount : Longint
      read GetFieldCount;
      { The number of fields in a record. }

    property FirstDataBlock : TffWord32
      read GetFirstDataBlock write SetFirstDataBlock;
      { The first data block in the chain of data blocks. }

    property FirstDeletedRecord : TffInt64
      read GetFirstDeletedRecord;
      { The offset of the first record in the deleted record chain. }

    property FirstFreeBlock : TffWord32
      read GetFirstFreeBlock write SetFirstFreeBlock;
      { The block number of the first free block in the deleted block chain. }

    property HasSequentialIndex : Longint
      read GetHasSequentialIndex write SetHasSequentialIndex;
      { Identifies whether the table has a sequential index. A value of zero
        means the table does not have a sequential index. A value of 1
        means the table does have a sequential index. }

    property IndexCount : Longint
      read GetIndexCount;
      { The number of indexes in the table. }

    property IndexHeaderBlockNum : TffWord32
      read GetIndexHeaderBlockNum;
      { The block number of the index header. }

    property LastAutoIncValue : TffWord32
      read GetLastAutoIncValue;
      { The last autoincrement value assigned to a record in the table. }

    property LastDataBlock : TffWord32
      read GetLastDataBlock write SetLastDataBlock;
      { The last data block in the chain of data blocks. }

    property Log2BlockSize : TffWord32
      read GetLog2BlockSize write SetLog2BlockSize;
      { log base 2 of BlockSize (e.g., 12, 13, 14, 15, or 16) }

    property RecordCount : Longint
      read GetRecordCount;
      { The number of records in the table. }

    property RecordLength : Longint
      read GetRecordLength;
      { The length of the record in bytes. }

    property RecordLengthPlusTrailer : Longint
      read GetRecLenPlusTrailer;
      { The length of the record plus the deletion link. }

    property RecordsPerBlock : Longint
      read GetRecordsPerBlock;
      { The number of records per data block. }

    property UsedBlocks : TffWord32
      read GetUsedBlocks write SetUsedBlocks;
      { The number of blocks in the file. }

  end;

  TffGeneralFileInfo = class
  protected
    { The following vars identify the BLOB fields in a record. }
    FBLOBFldCount : Integer;
      { The number of BLOB fields found. }
    FBLOBFlds : array[0..1023] of Integer;
      { Contains field number (zero-based) of each BLOB field. }
    FBLOBFldName : array[0..1023] of string;
      { Contains field description for each BLOB field. Each element of the
        array has a one-to-one correspondence with the same element in the
        BLOBFlds array. }

    { The following vars identify key fields for error reporting purposes. }
    FKeyFldCount : Integer;
      { The number of key fields found. }
    FKeyFlds : array[0..127] of Integer;
      { Contains field number (zero-based) of each key field used to uniquely
        identify a record. }
    FKeyFldName : array[0..127] of string;
      { Contains field description for each key field used to uniquely identify
        a record. Each element of the array has a one-to-one correspondence with
        the same element in the KeyFlds array. }
    FUniqueIndexName : string;
      { Name of the unique index used for the key fields. }

    FBlockSize : Longint;
    FDict : TffServerDataDict;
    FLog2BlockSize : TffWord32;
    FRecLenPlusTrailer : Longint;
    FRecordCount : Longint;
    FRecordsPerBlock : Longint;

    procedure CalcKeyFields; virtual;
    function GetBLOBFields(const Inx : Integer) : Integer;
    function GetBLOBFieldNames(const Inx : Integer) : string;
    function GetKeyFields(const Inx : Integer) : Integer;
    function GetKeyFieldNames(const Inx : Integer) : string;
    procedure IdentBLOBFields; virtual;

  public
    { Methods }
    constructor Create(Dict : TffServerDataDict;
                       FileHeaderBlock : IFileHeaderBlock); virtual;
    destructor Destroy; override;

    function KeyFieldValues(RecPtr : PffByteArray) : string; virtual;

    { Properties }
    property BLOBFieldCount : Integer
      read FBLOBFldCount;
      { The number of BLOB fields in a record. }
    property BLOBFields[const Inx : Integer] : Integer
      read GetBLOBFields;
      { Array of BLOB field numbers. Returns an integer that is a zero-based
        index into the dictionary's list of fields. }
    property BLOBFieldNames[const Inx : Integer] : string
      read GetBLOBFieldNames;
      { Array of BLOB field names. The elements of this array have a one-to-one
        correspondence with the BLOBFields array. }
    property BlockSize : Longint
      read FBlockSize;
      { The size in bytes of the file's blocks. }
    property Dict : TffServerDataDict
      read FDict;
      { The data dictionary associated with the table. }
    property KeyFieldCount : Integer
      read FKeyFldCount;
      { Returns the number of fields used to uniquely identify a record in
        the table. }
    property KeyFields[const Inx : Integer] : Integer
      read GetKeyFields;
      { Array of key field numbers. Returns an integer that is a zero-based
        index into the dictionary's list of fields. }
    property KeyFieldNames[const Inx : Integer] : string
      read GetKeyFieldNames;
      { Array of key field names. The elements of this array have a one-to-one
        correspondence with the KeyFields array. }
    property Log2BlockSize : TffWord32
      read FLog2BlockSize;
      { Calculated value representative of the file's block size. }
    property RecLenPlusTrailer : Longint
      read FRecLenPlusTrailer;
      { Record length plus # of trailing bytes for null field flags. }
    property RecordCount : Longint
      read FRecordCount;
      { The # of records in the file. }
    property RecordsPerBlock : Longint
      read FRecordsPerBlock;
      { The maximum # of records per block. }
    property UniqueIndexName : string
      read FUniqueIndexName;
      { Returns the name of the unique index used to identify records in the
        table. }
  end;

  IDataBlock = interface(ICommonBlock)
  ['{7580BD14-3A18-40D9-8091-390D0150DF25}']
    function GetRecCount : Longint;
    function GetRecLen : Longint;
    function GetNextDataBlock : TffWord32;
    function GetPrevDataBlock : TffWord32;

    procedure SetNextDataBlock(const Value : TffWord32);
    procedure SetPrevDataBlock(const Value : TffWord32);
    procedure SetRecCount(const Value : Longint);
    procedure SetRecLen(const Value : Longint);

    property RecordCount : Longint
      read GetRecCount write SetRecCount;
      { The maximum number of records in the block. }
    property RecordLen : Longint
      read GetRecLen write SetRecLen;
      { The length of each record. }
    property NextDataBlock : TffWord32
      read GetNextDataBlock write SetNextDataBlock;
      { The block # of the next data block. }
    property PrevDataBlock : TffWord32
      read GetPrevDataBlock write SetPrevDataBlock;
      { The block # of the previous data block. }
  end;

  IIndexBlock = interface(ICommonBlock)
  ['{88433E3F-F4AD-445C-841A-A409751E38FE}']
    function GetIndexBlockType : Byte;
    function GetIsLeafPage : Boolean;
    function GetNodeLevel : Byte;
    function GetKeysAreRefs : Boolean;
    function GetIndexNum : Word;
    function GetKeyLength : Word;
    function GetKeyCount : Longint;
    function GetMaxKeyCount : Longint;
    function GetPrevPageRef : TffWord32;

    property IndexBlockType : Byte
      read GetIndexBlockType;
      { The type of index block. Header blocks have value 0, B-Tree pages
        have value 1. }
    property IsLeafPage : Boolean
      read GetIsLeafPage;
      { Returns False if this is an internal B-Tree page or True if this is
        a leaf B-Tree page. }
    property NodeLevel : Byte
      read GetNodeLevel;
      { Returns the node level. Leaves have value 1, increments. }
    property KeysAreRefs : Boolean
      read GetKeysAreRefs;
      { Returns the value True if the keys in the index are record reference
        numbers. }
    property IndexNum : Word
      read GetIndexNum;
      { The index number with which the index page is associated. }
    property KeyLength : Word
      read GetKeyLength;
      { The length of each key. }
    property KeyCount : Longint
      read GetKeyCount;
      { The number of keys currently in the page. }
    property MaxKeyCount : Longint
      read GetMaxKeyCount;
      { The maximum number of keys that may be placed within the page. }
    property PrevPageRef : TffWord32
      read GetPrevPageRef;
      { Block number of the previous page. }
  end;

  IIndexHeaderBlock = interface(IIndexBlock)
  ['{B5B7D142-BB11-4325-8E2E-D4E3621A2FE3}']
  end;

  IBLOBBlock = interface(ICommonBlock)
  ['{D4D5737F-3295-47FC-A6BF-A5B00AE5F905}']
  end;

  IStreamBlock = interface(ICommonBlock)
  ['{648433B7-604C-49BC-87D0-338582B1B238}']
    function GetNextStrmBlock : TffWord32;
    function GetOwningStream : Longint;
    function GetStreamLength : Longint;
    function GetStreamType : Longint;

    property NextStreamBlock : TffWord32
      read GetNextStrmBlock;
      { Block number of the next stream block in the chain or ffc_W32NoValue. }

    property OwningStream : Longint
      read GetOwningStream;
      { Block number of the first block of the stream. }

    property StreamLength : Longint
      read GetStreamLength;
      { Returns the length of the stream. This value is filled only for the
        first stream block. }

    property StreamType : Longint
      read GetStreamType;
      { For dictionary blocks, this will contain the value of constant
        ffc_SigDictStream. If it is a user-defined stream, it will contain
        some user-defined value. }

  end;

{===Class declarations===============================================}

  TffFileBlock = class;  { forward declaration }
  TffFileInterface = class
    { This abstract class defines the interface to a FlashFiler table. This
      interface is used by TffRepair to open a table & retrieve blocks from
      the table.

      In the initialization section, specific instances of this class must use
      the Register method to indicate their availability for specific FF table
      versions. The Unregister method must be called during finalization to
      deregister availability.
    }
  protected
    FStartFFVersion : Longint;
    FEndFFVersion : Longint;
    FID : string;
    FOutputVersion : Longint;
      { When a table is packed, the FF version that is to be assigned to the
        table. }
    FRebuildProgress : TffReportRebuildProgressEvent;

    function GetDictBlockCount : Longint; virtual; abstract;
    function GetDictBlocks(const Inx : Longint) : IStreamBlock; virtual; abstract;
    function GetOnReportError : TffReportErrorEvent; virtual; abstract;
    function GetOnReportFix : TffReportFixEvent; virtual; abstract;

    procedure SetOnReportError(Value : TffReportErrorEvent); virtual; abstract;
    procedure SetOnReportFix(Value : TffReportFixEvent); virtual; abstract;
    procedure SetOutputVersion(const Value : Longint); virtual; abstract;

  public

    { ========= Registration methods ========= }
    class procedure Register(const ID : string); virtual;
      { Creates an instance of this object and adds it to the list of
        registered file interfaces. }

    class procedure Unregister;
      { Removes all instances of this class type from the list of
        registered file interfaces. }

    class function FindInterface(const FileName : string) : TffFileInterface;
      { Searchs the list of registered file interface for a file interface that
        handles the specified FlashFiler table. }

    procedure Initialize; virtual;
      { This method is called after the object is instantiated via the
        Register class method. }

    function Handles(const FileName : string) : Boolean; virtual;
      { This function is called by the FindInterface class function. This
        function must determine whether the file interface handles the specified
        FlashFiler table. The default implementation compares the file's version
        against the value of the StartVersion and EndVersion properties. }

    { ========= Functionality methods ========= }
    procedure Close; virtual; abstract;
      { Close the currently opened file. }

    function GetBlock(const BlockNumber : Longint) : ICommonBlock; virtual; abstract;
      { Returns a specific block from the file. }

    function GetFileHeaderBlock : IFileHeaderBlock; virtual; abstract;
      { Returns the file header block. }

    function GetFileInfo : TffGeneralFileInfo; virtual; abstract;
      { Returns general file information that is made available to blocks. }

    function GetIndexHeaderBlock : IIndexHeaderBlock; virtual; abstract;
      { Returns the index header block. }

    procedure Open(const Filename : string); virtual; abstract;
      { Open a file for analysis. }

    procedure Pack; virtual; abstract;

    { Properties }
    property DictBlockCount : Longint
      read GetDictBlockCount;
      { Returns the number of data dictionary blocks. }

    property DictBlocks[const Inx : Longint] : IStreamBlock
      read GetDictBlocks;
      { Returns the specified data dictionary block. }

    property EndFFVersion : Longint
      read FEndFFVersion;
      { The final version of FF this interface supports. }

    property ID : string
      read FID;

    property OnRebuildProgress : TffReportRebuildProgressEvent
      read FRebuildProgress write FRebuildProgress;
      { Event handler used to report progress of reindex or pack. }

    property OnReportError : TffReportErrorEvent
      read GetOnReportError write SetOnReportError;
      { This event is raised when an error is detected in the block. It may
        be raised during both verification & repair. }

    property OnReportFix : TffReportFixEvent
      read GetOnReportFix write SetOnReportFix;
      { This event is raised when an error is fixed. It is raised only during
        the repair of a file. }

    property OutputVersion : Longint
      read FOutputVersion write SetOutputVersion;
      { The FF version to be assigned to a table when the table is packed.
        Defaults to the current FF version. }

    property StartFFVersion : Longint
      read FStartFFVersion;
      { The first version of FF this interface supports. }

  end;

  TffFileBlock = class(TInterfacedObject, ICommonBlock)
    { Base class representing a file block. Classes implementing an interface
      supporting a specific type of block should inherit from this class &
      the appropriate interface. }
  protected

    FBlock : PffBlock;
    FBlockNum : TffWord32;
    FBufMgr : TffBufferManager;
    FFileInfo : PffFileInfo;
    FOnGetInfo : TffGetInfoEvent;
    FOnReportError : TffReportErrorEvent;
    FOnReportFix : TffReportFixEvent;
    FRelMethod : TffReleaseMethod;
    FTI : PffTransInfo;

    procedure DoReportError(const ErrCode : Integer;
                                  args : array of const); virtual;
    procedure DoReportFix(const ErrCode: Integer;
                                args : array of const); virtual;
    function GetBlockNum : TffWord32;
    function GetBlockType : TffBlockType; virtual;
    function GetLSN : TffWord32; virtual;
    function GetNextBlock : TffWord32; virtual;
    function GetOnGetInfo : TffGetInfoEvent; virtual;
    function GetOnReportError : TffReportErrorEvent; virtual;
    function GetOnReportFix : TffReportFixEvent; virtual;
    function GetRawData : PffBlock; virtual;
    function GetSignature : Longint; virtual;
    function GetThisBlock : TffWord32; virtual;

    { Property access }
    function GetPropertyCell(const Row, Column : Integer) : string; virtual;
    function GetPropertyColCaption(const Index : Integer) : string; virtual;
    function GetPropertyColCount : Integer; virtual;
    function GetPropertyColWidth(const Index : Integer) : Integer; virtual;
    function GetPropertyRowCount : Integer; virtual;

    { Data access }
    function GetDataCell(const Row, Column : Integer) : string; virtual;
    function GetDataColCaption(const Index : Integer) : string; virtual;
    function GetDataColCount : Integer; virtual;
    function GetDataColWidth(const Index : Integer) : Integer; virtual;
    function GetDataRowCount : Integer; virtual;

    procedure SetLSN(const Value : TffWord32); virtual;
    procedure SetNextBlock(const Value : TffWord32); virtual;
    procedure SetOnGetInfo(Value : TffGetInfoEvent); virtual;
    procedure SetOnReportError(Value : TffReportErrorEvent); virtual;
    procedure SetOnReportFix(Value : TffReportFixEvent); virtual;
    procedure SetSignature(const Value : Longint); virtual;
    procedure SetThisBlock(const Value : TffWord32); virtual;

    procedure VerifyRepair(const Repair : Boolean); virtual;
      { This method is used by both Verify & Repair. It carries out the actual
        verification &, if specified, repairing of problems. }
  public

    constructor Create(BufMgr : TffBufferManager;
                       FileInfo : PffFileInfo;
                       TI : PffTransInfo;
                 const BlockNum : TffWord32); virtual;
    destructor Destroy; override;

    procedure BeginUpdate; virtual;
      { Call this method prior to updating a file block. }
    procedure EndUpdate; virtual;
      { Call this method to commit changes to a file block. }

    function MapBlockTypeToStr(const BlockType : TffBlockType) : string; virtual;
      { Use this to retrieve a text string representing the block type. }

    function MapFlagsToStr(const Flags : Byte) : string;
      { Use this to retrieve a text string representing the flags for an
        index. }
        
    function MapSigToStr(const Signature : Longint) : string; virtual;
      { Use this to retrieve a text string representing the signature. }

    procedure Repair; virtual;
      { Call this method to have a block verify itself & repair any flaws it
        can repair on its own. }

    procedure Verify; virtual;
      { Call this method to have a block verify itself. }

    { Properties }
    property BlockNum : TffWord32
      read GetBlockNum;

    property BlockType : TffBlockType
      read GetBlockType;

    property LSN : TffWord32
      read GetLSN write SetLSN;

    property NextBlock : TffWord32
      read GetNextBlock write SetNextBlock;

    property OnGetInfo : TffGetInfoEvent
      read GetOnGetInfo write SetOnGetInfo;
    { This event is raised by a TffFileBlock instance when it needs to
      obtain information about the file containing the block. The parent file
      interface must supply a handler for this event. }

    property OnReportError : TffReportErrorEvent
      read GetOnReportError write SetOnReportError;
      { This event is raised when an error is detected in the block. It may
        be raised during both verification & repair. }

    property OnReportFix : TffReportFixEvent
      read GetOnReportFix write SetOnReportFix;
      { This event is raised when an error is fixed. It is raised only during
        the repair of a file. }

    property RawData : PffBlock
      read GetRawData;

    property Signature : Longint
      read GetSignature write SetSignature;

    property ThisBlock : TffWord32
      read GetThisBlock write SetThisBlock;
  end;

{ Utility functions }
function BooleanValue(const TrueStr, FalseStr : string;
                      const Value : Boolean) : string;
function FlagStr(const Flag : Byte; const ZeroStr, OneStr : string) : string;
function ByteToHex(const B : byte) : string;
procedure GenerateHexLines(Buf : pointer; BufLen : TffMemSize;
                           Strings: TStrings);
function Int64ToStr(const Value : TffInt64) : string;
function LongintToChars(const L : Longint) : string;
function LongintToHex(const L : Longint) : string;
function Mirror(const Value : string) : string;
function VersionToStr(const Version : Longint) : string;
function YesNoValue(const Value : Longint) : string;

const
  ciFileBlockColumns = 2;
  ciFileBlockRows = 5;

implementation

uses
  FFRepCnst,
  FFUtil,
  SysUtils;

var
  _FileInterfaces : TffPointerList;

{===Utility functions================================================}
function BooleanValue(const TrueStr, FalseStr : string;
                      const Value : Boolean) : string;
begin
  if Value then
    Result := TrueStr
  else
    Result := FalseStr;
end;
{--------}
function FlagStr(const Flag : Byte; const ZeroStr, OneStr : string) : string;
begin
  if Flag = 0 then
    Result := ZeroStr
  else
    Result := OneStr;
  Result := Result + '(' + IntToStr(Flag) + ')';
end;
{--------}
function ByteToHex(const B : byte) : string;
const
  HexChars : array [0..15] of AnsiChar = '0123456789abcdef';
begin
  Result := HexChars[B shr 4] + HexChars[B and $F];
end;
{--------}
procedure GenerateHexLines(Buf : pointer; BufLen : TffMemSize;
                           Strings : TStrings);
const
  HexPos : array [0..15] of byte =
    (1, 3, 5, 7, 10, 12, 14, 16, 19, 21, 23, 25, 28, 30, 32, 34);
  HexChar : array [0..15] of char = '0123456789ABCDEF';
var
  B : PffByteArray absolute Buf;
  ThisWidth,
  i, j : integer;
  Line : string[56];
  Work : byte;
begin
  Strings.Clear;
  if (BufLen = 0) or (Buf = nil) then
    Exit
  else begin
    for i := 0 to ((BufLen-1) shr 4) do begin
      FillChar(Line, 56, ' ');
      Line[0] := #55;
      Line[38] := '['; Line[55] := ']';
      if (BufLen >= 16) then
        ThisWidth := 16
      else
        ThisWidth := BufLen;
      for j := 0 to Pred(ThisWidth) do begin
        Work := B^[(i shl 4) + j];
        Line[HexPos[j]] := HexChar[Work shr 4];
        Line[HexPos[j]+1] := HexChar[Work and $F];
        if (Work < 32) then
          Work := ord('.');
        Line[39+j] := char(Work);
      end;
      Strings.Add(Line);
      dec(BufLen, ThisWidth);
    end;
  end;
end;
{--------}
function Int64ToStr(const Value : TffInt64) : string;
begin
  Result := IntToStr(Value.iHigh) + ':' + IntToStr(Value.iLow);
end;
{--------}
function LongintToChars(const L : Longint) : string;
var
  Inx : Integer;
  Val : Integer;
begin
  Result := Char(L shr 24) +
            Char((L shr 16) and $FF) +
            Char((L shr 8) and $FF) +
            Char(L and $FF);

  { Convert values 0 - 9 to corresponding digits. }
  for Inx := 1 to 4 do begin
    Val := Ord(Result[Inx]);
    if Val < 10 then
      Result[Inx] := Char(Val + 48);
  end;
end;
{--------}
function LongintToHex(const L : Longint) : string;
begin
  Result := ByteToHex(L shr 24) +
            ByteToHex((L shr 16) and $FF) +
            ByteToHex((L shr 8) and $FF) +
            ByteToHex(L and $FF);
end;
{--------}
function Mirror(const Value : string) : string;
var
  Inx : Integer;
  Len : Integer;
begin
  Len := Length(Value);
  SetLength(Result, Len);
  for Inx := 1 to Len do
    Result[Len - Pred(Inx)] := Value[Inx];
end;
{--------}
function VersionToStr(const Version : Longint) : string;
begin
  Result := Format('%5.4f', [Version / 10000.0]);
end;
{--------}
function YesNoValue(const Value : Longint) : string;
begin
  if Value = 0 then
    Result := 'No (0)'
  else
    Result := 'Yes (' + IntToStr(Value) + ')';
end;
{====================================================================}

{===TffGeneralFileInfo===============================================}
constructor TffGeneralFileInfo.Create(Dict : TffServerDataDict;
                                      FileHeaderBlock : IFileHeaderBlock);
begin
  inherited Create;

  FDict := TffServerDataDict.Create(Dict.BlockSize);
  FDict.Assign(Dict);

  FBlockSize := FileHeaderBlock.BlockSize;
  FLog2BlockSize := FileHeaderBlock.Log2BlockSize;
  FRecLenPlusTrailer := FileHeaderBlock.RecordLengthPlusTrailer;
  FRecordCount := FileHeaderBlock.RecordCount;
  FRecordsPerBlock := FileHeaderBlock.RecordsPerBlock;

  IdentBLOBFields;
  CalcKeyFields;  
end;
{--------}
destructor TffGeneralFileInfo.Destroy;
begin
  FDict.Free;
  inherited;
end;
{--------}
procedure TffGeneralFileInfo.CalcKeyFields;
var
  Inx : Integer;
  IndexDesc : PffIndexDescriptor;
begin
  if FKeyFldCount = 0 then begin
    { Determine which fields will be used to uniquely identify each
      record.

      Strategy: Find the first unique index. If that is found, use its fields
        to identify the record. If one is not found then use first 4 fields. }

    FillChar(FKeyFlds, SizeOf(FKeyFlds), 0);
    FKeyFldCount := 0;
    IndexDesc := nil;
    for Inx := 1 to Pred(FDict.IndexCount) do begin
      { Skip Sequential Access Index. }
      if not FDict.IndexAllowDups[Inx] then begin
        IndexDesc := FDict.IndexDescriptor[Inx];
        Break;
      end;  { if }
    end;  { for }

    if Assigned(IndexDesc) then begin
      { Records will be identified using a unique index. }
      FUniqueIndexName := IndexDesc^.idName;
      for Inx := 0 to Pred(IndexDesc^.idCount) do begin
        FKeyFlds[Inx] := IndexDesc^.idFields[Inx];
        FKeyFldName[Inx] := FDict.FieldName[FKeyFlds[Inx]];
      end;  { for }
      FKeyFldCount := IndexDesc^.idCount;
    end
    else begin
      FKeyFldCount := FFMinI(4, FDict.FieldCount);
      FUniqueIndexName := 'No unique index. Records identified using fields 1 ' +
                          'through ' + IntToStr(FKeyFldCount) + ' of the table.';
      for Inx := 0 to Pred(FKeyFldCount) do begin
        FKeyFlds[Inx] := Inx;
        FKeyFldName[Inx] := FDict.FieldDesc[Inx];
      end;  { for }
    end;  { if..else }
  end;  { if }
end;
{--------}
function TffGeneralFileInfo.GetBLOBFields(const Inx : Integer) : Integer;
begin
  Result := FBLOBFlds[Inx];
end;
{--------}
function TffGeneralFileInfo.GetBLOBFieldNames(const Inx : Integer) : string;
begin
  Result := FBLOBFldName[Inx];
end;
{--------}
function TffGeneralFileInfo.GetKeyFields(const Inx : Integer) : Integer;
begin
  Result := FKeyFlds[Inx];
end;
{--------}
function TffGeneralFileInfo.GetKeyFieldNames(const Inx : Integer) : string;
begin
  Result := FKeyFldName[Inx];
end;
{--------}
procedure TffGeneralFileInfo.IdentBLOBFields;
var
  Inx : Integer;
begin
  FillChar(FBLOBFlds, SizeOf(FBLOBFlds), 0);
  FBLOBFldCount := 0;
  for Inx := 0 to Pred(FDict.FieldCount) do begin
    if FDict.FieldType[Inx] in [fftBLOB..fftBLOBTypedBin] then begin
      FBLOBFlds[FBLOBFldCount] := Inx;
      FBLOBFldName[FBLOBFldCount] := FDict.FieldName[Inx];
      inc(FBLOBFldCount);
    end;  { if }
  end;  { for }
end;
{--------}
function TffGeneralFileInfo.KeyFieldValues(RecPtr : PffByteArray) : string;
var
  Inx : Integer;
  FieldValue : TffVCheckValue;
  IsNull : Boolean;
begin
  Result := '';
  for Inx := 0 to Pred(FKeyFldCount) do begin
    if Result <> '' then
      Result := Result + '; ';
    FillChar(FieldValue, SizeOf(FieldValue), 0);
    FDict.GetRecordField(FKeyFlds[Inx], RecPtr, IsNull, @FieldValue);
    if IsNull then
      Result := Result + Format('%s: %s',
                                [FKeyFldName[Inx], '<null>'])
    else
      Result := Result + Format('%s: %s',
                                [FKeyFldName[Inx],
                                 FFVCheckValToString
                                   (FieldValue,
                                    FDict.FieldType[FKeyFlds[Inx]])
                                ]);
  end;  { for }
end;
{====================================================================}

{===TffFileInterface=================================================}
function TffFileInterface.Handles(const FileName : string) : Boolean;
var
  CharsRead : Integer;
  FileVersion : Longint;
  Stream : TFileStream;
  Block : TffBlock;
  FileHeader : PffBlockHeaderFile;
begin
  Result := False;
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    { Read the file header. }
    CharsRead := Stream.Read(Block, 4096);
    if CharsRead = 4096 then begin
      FileHeader := PffBlockHeaderFile(@Block);
      if FileHeader^.bhfSignature = ffc_SigHeaderBlock then begin
        { Check the version. }
        FileVersion := FileHeader^.bhfFFVersion;
        Result := (FileVersion >= StartFFVersion) and (FileVersion <= EndFFVersion);
      end;
    end
    else
      raise Exception.CreateFmt('"%s" is not a FlashFiler table.', [FileName]);
  finally
    Stream.Free;
  end;
end;
{--------}
class procedure TffFileInterface.Register(const ID : string);
var
  FileInterface: TffFileInterface;
begin
  FileInterface := Create;
  try
    FileInterface.Initialize;
    _FileInterfaces.Append(FileInterface);
  except
    FileInterface.Free;
  end;
  FileInterface.FID := ID;
end;
{--------}
class procedure TffFileInterface.Unregister;
var
  wInx : Integer;
begin
  if _FileInterfaces = nil then
    Exit;
  { Free every instance of this class. }
  for wInx := Pred(_FileInterfaces.Count) downto 0 do
    with TffFileInterface(_FileInterfaces.Pointers[wInx]) do
      if (ClassType = Self) then begin
        Free;
        _FileInterfaces.RemoveAt(wInx);
      end;
end;
{--------}
class function TffFileInterface.FindInterface(const FileName : string) : TffFileInterface;
var
  wInx : Integer;
begin
  Result := nil;
  for wInx := 0 to Pred(_FileInterfaces.Count) do
    with TffFileInterface(_FileInterfaces.Pointers[wInx]) do
      if Handles(FileName) then begin
        Result := _FileInterfaces.Pointers[wInx];
        Break;
      end;
end;
{--------}
procedure TffFileInterface.Initialize;
begin
  { Descendant classes may override this method for custom initialization. }
end;
{====================================================================}

{===TffFileBlock=====================================================}
constructor TffFileBlock.Create(BufMgr : TffBufferManager;
                                FileInfo : PffFileInfo;
                                TI : PffTransInfo;
                          const BlockNum : TffWord32);
begin
  inherited Create;
  FBufMgr := BufMgr;
  FBlock := FBufMgr.GetBlock(FileInfo, BlockNum, TI, ffc_ReadOnly, FRelMethod);
  FFileInfo := FileInfo;
  FTI := TI;
  FBlockNum := BlockNum;
end;
{--------}
destructor TffFileBlock.Destroy;
begin
  try
    if Assigned(FRelMethod) and Assigned(FBlock) then
      FRelMethod(FBlock);
  finally
    inherited;
  end;
end;
{--------}
procedure TffFileBlock.BeginUpdate;
begin
  { Do nothing }
end;
{--------}
procedure TffFileBlock.EndUpdate;
begin
  { Do nothing }
end;
{--------}
procedure TffFileBlock.DoReportError(const ErrCode : Integer;
                                     args : array of const);
begin
  if Assigned(FOnReportError) then
    FOnReportError(Self, ErrCode,
                   Format(rcErrStr[ErrCode], args));
end;
{--------}
procedure TffFileBlock.DoReportFix(const ErrCode : Integer;
                                         args : array of const);
begin
  if Assigned(FOnReportError) then
    FOnReportFix(Self, ErrCode,
                 Format(rcFixStr[ErrCode], args));
end;
{--------}
function TffFileBlock.GetBlockNum : TffWord32;
begin
  Result := FBlockNum;
end;
{--------}
function TffFileBlock.GetBlockType : TffBlockType;
begin
  case PffBlockCommonHeader(FBlock)^.bchSignature of
    ffc_SigHeaderBlock : Result := btFileHeader;
    ffc_SigDataBlock   : Result := btData;
    ffc_SigIndexBlock  :
      begin
        if PffBlockHeaderIndex(FBlock)^.bhiBlockType = 0 then
          Result := btIndexHeader
        else
          Result := btIndex;
      end;
    ffc_SigBLOBBlock   : Result := btBLOB;
    ffc_SigStreamBlock : Result := btStream;
    ffc_SigFreeBlock   : Result := btFree;
  else
    Result := btUnknown;
  end;  { case }
end;
{--------}
function TffFileBlock.GetDataCell(const Row, Column : Integer) : string;
begin
  Result := '';
end;
{--------}
function TffFileBlock.GetDataColCaption(const Index : Integer) : string;
begin
  Result := '';
end;
{--------}
function TffFileBlock.GetDataColCount : Integer;
begin
  Result := 0;
end;
{--------}
function TffFileBlock.GetDataColWidth(const Index : Integer) : Integer;
begin
  Result := 0;
end;
{--------}
function TffFileBlock.GetDataRowCount : Integer;
begin
  Result := 0;
end;
{--------}
function TffFileBlock.GetLSN : TffWord32;
begin
  Result := PffBlockCommonHeader(FBlock)^.bchLSN;
end;
{--------}
function TffFileBlock.GetNextBlock : TffWord32;
begin
  Result := PffBlockCommonHeader(FBlock)^.bchNextBlock;
end;
{--------}
function TffFileBlock.GetOnGetInfo : TffGetInfoEvent;
begin
  Result := FOnGetInfo;
end;
{--------}
function TffFileBlock.GetOnReportError : TffReportErrorEvent;
begin
  Result := FOnReportError;
end;
{--------}
function TffFileBlock.GetOnReportFix : TffReportFixEvent;
begin
  Result := FOnReportFix;
end;
{--------}
function TffFileBlock.GetRawData : PffBlock;
begin
  Result := FBlock;
end;
{--------}
function TffFileBlock.GetSignature : Longint;
begin
  Result := PffBlockCommonHeader(FBlock)^.bchSignature;
end;
{--------}
function TffFileBlock.GetThisBlock : TffWord32;
begin
  Result := PffBlockCommonHeader(FBlock)^.bchThisBlock;
end;
{--------}
function TffFileBlock.GetPropertyCell(const Row, Column : Integer) : string;
begin
  if Column > Pred(ciFileBlockColumns) then
    raise Exception.CreateFmt
            ('Cannot ask for cell in column %d when there are only %d columns in the view',
             [Column, ciFileBlockColumns]);

  case Row of
    0 : if Column = 0 then
          Result := 'Block type'
        else
          Result := MapBlockTypeToStr(GetBlockType);
    1 : if Column = 0 then
          Result := 'Signature'
        else
          Result := MapSigToStr(GetSignature);
    2 : if Column = 0 then
          Result := 'This block'
        else
          Result := IntToStr(GetThisBlock);
    3 : if Column = 0 then
          Result := 'Next block'
        else
          Result := IntToStr(GetNextBlock);
    4 : if Column = 0 then
          Result := 'LSN'
        else
          Result := IntToStr(GetLSN);
  else
    raise Exception.CreateFmt
            ('Cannot ask for cell in row %d when there are only %d rows in the view',
             [Row, ciFileBlockRows]);
  end;  { case }
end;
{--------}
function TffFileBlock.GetPropertyColCaption(const Index : Integer) : string;
begin
  case Index of
    0 : Result := 'Property';
    1 : Result := 'Value';
  else
    raise Exception.CreateFmt
            ('Cannot ask for caption %d when there are only %d columns in the view',
             [Index, ciFileBlockColumns]);
  end;  { case }
end;
{--------}
function TffFileBlock.GetPropertyColCount : Integer;
begin
  Result := ciFileBlockColumns;
end;
{--------}
function TffFileBlock.GetPropertyColWidth(const Index : Integer) : Integer;
begin
  case Index of
    0 : Result := 150;
    1 : Result := 150;
  else
    raise Exception.CreateFmt
            ('Cannot ask for width %d when there are only %d columns in the view',
             [Index, ciFileBlockColumns]);
  end;  { case }
end;
{--------}
function TffFileBlock.GetPropertyRowCount : Integer;
begin
  Result := ciFileBlockRows;
end;
{--------}
function TffFileBlock.MapBlockTypeToStr(const BlockType : TffBlockType) : string;
begin
  case BlockType of
    btUnknown    : Result := 'Unknown';
    btFileHeader : Result := 'File header';
    btIndexHeader : Result := 'Index header';
    btData       : Result := 'Data';
    btIndex      : Result := 'Index';
    btBLOB       : Result := 'BLOB';
    btStream     : Result := 'Stream';
    btFree       : Result := 'Free';
  end;  { case }
end;
{--------}
function TffFileBlock.MapFlagsToStr(const Flags : Byte) : string;
var
  FlagSet : Boolean;
begin
  FlagSet := False;
  Result := IntToStr(Flags);
  if Flags > 0 then begin
    Result := Result + ' [';
    if (Flags and ffc_InxFlagAllowDups) <> 0 then begin
      Result := Result + ' Allow dups';
      FlagSet := True;
    end;

    if (Flags and ffc_InxFlagKeysAreRefs) <> 0 then begin
      if FlagSet then
        Result := Result + ', ';
      Result := Result + 'Keys are refs'
    end;  { if }
    Result := Result + ']';
  end;  { if }
end;
{--------}
function TffFileBlock.MapSigToStr(const Signature : Longint) : string;
begin
  Result := Mirror(LongintToChars(Signature)) + ' (' +
            LongintToHex(Signature) + ')';
end;
{--------}
procedure TffFileBlock.Repair;
begin
  try
    VerifyRepair(True);
  except
    on E:Exception do
      ShowMessage(E.Message);
  end;
end;
{--------}
procedure TffFileBlock.SetLSN(const Value : TffWord32);
begin
  PffBlockCommonHeader(FBlock)^.bchLSN := Value;
end;
{--------}
procedure TffFileBlock.SetNextBlock(const Value : TffWord32);
begin
  PffBlockCommonHeader(FBlock)^.bchNextBlock := Value;
end;
{--------}
procedure TffFileBlock.SetOnGetInfo(Value : TffGetInfoEvent);
begin
  FOnGetInfo := Value;
end;
{--------}
procedure TffFileBlock.SetOnReportError(Value : TffReportErrorEvent);
begin
  FOnReportError := Value;
end;
{--------}
procedure TffFileBlock.SetOnReportFix(Value : TffReportFixEvent);
begin
  FOnReportFix := Value;
end;
{--------}
procedure TffFileBlock.SetSignature(const Value : Longint);
begin
  PffBlockCommonHeader(FBlock)^.bchSignature := Value;
end;
{--------}
procedure TffFileBlock.SetThisBlock(const Value : TffWord32);
begin
  PffBlockCommonHeader(FBlock)^.bchThisBlock := Value;
end;
{--------}
procedure TffFileBlock.Verify;
begin
  VerifyRepair(False);
end;
{--------}
procedure TffFileBlock.VerifyRepair(const Repair : Boolean);
var
  Block : PffBlock;
  RelMethod : TffReleaseMethod;
  Modified : Boolean;
begin
  Modified := False;
  try
    { Verify the block type. }
    if BlockType = btUnknown then begin
      DoReportError(rciUnknownBlockType,
                    [PffBlockCommonHeader(FBlock)^.bchSignature]);
      if Repair then begin
        BeginUpdate;
        Modified := True;
        { Mark this as a free block. }
        PffBlockCommonHeader(FBlock)^.bchSignature := ffc_SigFreeBlock;
        DoReportFix(rciUnknownBlockType,
                    [BlockNum]);
      end;
    end;

    { Can't do much with the LSN. }

    { Verify the next block is a valid block. }
    if NextBlock <> ffc_W32NoValue then
      try
        Block := FBufMgr.GetBlock(FFileInfo, NextBlock, FTI, ffc_ReadOnly,
                                  RelMethod);
        RelMethod(Block);
      except
        DoReportError(rciInvalidBlockRefNext, [NextBlock]);
      end;

    { Verify ThisBlock matches this block number. }
    if ThisBlock <> FBlockNum then begin
      DoReportError(rciInvalidThisBlock, [FBlockNum, ThisBlock]);
      if Repair then begin
        BeginUpdate;
        Modified := True;
        ThisBlock := FBlockNum;
        DoReportFix(rciInvalidThisBlock, [FBlockNum]);
      end;
    end;
  finally
    if Modified then
      EndUpdate;
  end;
end;
{====================================================================}


initialization
  _FileInterfaces := TffPointerList.Create;

finalization

  _FileInterfaces.Free;
    { Assumption: Units registering comparator classes will also unregister
      them. }
  _FileInterfaces := nil;

end.
