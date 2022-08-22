{**********************************************************}
{                                                          }
{  TinyDB Database Engine                                  }
{  Version 2.94 32/64 bit                                  }
{                                                          }
{  Author: DayDream Software                               }
{  Email: haoxg@21cn.com                                   }
{  URL: http://www.tinydb.com                              }
{  Last Modified Date: 2005-10-31                          }
{                                                          }
{  Refreshed and modification for unicode by Jaro Benes    }
{   19.XI.2009 + simple translation to tiny English        }
{   26.III.2019 + corrected 64-bit compatibility           }
{**********************************************************}

unit TinyDB;

{$I TinyDB.inc}
{$WARNINGS OFF}
{.$DEFINE __SQL}
{.$UNDEF SUPP_INLINE}

interface

uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  Dialogs, Db, StdCtrls, ExtCtrls, Graphics, Math, {$IFDEF DELPHI_7_UP}Types, {$ENDIF}
  {$IFDEF DELPHI_6_UP} Variants, RTLConsts, {$ENDIF}
  {$IFDEF DELPHI_17_UP}System.Generics.Collections, {$ENDIF}
  {$IFDEF UNICODE}AnsiStrings, {$ENDIF}
  DbConsts, Consts;

const
  tdbWebsite = 'http://www.tinydb.com'; {TinyDB web site}
  tdbSupportEmail = 'haoxg@21cn.com';   {TinyDB's Support Email}

  tdbSoftName = 'TinyDB';
  tdbSoftVer = '2.94';                  {Database engine version number}
  tdbFileFmtVer = '2.0';                {Database file format version number}
  tdbDBFileExt = '.tdb';                {Default extension}
  tdbMaxTable = 256;                    {Maximum number of tables}
  tdbMaxField = 96;                     {Maximum number of fields}
  tdbMaxIndex = 8;                      {A table allows you to define the maximum index number}
  tdbMaxFieldNameChar = 32;             {Maximum number of characters in field names}
  tdbMaxTableNameChar = 32;             {Table name maximum number of characters}
  tdbMaxIndexNameChar = 32;             {Index name of the largest number of characters}
  tdbMaxAlgoNameChar = 32;              {Method name of the maximum number of characters}
  tdbMaxCommentsChar = 256;             {Maximum number of bytes Database Notes}
  tdbMaxExtDataSize = 1024*2;           {Number of bytes of additional data blocks}
  tdbMaxHashPwdSize = 128;              {Hash database password after the maximum number of bytes}
  tdbRndBufferSize = 256;               {Random padding size}
  tdbRecTabUnitNum = 1024;              {Log the number of blocks of elements in an integer multiple of the number of}
  tdbIdxTabUnitNum = 1024;              {Index table block number of elements of an integer multiple of the number of}
  tdbMaxTextFieldSize = 8192;           {Text field is the maximum length of}
  tdbBlobSizeUnitNum = 64;              {Indefinite length (Blob)-type field stores an integer multiple of the length of the number of}
  tdbMaxMultiIndexFields = 8;           {The maximum number of fields in composite index}
  tdbDefaultLockRetryCount = 20;        {Wait for locked resources, the number of retries defaults}
  tdbDefaultLockWaitTime = 100;         {Twice the time interval between retry the default value (milliseconds)}
  tdbDefAutoFlushInterval = 60*1000;    {Auto Flush the time interval default value (ms) }

  SGeneralError = 'General error';
  SAccessDenied = 'Access denied';
  STableNotFound = 'Table ''%s'' not found';
  SBookmarkNotFound = 'Bookmark not found';
  SFieldCountMismatch = 'Field count mismatch';
  STooManyTables = 'Too many tables';
  STooManyFields = 'Too many fields';
  STooManyIndexes = 'Too many indexes';
  SDuplicateTableName = 'Duplicate table name ''%s''';
  SDuplicateIndexName = 'Duplicate index name ''%s''';
  SDuplicatePrimaryIndex = 'Duplicate primary index';
  SDuplicateAutoIncField = 'Duplicate AutoInc field';
  SInvalidDatabaseName = 'Invalid database name ''%s''';
  SInvalidTableName = 'Invalid table name ''%s''';
  SInvalidFieldName = 'Invalid field name ''%s''';
  SInvalidIndexName = 'Invalid index name ''%s''';
  SInvalidDatabase = 'Database file ''%s'' is a invalid TinyDB';
  SInvalidVersion100 = 'Version 1.x is not compatible';
  SInvalidVersion200 = 'Version 2.x is not compatible';
  SInvalidVersionTooHigh = 'Version of database file is too High';
  SFieldNameExpected = 'Field name expected';
  SFailToCreateIndex = 'Fail to create index';
  SInvalidUniqueFieldValue = 'Invalid unique field value ''%s''';
  SInvalidMultiIndex = 'Invalid complex index';
  SSQLInvalid = 'Invalid SQL statement: ''%s''';
  SSQLSyntaxErr = 'Syntax error in SQL statement: ''%s''';
  SSQLInvalidChar = 'Invalid SQL character: ''%s''';
  SCompressAlgNotFound = 'Compression algorithm module ''%s'' not found';
  SEncryptAlgNotFound = 'Encryption algorithm module ''%s'' not found';
  SDatabaseReadOnly = 'Cannot modify a read-only database';
  SNoRecords = 'No records';
  SWaitForUnlockTimeOut = 'Wait for unlock time out';

{$IFNDEF SUPP_INLINE}
const
  ccCSet = ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']'];
{$ENDIF}

type
  {$IFNDEF UNICODE}
  //When is not used unicode Delphi (2009/2010/XE/XE2...), this supplement use only.
  TRecordBuffer = PChar;
  {$ENDIF}

  PMemoryStream = ^TMemoryStream;
  PBoolean = ^Boolean;
  PWordBool = ^WordBool;
  PLargeInt = ^LargeInt;
  TIntegerAry = array of Integer;

  TTinyDBMediumType = (mtDisk, mtMemory);
  TTDIndexOption = (tiPrimary, tiUnique, tiDescending, tiCaseInsensitive);
  TTDIndexOptions = set of TTDIndexOption;
  TTDKeyIndex = (tkLookup, tkRangeStart, tkRangeEnd, tkSave);

  TCompressLevel = (clMaximum, clNormal, clFast, clSuperFast);
  TEncryptMode = (emCTS, emCBC, emCFB, emOFB, emECB);

  TFieldDataProcessMode = (fdDefault, fdOriginal);
  TFieldDPModeAry = array of TFieldDataProcessMode;

  TOnProgressEvent = procedure (Sender: TObject; Percent: Integer) of object;

  //---------------------------
  //---Database file format used to define record types--------

  // Database file header
  TFileHeader = packed record
    SoftName: array[0..6] of AnsiChar;
    FileFmtVer: array[0..4] of AnsiChar;
  end;

  // Additional data block
  TExtData = array[0..tdbMaxExtDataSize-1] of Byte;
  TExtDataBlock = packed record
    Comments: array[0..tdbMaxCommentsChar] of AnsiChar; // Database Notes
    Data: TExtData;                                    // Store user-defined data
  end;

  TAlgoNameString = array[0..tdbMaxAlgoNameChar] of AnsiChar;
  // Database option settings
  TDBOptions = packed record
    CompressBlob: Boolean;             //   If compressed Blob field (only an effect after the database operation)
    CompressLevel: TCompressLevel;     //   Compression level
    CompressAlgoName: TAlgoNameString; //   Compression algorithm name
    Encrypt: Boolean;                  //   Database is encrypted (which determines whether the entire database encryption)
    EncryptMode: TEncryptMode;         //   Encryption Mode
    EncryptAlgoName: TAlgoNameString;  //   Encryption algorithm name of
    CRC32: Boolean;                    //   Write BLOB fields whether CRC32 checkout
    RandomBuffer: array[0..tdbRndBufferSize-1] of AnsiChar;  //   Random fill
    HashPassword: array[0..tdbMaxHashPwdSize] of AnsiChar;   //   Hash algorithm after the database access after the password
    Reserved: array[0..15] of Byte;
  end;

  // All the tables in the database
  TTableTab = packed record
    TableCount: Integer;      // Table Number
    Reserved: Integer;
    TableHeaderOffset: array[0..tdbMaxTable-1] of Integer;   //   Header offset
  end;

  //  Field Project
  TFieldTabItem = packed record
    FieldName: array[0..tdbMaxFieldNameChar] of AnsiChar;  //   Field name
    FieldType: TFieldType;                                 //   Field Type
    FieldSize: Integer;                                    //   Field Size
    DPMode: TFieldDataProcessMode;                         //   The field of data processing methods
    Reserved: Integer;
  end;

  // Index head
  TIndexHeader = packed record
    IndexName: array [0..tdbMaxIndexNameChar] of AnsiChar;    //   Index name
    IndexOptions: TTDIndexOptions;                            //   Index type
    FieldIdx: array [0..tdbMaxMultiIndexFields-1] of Integer; //   Index field
    IndexOffset: Integer;                                     //   Index of items to offset
    StartIndex: Integer;                                      //   The first index of the item subscript
    Reserved: Integer;
  end;

  TTableNameString = array[0..tdbMaxTableNameChar] of AnsiChar;
  // Header
  TTableHeader = packed record
    //    The following four members can not tamper with
    TableName: TTableNameString;  //  Table name
    RecTabOffset: Integer;        //  Log entry table offset
    RecordTotal: Integer;         //  Record the total number, including the records to delete tag
    AutoIncCounter: Integer;      //  Auto-growth counter

    FieldCount: Integer;          //   Field Total
    FieldTab: array [0..tdbMaxField-1] of TFieldTabItem;   //  Field in the table
    IndexCount: Integer;                                   //  Index of the total number of
    IndexHeader: array [0..tdbMaxIndex-1] of TIndexHeader; //  Index header information
    Reserved: array [0..15] of Byte;
  end;

  // Records Project
  TRecordTabItem = packed record
    DataOffset: Integer; // Record Data Offset
    DeleteFlag: Boolean; // Remove the mark, for True, said to delete
  end;
  TRecordTabItems = array of TRecordTabItem;
  PRecordTabItems = ^TRecordTabItems;

  // Index Project
  TIndexTabItem = packed record
    RecIndex: Integer; // Point to the file RecordTable of an element (0-based)
    Next: Integer;     // The next index items Index (0-based)
  end;
  TIndexTabItems = array of TIndexTabItem;

  // Indefinite length of the field is the first
  TBlobFieldHeader = packed record
    DataOffset: Integer; // Offset the actual data
    DataSize: Integer;   // Of the effective length of the actual data
    AreaSize: Integer;   // Reserved for the total length of Blob
    Reserved1: Integer;
    Reserved2: Integer;
  end;
  PBlobFieldHeader = ^TBlobFieldHeader;

  //---------------------------
  //--Is used in memory management database record types-----
  TMemRecTabItem = record
    DataOffset: Integer; // Point to record data in the file offset position
    RecIndex: Integer;   // This recorded in the file RecordTab the next label (0-based)
  end;
  PMemRecTabItem = ^TMemRecTabItem;

  TMemQryRecItem = record
    DataOffsets: array of Integer;
  end;

  // Of use to the type of TDataSet

  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    Bookmark: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  // Field project, CreateTable the parameter type
  TFieldItem = record
    FieldName: string[tdbMaxFieldNameChar+1];   //   Field Name
    FieldType: TFieldType;                      //   Field Type
    DataSize: Integer;                          //   Field Size
    DPMode: TFieldDataProcessMode;              //   Field data processing methods
  end;

//-------------------------------------------------------------------

{ forwarded classes }

  TTinyAboutBox = class;
  TTinyIndexDef = class;
  TTinyIndexDefs = class;
  TTinyTableDef = class;
  TTinyTableDefs = class;
  TTinyDBFileIO = class;
  TTinyTableIO = class;
  TTDEDataSet = class;
  TTinyTable = class;
  {$IFDEF __SQL}TTinyQuery = class;{$ENDIF}
  TTinyDatabase = class;
  TTinySession = class;
  TTinySessionList = class;
  TTinyBlobStream = class;
  TExprNodes = class;
  TExprParserBase = class;

//-------------------------------------------------------------------

{ TTinyDefCollection }

  TTinyDefCollection = class(TOwnedCollection)
  private
  protected
    procedure SetItemName(AItem: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent; AClass: TCollectionItemClass);
    function Find(const AName: string): TNamedItem;
    procedure GetItemNames(List: TStrings);
    function IndexOf(const AName: string): Integer;
  end;

{ TTinyTableDef }

  TTinyTableDef = class(TNamedItem)
  private
    FTableIdx: Integer;
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property TableIdx: Integer read FTableIdx write FTableIdx;
  published
  end;

{ TTinyTableDefs }

  TTinyTableDefs = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TTinyTableDef;
  protected
  public
    constructor Create(AOwner: TPersistent);
    function IndexOf(const Name: string): Integer;
    function Find(const Name: string): TTinyTableDef;
    property Items[Index: Integer]: TTinyTableDef read GetItem; default;
  end;

{ TTinyFieldDef }

  TTinyFieldDef = class(TNamedItem)
  private
    FFieldType: TFieldType;
    FFieldSize: Integer;
    FDPMode: TFieldDataProcessMode;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property FieldType: TFieldType read FFieldType write FFieldType;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property DPMode: TFieldDataProcessMode read FDPMode write FDPMode;
  end;

{ TTinyFieldDefs }

  TTinyFieldDefs = class(TTinyDefCollection)
  private
    function GetFieldDef(Index: Integer): TTinyFieldDef;
    procedure SetFieldDef(Index: Integer; Value: TTinyFieldDef);
  protected
    procedure SetItemName(AItem: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function AddIndexDef: TTinyFieldDef;
    function Find(const Name: string): TTinyFieldDef;
    property Items[Index: Integer]: TTinyFieldDef read GetFieldDef write SetFieldDef; default;
  end;

{ TTinyIndexDef }

  TTinyIndexDef = class(TNamedItem)
  private
    FOptions: TTDIndexOptions;
    FFieldIdxes: TIntegerAry;    // Physical Field Number

    procedure SetOptions(Value: TTDIndexOptions);
  protected
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property FieldIdxes: TIntegerAry read FFieldIdxes write FFieldIdxes;
  published
    property Options: TTDIndexOptions read FOptions write SetOptions default [];
  end;

{ TTinyIndexDefs }

  TTinyIndexDefs = class(TTinyDefCollection)
  private
    function GetIndexDef(Index: Integer): TTinyIndexDef;
    procedure SetIndexDef(Index: Integer; Value: TTinyIndexDef);
  protected
    procedure SetItemName(AItem: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent);
    function AddIndexDef: TTinyIndexDef;
    function Find(const Name: string): TTinyIndexDef;
    property Items[Index: Integer]: TTinyIndexDef read GetIndexDef write SetIndexDef; default;
  end;

{ TTinyBlobStream }

  TTinyBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TTinyTable;
    FMode: TBlobStreamMode;
    FFieldNo: Integer;
    FOpened: Boolean;
    FModified: Boolean;
    procedure LoadBlobData;
    procedure SaveBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    {$IFDEF DELPHI_17_UP}
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint;  overload; override;
    {$ENDIF}
    function Write(const Buffer; Count: Longint): Longint;  overload; override;
    {$IFDEF DELPHI_17_UP}
    procedure SetSize(NewSize: Longint); overload; override;
    procedure SetSize(const NewSize: Int64); overload; override;
    {$ENDIF}
    procedure Truncate;
  end;

{ TOptimBlobStream }

  TOptimBlobStream = class(TMemoryStream)
  private
    FDataSet: TTDEDataSet;
    FFldDataOffset: Integer;
    FShouldEncrypt: Boolean;
    FShouldCompress: Boolean;
    FDataLoaded: Boolean;

    procedure LoadBlobData;
  protected
    function Realloc(var NewCapacity: {$IFDEF DELPHI_28_UP}NativeInt{$ELSE}Longint{$ENDIF}): Pointer; override;
  public
    constructor Create(ADataSet: TTDEDataSet);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure SetSize(NewSize: Longint); override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    {$IFDEF DELPHI_16_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    {$ENDIF}
    procedure Init(FldDataOffset: Integer; ShouldEncrypt, ShouldCompress: Boolean);

    property DataLoaded: Boolean read FDataLoaded;
  end;

{ TCachedFileStream }

  TCachedFileStream = class(TStream)
  private
    FCacheStream: TMemoryStream;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; overload; override;
    {$IFDEF DELPHI_16_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; overload; override;
    {$ENDIF}
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
  end;

{ TExprNode }

  TTinyOperator = (
    toNOTDEFINED, toISBLANK, toNOTBLANK,
    toEQ, toNE, toGT, toLT, toGE, toLE,
    toNOT, toAND, toOR,
    toADD, toSUB, toMUL, toDIV, toMOD,
    toLIKE, toIN, toASSIGN
    );
  TTinyFunction = (
    tfUnknown, tfUpper, tfLower, tfSubString, tfTrim, tfTrimLeft, tfTrimRight,
    tfYear, tfMonth, tfDay, tfHour, tfMinute, tfSecond, tfGetDate
    );

  TStrCompOption = (scCaseInsensitive, scNoPartialCompare);
  TStrCompOptions = set of TStrCompOption;

  TExprNodeKind = (enField, enConst, enFunc, enOperator);
  TExprToken = (
    etEnd, etSymbol, etName, etNumLiteral, etCharLiteral, etLParen, etRParen,
    etEQ, etNE, etGE, etLE, etGT, etLT, etADD, etSUB, etMUL, etDIV,
    etComma, etAsterisk, etLIKE, etISNULL, etISNOTNULL, etIN);
  TExprTokenSet = set of TExprToken;
  TChrSet = set of AnsiChar;

  TExprNode = class
  public
    FExprNodes: TExprNodes;
    FNext: TExprNode;
    FKind: TExprNodeKind;
    FOperator: TTinyOperator;
    FFunction: TTinyFunction;
    FSymbol: string;
    FData: PAnsiChar;
    FDataSize: Integer;
    FLeft: TExprNode;
    FRight: TExprNode;
    FDataType: TFieldType;
    FArgs: TList;
    FIsBlobField: Boolean;
    FFieldIdx: Integer;
    FBlobData: AnsiString;
    FPartialLength: Integer;

    constructor Create(ExprNodes: TExprNodes);
    destructor Destroy; override;

    procedure Calculate(Options: TStrCompOptions);
    procedure EvaluateOperator(ResultNode: TExprNode; iOperator: TTinyOperator;
      LeftNode, RightNode: TExprNode; Args: TList; Options: TStrCompOptions);
    procedure EvaluateFunction(ResultNode: TExprNode; AFunction: TTinyFunction; Args: TList);

    function IsIntegerType: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function IsLargeIntType: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function IsFloatType: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function IsTemporalType: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function IsStringType: Boolean;
    function IsBooleanType: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function IsNumericType: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function IsTemporalStringType: Boolean;

    procedure SetDataSize(Size: Integer);
    function GetDataSet: TTDEDataSet;
    function AsBoolean: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ConvertStringToDateTime(DstType: TFieldType);
    class function FuncNameToEnum(const FuncName: string): TTinyFunction;

    property DataSize: Integer read FDataSize write SetDataSize;

  end;

{ TExprNodes }

  TExprNodes = class
  private
    FExprParser: TExprParserBase;
    FNodes: TExprNode;
    FRoot: TExprNode;
  public
    constructor Create(AExprParser: TExprParserBase);
    destructor Destroy; override;
    procedure Clear;
    function NewNode(NodeKind: TExprNodeKind;
                     DataType: TFieldType;
                     ADataSize: Integer;
                     iOperator: TTinyOperator;
                     Left, Right: TExprNode): TExprNode;
    function NewFuncNode(const FuncName: string): TExprNode;
    function NewFieldNode(const FieldName: string): TExprNode;

    property Root: TExprNode read FRoot write FRoot;
  end;

{ TSyntaxParserBase }

  TSyntaxParserBase = class
  protected
    FText: string;
    FTokenString: string;
    FToken: TExprToken;
    FPrevToken: TExprToken;
    FSourcePtr: PChar;
    FTokenPtr: PChar;

    procedure SetText(const Value: string);
    {$IFNDEF UNICODE}
    function IsKatakana(const Chr: Byte): Boolean;
    {$ENDIF}
    procedure Skip(var P: PChar; TheSet: TChrSet);
    function TokenName: string;
    function TokenSymbolIs(const S: string): Boolean;
    function TokenSymbolsIs(const A: array of string): Boolean;
    procedure Rewind;

    procedure GetNextToken;
    function IsNextToken(const TokenName: string): Boolean;
    function SkipBeforeGetToken(Pos: PChar): PChar; virtual;
    function InternalGetNextToken(Pos: PChar): PChar; virtual; abstract;
  public
    {$IFDEF SUPP_INLINE}
    const ccCSet = ['A'..'Z', 'a'..'z', '0'..'9', '_', '.', '[', ']'];
    {$ENDIF}
    constructor Create;
    destructor Destroy; override;

    property Text: string read FText write SetText;
    property TokenString: string read FTokenString;
    property Token: TExprToken read FToken;
  end;

{ TExprParserBase }

  TExprParserBase = class(TSyntaxParserBase)
  protected
    FExprNodes: TExprNodes;
    FStrCompOpts: TStrCompOptions;

    function ParseExpr: TExprNode; virtual; abstract;
    function GetFieldDataType(const Name: string): TFieldType; virtual; abstract;
    function GetFieldValue(const Name: string): Variant; virtual; abstract;
    function GetFuncDataType(const Name: string): TFieldType; virtual; abstract;
    function TokenSymbolIsFunc(const S: string) : Boolean; virtual;
    procedure ParseFinished; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Parse(const AText: string); virtual;
    function Calculate(Options: TStrCompOptions = []): Variant; virtual;
  end;

{ TFilterParser }

  TFilterParser = class(TExprParserBase)
  protected
    FDataSet: TTDEDataSet;

    function NextTokenIsLParen: Boolean;
    procedure TypeCheckArithOp(Node: TExprNode);
    procedure TypeCheckLogicOp(Node: TExprNode);
    procedure TypeCheckInOp(Node: TExprNode);
    procedure TypeCheckRelationOp(Node: TExprNode);
    procedure TypeCheckLikeOp(Node: TExprNode);
    procedure TypeCheckFunction(Node: TExprNode);
    function ParseExpr2: TExprNode;
    function ParseExpr3: TExprNode;
    function ParseExpr4: TExprNode;
    function ParseExpr5: TExprNode;
    function ParseExpr6: TExprNode;
    function ParseExpr7: TExprNode;

    function InternalGetNextToken(Pos: PChar): PChar; override;
    function ParseExpr: TExprNode; override;
    function GetFieldDataType(const Name: string): TFieldType; override;
    function GetFieldValue(const Name: string): Variant; override;
    function GetFuncDataType(const Name: string): TFieldType; override;
    function TokenSymbolIsFunc(const S: string): Boolean; override;
  public
    constructor Create(ADataSet: TTDEDataSet);
    property DataSet: TTDEDataSet write FDataSet;
  end;

  {$IFDEF __SQL}

{ TSQLWhereExprParser }

  TSQLWhereExprParser = class(TFilterParser)
  protected
    function GetFieldValue(const Name: string): Variant; override;
  public
    constructor Create(ADataSet: TTDEDataSet);
  end;

{ TSQLParserBase }

  TSQLParserBase = class(TSyntaxParserBase)
  protected
    FQuery: TTinyQuery;
    FRowsAffected: Integer;

    function SkipBeforeGetToken(Pos: PChar): PChar; override;
    function InternalGetNextToken(Pos: PChar): PChar; override;
  public
    constructor Create(AQuery: TTinyQuery);
    destructor Destroy; override;

    procedure Parse(const ASQL: string); virtual;
    procedure Execute; virtual; abstract;

    property RowsAffected: Integer read FRowsAffected;
  end;

{ TSQLSelectParser }

  TNameItem = record
    RealName: string;
    AliasName: string;
  end;
  TTableNameItem = TNameItem;

  TSelectFieldItem = record
    TableName: string;
    RealFldName: string;
    AliasFldName: string;
    Index: Integer;
  end;

  TOrderByType = (obAsc, obDesc);
  TOrderByFieldItem = record
    FldName: string;
    Index: Integer;
    OrderByType: TOrderByType;
  end;

  TSQLSelectParser = class(TSQLParserBase)
  private
    FTopNum: Integer;
    FFromItems: array of TTableNameItem;
    FSelectItems: array of TSelectFieldItem;
    FWhereExprParser: TSQLWhereExprParser;
    FOrderByItems: array of TOrderByFieldItem;

    // FTableTab: TTableTab;                   //   All the tables in the database
    // FTableHeaders: array of TTableHeader;   //   Table header

    function ParseFrom: PChar;
    function ParseSelect: PChar;
    // function ParseWhere(Pos: PAnsiChar): PAnsiChar;
    // function ParseOrderBy(Pos: PAnsiChar): PAnsiChar;
  public
    constructor Create(AQuery: TTinyQuery);
    destructor Destroy; override;
    procedure Parse(const ASQL: string); override;
    procedure Execute; override;
  end;

{ TSQLParser }

  TSQLType = (stNONE, stSELECT, stINSERT, stDELETE, stUPDATE);

  TSQLParser = class(TSQLParserBase)
  private
    FSQLType: TSQLType;
  public
    constructor Create(AQuery: TTinyQuery);
    destructor Destroy; override;

    procedure Parse(const ASQL: string); override;
    procedure Execute; override;
  end;

{ TRecordsMap }

  TRecordsMap = class
  private
    FList: TList;
    FByIndexIdx: Integer;

    function GetCount: Integer;
    function GetItem(Index: Integer): Integer;
    procedure SetItem(Index, Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Value: Integer);
    procedure Delete(Index: Integer);
    procedure Clear;
    procedure DoAnd(Right, Result: TRecordsMap);
    procedure DoOr(Right, Result: TRecordsMap);
    procedure DoNot(Right, Result: TRecordsMap);

    property ByIndexIdx: Integer read FByIndexIdx write FByIndexIdx;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: Integer read GetItem write SetItem;
  end;
  {$ENDIF __SQL}

{ TDataProcessAlgo }

  TDataProcessAlgoClass = class of TDataProcessAlgo;

  TDataProcessAlgo = class
  private
    FOwner: TObject;
    FOnEncodeProgress: TOnProgressEvent;
    FOnDecodeProgress: TOnProgressEvent;
  protected
    procedure DoEncodeProgress(Percent: Integer);
    procedure DoDecodeProgress(Percent: Integer);
  public
    constructor Create(AOwner: TObject); virtual;
    destructor Destroy; override;

    procedure EncodeStream(Source, Dest: TMemoryStream; DataSize: Integer); virtual; abstract;
    procedure DecodeStream(Source, Dest: TMemoryStream; DataSize: Integer); virtual; abstract;

    property OnEncodeProgress: TOnProgressEvent read FOnEncodeProgress write FOnEncodeProgress;
    property OnDecodeProgress: TOnProgressEvent read FOnDecodeProgress write FOnDecodeProgress;
  end;

{ TCompressAlgo }

  TCompressAlgoClass = class of TCompressAlgo;

  TCompressAlgo = class(TDataProcessAlgo)
  protected
    procedure SetLevel(Value: TCompressLevel); virtual;
    function GetLevel: TCompressLevel; virtual;
  public
    property Level: TCompressLevel read GetLevel write SetLevel;
  end;

{ TEncryptAlgo }

  TEncryptAlgoClass = class of TEncryptAlgo;

  TEncryptAlgo = class(TDataProcessAlgo)
  protected
    procedure DoProgress(Current, Maximal: Integer; Encode: Boolean);
    procedure InternalCodeStream(Source, Dest: TMemoryStream; DataSize: Integer; Encode: Boolean);

    procedure SetMode(Value: TEncryptMode); virtual;
    function GetMode: TEncryptMode; virtual;
  public
    procedure InitKey(const Key: string); virtual; abstract;
    procedure Done; virtual;

    procedure EncodeStream(Source, Dest: TMemoryStream; DataSize: Integer); override;
    procedure DecodeStream(Source, Dest: TMemoryStream; DataSize: Integer); override;

    procedure EncodeBuffer(const Source; var Dest; DataSize: Integer); virtual; abstract;
    procedure DecodeBuffer(const Source; var Dest; DataSize: Integer); virtual; abstract;

    property Mode: TEncryptMode read GetMode write SetMode;
  end;

{ TDataProcessMgr }

  TDataProcessMgr = class
  protected
    FTinyDBFile: TTinyDBFileIO;
    FDPObject: TDataProcessAlgo;
  public
    constructor Create(AOwner: TTinyDBFileIO);
    destructor Destroy; override;
    class function CheckAlgoRegistered(const AlgoName: string): Integer; virtual;
    procedure SetAlgoName(const Value: string); virtual; abstract;
    procedure EncodeStream(Source, Dest: TMemoryStream; DataSize: Integer); virtual;
    procedure DecodeStream(Source, Dest: TMemoryStream; DataSize: Integer); virtual;
  end;

{ TCompressMgr }

  TCompressMgr = class(TDataProcessMgr)
  private
    function GetLevel: TCompressLevel;
    procedure SetLevel(const Value: TCompressLevel);
  public
    class function CheckAlgoRegistered(const AlgoName: string): Integer; override;
    procedure SetAlgoName(const Value: string); override;
    property Level: TCompressLevel read GetLevel write SetLevel;
  end;

{ TEncryptMgr }

  TEncryptMgr = class(TDataProcessMgr)
  protected
    function GetMode: TEncryptMode;
    procedure SetMode(const Value: TEncryptMode);
  public
    procedure InitKey(const Key: string);
    procedure Done;
    class function CheckAlgoRegistered(const AlgoName: string): Integer; override;
    procedure SetAlgoName(const Value: string); override;
    procedure EncodeStream(Source, Dest: TMemoryStream; DataSize: Integer); override;
    procedure DecodeStream(Source, Dest: TMemoryStream; DataSize: Integer); override;
    procedure EncodeBuffer(const Source; var Dest; DataSize: Integer); virtual;
    procedure DecodeBuffer(const Source; var Dest; DataSize: Integer); virtual;

    property Mode: TEncryptMode read GetMode write SetMode;
  end;

{ TFieldBufferItem }

  TFieldBufferItem = class
  private
    FBuffer: Pointer;            //  Field data
    FFieldType: TFieldType;      //  Field Type
    FFieldSize: Integer;         //  Field data size (of type string: size Field.DataSize; type BLOB pm: size Stream.Size
    FMemAlloc: Boolean;          //  Whether to allocate memory
    FActive: Boolean;            //  This field is valid

    function GetAsString: string;
    function GetAsAnsiString: AnsiString;
    function GetDataBuf: Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AllocBuffer;
    procedure FreeBuffer;
    function IsBlob: Boolean;

    property FieldType: TFieldType read FFieldType write FFieldType;
    property FieldSize: Integer read FFieldSize write FFieldSize;
    property Buffer: Pointer read FBuffer;
    property DataBuf: Pointer read GetDataBuf;
    property Active: Boolean read FActive write FActive;
    property AsString: string read GetAsString;
    property AsAnsiString: AnsiString read GetAsAnsiString;
  end;

{ TFieldBuffers }

  TFieldBuffers = class
  private
    FItems: TList;

    function GetCount: Integer;
    function GetItem(Index: Integer): TFieldBufferItem;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(FieldType: TFieldType; FieldSize: Integer); overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure Add(Buffer: Pointer; FieldType: TFieldType; FieldSize: Integer); overload;
    procedure Delete(Index: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure Clear;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TFieldBufferItem read GetItem;
  end;

{ TTinyDBFileStream }

  TTinyDBFileStream = class(TFileStream)
  private
    FOffset: Integer;  // To offset FOffset regarded as the beginning of the file
    FFlushed: Boolean; // Whether the contents of the file buffer is written to the media
    procedure SetOffset(Value: Integer);
  public
    constructor Create(const FileName: string; Mode: Word; AOffset: Integer = 0); overload;
    {$IFDEF COMPILER_6_UP}
    constructor Create(const FileName: string; Mode: Word; Rights: Cardinal; AOffset: Integer = 0); overload;
    {$ENDIF}
    function Write(const Buffer; Count: Longint): Longint; override;
    {$IFDEF COMPILER_6_UP}
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    {$ELSE}
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    {$ENDIF}
    procedure Flush; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    property Offset: Integer read FOffset;
    property Flushed: Boolean read FFlushed write FFlushed;
  end;

{ TTinyDBFileIO }

  TTinyDBFileIO = class
  private
    FDatabase: TTinyDatabase;
    FDatabaseName: string;         //   The file name or memory address of the name
    FMediumType: TTinyDBMediumType;
    FExclusive: Boolean;
    FDBStream: TStream;
    FStartOffset: Integer;
    FReadOnly: Boolean;
    FFileIsReadOnly: Boolean;      //   The database file is read-only
    FCompressMgr: TCompressMgr;    //   Compression Object
    FEncryptMgr: TEncryptMgr;      //   Encryption Object
    FDBOptions: TDBOptions;        //   Database Options
    FTableTab: TTableTab;          //   All the tables in the database
    FDPCSect: TRTLCriticalSection; //   Data processing (encryption compression) critical variable

    function GetIsOpen: Boolean;
    function GetFlushed: Boolean;
    procedure InitDBOptions;
    procedure InitTableTab;
    procedure DoOperationProgressEvent(ADatabase: TTinyDatabase; Pos, Max: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

  protected
    procedure DecodeMemoryStream(SrcStream, DstStream: TMemoryStream; Encrypt, Compress: Boolean); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure DecodeMemoryBuffer(SrcBuffer, DstBuffer: PAnsiChar; DataSize: Integer; Encrypt: Boolean); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure EncodeMemoryStream(SrcStream, DstStream: TMemoryStream; Encrypt, Compress: Boolean); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure EncodeMemoryBuffer(SrcBuffer, DstBuffer: PAnsiChar; DataSize: Integer; Encrypt: Boolean); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure OnCompressProgressEvent(Sender: TObject; Percent: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure OnUncompressProgressEvent(Sender: TObject; Percent: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure OnEncryptProgressEvent(Sender: TObject; Percent: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure OnDecryptProgressEvent(Sender: TObject; Percent: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function CheckDupTableName(const TableName: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function CheckDupIndexName(var TableHeader: TTableHeader; const IndexName: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function CheckDupPrimaryIndex(var TableHeader: TTableHeader; IndexOptions: TTDIndexOptions): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure CheckValidFields(Fields: array of TFieldItem);
    procedure CheckValidIndexFields(FieldNames: array of string; IndexOptions: TTDIndexOptions; var TableHeader: TTableHeader);

    function GetFieldIdxByName(const TableHeader: TTableHeader; const FieldName: string): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function GetIndexIdxByName(const TableHeader: TTableHeader; const IndexName: string): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function GetTableIdxByName(const TableName: string): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function GetTempFileName: string; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function ReCreate(NewCompressBlob: Boolean; NewCompressLevel: TCompressLevel; const NewCompressAlgoName: string;
      NewEncrypt: Boolean; const NewEncAlgoName, OldPassword, NewPassword: string; NewCRC32: Boolean): Boolean;
  public
    constructor Create(AOwner: TTinyDatabase);
    destructor Destroy; override;

    procedure Open(const ADatabaseName: string; AMediumType: TTinyDBMediumType; AExclusive, AReadOnly: Boolean; AStartOffset: Integer = 0); 
    procedure Close; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure Flush; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SetPassword(const Value: string): Boolean; 

    procedure Lock; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure Unlock; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    {special version export-import tables to/from dataset}
    function ImportDataset(iName: string; SrcTinyTable: TDataset): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function ExportDataset(iName: string; DstTinyTable: TDataset; ReCreateStruct: Boolean = True): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure ReadBuffer(var Buffer; Position, Count: Longint); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ReadDBVersion(var Dest: string); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ReadExtDataBlock(var Dest: TExtDataBlock); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function WriteExtDataBlock(var Dest: TExtDataBlock): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ReadDBOptions(var Dest: TDBOptions); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function WriteDBOptions(var Dest: TDBOptions): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ReadTableTab(var Dest: TTableTab); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function WriteTableTab(var Dest: TTableTab): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ReadTableHeader(TableIdx: Integer; var Dest: TTableHeader); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function WriteTableHeader(TableIdx: Integer; var Dest: TTableHeader): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    class function CheckValidTinyDB(ADBStream: TStream; StartOffset: Integer = 0): Boolean; overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    class function CheckValidTinyDB(const FileName: string; StartOffset: Integer = 0): Boolean; overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    class function CheckTinyDBVersion(ADBStream: TStream; StartOffset: Integer = 0): Boolean; overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    class function CheckTinyDBVersion(const FileName: string; StartOffset: Integer = 0): Boolean; overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetTableNames(List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetFieldNames(const TableName: string; List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetIndexNames(const TableName: string; List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure ReadFieldData(DstStream: TMemoryStream; RecTabItemOffset, DiskFieldOffset, FieldSize: Integer;
      IsBlob: Boolean; ShouldEncrypt, ShouldCompress: Boolean); overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ReadFieldData(DstStream: TMemoryStream; FieldDataOffset, FieldSize: Integer;
      IsBlob: Boolean; ShouldEncrypt, ShouldCompress: Boolean); overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ReadAllRecordTabItems(const TableHeader: TTableHeader;
      var Items: TRecordTabItems; var BlockOffsets: TIntegerAry); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ReadAllIndexTabItems(const TableHeader: TTableHeader; IndexIdx: Integer;
      var Items: TIndexTabItems; var BlockOffsets: TIntegerAry); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure WriteDeleteFlag(RecTabItemOffset: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function CreateDatabase(const DBFileName: string;
      CompressBlob: Boolean; CompressLevel: TCompressLevel; const CompressAlgoName: string;
      Encrypt: Boolean; const EncryptAlgoName, Password: string; CRC32: Boolean = False): Boolean; overload; 
    function CreateTable(const TableName: string; Fields: array of TFieldItem): Boolean; 
    function DeleteTable(const TableName: string): Boolean; 
    function CreateIndex(const TableName, IndexName: string; IndexOptions: TTDIndexOptions; FieldNames: array of string): Boolean;
    function DeleteIndex(const TableName, IndexName: string): Boolean;
    function RenameTable(const OldTableName, NewTableName: string): Boolean;
    function RenameField(const TableName, OldFieldName, NewFieldName: string): Boolean;
    function RenameIndex(const TableName, OldIndexName, NewIndexName: string): Boolean;
    function Compact(const Password: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function Repair(const Password: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function ChangePassword(const OldPassword, NewPassword: string; Check: Boolean = True): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function ChangeEncrypt(NewEncrypt: Boolean; const NewEncAlgo, OldPassword, NewPassword: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SetComments(const Value: string; const Password: string): Boolean; 
    function GetComments(var Value: string; const Password: string): Boolean;
    function SetExtData(Buffer: PAnsiChar; Size: Integer): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function GetExtData(Buffer: PAnsiChar): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    property DBStream: TStream read FDBStream;
    property DBOptions: TDBOptions read FDBOptions;
    property TableTab: TTableTab read FTableTab;
    property IsOpen: Boolean read GetIsOpen;
    property FileIsReadOnly: Boolean read FFileIsReadOnly;
    property Flushed: Boolean read GetFlushed;
  end;

{ TTinyTableIO }

  TOnAdjustIndexForAppendEvent = procedure(IndexIdx, InsertPos: Integer; MemRecTabItem: TMemRecTabItem) of object;
  TOnAdjustIndexForModifyEvent = procedure(IndexIdx, FromRecIdx, ToRecIdx: Integer) of object;

  TTinyTableIO = class
  private
    FDatabase: TTinyDatabase;
    FRefCount: Integer;                 //  Reference count, initialized to 0
    FTableName: string;                 //  This table, the table name
    FTableIdx: Integer;                 //  This table, the table number (0-based)
    FFieldDefs: TTinyFieldDefs;         //  Field definition (for internal use, does not allow users to change)
    FIndexDefs: TTinyIndexDefs;         //  Index definition (for internal use, does not allow users to change)
    FDiskRecSize: Integer;              //  The length of the data recorded in the database
    FDiskFieldOffsets: TIntegerAry;     //  Database, each field in the offset in the data row
    FAutoIncFieldIdx: Integer;          //  Automatic increase in the field of physical field number (0-based), compared with -1 no
    FTableHeader: TTableHeader;         //  Table header
    FInitRecordTab: TRecordTabItems;    //  Initialization use to the RecordTab
    FRecTabBlockOffsets: TIntegerAry;   //  Block offset table for each record
    FIdxTabBlockOffsets: array of TIntegerAry;  //  All the index table offset blocks
    FRecTabLists: array of TList;       //  All records set (FRecTabLists [0] as the physical order of records set, [1] for the first record of the index set 0 ...)
    procedure SetActive(Value: Boolean);
    procedure SetTableName(const Value: string);

    function GetActive: Boolean;
    function GetRecTabList(Index: Integer): TList;
    function GetTableIdxByName(const TableName: string): Integer;

    procedure InitFieldDefs;
    procedure InitIndexDefs;
    procedure InitRecTabList(ListIdx: Integer; ReadRecTabItems: Boolean = True);
    procedure InitAllRecTabLists;
    procedure InitDiskRecInfo;
    procedure InitAutoInc;

    procedure ClearMemRecTab(AList: TList);
    procedure AddMemRecTabItem(AList: TList; Value: TMemRecTabItem);
    procedure InsertMemRecTabItem(AList: TList; Index: Integer; Value: TMemRecTabItem);
    procedure DeleteMemRecTabItem(AList: TList; Index: Integer);
    function GetMemRecTabItem(AList: TList; Index: Integer): TMemRecTabItem;

    function ShouldEncrypt(FieldIdx: Integer): Boolean;
    function ShouldCompress(FieldIdx: Integer): Boolean;

    function GetRecTabItemOffset(ItemIdx: Integer): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function GetIdxTabItemOffset(IndexIdx: Integer; ItemIdx: Integer): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure AdjustIndexesForAppend(FieldBuffers: TFieldBuffers; RecDataOffset, RecTotal: Integer; OnAdjustIndex: TOnAdjustIndexForAppendEvent); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure AdjustIndexesForModify(FieldBuffers: TFieldBuffers; EditPhyRecordIdx: Integer; OnAdjustIndex: TOnAdjustIndexForModifyEvent); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure AdjustIndexesForDelete(DeletePhyRecordIdx: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure WriteDeleteFlag(PhyRecordIdx: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure AdjustStrFldInBuffer(FieldBuffers: TFieldBuffers); 
    procedure ClearAllRecTabLists;

  protected
    procedure Initialize;
    procedure Finalize;

  public
    constructor Create(AOwner: TTinyDatabase);
    destructor Destroy; override;

    procedure Open;
    procedure Close;
    procedure Refresh;

    procedure AppendRecordData(FieldBuffers: TFieldBuffers; Flush: Boolean;
      OnAdjustIndex: TOnAdjustIndexForAppendEvent); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ModifyRecordData(FieldBuffers: TFieldBuffers;
      PhyRecordIdx: Integer; Flush: Boolean;
      OnAdjustIndex: TOnAdjustIndexForModifyEvent); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure DeleteRecordData(PhyRecordIdx: Integer; Flush: Boolean); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure DeleteAllRecords; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure ReadFieldData(DstStream: TMemoryStream; DiskRecIndex, FieldIdx: Integer);
    procedure ReadRecordData(FieldBuffers: TFieldBuffers; RecTabList: TList; RecordIdx: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function CompFieldData(FieldBuffer1, FieldBuffer2: Pointer; FieldType: TFieldType;
      CaseInsensitive, PartialCompare: Boolean): Integer; 

    function SearchIndexedField(FieldBuffers: TFieldBuffers; RecTabList: TList; IndexIdx: Integer; var ResultState: Integer; EffFieldCount: Integer = 0; PartialCompare: Boolean = False): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SearchIndexedFieldBound(FieldBuffers: TFieldBuffers; RecTabList: TList; IndexIdx: Integer; LowBound: Boolean; var ResultState: Integer; EffFieldCount: Integer = 0; PartialCompare: Boolean = False): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SearchRangeStart(FieldBuffers: TFieldBuffers; RecTabList: TList; IndexIdx: Integer; var ResultState: Integer; EffFieldCount: Integer = 0): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SearchRangeEnd(FieldBuffers: TFieldBuffers; RecTabList: TList; IndexIdx: Integer; var ResultState: Integer; EffFieldCount: Integer = 0): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SearchInsertPos(FieldBuffers: TFieldBuffers; IndexIdx: Integer; var ResultState: Integer): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function CheckPrimaryFieldExists: Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function CheckUniqueFieldForAppend(FieldBuffers: TFieldBuffers): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function CheckUniqueFieldForModify(FieldBuffers: TFieldBuffers; PhyRecordIdx: Integer): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ConvertRecordIdx(SrcIndexIdx, SrcRecordIdx, DstIndexIdx: Integer; var DstRecordIdx: Integer); overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ConvertRecordIdx(SrcRecTabList: TList; SrcRecordIdx: Integer; DstRecTabList: TList; var DstRecordIdx: Integer); overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ConvertRecIdxForPhy(SrcIndexIdx, SrcRecordIdx: Integer; var DstRecordIdx: Integer); overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ConvertRecIdxForPhy(SrcRecTabList: TList; SrcRecordIdx: Integer; var DstRecordIdx: Integer); overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ConvertRecIdxForCur(SrcIndexIdx, SrcRecordIdx: Integer; RecTabList: TList; var DstRecordIdx: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    property Active: Boolean read GetActive write SetActive;
    property TableName: string read FTableName write SetTableName;
    property TableIdx: Integer read FTableIdx;
    property FieldDefs: TTinyFieldDefs read FFieldDefs;
    property IndexDefs: TTinyIndexDefs read FIndexDefs;
    property RecTabLists[Index: Integer]: TList read GetRecTabList;
  end;

{ TTDBDataSet }

  TTDBDataSet = class(TDataSet)
  private
    FDatabaseName: string;
    FDatabase: TTinyDatabase;
    FSessionName: string;

    procedure CheckDBSessionName;
    function GetDBSession: TTinySession;
    procedure SetSessionName(const Value: string);
  protected
    procedure SetDatabaseName(const Value: string); virtual;
    procedure Disconnect; virtual;
    procedure OpenCursor(InfoQuery: Boolean); override;
    procedure CloseCursor; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CloseDatabase(Database: TTinyDatabase);
    function OpenDatabase(IncRef: Boolean): TTinyDatabase; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    property Database: TTinyDatabase read FDatabase;
    property DBSession: TTinySession read GetDBSession;
  published
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property SessionName: string read FSessionName write SetSessionName;
  end;

{ TTDEDataSet }

  TTDEDataSet = class(TTDBDataSet)
  private
    FAboutBox: TTinyAboutBox;
    FMediumType: TTinyDBMediumType; // Database storage media type
    FFilterParser: TExprParserBase; // Filter Parser
    FCurRec: Integer;               // The current record number (0-based)
    FRecordSize: Integer;           // The length of recorded data in memory
    FRecBufSize: Integer;           // FRecordSize + SizeOf(TRecInfo);
    FFieldOffsets: TIntegerAry;     // ActiveBuffer data in each field in the offset, does not include calculated fields and search fields. Array subscript meaning FieldNo-1.
    FKeyBuffers: array[TTDKeyIndex] of TRecordBuffer;  // Key data storage, such as post-SetKey or SetRangeStart
    FKeyBuffer: TRecordBuffer;      // Points to an element in the FKeyBuffers
    FFilterBuffer: TRecordBuffer;   // Filter used in Record Buffer
    FFilterMapsToIndex: Boolean;    // Filter is optimized according to Index
    FCanModify: Boolean;            // Is it permissible to modify the database

    procedure SetMediumType(Value: TTinyDBMediumType);
    procedure SetPassword(const Value: string);
    procedure SetCRC32(Value: Boolean);
    function GetCRC32: Boolean;
    function GetCanAccess: Boolean;

    procedure InitRecordSize;
    procedure InitFieldOffsets;

    function FiltersAccept: Boolean;
    procedure SetFilterData(const Text: string; Options: TFilterOptions);
    procedure AllocKeyBuffers;
    procedure FreeKeyBuffers;
    procedure InitKeyBuffer(KeyIndex: TTDKeyIndex);
  protected
    { Overriden abstract methods (required) }
    function BufferToRecBuf(Buffer: TRecordBuffer): PRecInfo;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); overload; override;
    {$IFDEF DELPHI_17_UP}
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark); overload; override;
    {$ENDIF}
    {$IFDEF DELPHI_18_UP}
    procedure GetBookmarkData(Buffer: TRecBuf; Data: TBookmark); overload; override;
    {$ENDIF}
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    function GetRecordSize: Word; override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalHandleException; override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); overload; override;
    {$IFDEF DELPHI_18_UP}
    procedure InternalSetToRecord(Buffer: TRecBuf); overload; override;
    {$ENDIF}
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); overload; override;
    //procedure SetFieldData(Field: TField; Buffer: Pointer; NativeFormat: Boolean); overload; override;
    {$IFDEF DELPHI_17_UP}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    //procedure SetFieldData(Field: TField; Buffer: TValueBuffer; NativeFormat: Boolean); overload; override;
    {$ENDIF}
    function IsCursorOpen: Boolean; override;
    procedure DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean); override;

    { Additional overrides (optional) }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetCanModify: Boolean; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure DoAfterOpen; override;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;

    { Virtual functions }
    function GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean; virtual;
    procedure ActivateFilters; virtual;
    procedure DeactivateFilters; virtual;

    procedure ReadRecordData(Buffer: TRecordBuffer; RecordIdx: Integer); virtual;

  protected
    function GetFieldOffsetByFieldNo(FieldNo: Integer): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure ReadFieldData(DstStream: TMemoryStream; FieldDataOffset, FieldSize: Integer;
      IsBlob: Boolean; ShouldEncrypt, ShouldCompress: Boolean); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

  public
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    {$IFDEF DELPHI_17_UP}
    function GetFieldData(Field: TField; {$IFDEF DELPHI_18_UP}var {$ENDIF}Buffer: TValueBuffer): Boolean; overload; override;
    {$ENDIF}
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; overload; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Password: string write SetPassword;
    property CanAccess: Boolean read GetCanAccess;
    property CRC32: Boolean read GetCRC32 write SetCRC32;
  published
    property About: TTinyAboutBox read FAboutBox write FAboutBox;
    property MediumType: TTinyDBMediumType read FMediumType write SetMediumType default mtDisk;

    property Active;
    property AutoCalcFields;
    property Filter;
    property Filtered;
    property FilterOptions;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeScroll;
    property AfterScroll;
    property BeforeRefresh;
    property AfterRefresh;
    property OnCalcFields;
    property OnFilterRecord;
  end;

{ TTinyTable }

  {$IFDEF DXE2UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTinyTable = class(TTDEDataSet)
  private
    FTableName: string;           // This table, the table name
    FIndexDefs: TIndexDefs;       // Allows users to modify IndexDefs
    FIndexName: string;           // The name of the index of the current active
    FIndexIdx: Integer;           // The current active index number, that is FIndexDefs of the subscript
    FRecTabList: TList;           // Currently using the record set (TMemRecTabItem)
    FUpdateCount: Integer;        // BeginUpdate call count
    FSetRanged: Boolean;          // Does SetRange
    FEffFieldCount: Integer;      // The composite index lookup operation, specify a valid number of fields, the default is 0, said according to the actual number of fields in composite index terms
    FReadOnly: Boolean;           // This table is ReadOnly
    FMasterLink: TMasterDataLink; // Dealing with Master-detail
    FTableIO: TTinyTableIO;       // Point TinyDatabase in a TableIO

    FOnFilterProgress: TOnProgressEvent;

    procedure SetTableName(const Value: string);
    procedure SetIndexName(const Value: string);
    procedure SetReadOnly(Value: Boolean);
    procedure SetMasterFields(const Value: string); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure SetDataSource(Value: TDataSource);

    function GetTableIdx: Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function GetMasterFields: string; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure InitIndexDefs; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure InitCurRecordTab; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure ClearMemRecTab(AList: TList); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure AddMemRecTabItem(AList: TList; Value: TMemRecTabItem); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure InsertMemRecTabItem(AList: TList; Index: Integer; Value: TMemRecTabItem); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure DeleteMemRecTabItem(AList: TList; Index: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function GetMemRecTabItem(AList: TList; Index: Integer): TMemRecTabItem; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure SwitchToIndex(IndexIdx: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure AppendRecordData(Buffer: TRecordBuffer);
    procedure ModifyRecordData(Buffer: TRecordBuffer; RecordIdx: Integer);
    procedure DeleteRecordData(RecordIdx: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure DeleteAllRecords; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure OnAdjustIndexForAppend(IndexIdx, InsertPos: Integer; MemRecTabItem: TMemRecTabItem); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure OnAdjustIndexForModify(IndexIdx, FromRecIdx, ToRecIdx: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function SearchIndexedField(RecTabList: TList; IndexIdx: Integer; var ResultState: Integer; EffFieldCount: Integer = 0; PartialCompare: Boolean = False): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SearchIndexedFieldBound(RecTabList: TList; IndexIdx: Integer; LowBound: Boolean; var ResultState: Integer; EffFieldCount: Integer = 0; PartialCompare: Boolean = False): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SearchRangeStart(RecTabList: TList; IndexIdx: Integer; var ResultState: Integer; EffFieldCount: Integer = 0): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SearchRangeEnd(RecTabList: TList; IndexIdx: Integer; var ResultState: Integer; EffFieldCount: Integer = 0): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SearchKey(RecTabList: TList; IndexIdx: Integer; EffFieldCount: Integer = 0; Nearest: Boolean = False): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function SearchInsertPos(IndexIdx: Integer; var ResultState: Integer): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure SetKeyFields(KeyIndex: TTDKeyIndex; const Values: array of const); overload; 
    procedure SetKeyFields(IndexIdx: Integer; KeyIndex: TTDKeyIndex; const Values: array of const); overload; 
    procedure SetKeyBuffer(KeyIndex: TTDKeyIndex; Clear: Boolean); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function LocateRecord(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions; SyncCursor: Boolean): Boolean;
    function MapsToIndexForSearch(Fields: TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF}; CaseInsensitive: Boolean): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function CheckFilterMapsToIndex: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure MasterChanged(Sender: TObject); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure MasterDisabled(Sender: TObject); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure SetLinkRange(MasterFields: TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF});
    procedure CheckMasterRange; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure RecordBufferToFieldBuffers(RecordBuffer: TRecordBuffer; FieldBuffers: TFieldBuffers); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function FieldDefsStored: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function IndexDefsStored: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

  protected
    { Overriden abstract methods (required) }
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    procedure InternalRefresh; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); overload; override;
    {$IFDEF DELPHI_17_UP}
    procedure InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean); overload; override;
    {$ENDIF}
    {$IFDEF DELPHI_18_UP}
    procedure InternalAddRecord(Buffer: TRecBuf; Append: Boolean); overload; override;
    {$ENDIF}
    function IsCursorOpen: Boolean; override;
    { Other overrides }
    function GetRecordCount: Integer; override;
    function GetCanModify: Boolean; override;
    function GetDataSource: TDataSource; override;
    procedure SetDatabaseName(const Value: string); override;
    procedure DoAfterOpen; override;
    procedure ActivateFilters; override;
    procedure DeactivateFilters; override;
    procedure ReadRecordData(Buffer: TRecordBuffer; RecordIdx: Integer); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Post; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;

    procedure SetKey;
    procedure EditKey;
    function GotoKey: Boolean; overload;
    function GotoKey(const IndexName: string): Boolean; overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GotoNearest; overload;
    procedure GotoNearest(const IndexName: string); overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function FindKey(const KeyValues: array of const): Boolean; overload;
    function FindKey(const IndexName: string; const KeyValues: array of const): Boolean; overload;
    procedure FindNearest(const KeyValues: array of const); overload;
    procedure FindNearest(const IndexName: string; const KeyValues: array of const); overload;

    procedure SetRangeStart;
    procedure SetRangeEnd;
    procedure EditRangeStart;
    procedure EditRangeEnd;
    procedure ApplyRange; overload;
    procedure ApplyRange(const IndexName: string); overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure SetRange(const StartValues, EndValues: array of const); overload;
    procedure SetRange(const IndexName: string; const StartValues, EndValues: array of const); overload;
    procedure CancelRange;

    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override; 
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override; 

    procedure EmptyTable;
    procedure CreateTable;

    property TableIO: TTinyTableIO read FTableIO;
    property TableIdx: Integer read GetTableIdx;
    property IndexIdx: Integer read FIndexIdx;

  published
    property TableName: string read FTableName write SetTableName;
    property IndexName: string read FIndexName write SetIndexName;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property MasterFields: string read GetMasterFields write SetMasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property IndexDefs: TIndexDefs read FIndexDefs write FIndexDefs stored IndexDefsStored;
    property FieldDefs stored FieldDefsStored;

    property OnFilterProgress: TOnProgressEvent read FOnFilterProgress write FOnFilterProgress;

    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property OnDeleteError;
    property OnEditError;
    property OnNewRecord;
    property OnPostError;
  end;

  {$IFDEF __SQL}

{ TTinyQuery }

  {$IFDEF DXE2UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTinyQuery = class(TTDEDataSet)
  private
    FSQL: TStrings;
    FSQLParser: TSQLParser;

    procedure SetQuery(Value: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function GetRowsAffected: Integer;

  protected
    procedure InternalOpen; override;
    procedure InternalClose; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ExecSQL;
    property RowsAffected: Integer read GetRowsAffected;

  published
    property SQL: TStrings read FSQL write SetQuery;

  end;
  {$ENDIF __SQL}

{ TTinyDatabase }

  {$IFDEF DXE2UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTinyDatabase = class(TComponent)
  private
    FAboutBox: TTinyAboutBox;
    FDBFileIO: TTinyDBFileIO;
    FDataSets: TList;
    FKeepConnection: Boolean;
    FTemporary: Boolean;
    FHandleShared: Boolean;
    FExclusive: Boolean;
    FReadOnly: Boolean;
    FRefCount: Integer;
    FStreamedConnected: Boolean;
    FSession: TTinySession;
    FSessionName: string;
    FDatabaseName: string;
    FFileName: string;
    FMediumType: TTinyDBMediumType; // Database storage media type
    FStartOffset: Integer;          // As the file FStartOffset offset to the beginning of the file
    FCanAccess: Boolean;            // Whether to allow access to the database
    FPassword: string;              // Database Password (original)
    FPasswordModified: Boolean;     // The password is modified by the user
    FTableDefs: TTinyTableDefs;     // Table Definition (internal non-use)
    FTableIOs: TList;
    FFlushCacheAlways: Boolean;
    FAutoFlush: Boolean;
    FAutoFlushInterval: Integer;
    FAutoFlushTimer: TTimer;

    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FOnCompressProgress: TOnProgressEvent;
    FOnUncompressProgress: TOnProgressEvent;
    FOnEncryptProgress: TOnProgressEvent;
    FOnDecryptProgress: TOnProgressEvent;
    FOnOperationProgress: TOnProgressEvent;

    function GetConnected: Boolean;
    function GetEncrypted: Boolean;
    function GetEncryptAlgoName: string;
    function GetCompressed: Boolean;
    function GetCompressLevel: TCompressLevel;
    function GetCompressAlgoName: string;
    function GetCRC32: Boolean;
    function GetTableIOs(Index: Integer): TTinyTableIO;
    function GetFileSize: Integer;
    function GetFileDate: TDateTime;
    function GetFileIsReadOnly: Boolean;
    procedure SetDatabaseName(const Value: string);
    procedure SetFileName(const Value: string);
    procedure SetMediumType(const Value: TTinyDBMediumType);
    procedure SetExclusive(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetKeepConnection(const Value: Boolean);
    procedure SetSessionName(const Value: string);
    procedure SetConnected(const Value: Boolean);
    procedure SetPassword(Value: string);
    procedure SetCRC32(Value: Boolean);
    procedure SetAutoFlush(Value: Boolean);
    procedure SetAutoFlushInterval(Value: Integer);
    procedure SetStartOffset(Value: Integer);

    function CreateLoginDialog(const ADatabaseName: string): TForm;
    function ShowLoginDialog(const ADatabaseName: string; var APassword: string): Boolean;

    function TableIOByName(const Name: string): TTinyTableIO;
    function GetDBFileName: string;
    procedure CheckSessionName(Required: Boolean);
    procedure CheckInactive;
    procedure CheckDatabaseName;
    procedure InitTableIOs;
    procedure FreeTableIOs;
    procedure AddTableIO(const TableName: string);
    procedure DeleteTableIO(const TableName: string);
    procedure RenameTableIO(const OldTableName, NewTableName: string);
    procedure RefreshAllTableIOs;
    procedure RefreshAllDataSets;
    procedure InitTableDefs;

    procedure AutoFlushTimer(Sender: TObject);
  protected
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;
    procedure CheckCanAccess; virtual;
    procedure CheckReadOnly; virtual;
    function GetDataSet(Index: Integer): TTDEDataSet; virtual;
    function GetDataSetCount: Integer; virtual;
    procedure RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil); virtual;
    procedure UnRegisterClient(Client: TObject); virtual;
    procedure SendConnectEvent(Connecting: Boolean);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    property StreamedConnected: Boolean read FStreamedConnected write FStreamedConnected;
    property HandleShared: Boolean read FHandleShared;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    procedure CloseDataSets;
    procedure FlushCache;
    //procedure ApplyUpdates(const DataSets: array of TDBDataSet);
    //procedure StartTransaction;
    //procedure Commit;
    //procedure Rollback;
    procedure ValidateName(const Name: string); //{$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetTableNames(List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetFieldNames(const TableName: string; List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetIndexNames(const TableName: string; List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function TableExists(const TableName: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    class function GetCompressAlgoNames(List: TStrings): Integer;
    class function GetEncryptAlgoNames(List: TStrings): Integer;
    class function IsTinyDBFile(const FileName: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function CreateDatabase(): TMemoryStream; overload;
    function CreateDatabase(const DBFileName: string): Boolean; overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function CreateDatabase(const DBFileName: string; const Compressed: Boolean): Boolean; overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function CreateDatabase(const DBFileName: string;
      CompressBlob: Boolean; CompressLevel: TCompressLevel; const CompressAlgoName: string;
      Encrypt: Boolean; const EncryptAlgoName, Password: string; CRC32: Boolean = False): Boolean; overload; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function CreateTable(const TableName: string; Fields: array of TFieldItem): Boolean; 
    function DeleteTable(const TableName: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function CreateIndex(const TableName, IndexName: string; IndexOptions: TTDIndexOptions; FieldNames: array of string): Boolean;
    function DeleteIndex(const TableName, IndexName: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function RenameTable(const OldTableName, NewTableName: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function RenameField(const TableName, OldFieldName, NewFieldName: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function RenameIndex(const TableName, OldIndexName, NewIndexName: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function Compact: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function Repair: Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function ChangePassword(const NewPassword: string; Check: Boolean = True): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function ChangeEncrypt(NewEncrypt: Boolean; const NewEncAlgo, NewPassword: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    function SetComments(const Value: string): Boolean;
    function GetComments(var Value: string): Boolean;
    function SetExtData(Buffer: PAnsiChar; Size: Integer): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function GetExtData(Buffer: PAnsiChar): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    property DBFileIO: TTinyDBFileIO read FDBFileIO;
    property TableIOs[Index: Integer]: TTinyTableIO read GetTableIOs;
    property DataSets[Index: Integer]: TTDEDataSet read GetDataSet;
    property DataSetCount: Integer read GetDataSetCount;
    property Session: TTinySession read FSession;
    property TableDefs: TTinyTableDefs read FTableDefs;
    property Temporary: Boolean read FTemporary write FTemporary;
    property Password: string read FPassword write SetPassword;
    property CanAccess: Boolean read FCanAccess;
    property Encrypted: Boolean read GetEncrypted;
    property EncryptAlgoName: string read GetEncryptAlgoName;
    property Compressed: Boolean read GetCompressed;
    property CompressLevel: TCompressLevel read GetCompressLevel;
    property CompressAlgoName: string read GetCompressAlgoName;
    property CRC32: Boolean read GetCRC32 write SetCRC32;
    property FlushCacheAlways: Boolean read FFlushCacheAlways write FFlushCacheAlways;
    property FileSize: Integer read GetFileSize;
    property FileDate: TDateTime read GetFileDate;
    property FileIsReadOnly: Boolean read GetFileIsReadOnly;
    property StartOffset: Integer read FStartOffset write SetStartOffset;
  published
    property About: TTinyAboutBox read FAboutBox write FAboutBox;
    property FileName: string read FFileName write SetFileName;
    property DatabaseName: string read FDatabaseName write SetDatabaseName;
    property MediumType: TTinyDBMediumType read FMediumType write SetMediumType default mtDisk;
    property Connected: Boolean read GetConnected write SetConnected default False;
    property Exclusive: Boolean read FExclusive write SetExclusive default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property KeepConnection: Boolean read FKeepConnection write SetKeepConnection default False;
    property SessionName: string read FSessionName write SetSessionName;
    property AutoFlush: Boolean read FAutoFlush write SetAutoFlush default False;
    property AutoFlushInterval: Integer read FAutoFlushInterval write SetAutoFlushInterval default tdbDefAutoFlushInterval;

    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property OnCompressProgress: TOnProgressEvent read FOnCompressProgress write FOnCompressProgress;
    property OnUncompressProgress: TOnProgressEvent read FOnUncompressProgress write FOnUnCompressProgress;
    property OnEncryptProgress: TOnProgressEvent read FOnEncryptProgress write FOnEncryptProgress;
    property OnDecryptProgress: TOnProgressEvent read FOnDecryptProgress write FOnDecryptProgress;
    property OnOperationProgress: TOnProgressEvent read FOnOperationProgress write FOnOperationProgress;
  end;

{ TTinySession }

  TTinyDatabaseEvent = (dbOpen, dbClose, dbAdd, dbRemove, dbAddAlias, dbDeleteAlias,
    dbAddDriver, dbDeleteDriver);

  TTinyDatabaseNotifyEvent = procedure(DBEvent: TTinyDatabaseEvent; const Param) of object;

  {$IFDEF DXE2UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TTinySession = class(TComponent)
  private
    FAboutBox: TTinyAboutBox;
    FActive: Boolean;
    FDatabases: TList;
    FKeepConnections: Boolean;
    FDefault: Boolean;
    FSessionName: string;
    FSessionNumber: Integer;
    FAutoSessionName: Boolean;
    FSQLHourGlass: Boolean;
    FLockCount: Integer;
    FStreamedActive: Boolean;
    FUpdatingAutoSessionName: Boolean;
    FLockRetryCount: Integer;                   // Waiting to lock resources, retry
    FLockWaitTime: Integer;                     // The time between the two retry interval (milliseconds)
    FPasswords: TStrings;
    FOnStartup: TNotifyEvent;
    FOnDBNotify: TTinyDatabaseNotifyEvent;

    procedure CheckInactive;
    function GetActive: Boolean;
    function GetDatabase(Index: Integer): TTinyDatabase;
    function GetDatabaseCount: Integer;
    procedure SetActive(Value: Boolean);
    procedure SetAutoSessionName(Value: Boolean);
    procedure SetSessionName(const Value: string);
    procedure SetSessionNames;
    procedure SetLockRetryCount(Value: Integer);
    procedure SetLockWaitTime(Value: Integer);
    function SessionNameStored: Boolean;
    procedure ValidateAutoSession(AOwner: TComponent; AllSessions: Boolean);
    function DoFindDatabase(const DatabaseName: string; AOwner: TComponent): TTinyDatabase;
    function DoOpenDatabase(const DatabaseName: string; AOwner: TComponent;
      ADataSet: TTDBDataSet; IncRef: Boolean): TTinyDatabase;
    procedure AddDatabase(Value: TTinyDatabase);
    procedure RemoveDatabase(Value: TTinyDatabase);
    procedure DBNotification(DBEvent: TTinyDatabaseEvent; const Param);
    procedure LockSession;
    procedure UnlockSession;
    procedure StartSession(Value: Boolean);
    procedure UpdateAutoSessionName;
    function GetPasswordIndex(const Password: string): Integer;

  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SetName(const NewName: TComponentName); override;
    property OnDBNotify: TTinyDatabaseNotifyEvent read FOnDBNotify write FOnDBNotify;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function OpenDatabase(const DatabaseName: string): TTinyDatabase; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure CloseDatabase(Database: TTinyDatabase); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function FindDatabase(const DatabaseName: string): TTinyDatabase; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure DropConnections;
    procedure GetDatabaseNames(List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetTableNames(const DatabaseName: string; List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetFieldNames(const DatabaseName, TableName: string; List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetIndexNames(const DatabaseName, TableName: string; List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    procedure AddPassword(const Password: string); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure RemovePassword(const Password: string); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure RemoveAllPasswords; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

    property DatabaseCount: Integer read GetDatabaseCount;
    property Databases[Index: Integer]: TTinyDatabase read GetDatabase;

  published
    property About: TTinyAboutBox read FAboutBox write FAboutBox;
    property Active: Boolean read GetActive write SetActive default False;
    property AutoSessionName: Boolean read FAutoSessionName write SetAutoSessionName default False;
    property KeepConnections: Boolean read FKeepConnections write FKeepConnections default False;
    property SessionName: string read FSessionName write SetSessionName stored SessionNameStored;
    property SQLHourGlass: Boolean read FSQLHourGlass write FSQLHourGlass default True;
    property LockRetryCount: Integer read FLockRetryCount write SetLockRetryCount default tdbDefaultLockRetryCount;
    property LockWaitTime: Integer read FLockWaitTime write SetLockWaitTime default tdbDefaultLockWaitTime;
    property OnStartup: TNotifyEvent read FOnStartup write FOnStartup;
  end;

{ TTinySessionList }

  TTinySessionList = class
    FSessions: TThreadList;
    FSessionNumbers: TBits;
    procedure AddSession(ASession: TTinySession);
    procedure CloseAll;
    function GetCount: Integer;
    function GetSession(Index: Integer): TTinySession;
    function GetSessionByName(const SessionName: string): TTinySession;
  public
    constructor Create;
    destructor Destroy; override;
    function FindSession(const SessionName: string): TTinySession; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    procedure GetSessionNames(List: TStrings); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    function OpenSession(const SessionName: string): TTinySession; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
    property Count: Integer read GetCount;
    property Sessions[Index: Integer]: TTinySession read GetSession; default;
    property List[const SessionName: string]: TTinySession read GetSessionByName;
  end;

{ TTinyAboutBox }

  TTinyAboutBox = class
  end;

// Register Algorithm Routines
procedure RegisterCompressClass(AClass: TCompressAlgoClass; AlgoName: string);
procedure RegisterEncryptClass(AClass: TEncryptAlgoClass; AlgoName: string);

// Hashing & Encryption Routines
function HashMD5(const Source: AnsiString; Digest: Pointer = nil): AnsiString;
function HashSHA(const Source: AnsiString; Digest: Pointer = nil): AnsiString;
function HashSHA1(const Source: AnsiString; Digest: Pointer = nil): AnsiString;
function CheckSumCRC32(const Data; DataSize: Integer): Longword;
procedure EncryptBuffer(Buffer: TRecordBuffer; DataSize: Integer;
  EncAlgo: string; EncMode: TEncryptMode; Password: string);
procedure DecryptBuffer(Buffer: TRecordBuffer; DataSize: Integer;
  EncAlgo: string; EncMode: TEncryptMode; Password: string);
procedure EncryptBufferBlowfish(Buffer: TRecordBuffer; DataSize: Integer; Password: string);
procedure DecryptBufferBlowfish(Buffer: TRecordBuffer; DataSize: Integer; Password: string);

// Misc Routines
function FieldItem(FieldName: string; FieldType: TFieldType; DataSize: Integer = 0; DPMode: TFieldDataProcessMode = fdDefault): TFieldItem; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
function PointerToStr(P: Pointer): string; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

var
  Session: TTinySession;
  Sessions: TTinySessionList;

implementation

{$WRITEABLECONST ON}

uses
  Compress_Zlib,
  EncryptBase, Enc_Blowfish, Enc_Twofish, Enc_Gost,
  HashBase, Hash_MD, Hash_SHA, Hash_CheckSum;

const
  TinyDBFieldTypes = [ftAutoInc, ftString, ftFixedChar,
    ftSmallint, ftInteger, ftWord, ftLargeint, ftBoolean, ftFloat, ftCurrency,
    ftDate, ftTime, ftDateTime, ftBlob, ftMemo, ftFmtMemo, ftGraphic];

  StringFieldTypes = [ftString, ftFixedChar, ftWideString, ftGuid];
  BlobFieldTypes = [ftBlob, ftMemo, ftGraphic, ftFmtMemo, ftParadoxOle, ftDBaseOle,
    ftTypedBinary, ftOraBlob, ftOraClob];

const
  // Save the compression type of registered
  FCompressClassList: TStringList = nil;
  // Registered save the encryption type
  FEncryptClassList: TStringList = nil;

  // TinyDB default Hash Algorithm Class
  FTinyDBDefaultHashClass: THashClass = THash_SHA;
  {Hash is stored on the database used for password authentication Hash Algorithm Class}
  FTinyDBCheckPwdHashClass: THashClass = THash_SHA;
  // The default encryption algorithm TinyDB
  FTinyDBDefaultEncAlgo = 'Blowfish';
  // TinyDB default encryption mode of
  FTinyDBDefaultEncMode = emCTS;

//supplements const
const
  SAutoSessionActive = 'Cannot modify SessionName while AutoSessionName is enabled';
  SAutoSessionExclusive = 'Cannot enable AutoSessionName property with more than one session on a form or data-module';
  SAutoSessionExists = 'Cannot add a session to the form or data-module while session ''%s'' has AutoSessionName enabled';
  SDatabaseClosed = 'Cannot perform this operation on a closed database';
  SDatabaseNameMissing = 'Database name missing';
  SDatabaseOpen = 'Cannot perform this operation on an open database';
  SDuplicateDatabaseName = 'Duplicate database name ''%s''';
  SDuplicateSessionName = 'Duplicate session name ''%s''';
  SInvalidSessionName = 'Invalid session name %s';
  SSessionActive = 'Cannot perform this operation on an active session';
  SSessionNameMissing = 'Session name missing';

{$IFNDEF UNICODE}
function CharInSet(C: Char; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

var
  FSessionCSect: TRTLCriticalSection;

type

{ TTinyDBLoginForm }

  TTinyDBLoginForm = class(TForm)
  public
    constructor CreateNew(AOwner: TComponent); reintroduce;
  end;

{ Misc proc }

{$WRITEABLECONST ON}
procedure ShowNagScreen(AComponent: TComponent);
const
  FirstShow: Boolean = True;
begin
{$IFDEF TDB_SHOW_NAGSCREEN}
  if not (csDesigning in AComponent.ComponentState) then
  begin
    if FirstShow then
    begin
      MessageBox(0, PAnsiChar(
        'You use UNREGISTERED version of TinyDB.' + #13 +
        'Please register at ' + tdbWebsite + #13 +
        '(Registered users will get full source code of TinyDB!)' + #13 +
        #13 +
        'Thanks!'),
        'TinyDB Engine',
        MB_OK + MB_ICONINFORMATION);
      FirstShow := False;
    end;
  end;
{$ENDIF}
end;
{$WRITEABLECONST OFF}

procedure RegisterCompressClass(AClass: TCompressAlgoClass; AlgoName: string);
var
  I: Integer;
begin
  if FCompressClassList = nil then
    FCompressClassList := TStringList.Create;

  I := FCompressClassList.IndexOfObject(Pointer(AClass));
  if I < 0 then FCompressClassList.AddObject(AlgoName, Pointer(AClass))
  else FCompressClassList[I] := AlgoName;
end;

procedure RegisterEncryptClass(AClass: TEncryptAlgoClass; AlgoName: string);
var
  I: Integer;
begin
  if FEncryptClassList = nil then
    FEncryptClassList := TStringList.Create;

  I := FEncryptClassList.IndexOfObject(Pointer(AClass));
  if I < 0 then FEncryptClassList.AddObject(AlgoName, Pointer(AClass))
  else FEncryptClassList[I] := AlgoName;
end;

function DefaultSession: TTinySession; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  Result := TinyDB.Session;
end;

function FieldItem(FieldName: string; FieldType: TFieldType; DataSize: Integer = 0; DPMode: TFieldDataProcessMode = fdDefault): TFieldItem;
begin
  Result.FieldName := AnsiString(FieldName);
  Result.FieldType := FieldType;
  Result.DataSize := DataSize;
  Result.DPMode := DPMode;
end;

function GetFieldSize(FieldType: TFieldType; StringLength: Integer = 0): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  if not (FieldType in TinyDBFieldTypes) then
    DatabaseError(SGeneralError);

  Result := 0;
  if FieldType in StringFieldTypes then
    Result := StringLength + 1
  else if FieldType in BlobFieldTypes then
    Result := 0
  else
  begin
    case FieldType of
      ftBoolean: Result := SizeOf(WordBool);
      ftDateTime,
      ftCurrency,
      ftFloat: Result := SizeOf(Double);
      ftTime,
      ftDate,
      ftAutoInc,
      ftInteger: Result := SizeOf(Integer);
      ftSmallint: Result := SizeOf(SmallInt);
      ftWord: Result := SizeOf(Word);
      ftLargeint: Result := SizeOf(Largeint);
    else
      DatabaseError(SGeneralError);
    end;
  end;
end;

function PointerToStr(P: Pointer): string;
var
  V: Longword;
begin
  V := Longword(P);
  Result := ':' + IntToHex(V, 8);
end;

function StrToPointer(S: string): Pointer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  P: Longword;
  I, J: Integer;
  C: Char;
begin
  Result := nil;
  if S[1] <> ':' then Exit;
  Delete(S, 1, 1);
  S := UpperCase(S);
  P := 0;
  for I := 1 to Length(S) do
  begin
    C := S[I];
    if (C >= '0') and (C <= '9') then J := Ord(C) - Ord('0')
    else J := Ord(C) - Ord('A') + 10;
    P := (P shl 4) + Longword(J);
  end;
  Result := Pointer(P);
end;

function IsPointerStr(S: AnsiString): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}

  function IsHexChar(C: AnsiChar): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
  begin
    Result := (C >= '0') and (C <= '9') or
              (C >= 'A') and (C <= 'F') or
              (C >= 'a') and (C <= 'f');
  end;

  function IsHexStr(S: AnsiString): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
  var
    I: Integer;
  begin
    for I := 1 to Length(S) do
    begin
      if not IsHexChar(S[I]) then
      begin
        Result := False;
        Exit;
      end;
    end;
    Result := True;
  end;

begin
  Result := False;
  if S[1] <> ':' then Exit;
  Delete(S, 1, 1);
  if not IsHexStr(S) then Exit;
  Result := True;
end;

//-----------------------------------------------------------------------------
{Calculation LIKE expression to support wildcard '%' and '_'}
// Value:   {Master Series}
// Pattern:   {Substring}
// CaseSensitive: {Case Sensitive}
// Example: LikeString('abcdefg', 'abc%', True);
//-----------------------------------------------------------------------------
{
function LikeString(Value, Pattern: WideString; CaseSensitive: Boolean): Boolean;
const
  MultiWildChar = '%';
  SingleWildChar = '_';

  function MatchPattern(ValueStart, PatternStart: Integer): Boolean;
  begin
    if (Pattern[PatternStart] = MultiWildChar) and (Pattern[PatternStart + 1] = #0) then
      Result := True
    else if (Value[ValueStart] = #0) and (Pattern[PatternStart] <> #0) then
      Result := False
    else if (Value[ValueStart] = #0) then
      Result := True
    else
    begin
      case Pattern[PatternStart] of
        MultiWildChar:
          begin
            if MatchPattern(ValueStart, PatternStart + 1) then
              Result := True
            else
              Result := MatchPattern(ValueStart + 1, PatternStart);
          end;
        SingleWildChar:
          Result := MatchPattern(ValueStart + 1, PatternStart + 1);
        else
          begin
            if CaseSensitive and (Value[ValueStart] = Pattern[PatternStart]) or
              not CaseSensitive and (UpperCase(Value[ValueStart]) = UpperCase(Pattern[PatternStart])) then
              Result := MatchPattern(ValueStart + 1, PatternStart + 1)
            else
              Result := False;
          end;
        end;
    end;
  end;

begin
  if Value = '' then Value := #0;
  if Pattern = '' then Pattern := #0;
  Result := MatchPattern(1, 1);
end;
}
function LikeString(Value, Pattern: WideString; CaseSensitive: Boolean): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
const
  MultiWildChar = '%';
  SingleWildChar = '_';
var
  ValuePtr, PatternPtr: PWideChar;
  I: Integer;
  B: Boolean;
begin
  ValuePtr := PWideChar(Value);
  PatternPtr := PWideChar(Pattern);

  while True do
  begin
    if (CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT,
      PatternPtr, Length(PatternPtr), WideChar(MultiWildChar), 1) - 2 = 0)
    then
    begin
      Result := True;
      Exit;
    end
    else
    if (ValuePtr^ = #0) and (PatternPtr^ <> #0) then
    begin
      Result := False;
      Exit;
    end
    else
    if (ValuePtr^ = #0) then
    begin
      Result := True;
      Exit;
    end
    else
    begin
      case PatternPtr^ of
        MultiWildChar:
          begin
            for I := 0 to Length(ValuePtr) - 1 do
            begin
              if LikeString(ValuePtr + I, PatternPtr + 1, CaseSensitive) then
              begin
                Result := True;
                Exit;
              end;
            end;
            Result := False;
            Exit;
          end;
        SingleWildChar:
          begin
            Inc(ValuePtr);
            Inc(PatternPtr);
          end;
      else
        begin
          B := False;
          if CaseSensitive then
          begin
            if (CompareStringW(LOCALE_USER_DEFAULT, SORT_STRINGSORT,
              PatternPtr, 1, ValuePtr, 1) - 2 = 0)
            then
              B := True;
          end
          else
          begin
            if (CompareStringW(LOCALE_USER_DEFAULT, NORM_IGNORECASE + SORT_STRINGSORT,
              PatternPtr, 1, ValuePtr, 1) - 2 = 0)
            then
              B := True;
          end;

          if B then
          begin
            Inc(ValuePtr);
            Inc(PatternPtr);
          end
          else
          begin
            Result := False;
            Exit;
          end;
        end;
      end; // case
    end;
  end;
end;

function TinyDBCompareString(const S1, S2: AnsiString; PartialCompare: Boolean;
  PartialLength: Integer; CaseInsensitive: Boolean): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  _S1, _S2: string;
begin
  if not PartialCompare then
    PartialLength := -1;

  _S1 := string(S1);
  _S2 := string(S2);

  if CaseInsensitive then
  begin
    Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE, PChar(_S1),
      PartialLength, PChar(_S2), PartialLength) - 2;
  end
  else
  begin
    Result := CompareString(LOCALE_USER_DEFAULT, 0, PChar(_S1),
      PartialLength, PChar(_S2), PartialLength) - 2;
  end;
end;

function IsValidDBName(S: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  I: Integer;
begin
  Result := False;
  if S = '' then Exit;
  for I := 1 to Length(S) do
    if S[I] < ' ' then Exit;
  Result := True;
end;

function IsInt(const S: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  E, R: Integer;
begin
  Val(S, R, E);
  Result := E = 0;
  E := R; // avoid hints
end;

procedure RandomFillBuffer(Buffer: PAnsiChar; Size: Integer; FromVal, ToVal: Byte); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  I: Integer;
begin
  Randomize;
  for I := 0 to Size - 1 do
  begin
    Buffer^ := AnsiChar(Chr(Random(ToVal-FromVal+1)+FromVal));
    Inc(Buffer);
  end;
end;

procedure ScanBlanks(const S: string; var Pos: Integer); {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  I: Integer;
begin
  I := Pos;
  while (I <= Length(S)) and (S[I] = ' ') do Inc(I);
  Pos := I;
end;

function ScanNumber(const S: string; var Pos: Integer;
  var Number: Word; var CharCount: Byte): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  I: Integer;
  N: Word;
begin
  Result := False;
  CharCount := 0;
  ScanBlanks(S, Pos);
  I := Pos;
  N := 0;
  while (I <= Length(S)) and CharInSet(S[I], ['0'..'9']) and (N < 1000) do
  begin
    N := N * 10 + (Ord(S[I]) - Ord('0'));
    Inc(I);
  end;
  if I > Pos then
  begin
    CharCount := I - Pos;
    Pos := I;
    Number := N;
    Result := True;
  end;
end;

function ScanString(const S: string; var Pos: Integer;
  const Symbol: string): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  Result := False;
  if Symbol <> '' then
  begin
    ScanBlanks(S, Pos);
    if SysUtils.AnsiCompareText(Symbol, Copy(S, Pos, Length(Symbol))) = 0 then
    begin
      Inc(Pos, Length(Symbol));
      Result := True;
    end;
  end;
end;

function ScanChar(const S: string; var Pos: Integer; Ch: Char): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  Result := False;
  ScanBlanks(S, Pos);
  if (Pos <= Length(S)) and (S[Pos] = Ch) then
  begin
    Inc(Pos);
    Result := True;
  end;
end;

function ScanDate(const S: string; var Pos: Integer; var Date: TDateTime): Boolean;

  function CurrentYear: Word; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
  var
    SystemTime: TSystemTime;
  begin
    GetLocalTime(SystemTime);
    Result := SystemTime.wYear;
  end;

var
  N1, N2, N3, Y, M, D: Word;
  L1, L2, L3, YearLen: Byte;
  CenturyBase: Integer;
  SDateSeparator: Char;
begin
  Result := False;
  SDateSeparator := {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}DateSeparator;
  if not (ScanNumber(S, Pos, N1, L1) and ScanChar(S, Pos, SDateSeparator) and
    ScanNumber(S, Pos, N2, L2) and ScanChar(S, Pos, SDateSeparator) and
    ScanNumber(S, Pos, N3, L3))
  then
    Exit;

  M := N1;
  D := N2;
  Y := N3;
  YearLen := L3;

  if YearLen <= 2 then
  begin
    CenturyBase := CurrentYear - {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow;
    Inc(Y, CenturyBase div 100 * 100);
    if ({$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}TwoDigitYearCenturyWindow > 0) and (Y < CenturyBase) then
      Inc(Y, 100);
  end;

  Result := True;
  try
    Date := EncodeDate(Y, M, D);
  except
    Result := False;
  end;
end;

function ScanTime(const S: string; var Pos: Integer; var Time: TDateTime): Boolean;
var
  BaseHour: Integer;
  Hour, Min, Sec, MSec: Word;
  Junk: Byte;
  STimeSeparator: Char;
begin
  Result := False;
  STimeSeparator := {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}TimeSeparator;
  BaseHour := -1;
  if ScanString(S, Pos, {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}TimeAMString) or ScanString(S, Pos, 'AM') then
    BaseHour := 0
  else if ScanString(S, Pos, {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}TimePMString) or ScanString(S, Pos, 'PM') then
    BaseHour := 12;
  if BaseHour >= 0 then ScanBlanks(S, Pos);
  if not ScanNumber(S, Pos, Hour, Junk) then Exit;
  Min := 0;
  if ScanChar(S, Pos, STimeSeparator) then
    if not ScanNumber(S, Pos, Min, Junk) then Exit;
  Sec := 0;
  if ScanChar(S, Pos, STimeSeparator) then
    if not ScanNumber(S, Pos, Sec, Junk) then Exit;
  MSec := 0;
  if ScanChar(S, Pos, {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}DecimalSeparator) then
    if not ScanNumber(S, Pos, MSec, Junk) then Exit;
  if BaseHour < 0 then
    if ScanString(S, Pos, {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}TimeAMString) or ScanString(S, Pos, 'AM') then
      BaseHour := 0
    else
      if ScanString(S, Pos, {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}TimePMString) or ScanString(S, Pos, 'PM') then
        BaseHour := 12;
  if BaseHour >= 0 then
  begin
    if (Hour = 0) or (Hour > 12) then Exit;
    if Hour = 12 then Hour := 0;
    Inc(Hour, BaseHour);
  end;
  ScanBlanks(S, Pos);

  Result := True;
  try
    Time := EncodeTime(Hour, Min, Sec, MSec);
  except
    Result := False;
  end;
end;

function DbStrToDate(const S: string; var Date: TDateTime): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  Pos: Integer;
begin
  Result := True;
  Pos := 1;
  if not ScanDate(S, Pos, Date) or (Pos <= Length(S)) then
    Result := False;
end;

function DbStrToTime(const S: string; var Date: TDateTime): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  Pos: Integer;
begin
  Result := True;
  Pos := 1;
  if not ScanTime(S, Pos, Date) or (Pos <= Length(S)) then
    Result := False;
end;

function DbStrToDateTime(const S: string; var DateTime: TDateTime): Boolean; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
var
  Pos: Integer;
  Date, Time: TDateTime;
begin
  Result := True;
  Pos := 1;
  Time := 0;
  if not ScanDate(S, Pos, Date) or not ((Pos > Length(S)) or
    ScanTime(S, Pos, Time))
  then
  begin   //  Try time only
    Pos := 1;
    if not ScanTime(S, Pos, DateTime) or (Pos <= Length(S)) then
      Result := False;
  end
  else
    if Date >= 0 then
      DateTime := Date + Time
    else
      DateTime := Date - Time;
end;

function CompareDbDateTime(const MSecsA, MSecsB: Double): Integer; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  if Abs(MSecsA - MSecsB) < 1000 then
    Result := 0
  else if MSecsA < MSecsB then
    Result := -1
  else
    Result := 1;
end;

function Hash(HashClass: THashClass; const Source: AnsiString; Digest: Pointer = nil): AnsiString; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  Result := HashClass.CalcString(Digest, Source);
end;

function HashMD5(const Source: AnsiString; Digest: Pointer = nil): AnsiString; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  Result := Hash(THash_MD5, Source, Digest);
end;

function HashSHA(const Source: AnsiString; Digest: Pointer = nil): AnsiString; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  Result := Hash(THash_SHA, Source, Digest);
end;

function HashSHA1(const Source: AnsiString; Digest: Pointer = nil): AnsiString; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  Result := Hash(THash_SHA1, Source, Digest);
end;

function CheckSumCRC32(const Data; DataSize: Integer): Longword; {$IFDEF SUPP_INLINE} inline; {$ENDIF}
begin
  THash_CRC32.CalcBuffer(@Result, Data, DataSize);
end;

procedure EncryptBuffer(Buffer: TRecordBuffer; DataSize: Integer;
  EncAlgo: string; EncMode: TEncryptMode; Password: string);
var
  EncObj: TEncryptMgr;
begin
  EncObj := TEncryptMgr.Create(nil);
  try
    EncObj.SetAlgoName(EncAlgo);
    EncObj.Mode := EncMode;
    EncObj.InitKey(Password);
    EncObj.EncodeBuffer(Buffer^, Buffer^, DataSize);
  finally
    EncObj.Free;
  end;
end;

procedure DecryptBuffer(Buffer: TRecordBuffer; DataSize: Integer;
  EncAlgo: string; EncMode: TEncryptMode; Password: string);
var
  EncObj: TEncryptMgr;
begin
  EncObj := TEncryptMgr.Create(nil);
  try
    EncObj.SetAlgoName(EncAlgo);
    EncObj.Mode := EncMode;
    EncObj.InitKey(Password);
    EncObj.DecodeBuffer(Buffer^, Buffer^, DataSize);
  finally
    EncObj.Free;
  end;
end;

procedure EncryptBufferBlowfish(Buffer: TRecordBuffer; DataSize: Integer; Password: string);
begin
  EncryptBuffer(Buffer, DataSize, 'Blowfish', FTinyDBDefaultEncMode, Password);
end;

procedure DecryptBufferBlowfish(Buffer: TRecordBuffer; DataSize: Integer; Password: string);
begin
  DecryptBuffer(Buffer, DataSize, 'Blowfish', FTinyDBDefaultEncMode, Password);
end;

{ TTinyDefCollection }

constructor TTinyDefCollection.Create(AOwner: TPersistent; AClass: TCollectionItemClass);
begin
  inherited Create(AOwner, AClass);
end;

procedure TTinyDefCollection.SetItemName(AItem: TCollectionItem);
begin
  with TNamedItem(AItem) do
    if (Name = '') then
      Name := Copy(ClassName, 2, 5) + IntToStr(ID+1);
end;

function TTinyDefCollection.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if SysUtils.AnsiCompareText(TNamedItem(Items[Result]).Name, AName) = 0 then Exit;
  Result := -1;
end;

function TTinyDefCollection.Find(const AName: string): TNamedItem;
var
  I: Integer;
begin
  I := IndexOf(AName);
  if I < 0 then Result := nil else Result := TNamedItem(Items[I]);
end;

procedure TTinyDefCollection.GetItemNames(List: TStrings);
var
  I: Integer;
begin
  List.BeginUpdate;
  try
    List.Clear;
    for I := 0 to Count - 1 do
      with TNamedItem(Items[I]) do
        if Name <> '' then List.Add(Name);
  finally
    List.EndUpdate;
  end;
end;

{ TTinyTableDef }

constructor TTinyTableDef.Create(Collection: TCollection);
begin
  inherited Create(Collection);
end;

destructor TTinyTableDef.Destroy;
begin
  inherited Destroy;
end;

{ TTinyTableDefs }

constructor TTinyTableDefs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTinyTableDef);
end;

function TTinyTableDefs.GetItem(Index: Integer): TTinyTableDef;
begin
  Result := TTinyTableDef(inherited Items[Index]);
end;

function TTinyTableDefs.IndexOf(const Name: string): Integer;
var
  Item: TTinyTableDef;
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Item := GetItem(I);
    if SysUtils.AnsiCompareText(Item.Name, Name) = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := -1;
end;

function TTinyTableDefs.Find(const Name: string): TTinyTableDef;
var
  I: Integer;
begin
  I := IndexOf(Name);
  if I = -1 then
    DatabaseErrorFmt(STableNotFound, [Name]);
  Result := Items[I];
end;

{ TTinyFieldDef }

constructor TTinyFieldDef.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFieldType := ftUnknown;
  FFieldSize := 0;
  FDPMode := fdDefault;
end;

destructor TTinyFieldDef.Destroy;
begin
  inherited Destroy;
end;

procedure TTinyFieldDef.Assign(Source: TPersistent);
var
  S: TTinyFieldDef;
begin
  if Source is TTinyFieldDef then
  begin
    if Collection <> nil then Collection.BeginUpdate;
    try
      S := TTinyFieldDef(Source);
      Name := S.Name;
      FFieldType := S.FFieldType;
      FFieldSize := S.FFieldSize;
      FDPMode := S.FDPMode;
    finally
      if Collection <> nil then Collection.EndUpdate;
    end;
  end
  else
    inherited;
end;

{ TTinyFieldDefs }

constructor TTinyFieldDefs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTinyFieldDef);
end;

function TTinyFieldDefs.AddIndexDef: TTinyFieldDef;
begin
  Result := TTinyFieldDef(inherited Add);
end;

function TTinyFieldDefs.Find(const Name: string): TTinyFieldDef;
begin
  Result := TTinyFieldDef(inherited Find(Name));
  if Result = nil then DatabaseErrorFmt(SFieldNotFound, [Name]);
end;

function TTinyFieldDefs.GetFieldDef(Index: Integer): TTinyFieldDef;
begin
  Result := TTinyFieldDef(inherited Items[Index]);
end;

procedure TTinyFieldDefs.SetFieldDef(Index: Integer; Value: TTinyFieldDef);
begin
  inherited Items[Index] := Value;
end;

procedure TTinyFieldDefs.SetItemName(AItem: TCollectionItem);
begin
  with TNamedItem(AItem) do
    if Name = '' then
      Name := Copy(ClassName, 2, 5) + IntToStr(ID+1);
end;

{ TTinyIndexDef }

constructor TTinyIndexDef.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FOptions := [];
end;

destructor TTinyIndexDef.Destroy;
begin
  inherited Destroy;
end;

procedure TTinyIndexDef.Assign(Source: TPersistent);
var
  S: TTinyIndexDef;
begin
  if Source is TTinyIndexDef then
  begin
    if Collection <> nil then Collection.BeginUpdate;
    try
      S := TTinyIndexDef(Source);
      Name := S.Name;
      Options := S.Options;
      FieldIdxes := S.FieldIdxes;
    finally
      if Collection <> nil then Collection.EndUpdate;
    end;
  end
  else
    inherited;
end;

procedure TTinyIndexDef.SetOptions(Value: TTDIndexOptions);
begin
  FOptions := Value;
end;

{ TTinyIndexDefs }

function TTinyIndexDefs.GetIndexDef(Index: Integer): TTinyIndexDef;
begin
  Result := TTinyIndexDef(inherited Items[Index]);
end;

procedure TTinyIndexDefs.SetIndexDef(Index: Integer; Value: TTinyIndexDef);
begin
  inherited Items[Index] := Value;
end;

procedure TTinyIndexDefs.SetItemName(AItem: TCollectionItem);
begin
  with TNamedItem(AItem) do
    if Name = '' then
      Name := Copy(ClassName, 2, 5) + IntToStr(ID+1);
end;

constructor TTinyIndexDefs.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTinyIndexDef);
end;

function TTinyIndexDefs.AddIndexDef: TTinyIndexDef;
begin
  Result := TTinyIndexDef(inherited Add);
end;

function TTinyIndexDefs.Find(const Name: string): TTinyIndexDef;
begin
  Result := TTinyIndexDef(inherited Find(Name));
  if Result = nil then DatabaseErrorFmt(SIndexNotFound, [Name]);
end;

{ TTinyBlobStream }

constructor TTinyBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  FMode := Mode;
  FField := Field;
  FDataSet := FField.DataSet as TTinyTable;
  FFieldNo := FField.FieldNo;

  if Mode in [bmRead, bmReadWrite] then
  begin
    LoadBlobData;
  end;

  FOpened := True;
  if Mode = bmWrite then Truncate;
end;

destructor TTinyBlobStream.Destroy;
begin
  inherited Destroy;
end;

function TTinyBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  if FMode in [bmWrite, bmReadWrite] then
  begin
    FModified := True;
    SaveBlobData;
    FField.Modified := True;
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  end;
end;

{$IFDEF DELPHI_17_UP}
function TTinyBlobStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Offset, Count);
  if FMode in [bmWrite, bmReadWrite] then
  begin
    FModified := True;
    SaveBlobData;
    FField.Modified := True;
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  end;
end;
{$ENDIF}

procedure TTinyBlobStream.LoadBlobData;
var
  RecBuf: TRecordBuffer;
  BlobStream: TMemoryStream;
begin
  if FDataSet.GetActiveRecBuf(RecBuf) then
  begin
    Inc(RecBuf, FDataSet.GetFieldOffsetByFieldNo(FFieldNo));
    BlobStream := PMemoryStream(RecBuf)^;
    if Assigned(BlobStream) then
    begin
      Clear;
      CopyFrom(BlobStream, 0);
      Position := 0;
    end;
  end;
end;

procedure TTinyBlobStream.SaveBlobData;
var
  RecBuf: TRecordBuffer;
  BlobStream: TMemoryStream;
  SavePos: Integer;
begin
  if FDataSet.GetActiveRecBuf(RecBuf) then
  begin
    SavePos := Position;
    try
      Inc(RecBuf, FDataSet.GetFieldOffsetByFieldNo(FFieldNo));
      BlobStream := PMemoryStream(RecBuf)^;
      if Assigned(BlobStream) then
      begin
        BlobStream.Clear;
        BlobStream.CopyFrom(Self, 0);
      end;
    finally
      Position := SavePos;
    end;
  end;
end;

{$IFDEF DELPHI_17_UP}
procedure TTinyBlobStream.SetSize(NewSize: Integer);
begin
  inherited SetSize(NewSize);
end;

procedure TTinyBlobStream.SetSize(const NewSize: Int64);
begin
  inherited SetSize(NewSize);
end;
{$ENDIF}

procedure TTinyBlobStream.Truncate;
begin
  if FOpened then
  begin
    Clear;
    SaveBlobData;
    FField.Modified := True;
    FModified := True;
  end;
end;

{ TOptimBlobStream }

constructor TOptimBlobStream.Create(ADataSet: TTDEDataSet);
begin
  FDataSet := ADataSet;
  FFldDataOffset := 0;
  FDataLoaded := False;
end;

destructor TOptimBlobStream.Destroy;
begin
  FDataSet := nil;
  inherited;
end;

procedure TOptimBlobStream.Init(FldDataOffset: Integer; ShouldEncrypt, ShouldCompress: Boolean);
begin
  FFldDataOffset := FldDataOffset;
  FShouldEncrypt := ShouldEncrypt;
  FShouldCompress := ShouldCompress;
  FDataLoaded := False;
end;

procedure TOptimBlobStream.LoadBlobData;
begin
  if FFldDataOffset = 0 then Exit;
  if FDataLoaded then Exit;
  FDataLoaded := True;
  {Read BLOB data from the database into memory}
  FDataSet.ReadFieldData(Self, FFldDataOffset, SizeOf(TBlobFieldHeader),
    True, FShouldEncrypt, FShouldCompress);
end;

function TOptimBlobStream.Realloc(var NewCapacity: {$IFDEF DELPHI_28_UP}NativeInt{$ELSE}Longint{$ENDIF}): Pointer;
begin
  FDataLoaded := True;
  Result := inherited Realloc(NewCapacity);
end;

function TOptimBlobStream.Read(var Buffer; Count: Integer): Longint;
begin
  LoadBlobData;
  Result := inherited Read(Buffer, Count);
end;

function TOptimBlobStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  LoadBlobData;
  Result := inherited Seek(Offset, Origin);
end;

{$IFDEF DELPHI_16_UP}
function TOptimBlobStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  LoadBlobData;
  Result := inherited Seek(Offset, Origin);
end;
{$ENDIF}

procedure TOptimBlobStream.SetSize(NewSize: Integer);
begin
  FDataLoaded := True;
  inherited;
end;

function TOptimBlobStream.Write(const Buffer; Count: Integer): Longint;
begin
  FDataLoaded := True;
  Result := inherited Write(Buffer, Count);
end;

{ TCachedFileStream }

constructor TCachedFileStream.Create(const FileName: string; Mode: Word);
begin
  // inherited;
  FCacheStream := TMemoryStream.Create;
  FCacheStream.LoadFromFile(FileName);
end;

destructor TCachedFileStream.Destroy;
begin
  FreeAndNil(FCacheStream);
  // inherited;
end;

function TCachedFileStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FCacheStream.Read(Buffer, Count);
end;

function TCachedFileStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FCacheStream.Write(Buffer, Count);
end;

function TCachedFileStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  Result := FCacheStream.Seek(Offset, Origin);
end;

{$IFDEF DELPHI_16_UP}
function TCachedFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FCacheStream.Seek(Offset, Origin);
end;
{$ENDIF}

{ TExprNode }

constructor TExprNode.Create(ExprNodes: TExprNodes);
begin
  FExprNodes := ExprNodes;
  FArgs := nil;
  FPartialLength := -1;
end;

destructor TExprNode.Destroy;
begin
  if FArgs <> nil then FreeAndNil(FArgs);
  SetDataSize(0);
  inherited;
end;

procedure TExprNode.Calculate(Options: TStrCompOptions);
var
  I: Integer;
  RecBuf: TRecordBuffer;
begin
  case FKind of
    enField:
      begin
        if FIsBlobField then
        begin
          with GetDataSet do
          begin
            FBlobData := Fields[FFieldIdx].{$IFDEF UNICODE}AsAnsiString{$ELSE}AsString{$ENDIF};
            FData := @FBlobData[1];
          end;
        end 
        else
        begin
          if FData = nil then
            with GetDataSet do
            begin
              GetActiveRecBuf(RecBuf);
              FData := {$IFDEF UNICODE}PAnsiChar(RecBuf){$ELSE}RecBuf{$ENDIF} + GetFieldOffsetByFieldNo(Fields[FFieldIdx].FieldNo);
            end;
        end;
      end;
    enFunc:
      begin
        for I := 0 to FArgs.Count - 1 do
          TExprNode(FArgs[I]).Calculate(Options);
        EvaluateFunction(Self, FFunction, FArgs);
      end;
    enOperator:
      begin
        case FOperator of
          toOR:
            begin
              FLeft.Calculate(Options);
              if not PBoolean(FLeft.FData)^ then FRight.Calculate(Options);
              EvaluateOperator(Self, FOperator, FLeft, FRight, nil, Options);
            end;
          toAND:
            begin
              FLeft.Calculate(Options);
              if PBoolean(FLeft.FData)^ then FRight.Calculate(Options);
              EvaluateOperator(Self, FOperator, FLeft, FRight, nil, Options);
            end;
          toNOT:
            begin
              FLeft.Calculate(Options);
              EvaluateOperator(Self, FOperator, FLeft, FRight, nil, Options);
            end;
          toEQ, toNE, toGT, toLT, toGE, toLE,
          toADD, toSUB, toMUL, toDIV,
          toLIKE:
            begin
              FLeft.Calculate(Options);
              FRight.Calculate(Options);
              EvaluateOperator(Self, FOperator, FLeft, FRight, nil, Options);
            end;
          toIN:
            begin
              FLeft.Calculate(Options);
              for I := 0 to FArgs.Count - 1 do
                TExprNode(FArgs[I]).Calculate(Options);
              EvaluateOperator(Self, FOperator, FLeft, nil, FArgs, Options);
            end;
        end;
      end;
  end;
end;

procedure TExprNode.EvaluateOperator(ResultNode: TExprNode; iOperator: TTinyOperator;
  LeftNode, RightNode: TExprNode; Args: TList; Options: TStrCompOptions);
var
  TempTimeStamp: TTimeStamp;
  W1, W2: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
  I: Integer;
begin
  case iOperator of
    toOR:
      begin
        if PBoolean(LeftNode.FData)^ then
          PBoolean(ResultNode.FData)^ := True
        else
          PBoolean(ResultNode.FData)^ := PBoolean(RightNode.FData)^;
      end;
    toAND:
      begin
        if PBoolean(LeftNode.FData)^ then
          PBoolean(ResultNode.FData)^ := PBoolean(RightNode.FData)^
        else
          PBoolean(ResultNode.FData)^ := False;
      end;
    toNOT:
      begin
        PBoolean(ResultNode.FData)^ := not PBoolean(LeftNode.FData)^;
      end;
    toEQ:   //-----------------------------------------------------------------
      begin
        case LeftNode.FDataType of
          ftBoolean:
              PBoolean(ResultNode.FData)^ := PBoolean(LeftNode.FData)^ = PBoolean(RightNode.FData)^;
          ftSmallInt:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ = PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ = PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := Integer(PSmallInt(LeftNode.FData)^) = Integer(PWord(RightNode.FData)^);
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ = PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ = PDouble(RightNode.FData)^;
            end;
          ftInteger, ftAutoInc:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ = PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ = PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ = PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ = PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ = PDouble(RightNode.FData)^;
            end;
          ftWord:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := Integer(PWord(LeftNode.FData)^) = Integer(PSmallInt(RightNode.FData)^);
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ = PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ = PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ = PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ = PDouble(RightNode.FData)^;
            end;
          ftLargeInt:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ = PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ = PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ = PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ = PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ = PDouble(RightNode.FData)^;
            end;
          ftFloat, ftCurrency:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ = PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ = PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ = PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ = PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ = PDouble(RightNode.FData)^;
            end;
          ftDate:
            case RightNode.FDataType of
              ftDate:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ = PInteger(RightNode.FData)^;
              ftDateTime:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(RightNode.FData)^);
                  PBoolean(ResultNode.FData)^ := TempTimeStamp.Date = PInteger(LeftNode.FData)^;
                end;
            end;
          ftTime:
            case RightNode.FDataType of
              ftTime:
                PBoolean(ResultNode.FData)^ := CompareDbDateTime(PInteger(LeftNode.FData)^, PInteger(RightNode.FData)^) = 0;
            end;
          ftDateTime:
            case RightNode.FDataType of
              ftDate:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  PBoolean(ResultNode.FData)^ := TempTimeStamp.Date = PInteger(RightNode.FData)^;
                end;
              ftDateTime:
                PBoolean(ResultNode.FData)^ := CompareDbDateTime(PDouble(LeftNode.FData)^, PDouble(RightNode.FData)^) = 0;
            end;
          ftString, ftFixedChar, ftBlob, ftMemo, ftFmtMemo, ftGraphic:
            case RightNode.FDataType of
              ftString, ftFixedChar, ftBlob, ftMemo, ftFmtMemo, ftGraphic:
                PBoolean(ResultNode.FData)^ := TinyDBCompareString(LeftNode.FData, RightNode.FData,
                  not (scNoPartialCompare in Options), RightNode.FPartialLength, scCaseInsensitive in Options) = 0;
            end;
        end;
      end;
    toNE:   //-----------------------------------------------------------------
      begin
        EvaluateOperator(ResultNode, toEQ, LeftNode, RightNode, nil, Options);
        PBoolean(ResultNode.FData)^ := not PBoolean(ResultNode.FData)^;
      end;
    toGT:   //-----------------------------------------------------------------
      begin
        case LeftNode.FDataType of
          ftBoolean:
              PBoolean(ResultNode.FData)^ := PBoolean(LeftNode.FData)^ > PBoolean(RightNode.FData)^;
          ftSmallInt:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ > PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ > PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := Integer(PSmallInt(LeftNode.FData)^) > Integer(PWord(RightNode.FData)^);
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ > PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ > PDouble(RightNode.FData)^;
            end;
          ftInteger, ftAutoInc:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ > PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ > PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ > PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ > PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ > PDouble(RightNode.FData)^;
            end;
          ftWord:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := Integer(PWord(LeftNode.FData)^) > Integer(PSmallInt(RightNode.FData)^);
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ > PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ > PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ > PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ > PDouble(RightNode.FData)^;
            end;
          ftLargeInt:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ > PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ > PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ > PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ > PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ > PDouble(RightNode.FData)^;
            end;
          ftFloat, ftCurrency:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ > PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ > PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ > PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ > PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ > PDouble(RightNode.FData)^;
            end;
          ftDate:
            case RightNode.FDataType of
              ftDate:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ > PInteger(RightNode.FData)^;
              ftDateTime:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(RightNode.FData)^);
                  PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ > TempTimeStamp.Date;
                end;
            end;
          ftTime:
            case RightNode.FDataType of
              ftTime:
                PBoolean(ResultNode.FData)^ := CompareDbDateTime(PInteger(LeftNode.FData)^, PInteger(RightNode.FData)^) > 0;
            end;
          ftDateTime:
            case RightNode.FDataType of
              ftDate:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  PBoolean(ResultNode.FData)^ := TempTimeStamp.Date > PInteger(RightNode.FData)^;
                end;
              ftDateTime:
                PBoolean(ResultNode.FData)^ := CompareDbDateTime(PDouble(LeftNode.FData)^, PDouble(RightNode.FData)^) > 0;
            end;
          ftString, ftFixedChar, ftBlob, ftMemo, ftFmtMemo, ftGraphic:
            case RightNode.FDataType of
              ftString, ftFixedChar, ftBlob, ftMemo, ftFmtMemo, ftGraphic:
                PBoolean(ResultNode.FData)^ := TinyDBCompareString(LeftNode.FData, RightNode.FData,
                  not (scNoPartialCompare in Options), RightNode.FPartialLength, scCaseInsensitive in Options) > 0;
            end;
        end;
      end;
    toLT:   //-----------------------------------------------------------------
      begin
        case LeftNode.FDataType of
          ftBoolean:
              PBoolean(ResultNode.FData)^ := PBoolean(LeftNode.FData)^ < PBoolean(RightNode.FData)^;
          ftSmallInt:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ < PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ < PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := Integer(PSmallInt(LeftNode.FData)^) < Integer(PWord(RightNode.FData)^);
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ < PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ < PDouble(RightNode.FData)^;
            end;
          ftInteger, ftAutoInc:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ < PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ < PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ < PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ < PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ < PDouble(RightNode.FData)^;
            end;
          ftWord:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := Integer(PWord(LeftNode.FData)^) < Integer(PSmallInt(RightNode.FData)^);
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ < PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ < PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ < PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PWord(LeftNode.FData)^ < PDouble(RightNode.FData)^;
            end;
          ftLargeInt:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ < PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ < PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ < PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ < PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ < PDouble(RightNode.FData)^;
            end;
          ftFloat, ftCurrency:
            case RightNode.FDataType of
              ftSmallInt:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ < PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ < PInteger(RightNode.FData)^;
              ftWord:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ < PWord(RightNode.FData)^;
              ftLargeInt:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ < PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PBoolean(ResultNode.FData)^ := PDouble(LeftNode.FData)^ < PDouble(RightNode.FData)^;
            end;
          ftDate:
            case RightNode.FDataType of
              ftDate:
                PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ < PInteger(RightNode.FData)^;
              ftDateTime:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(RightNode.FData)^);
                  PBoolean(ResultNode.FData)^ := PInteger(LeftNode.FData)^ < TempTimeStamp.Date;
                end;
            end;
          ftTime:
            case RightNode.FDataType of
              ftTime:
                PBoolean(ResultNode.FData)^ := CompareDbDateTime(PInteger(LeftNode.FData)^, PInteger(RightNode.FData)^) < 0;
            end;
          ftDateTime:
            case RightNode.FDataType of
              ftDate:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  PBoolean(ResultNode.FData)^ := TempTimeStamp.Date < PInteger(RightNode.FData)^;
                end;
              ftDateTime:
                PBoolean(ResultNode.FData)^ := CompareDbDateTime(PDouble(LeftNode.FData)^, PDouble(RightNode.FData)^) < 0;
            end;
          ftString, ftFixedChar, ftBlob, ftMemo, ftFmtMemo, ftGraphic:
            case RightNode.FDataType of
              ftString, ftFixedChar, ftBlob, ftMemo, ftFmtMemo, ftGraphic:
                PBoolean(ResultNode.FData)^ := TinyDBCompareString(LeftNode.FData, RightNode.FData,
                  not (scNoPartialCompare in Options), RightNode.FPartialLength, scCaseInsensitive in Options) < 0;
            end;
        end;
      end;
    toGE:   //-----------------------------------------------------------------
      begin
        EvaluateOperator(ResultNode, toLT, LeftNode, RightNode, nil, Options);
        PBoolean(ResultNode.FData)^ := not PBoolean(ResultNode.FData)^;
      end;
    toLE:   //-----------------------------------------------------------------
      begin
        EvaluateOperator(ResultNode, toGT, LeftNode, RightNode, nil, Options);
        PBoolean(ResultNode.FData)^ := not PBoolean(ResultNode.FData)^;
      end;
    toADD:  //-----------------------------------------------------------------
      begin
        case LeftNode.FDataType of
          ftSmallInt:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ + PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PInteger(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ + PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := Integer(PSmallInt(LeftNode.FData)^) + Integer(PWord(RightNode.FData)^);
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ + PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ + PDouble(RightNode.FData)^;
            end;
          ftInteger, ftAutoInc:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PWord(RightNode.FData)^;
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PDouble(RightNode.FData)^;
            end;
          ftWord:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := Integer(PWord(LeftNode.FData)^) + Integer(PSmallInt(RightNode.FData)^);
              ftInteger, ftAutoInc:
                PInteger(ResultNode.FData)^ := PWord(LeftNode.FData)^ + PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := PWord(LeftNode.FData)^ + PWord(RightNode.FData)^;
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PWord(LeftNode.FData)^ + PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PWord(LeftNode.FData)^ + PDouble(RightNode.FData)^;
            end;
          ftLargeInt:
            case RightNode.FDataType of
              ftSmallInt:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ + PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ + PInteger(RightNode.FData)^;
              ftWord:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ + PWord(RightNode.FData)^;
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ + PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ + PDouble(RightNode.FData)^;
            end;
          ftFloat, ftCurrency:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ + PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ + PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ + PWord(RightNode.FData)^;
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ + PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ + PDouble(RightNode.FData)^;
            end;
          ftDate:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PWord(RightNode.FData)^;
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PDouble(RightNode.FData)^;
            end;
          ftTime:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PWord(RightNode.FData)^;
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ + PDouble(RightNode.FData)^;
            end;
          ftDateTime:
            case RightNode.FDataType of
              ftSmallInt:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Inc(TempTimeStamp.Date, PSmallInt(RightNode.FData)^);
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
              ftInteger, ftAutoInc:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Inc(TempTimeStamp.Date, PInteger(RightNode.FData)^);
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
              ftWord:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Inc(TempTimeStamp.Date, PWord(RightNode.FData)^);
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
              ftLargeInt:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Inc(TempTimeStamp.Date, PLargeInt(RightNode.FData)^);
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
              ftFloat, ftCurrency:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Inc(TempTimeStamp.Date, Trunc(PDouble(RightNode.FData)^));
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
            end;
          ftString, ftFixedChar, ftBlob, ftMemo, ftFmtMemo, ftGraphic:
            case RightNode.FDataType of
              ftString, ftFixedChar, ftBlob, ftMemo, ftFmtMemo, ftGraphic:
                begin
                  ResultNode.DataSize := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(LeftNode.FData) + {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(RightNode.FData) + 1;
                  {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrCopy(ResultNode.FData, LeftNode.FData);
                  {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrCat(ResultNode.FData, RightNode.FData);
                end;
            end;
        end;
      end;
    toSUB:  //-----------------------------------------------------------------
      begin
        case LeftNode.FDataType of
          ftSmallInt:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ - PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PInteger(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ - PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := Integer(PSmallInt(LeftNode.FData)^) - Integer(PWord(RightNode.FData)^);
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ - PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ - PDouble(RightNode.FData)^;
            end;
          ftInteger, ftAutoInc:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PWord(RightNode.FData)^;
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PDouble(RightNode.FData)^;
            end;
          ftWord:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := Integer(PWord(LeftNode.FData)^) - Integer(PSmallInt(RightNode.FData)^);
              ftInteger, ftAutoInc:
                PInteger(ResultNode.FData)^ := PWord(LeftNode.FData)^ - PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := PWord(LeftNode.FData)^ - PWord(RightNode.FData)^;
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PWord(LeftNode.FData)^ - PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PWord(LeftNode.FData)^ - PDouble(RightNode.FData)^;
            end;
          ftLargeInt:
            case RightNode.FDataType of
              ftSmallInt:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ - PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ - PInteger(RightNode.FData)^;
              ftWord:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ - PWord(RightNode.FData)^;
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ - PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ - PDouble(RightNode.FData)^;
            end;
          ftFloat, ftCurrency:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ - PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ - PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ - PWord(RightNode.FData)^;
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ - PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ - PDouble(RightNode.FData)^;
            end;
          ftDate:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc, ftDate:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PWord(RightNode.FData)^;
              ftLargeInt:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - Trunc(PDouble(RightNode.FData)^);
            end;
          ftTime:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc, ftTime:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PWord(RightNode.FData)^;
              ftLargeInt:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ - Trunc(PDouble(RightNode.FData)^);
            end;
          ftDateTime:
            case RightNode.FDataType of
              ftSmallInt:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Dec(TempTimeStamp.Date, PSmallInt(RightNode.FData)^);
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
              ftInteger, ftAutoInc:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Dec(TempTimeStamp.Date, PInteger(RightNode.FData)^);
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
              ftWord:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Dec(TempTimeStamp.Date, PWord(RightNode.FData)^);
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
              ftLargeInt:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Dec(TempTimeStamp.Date, PLargeInt(RightNode.FData)^);
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
              ftFloat, ftCurrency:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  Dec(TempTimeStamp.Date, Trunc(PDouble(RightNode.FData)^));
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp);
                end;
              ftDate, ftTime:
                begin
                  TempTimeStamp := MSecsToTimeStamp(PDouble(LeftNode.FData)^);
                  PDouble(ResultNode.FData)^ := TimeStampToMSecs(TempTimeStamp) - PInteger(RightNode.FData)^;
                end;
              ftDateTime:
                begin
                  PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ - PDouble(RightNode.FData)^;
                end;
            end;
        end;
      end;
    toMUL:  //-----------------------------------------------------------------
      begin
        case LeftNode.FDataType of
          ftSmallInt:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ * PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PInteger(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ * PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := Integer(PSmallInt(LeftNode.FData)^) * Integer(PWord(RightNode.FData)^);
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ * PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ * PDouble(RightNode.FData)^;
            end;
          ftInteger, ftAutoInc:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ * PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ * PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := PInteger(LeftNode.FData)^ * PWord(RightNode.FData)^;
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PInteger(LeftNode.FData)^ * PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ * PDouble(RightNode.FData)^;
            end;
          ftWord:
            case RightNode.FDataType of
              ftSmallInt:
                PInteger(ResultNode.FData)^ := Integer(PWord(LeftNode.FData)^) * Integer(PSmallInt(RightNode.FData)^);
              ftInteger, ftAutoInc:
                PInteger(ResultNode.FData)^ := PWord(LeftNode.FData)^ * PInteger(RightNode.FData)^;
              ftWord:
                PInteger(ResultNode.FData)^ := PWord(LeftNode.FData)^ * PWord(RightNode.FData)^;
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PWord(LeftNode.FData)^ * PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PWord(LeftNode.FData)^ * PDouble(RightNode.FData)^;
            end;
          ftLargeInt:
            case RightNode.FDataType of
              ftSmallInt:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ * PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ * PInteger(RightNode.FData)^;
              ftWord:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ * PWord(RightNode.FData)^;
              ftLargeInt:
                PLargeInt(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ * PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ * PDouble(RightNode.FData)^;
            end;
          ftFloat, ftCurrency:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ * PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ * PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ * PWord(RightNode.FData)^;
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ * PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ * PDouble(RightNode.FData)^;
            end;
        end;
      end;
    toDIV:  //-----------------------------------------------------------------
      begin
        case LeftNode.FDataType of
          ftSmallInt:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ / PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ / PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := Integer(PSmallInt(LeftNode.FData)^) / Integer(PWord(RightNode.FData)^);
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ / PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PSmallInt(LeftNode.FData)^ / PDouble(RightNode.FData)^;
            end;
          ftInteger, ftAutoInc:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ / PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ / PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ / PWord(RightNode.FData)^;
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ / PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PInteger(LeftNode.FData)^ / PDouble(RightNode.FData)^;
            end;
          ftWord:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := Integer(PWord(LeftNode.FData)^) / Integer(PSmallInt(RightNode.FData)^);
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PWord(LeftNode.FData)^ / PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := PWord(LeftNode.FData)^ / PWord(RightNode.FData)^;
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PWord(LeftNode.FData)^ / PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PWord(LeftNode.FData)^ / PDouble(RightNode.FData)^;
            end;
          ftLargeInt:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ / PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ / PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ / PWord(RightNode.FData)^;
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ / PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PLargeInt(LeftNode.FData)^ / PDouble(RightNode.FData)^;
            end;
          ftFloat, ftCurrency:
            case RightNode.FDataType of
              ftSmallInt:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ / PSmallInt(RightNode.FData)^;
              ftInteger, ftAutoInc:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ / PInteger(RightNode.FData)^;
              ftWord:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ / PWord(RightNode.FData)^;
              ftLargeInt:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ / PLargeInt(RightNode.FData)^;
              ftFloat, ftCurrency:
                PDouble(ResultNode.FData)^ := PDouble(LeftNode.FData)^ / PDouble(RightNode.FData)^;
            end;
        end;
      end;
    toLIKE: //-----------------------------------------------------------------
      begin
        W1 := {$IFDEF UNICODE}string{$ENDIF}(LeftNode.FData);
        W2 := {$IFDEF UNICODE}string{$ENDIF}(RightNode.FData);
        PBoolean(ResultNode.FData)^ := LikeString(W1, W2, not (scCaseInsensitive in Options));
      end;
    toIN:   //-----------------------------------------------------------------
      begin
        for I := 0 to Args.Count - 1 do
        begin
          EvaluateOperator(ResultNode, toEQ, LeftNode, TExprNode(Args[I]), nil, Options);
          if PBoolean(ResultNode.FData)^ then Break;
        end;
      end;
  end;
end;

procedure TExprNode.EvaluateFunction(ResultNode: TExprNode; AFunction: TTinyFunction; Args: TList);
var
  TempNode: TExprNode;
  TempTimeStamp: TTimeStamp;
  Year, Month, Day, Hour, Minute, Second, MSec: Word;
begin
  case AFunction of
    tfUpper:
      begin
        TempNode := TExprNode(Args[0]);
        ResultNode.DataSize := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(TempNode.FData) + 1;
        {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrCopy(ResultNode.FData, {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrUpper(TempNode.FData));
      end;
    tfLower:
      begin
        TempNode := TExprNode(Args[0]);
        ResultNode.DataSize := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(TempNode.FData) + 1;
        {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrCopy(ResultNode.FData, {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLower(TempNode.FData));
      end;
    tfSubString:
      begin
        ResultNode.DataSize := PInteger(TExprNode(Args[2]).FData)^ + 1;  //Sub Length
        {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(ResultNode.FData,
          TExprNode(Args[0]).FData + PInteger(TExprNode(Args[1]).FData)^ - 1,
          PInteger(TExprNode(Args[2]).FData)^);
      end;
    tfTrim:
      begin
        TempNode := TExprNode(Args[0]);
        ResultNode.DataSize := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(TempNode.FData) + 1;
        {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrCopy(ResultNode.FData, PAnsiChar(Trim(AnsiString(TempNode.FData))));
      end;
    tfTrimLeft:
      begin
        TempNode := TExprNode(Args[0]);
        ResultNode.DataSize := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(TempNode.FData) + 1;
        {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrCopy(ResultNode.FData, PAnsiChar(TrimLeft(AnsiString(TempNode.FData))));
      end;
    tfTrimRight:
      begin
        TempNode := TExprNode(Args[0]);
        ResultNode.DataSize := {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLen(TempNode.FData) + 1;
        {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrCopy(ResultNode.FData, PAnsiChar(TrimRight(AnsiString(TempNode.FData))));
      end;
    tfYear, tfMonth, tfDay:
      begin
        TempNode := TExprNode(Args[0]);
        case TempNode.FDataType of
          ftDate:
            begin
              TempTimeStamp.Date := PInteger(TempNode.FData)^;
              TempTimeStamp.Time := 0;
              DecodeDate(TimeStampToDateTime(TempTimeStamp), Year, Month, Day);
            end;
          ftDateTime:
            begin
              TempTimeStamp := MSecsToTimeStamp(PDouble(TempNode.FData)^);
              DecodeDate(TimeStampToDateTime(TempTimeStamp), Year, Month, Day);
            end;
        end;
        case AFunction of
          tfYear:  PInteger(ResultNode.FData)^ := Year;
          tfMonth: PInteger(ResultNode.FData)^ := Month;
          tfDay:   PInteger(ResultNode.FData)^ := Day;
        end;
      end;
    tfHour, tfMinute, tfSecond:
      begin
        TempNode := TExprNode(Args[0]);
        case TempNode.FDataType of
          ftTime:
            begin
              TempTimeStamp.Date := Trunc(Date);
              TempTimeStamp.Time := PInteger(TempNode.FData)^;
              DecodeTime(TimeStampToDateTime(TempTimeStamp), Hour, Minute, Second, MSec);
            end;
          ftDateTime:
            begin
              TempTimeStamp := MSecsToTimeStamp(PDouble(TempNode.FData)^);
              DecodeTime(TimeStampToDateTime(TempTimeStamp), Hour, Minute, Second, MSec);
            end;
        end;
        case AFunction of
          tfHour:   PInteger(ResultNode.FData)^ := Hour;
          tfMinute: PInteger(ResultNode.FData)^ := Minute;
          tfSecond: PInteger(ResultNode.FData)^ := Second;
        end;
      end;
  end;
end;

function TExprNode.IsIntegerType: Boolean;
begin
  Result := FDataType in [ftSmallint, ftInteger, ftWord, ftAutoInc];
end;

function TExprNode.IsLargeIntType: Boolean;
begin
  Result := FDataType in [ftLargeInt];
end;

function TExprNode.IsFloatType: Boolean;
begin
  Result := FDataType in [ftFloat, ftCurrency];
end;

function TExprNode.IsTemporalType: Boolean;
begin
  Result := FDataType in [ftDate, ftTime, ftDateTime];
end;

function TExprNode.IsStringType: Boolean;
begin
  Result := (FDataType in StringFieldTypes) or (FDataType in BlobFieldTypes);
end;

function TExprNode.IsBooleanType: Boolean;
begin
  Result := (FDataType = ftBoolean);
end;

function TExprNode.IsNumericType: Boolean;
begin
  Result := IsIntegerType or IsLargeIntType or IsFloatType;
end;

function TExprNode.IsTemporalStringType: Boolean;
var
  TempStr: string;
  TempDateTime: TDateTime;
begin
  TempStr := string(FData);
  Result := IsStringType and (FKind = enConst);
  if Result then
  begin
    Result := DbStrToDateTime(TempStr, TempDateTime);
    if not Result then
    begin
      Result := True;
      try
        StrToDateTime(TempStr);
      except
        Result := False;
      end;
    end;
  end;
end;

procedure TExprNode.SetDataSize(Size: Integer);
begin
  if FDataSize <> Size then
  begin
    if Size > 0 then
    begin
      if FDataSize = 0 then
        FData := AllocMem(Size)
      else
        ReallocMem(FData, Size);
    end 
    else
    begin
      FreeMem(FData);
      FData := nil;
    end;
    FDataSize := Size;
  end;
end;

function TExprNode.GetDataSet: TTDEDataSet;
begin
  Result := (FExprNodes.FExprParser as TFilterParser).FDataSet;
end;

function TExprNode.AsBoolean: Boolean;
begin
  Result := PBoolean(FData)^;
end;

procedure TExprNode.ConvertStringToDateTime(DstType: TFieldType);
var
  DateTimeString: string;
  DateTime: TDateTime;
begin
  DateTimeString := Trim(string(FData));

  case DstType of
    ftDate:
      begin
        if not DbStrToDate(DateTimeString, DateTime) then
          DateTime := StrToDate(DateTimeString);
        DataSize := SizeOf(Integer);
        FDataType := ftDate;
        PInteger(FData)^ := DateTimeToTimeStamp(DateTime).Date;
      end;
    ftTime:
      begin
        if not DbStrToTime(DateTimeString, DateTime) then
          DateTime := StrToTime(DateTimeString);
        DataSize := SizeOf(Integer);
        FDataType := ftTime;
        PInteger(FData)^ := DateTimeToTimeStamp(DateTime).Time;
      end;
    ftDateTime:
      begin
        if not DbStrToDateTime(DateTimeString, DateTime) then
          DateTime := StrToDateTime(DateTimeString);
        DataSize := SizeOf(Double);
        FDataType := ftDateTime;
        PDouble(FData)^ := DateTime; //--get out--TimeStampToMSecs(DateTimeToTimeStamp(DateTime));
      end;
  end;
end;

class function TExprNode.FuncNameToEnum(const FuncName: string): TTinyFunction;
var
  FuncNames: array[TTinyFunction] of string;
  I: TTinyFunction;
begin
  FuncNames[tfUpper] := 'UPPER';
  FuncNames[tfLower] := 'LOWER';
  FuncNames[tfSubString] := 'SUBSTRING';
  FuncNames[tfTrim] := 'TRIM';
  FuncNames[tfTrimLeft] := 'TRIMLEFT';
  FuncNames[tfTrimRight] := 'TRIMRIGHT';
  FuncNames[tfYear] := 'YEAR';
  FuncNames[tfMonth] := 'MONTH';
  FuncNames[tfDay] := 'DAY';
  FuncNames[tfHour] := 'HOUR';
  FuncNames[tfMinute] := 'MINUTE';
  FuncNames[tfSecond] := 'SECOND';
  FuncNames[tfGetDate] := 'GETDATE';

  for I := Low(FuncNames) to High(FuncNames) do
  begin
    if SysUtils.AnsiSameText(FuncName, FuncNames[I]) then
    begin
      Result := I;
      Exit;
    end;
  end;
  Result := tfUnknown;
end;

{ TExprNodes }

constructor TExprNodes.Create(AExprParser: TExprParserBase);
begin
  FExprParser := AExprParser;
  FNodes := nil;
  FRoot := nil;
end;

destructor TExprNodes.Destroy;
begin
  Clear;
  inherited;
end;

procedure TExprNodes.Clear;
var
  Node: TExprNode;
begin
  while FNodes <> nil do
  begin
    Node := FNodes;
    FNodes := Node.FNext;
    Node.Free;
  end;
  FNodes := nil;
end;

function TExprNodes.NewNode(NodeKind: TExprNodeKind; DataType: TFieldType;
  ADataSize: Integer; iOperator: TTinyOperator; Left, Right: TExprNode): TExprNode;
begin
  Result := TExprNode.Create(Self);
  with Result do
  begin
    FNext := FNodes;

    FKind := NodeKind;
    FDataType := DataType;
    DataSize := ADataSize;
    FOperator := iOperator;
    FLeft := Left;
    FRight := Right;
  end;
  FNodes := Result;
end;

function TExprNodes.NewFuncNode(const FuncName: string): TExprNode;
begin
  Result := TExprNode.Create(Self);
  with Result do
  begin
    FNext := FNodes;

    FKind := enFunc;
    FDataType := FExprParser.GetFuncDataType(FuncName);
    DataSize := 0;
    FSymbol := FuncName;
    FOperator := toNOTDEFINED;
    FFunction := FuncNameToEnum(FuncName);
    FLeft := nil;
    FRight := nil;
  end;
  FNodes := Result;
end;

function TExprNodes.NewFieldNode(const FieldName: string): TExprNode;
begin
  Result := TExprNode.Create(Self);
  with Result do
  begin
    FNext := FNodes;

    FKind := enField;
    FDataType := FExprParser.GetFieldDataType(FieldName);
    DataSize := 0;
    FData := nil;
    FSymbol := FieldName;
    FOperator := toNOTDEFINED;
    FLeft := nil;
    FRight := nil;
    FIsBlobField := GetDataSet.FieldByName(FieldName).IsBlob;
    FFieldIdx := GetDataSet.FieldByName(FieldName).Index;
  end;
  FNodes := Result;
end;

{ TSyntaxParserBase }

constructor TSyntaxParserBase.Create;
begin
  inherited;
end;

destructor TSyntaxParserBase.Destroy;
begin
  inherited;
end;

procedure TSyntaxParserBase.SetText(const Value: string);
begin
  FText := Value;
  FSourcePtr := PChar(FText);
end;

{$IFNDEF UNICODE}
function TSyntaxParserBase.IsKatakana(const Chr: Byte): Boolean;
begin
  Result := (SysLocale.PriLangID = LANG_JAPANESE) and (Chr in [$A1..$DF]);
end;
{$ENDIF}

function TSyntaxParserBase.IsNextToken(const TokenName: string): Boolean;
var
  LTokenPtr: PChar;
  LSourcePtr: PChar;
  LPrevToken: TExprToken;
begin
  LPrevToken := FPrevToken;
  LTokenPtr := FTokenPtr;
  LSourcePtr := FSourcePtr;
  try
    FTokenString := '';
    LTokenPtr := SkipBeforeGetToken(FSourcePtr);
    LSourcePtr := InternalGetNextToken(FTokenPtr);
    Result := SysUtils.AnsiSameText(FTokenString, TokenName);
  finally
    FPrevToken := LPrevToken;
    FTokenPtr := LTokenPtr;
    FSourcePtr := LSourcePtr;
  end;
end;

procedure TSyntaxParserBase.Skip(var P: PChar; TheSet: TChrSet);
begin
  while True do
  begin
    if CharInSet(P^, LeadBytes) then
      Inc(P, 2)
    else
      if CharInSet(P^, TheSet){$IFNDEF UNICODE}or IsKatakana(Byte(P^)){$ENDIF} then
        Inc(P)
      else
        Exit;
  end;
end;

function TSyntaxParserBase.TokenName: string;
begin
  if FSourcePtr = FTokenPtr then Result := SExprNothing else
  begin
    SetString(Result, FTokenPtr, FSourcePtr - FTokenPtr);
    Result := '''' + Result + '''';
  end;
end;

function TSyntaxParserBase.TokenSymbolIs(const S: string): Boolean;
begin
  Result := (FToken = etSymbol) and (SysUtils.AnsiCompareText(FTokenString, S) = 0);
end;

function TSyntaxParserBase.TokenSymbolsIs(const A: array of string): Boolean;
var
  I: Integer;
begin
  for I := Low(A) to High(A) do
  begin
    Result := (FToken = etSymbol) and (SysUtils.AnsiCompareText(FTokenString, A[I]) = 0);
    if Result then Exit;
  end;
  Result := False;  
end;

procedure TSyntaxParserBase.Rewind;
begin
  FSourcePtr := PChar(FText);
  FTokenPtr := FSourcePtr;
  FTokenString := '';
end;

function TSyntaxParserBase.SkipBeforeGetToken(Pos: PChar): PChar;
var
  P: PChar;
begin
  P := Pos;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  {Note signs deal with Universal /**/}
  if (P^ <> #0) and (P^ = '/') and (P[1] <> #0) and (P[1] = '*')then
  begin
    P := P + 2;
    while (P^ <> #0) and (P^ <> '*') do Inc(P);
    if (P^ = '*') and (P[1] <> #0) and (P[1] =  '/')  then
      P := P + 2
    else
      DatabaseErrorFmt(SExprInvalidChar, [P^]);
  end;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  Result := P;
end;

procedure TSyntaxParserBase.GetNextToken;
begin
  FPrevToken := FToken;
  FTokenString := '';
  FTokenPtr := SkipBeforeGetToken(FSourcePtr);
  FSourcePtr := InternalGetNextToken(FTokenPtr);
end;

{ TExprParserBase }

constructor TExprParserBase.Create;
begin
  FExprNodes := TExprNodes.Create(Self);
end;

destructor TExprParserBase.Destroy;
begin
  FreeAndNil(FExprNodes);
  inherited;
end;

procedure  TExprParserBase.Parse(const AText: string);
begin
  FExprNodes.Clear;
  Text := AText;
  GetNextToken;
  FExprNodes.Root := ParseExpr;
  ParseFinished;
end;

procedure TExprParserBase.ParseFinished;
begin
  if FToken <> etEnd then DatabaseError(SExprTermination);
end;

function TExprParserBase.Calculate(Options: TStrCompOptions = []): Variant;
begin
  FStrCompOpts := Options;
  FExprNodes.Root.Calculate(Options);
  Result := FExprNodes.Root.AsBoolean;
end;

function TExprParserBase.TokenSymbolIsFunc(const S: string) : Boolean;
begin
  Result := False;
end;

{ TFilterParser }

constructor TFilterParser.Create(ADataSet: TTDEDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

function TFilterParser.GetFieldDataType(const Name: string): TFieldType;
begin
  Result := FDataSet.FieldByName(Name).DataType;
end;

function TFilterParser.GetFieldValue(const Name: string): Variant;
begin
  Result := FDataSet.FieldByName(Name).Value;
end;

function TFilterParser.GetFuncDataType(const Name: string): TFieldType;
begin
  Result := ftUnknown;
  if SysUtils.AnsiSameText(Name, 'YEAR') or
     SysUtils.AnsiSameText(Name, 'MONTH') or
     SysUtils.AnsiSameText(Name, 'DAY') or
     SysUtils.AnsiSameText(Name, 'HOUR') or
     SysUtils.AnsiSameText(Name, 'MINUTE') or
     SysUtils.AnsiSameText(Name, 'SECOND')
  then
  begin
    Result := ftInteger;
  end
  else
  if SysUtils.AnsiSameText(Name, 'GETDATE') then
  begin
    Result := ftDateTime;
  end;
end;

function TFilterParser.TokenSymbolIsFunc(const S: string): Boolean;
begin
  Result := TExprNode.FuncNameToEnum(S) <> tfUnknown;
end;

function TFilterParser.InternalGetNextToken(Pos: PChar): PChar;
var
  P, TokenStart: PChar;
  L: Integer;
  StrBuf: array[0..255] of Char;
begin
  P := Pos;
  case P^ of
    'A'..'Z', 'a'..'z', '_', #$81..#$fe:
      begin
        TokenStart := P;
        if not SysLocale.FarEast then
        begin
          Inc(P);
          while CharInSet(P^, ccCSet) do
            Inc(P);
        end
        else
          Skip(P, ccCSet);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etSymbol;
        if SysUtils.AnsiSameText(FTokenString, 'LIKE') then
          FToken := etLIKE
        else if SysUtils.AnsiSameText(FTokenString, 'IN') then
          FToken := etIN
        {
        else if SysUtils.AnsiCompareText(FTokenString, 'IS') = 0 then
        begin
          while (P^ <> #0) and (P^ <= ' ') do Inc(P);
          TokenStart := P;
          Skip(P, ['A'..'Z', 'a'..'z']);
          SetString(FTokenString, TokenStart, P - TokenStart);
          if SysUtils.AnsiCompareText(FTokenString, 'NOT')= 0 then
          begin
            while (P^ <> #0) and (P^ <= ' ') do Inc(P);
            TokenStart := P;
            Skip(P, ['A'..'Z', 'a'..'z']);
            SetString(FTokenString, TokenStart, P - TokenStart);
            if SysUtils.AnsiCompareText(FTokenString, 'NULL') = 0 then
              FToken := etISNOTNULL
            else
              DatabaseError(SInvalidKeywordUse);
          end
          else if SysUtils.AnsiCompareText (FTokenString, 'NULL') = 0  then
          begin
            FToken := etISNULL;
          end
          else
            DatabaseError(SInvalidKeywordUse);
        end;
        }
      end;
    '[':
      begin
        Inc(P);
        TokenStart := P;
        P := AnsiStrScan(P, ']');
        if P = nil then DatabaseError(SExprNameError);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etName;
        Inc(P);
      end;
    '''':
      begin
        Inc(P);
        L := 0;
        while True do
        begin
          if P^ = #0 then DatabaseError(SExprStringError);
          if P^ = '''' then
          begin
            Inc(P);
            if P^ <> '''' then Break;
          end;
          if L < {$IFDEF UNICODE}Length{$ELSE}SizeOF{$ENDIF}(StrBuf) then
          begin
            StrBuf[L] := P^;
            Inc(L);
          end;
          Inc(P);
        end;
        SetString(FTokenString, StrBuf, L);
        FToken := etCharLiteral;
      end;
    '-', '0'..'9':
      begin
        if (P^ = '-') and (FPrevToken in [etCharLiteral, etNumLiteral, etName, etSymbol, etRParen]) then
        begin
          FToken := etSUB;
          Inc(P);
        end 
        else
        begin
          TokenStart := P;
          Inc(P);
          while CharInSet(P^, ['0'..'9', {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}DecimalSeparator, 'e', 'E', '+', '-']) do
          begin
            if CharInSet(P^, ['+', '-']) and not CharInSet((P-1)^, ['e', 'E']) and (P <> TokenStart) then Break;
            Inc(P);
          end;
          if ((P-1)^ = ',') and ({$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}DecimalSeparator = ',') and (P^ = ' ') then Dec(P);
          SetString(FTokenString, TokenStart, P - TokenStart);
          FToken := etNumLiteral;
        end;
      end;
    '(':
      begin
        Inc(P);
        FToken := etLParen;
      end;
    ')':
      begin
        Inc(P);
        FToken := etRParen;
      end;
    '<':
      begin
        Inc(P);
        case P^ of
          '=':
            begin
              Inc(P);
              FToken := etLE;
            end;
          '>':
            begin
              Inc(P);
              FToken := etNE;
            end;
        else
          FToken := etLT;
        end;
      end;
    '=':
      begin
        Inc(P);
        FToken := etEQ;
      end;
    '>':
      begin
        Inc(P);
        if P^ = '=' then
        begin
          Inc(P);
          FToken := etGE;
        end else
          FToken := etGT;
      end;
    '+':
      begin
        Inc(P);
        FToken := etADD;
      end;
    '*':
      begin
        Inc(P);
        FToken := etMUL;
      end;
    '/':
      begin
        Inc(P);
        FToken := etDIV;
      end;
    ',':
      begin
        Inc(P);
        FToken := etComma;
      end;
    #0:
      FToken := etEnd;
  else
    DatabaseErrorFmt(SExprInvalidChar, [P^]);
  end;
  Result := P;
end;

function TFilterParser.NextTokenIsLParen: Boolean;
var
  P : PChar;
begin
  P := FSourcePtr;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  Result := P^ = '(';
end;

function TFilterParser.ParseExpr: TExprNode;
begin
  Result := ParseExpr2;
  while TokenSymbolIs('OR') do
  begin
    GetNextToken;
    Result := FExprNodes.NewNode(enOperator, ftBoolean, SizeOf(Boolean), toOR, Result, ParseExpr2);
    TypeCheckLogicOp(Result);
  end;
end;

function TFilterParser.ParseExpr2: TExprNode;
begin
  Result := ParseExpr3;
  while TokenSymbolIs('AND') do
  begin
    GetNextToken;
    Result := FExprNodes.NewNode(enOperator, ftBoolean, SizeOf(Boolean), toAND, Result, ParseExpr3);
    TypeCheckLogicOp(Result);
  end;
end;

function TFilterParser.ParseExpr3: TExprNode;
begin
  if TokenSymbolIs('NOT') then
  begin
    GetNextToken;
    Result := FExprNodes.NewNode(enOperator, ftBoolean, SizeOf(Boolean), toNOT, ParseExpr4, nil);
    TypeCheckLogicOp(Result);
  end
  else
    Result := ParseExpr4;
end;

function TFilterParser.ParseExpr4: TExprNode;
const
  Operators: array[etEQ..etLT] of TTinyOperator = (
    toEQ, toNE, toGE, toLE, toGT, toLT);
var
  vOperator: TTinyOperator;
  Left, Right: TExprNode;
begin
  Result := ParseExpr5;
  if (FToken in [etEQ..etLT]) or (FToken = etLIKE) or (FToken = etIN) then
  begin
    case FToken of
      etEQ..etLT:
        vOperator := Operators[FToken];
      etLIKE:
        vOperator := toLIKE;
      etIN:
        vOperator := toIN;
    else
      vOperator := toNOTDEFINED;
    end;
    GetNextToken;
    Left := Result;
    if vOperator = toIN then
    begin
      if FToken <> etLParen then
        DatabaseErrorFmt(SExprNoLParen, [TokenName]);
      GetNextToken;
      Result := FExprNodes.NewNode(enOperator, ftBoolean, SizeOf(Boolean), toIN, Left, nil);
      if FToken <> etRParen then
      begin
        Result.FArgs := TList.Create;
        repeat
          Right := ParseExpr;
          Result.FArgs.Add(Right);
          if (FToken <> etComma) and (FToken <> etRParen) then
            DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
          if FToken = etComma then GetNextToken;
        until (FToken = etRParen) or (FToken = etEnd);
        if FToken <> etRParen then
          DatabaseErrorFmt(SExprNoRParen, [TokenName]);
        TypeCheckInOp(Result);
        GetNextToken;
      end
      else
        DatabaseError(SExprEmptyInList);
    end
    else
    begin
      Right := ParseExpr5;
      Result := FExprNodes.NewNode(enOperator, ftBoolean, SizeOf(Boolean), vOperator, Left, Right);
      case vOperator of
        toEQ, toNE, toGE, toLE, toGT, toLT:
          TypeCheckRelationOp(Result);
        toLIKE:
          TypeCheckLikeOp(Result);
      end;
    end;
  end;
end;

function TFilterParser.ParseExpr5: TExprNode;
const
  Operators: array[etADD..etDIV] of TTinyOperator = (
    toADD, toSUB, toMUL, toDIV);
var
  vOperator: TTinyOperator;
  Left, Right: TExprNode;
begin
  Result := ParseExpr6;
  while FToken in [etADD, etSUB] do
  begin
    vOperator := Operators[FToken];
    Left := Result;
    GetNextToken;
    Right := ParseExpr6;
    Result := FExprNodes.NewNode(enOperator, ftUnknown, 0, vOperator, Left, Right);
    TypeCheckArithOp(Result);
  end;
end;

function TFilterParser.ParseExpr6: TExprNode;
const
  Operators: array[etADD..etDIV] of TTinyOperator = (
    toADD, toSUB, toMUL, toDIV);
var
  vOperator: TTinyOperator;
  Left, Right: TExprNode;
begin
  Result := ParseExpr7;
  while FToken in [etMUL, etDIV] do
  begin
    vOperator := Operators[FToken];
    Left := Result;
    GetNextToken;
    Right := ParseExpr7;
    Result := FExprNodes.NewNode(enOperator, ftUnknown, 0, vOperator, Left, Right);
    TypeCheckArithOp(Result);
  end;
end;

function TFilterParser.ParseExpr7: TExprNode;
var
  FuncName: string;
begin
  case FToken of
    etSymbol:
      if NextTokenIsLParen and TokenSymbolIsFunc(FTokenString) then
      begin
        FuncName := FTokenString;
        GetNextToken;
        if FToken <> etLParen then
          DatabaseErrorFmt(SExprNoLParen, [TokenName]);
        GetNextToken;
        if SysUtils.AnsiSameText(FuncName,'COUNT') and (FToken = etMUL) then
        begin
          FuncName := 'COUNT(*)';
          GetNextToken;
        end;
        Result := FExprNodes.NewFuncNode(FuncName);
        if FToken <> etRParen then
        begin
          Result.FArgs := TList.Create;
          repeat
            Result.FArgs.Add(ParseExpr);
            if (FToken <> etComma) and (FToken <> etRParen) then
              DatabaseErrorFmt(SExprNoRParenOrComma, [TokenName]);
            if FToken = etComma then GetNextToken;
          until (FToken = etRParen) or (FToken = etEnd);
        end
        else
          Result.FArgs := nil;
        TypeCheckFunction(Result);
      end
      else
      if TokenSymbolIs(STextTrue) then
      begin
        Result := FExprNodes.NewNode(enConst, ftBoolean, SizeOf(Boolean), toNOTDEFINED, nil, nil);
        PBoolean(Result.FData)^ := True;
      end
      else
      if TokenSymbolIs(STextFalse) then
      begin
        Result := FExprNodes.NewNode(enConst, ftBoolean, SizeOf(Boolean), toNOTDEFINED, nil, nil);
        PBoolean(Result.FData)^ := False;
      end
      else
      begin
        Result := FExprNodes.NewFieldNode(FTokenString);
      end;
    etName:
      begin
        Result := FExprNodes.NewFieldNode(FTokenString);
      end;
    etNumLiteral:
      begin
        if IsInt(FTokenString) then
        begin
          Result := FExprNodes.NewNode(enConst, ftInteger, SizeOf(Integer), toNOTDEFINED, nil, nil);
          PInteger(Result.FData)^ := StrToInt(FTokenString);
        end
        else
        begin
          Result := FExprNodes.NewNode(enConst, ftFloat, SizeOf(Double), toNOTDEFINED, nil, nil);
          PDouble(Result.FData)^ := StrToFloat(FTokenString);
        end;
      end;
    etCharLiteral:
      begin
        Result := FExprNodes.NewNode(enConst, ftString, Length(FTokenString) + 1, toNOTDEFINED, nil, nil);
        {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrPCopy(Result.FData, AnsiString(FTokenString));
        Result.FPartialLength := Pos('*', string(Result.FData)) - 1;
      end;
    etLParen:
      begin
        GetNextToken;
        Result := ParseExpr;
        if FToken <> etRParen then DatabaseErrorFmt(SExprNoRParen, [TokenName]);
      end;
  else
    DatabaseErrorFmt(SExprExpected, [TokenName]);
    Result := nil;
  end;
  GetNextToken;
end;

procedure TFilterParser.TypeCheckArithOp(Node: TExprNode);

  function CompareNumTypePRI(DataType1, DataType2: TFieldType): Integer;
  var
    Value: array[TFieldType] of Integer;
  begin
    Value[ftSmallInt] := 1;
    Value[ftWord] := 2;
    Value[ftInteger] := 3;
    Value[ftAutoInc] := 3;
    Value[ftLargeInt] := 4;
    Value[ftFloat] := 5;
    Value[ftCurrency] := 6;

    if Value[DataType1] > Value[DataType2] then
      Result := 1
    else if Value[DataType1] < Value[DataType2] then
      Result := -1
    else
      Result := 0;
  end;

var
  Match: Boolean;
  CompResult: Integer;
begin
  Match := True;
  with Node do
  begin
    if FLeft.IsNumericType then
    begin
      if FRight.IsNumericType then
      begin
        CompResult := CompareNumTypePRI(FLeft.FDataType, FRight.FDataType);
        if CompResult >= 0 then
        begin
          FDataType := FLeft.FDataType;
          DataSize := GetFieldSize(FLeft.FDataType);
        end
        else
        begin
          FDataType := FRight.FDataType;
          DataSize := GetFieldSize(FRight.FDataType);
        end;
        if FDataType in [ftSmallInt, ftWord] then
        begin
          FDataType := ftInteger;
          DataSize := SizeOf(Integer);
        end;

        if FOperator = toDIV then
        begin
          FDataType := ftFloat;
          DataSize := SizeOf(Double);
        end;
      end
      else
        Match := False;
    end
    else
    if FLeft.IsTemporalType then
    begin
      if FOperator = toSUB then
      begin
        if FRight.IsTemporalStringType then
          FRight.ConvertStringToDateTime(FLeft.FDataType);

        if (FLeft.FDataType = ftDate) and
           ((FRight.FDataType = ftDate) or FRight.IsNumericType)
        then
        begin
          FDataType := ftInteger;
          DataSize := SizeOf(Integer);
        end
        else
        if (FLeft.FDataType = ftTime) and
           ((FRight.FDataType = ftTime) or FRight.IsNumericType)
        then
        begin
          FDataType := ftInteger;
          DataSize := SizeOf(Integer);
        end
        else
        if FRight.IsTemporalType or FRight.IsNumericType then
        begin
          FDataType := ftFloat;
          DataSize := SizeOf(Double);
        end
        else
          Match := False;
      end
      else
      if FRight.IsNumericType and (FOperator = toADD) then
      begin
        FDataType := FLeft.FDataType;
        DataSize := GetFieldSize(FLeft.FDataType);
      end
      else
        Match := False;
    end
    else
    if FLeft.IsStringType then
    begin
      if FRight.IsStringType and (FOperator = toADD) then
        FDataType := ftString
      else
      if FLeft.IsTemporalStringType and FRight.IsTemporalType and (FOperator = toSUB) then
      begin
        FLeft.ConvertStringToDateTime(ftDateTime);
        FDataType := ftFloat;
        DataSize := SizeOf(Double);
      end
      else
        Match := False;
    end
    else
      Match := False;
  end;
  if not Match then
    DatabaseError(SExprTypeMis);
end;

procedure TFilterParser.TypeCheckLogicOp(Node: TExprNode);
begin
  with Node do
  begin
    if FLeft <> nil then
      if not FLeft.IsBooleanType then
        DatabaseError(SExprTypeMis);
    if FRight <> nil then
      if not FRight.IsBooleanType then
        DatabaseError(SExprTypeMis);
  end;
end;

procedure TFilterParser.TypeCheckInOp(Node: TExprNode);
var
  I: Integer;
  TempNode: TExprNode;
  Match: Boolean;
begin
  Match := True;
  with Node do
  begin
    for I := 0 to FArgs.Count - 1 do
    begin
      TempNode := TExprNode(FArgs[I]);
      if FLeft.IsNumericType then
      begin
        if not TempNode.IsNumericType then
          Match := False;
      end
      else
      if FLeft.IsStringType then
      begin
        if not TempNode.IsStringType then
          Match := False;
      end
      else
      if FLeft.IsTemporalType then
      begin
        if TempNode.IsTemporalStringType then
          TempNode.ConvertStringToDateTime(FLeft.FDataType)
        else
          if not TempNode.IsTemporalType then
            Match := False;
      end
      else
      if FLeft.IsBooleanType then
      begin
        if not TempNode.IsBooleanType then
          Match := False;
      end;
      if not Match then
        Break;
    end;
  end;
  if not Match then
    DatabaseError(SExprTypeMis);
end;

procedure TFilterParser.TypeCheckRelationOp(Node: TExprNode);
var
  Match: Boolean;
begin
  Match := True;
  with Node do
  begin
    if FLeft.IsNumericType then
    begin
      if not FRight.IsNumericType then
        Match := False;
    end
    else
    if FLeft.IsTemporalType then
    begin
      if FRight.IsTemporalStringType then
        FRight.ConvertStringToDateTime(FLeft.FDataType)
      else
        if not FRight.IsTemporalType then
          Match := False;
    end
    else
    if FLeft.IsStringType then
    begin
      if FRight.IsTemporalType and FLeft.IsTemporalStringType then
        FLeft.ConvertStringToDateTime(FRight.FDataType)
      else
        if not FRight.IsStringType then
          Match := False;
    end
    else
    if FLeft.IsBooleanType then
    begin
      if not FRight.IsBooleanType then
        Match := False;
      if (FOperator <> toEQ) or (FOperator <> toNE) then
        Match := False;
    end
    else
      Match := False;
  end;
  if not Match then
    DatabaseError(SExprTypeMis);
end;

procedure TFilterParser.TypeCheckLikeOp(Node: TExprNode);
begin
  with Node do
  begin
    if not FLeft.IsStringType or not FRight.IsStringType then
      DatabaseError(SExprTypeMis);
  end;
end;

procedure TFilterParser.TypeCheckFunction(Node: TExprNode);
begin
  with Node do
  begin
    case FFunction of
      tfUpper, tfLower,
      tfTrim, tfTrimLeft, tfTrimRight:
        begin
          if (FArgs = nil) or (FArgs.Count <> 1) then
            DatabaseError(SExprTypeMis);
          if not TExprNode(FArgs[0]).IsStringType then
            DatabaseError(SExprTypeMis);
          FDataType := ftString;
        end;
      tfSubString:
        begin
          if (FArgs = nil) or (FArgs.Count <> 3) then
            DatabaseError(SExprTypeMis);
          if not (TExprNode(FArgs[0]).IsStringType and
                  TExprNode(FArgs[1]).IsIntegerType and
                  (TExprNode(FArgs[1]).FKind = enConst) and
                  TExprNode(FArgs[2]).IsIntegerType and
                  (TExprNode(FArgs[2]).FKind = enConst) )
          then
            DatabaseError(SExprTypeMis);
          FDataType := ftString;
        end;
      tfYear, tfMonth, tfDay:
        begin
          if (FArgs = nil) or (FArgs.Count <> 1) then
            DatabaseError(SExprTypeMis);
          if not ((TExprNode(FArgs[0]).FDataType = ftDate)
            or (TExprNode(FArgs[0]).FDataType = ftDateTime))
          then
            DatabaseError(SExprTypeMis);
          FDataType := ftInteger;
          DataSize := SizeOf(Integer);
        end;
      tfHour, tfMinute, tfSecond:
        begin
          if (FArgs = nil) or (FArgs.Count <> 1) then
            DatabaseError(SExprTypeMis);
          if not ((TExprNode(FArgs[0]).FDataType = ftTime) or
                  (TExprNode(FArgs[0]).FDataType = ftDateTime))
          then
            DatabaseError(SExprTypeMis);
          FDataType := ftInteger;
          DataSize := SizeOf(Integer);
        end;
      tfGetDate:
        begin
          if (FArgs <> nil) and (FArgs.Count <> 0) then
            DatabaseError(SExprTypeMis);
          FDataType := ftDateTime;
          DataSize := SizeOf(Double);
          FKind := enConst;
          PDouble(FData)^ := TimeStampToMSecs(DateTimeToTimeStamp(Now));
        end;
    end;
  end;
end;

{$IFDEF __SQL}

{ TSQLWhereExprParser }

constructor TSQLWhereExprParser.Create(ADataSet: TTDEDataSet);
begin
  inherited;
end;

function TSQLWhereExprParser.GetFieldValue(const Name: string): Variant;
begin

end;

{ TSQLParserBase }

constructor TSQLParserBase.Create(AQuery: TTinyQuery);
begin
  inherited Create;
  FQuery := AQuery;
  FRowsAffected := 0;
end;

destructor TSQLParserBase.Destroy;
begin
  inherited;
end;

function TSQLParserBase.SkipBeforeGetToken(Pos: PChar): PChar;
var
  P: PChar;
begin
  P := inherited SkipBeforeGetToken(Pos);
  {Deal with SQL Notes sign "--"}
  if (P^ <> #0) and (P^ = '-') and (P[1] <> #0) and (P[1] = '-')then
  begin
    P := P + 2;
    while (P^ <> #0) and (P^ <> #13) and (P^ <> #10) do Inc(P);
  end;
  while (P^ <> #0) and (P^ <= ' ') do Inc(P);
  Result := P;
end;

function TSQLParserBase.InternalGetNextToken(Pos: PChar): PChar;
var
  P, TokenStart: PChar;
  L: Integer;
  StrBuf: array[0..255] of Char;
begin
  P := Pos;
  case P^ of
    'A'..'Z', 'a'..'z', '_', #$81..#$fe:
      begin
        TokenStart := P;
        if not SysLocale.FarEast then
        begin
          Inc(P);
          while CharInSet(P^, ccCSet) do
            Inc(P);
        end
        else
          Skip(P, ccCSet);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etSymbol;
      end;
    '[':
      begin
        Inc(P);
        TokenStart := P;
        P := AnsiStrScan(P, ']');
        if P = nil then DatabaseError(SExprNameError);
        SetString(FTokenString, TokenStart, P - TokenStart);
        FToken := etName;
        Inc(P);
      end;
    '''':
      begin
        Inc(P);
        L := 0;
        while True do
        begin
          if P^ = #0 then DatabaseError(SExprStringError);
          if P^ = '''' then
          begin
            Inc(P);
            if P^ <> '''' then Break;
          end;
          if L < {$IFDEF UNICODE}Length{$ELSE}SizeOF{$ENDIF}(StrBuf) then
          begin
            StrBuf[L] := P^;
            Inc(L);
          end;
          Inc(P);
        end;
        SetString(FTokenString, StrBuf, L);
        FToken := etCharLiteral;
      end;
    '-', '0'..'9':
      begin
        if (P^ = '-') and (FPrevToken in [etCharLiteral,etNumLiteral,etName,etSymbol,etRParen]) then
        begin
          FToken := etSUB;
          Inc(P);
        end else
        begin
          TokenStart := P;
          Inc(P);
          while CharInSet(P^, ['0'..'9', {$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}DecimalSeparator, 'e', 'E', '+', '-']) do
          begin
            if CharInSet(P^, ['+', '-']) and not CharInSet((P-1)^, ['e', 'E']) and (P <> TokenStart) then
              Break;
            Inc(P);
          end;
          if ((P-1)^ = ',') and ({$IFDEF DELPHI_15_UP}FormatSettings.{$ENDIF}DecimalSeparator = ',') and (P^ = ' ') then Dec(P);
          SetString(FTokenString, TokenStart, P - TokenStart);
          FToken := etNumLiteral;
        end;
      end;
    '(':
      begin
        Inc(P);
        FToken := etLParen;
      end;
    ')':
      begin
        Inc(P);
        FToken := etRParen;
      end;
    '*':
      begin
        Inc(P);
        FToken := etAsterisk;
      end;
    ',':
      begin
        Inc(P);
        FToken := etComma;
      end;
    #0:
      FToken := etEnd;
  else
    DatabaseErrorFmt(SSQLInvalidChar, [P^]);
  end;
  Result := P;
end;

procedure TSQLParserBase.Parse(const ASQL: string);
begin
  Text := ASQL;
end;

{ TSQLSelectParser }

constructor TSQLSelectParser.Create(AQuery: TTinyQuery);
begin
  inherited;
  FWhereExprParser := TSQLWhereExprParser.Create(AQuery);
end;

destructor TSQLSelectParser.Destroy;
begin
  FreeAndNil(FWhereExprParser);
  inherited;
end;

function TSQLSelectParser.ParseFrom: PChar;

  function CheckOutOfSection: Boolean;
  begin
    Result := TokenSymbolIs('WHERE') or TokenSymbolIs('ORDER') or (FToken = etEnd);
  end;

var
  TableName, AliasTableName: string;
begin
  Rewind;
  while not TokenSymbolIs('FROM') and (FToken <> etEnd) do GetNextToken;

  if TokenSymbolIs('FROM') then
  begin
    GetNextToken;
    while True do
    begin
      TableName := '';
      AliasTableName := '';

      if CheckOutOfSection then
      begin
        if Length(FFromItems) = 0 then
          DatabaseErrorFmt(SSQLInvalid, [Text]);
        Break;
      end;

      if (FToken = etSymbol) or (FToken = etName) then
        TableName := FTokenString
      else
        DatabaseErrorFmt(SSQLInvalid, [Text]);

      GetNextToken;
      if not CheckOutOfSection then
      begin
        if TokenSymbolIs('AS') then
        begin
          GetNextToken;
          if CheckOutOfSection then
            DatabaseErrorFmt(SSQLInvalid, [Text]);
          if (FToken = etSymbol) or (FToken = etName) then
          begin
            AliasTableName := FTokenString;
            GetNextToken;
          end else
            DatabaseErrorFmt(SSQLInvalid, [Text]);
        end
        else if (FToken = etSymbol) or (FToken = etName) then
        begin
          AliasTableName := FTokenString;
          GetNextToken;
        end;
      end;

      if FToken = etComma then
      begin
        GetNextToken;
        if CheckOutOfSection then
          DatabaseErrorFmt(SSQLInvalid, [Text]);
      end else
      begin
        if not CheckOutOfSection then
          DatabaseErrorFmt(SSQLInvalid, [Text]);
      end;

      if AliasTableName = '' then AliasTableName := TableName;
      SetLength(FFromItems, Length(FFromItems) + 1);
      with FFromItems[High(FFromItems)] do
      begin
        RealName := TableName;
        AliasName := AliasTableName;
      end;
    end;

    //for I := 0 to High(FFromItems) do
    //  Showmessage(FFromItems[i].Realname + ',' + FFromItems[i].Aliasname);
  end
  else
  begin
    DatabaseErrorFmt(SSQLInvalid, [Text]);
  end;
  Result := FSourcePtr;
end;

function TSQLSelectParser.ParseSelect: PChar;
begin
  Rewind;
  GetNextToken; // 'SELECT'
  GetNextToken;
  {With "TOP" clause}
  if TokenSymbolIs('TOP') then
  begin
    GetNextToken;
    if FToken = etNumLiteral then
    begin
      FTopNum := StrToInt(FTokenString);
      if FTopNum <= 0 then
        DatabaseErrorFmt(SSQLInvalid, [Text]);
    end
    else
      DatabaseErrorFmt(SSQLInvalid, [Text]);
    GetNextToken;
  end;
  { TODO : uncompleted}
  Result := FSourcePtr;
end;

procedure TSQLSelectParser.Parse(const ASQL: string);
begin
  inherited;
  SetLength(FSelectItems, 0);
  SetLength(FFromItems, 0);
  SetLength(FOrderByItems, 0);
  FTopNum := -1;

  GetNextToken;
  ParseFrom;
  ParseSelect;
end;

procedure TSQLSelectParser.Execute;
begin
  // showmessage('test');
end;

{ TSQLParser }

constructor TSQLParser.Create(AQuery: TTinyQuery);
begin
  inherited;
  FSQLType := stNONE;
end;

destructor TSQLParser.Destroy;
begin
  inherited;
end;

procedure TSQLParser.Parse(const ASQL: string);
begin
  inherited;
  GetNextToken;
  if TokenSymbolIs('SELECT') then
    FSQLType := stSELECT
  else
  if TokenSymbolIs('INSERT') then
    FSQLType := stINSERT
  else
  if TokenSymbolIs('DELETE') then
    FSQLType := stDELETE
  else
  if TokenSymbolIs('UPDATE') then
    FSQLType := stUPDATE
  else
  begin
    FSQLType := stNONE;
    DatabaseErrorFmt(SSQLInvalid, [ASQL]);
  end;
end;

procedure TSQLParser.Execute;
var
  SQLParser: TSQLParserBase;
begin
  case FSQLType of
    stSELECT:  SQLParser := TSQLSelectParser.Create(FQuery);
    //stINSERT:  SQLParser := TSQLInsertParser.Create(FQuery);
    //stDELETE:  SQLParser := TSQLDeleteParser.Create(FQuery);
    //stUPDATE:  SQLParser := TSQLUpdateParser.Create(FQuery);
  else
    SQLParser := nil;
  end;
  try
    if SQLParser <> nil then
    begin
      SQLParser.Parse(Text);
      SQLParser.Execute;
      FRowsAffected := SQLParser.RowsAffected;
    end;
  finally
    SQLParser.Free;
  end;
end;

{ TRecordsMap }

constructor TRecordsMap.Create;
begin
  FList := TList.Create;
  FByIndexIdx := -1;
end;

destructor TRecordsMap.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

procedure TRecordsMap.Add(Value: Integer);
begin
  FList.Add(Pointer(Value));
end;

procedure TRecordsMap.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

procedure TRecordsMap.Clear;
begin
  FList.Clear;
end;

procedure TRecordsMap.DoAnd(Right, Result: TRecordsMap);
begin

end;

procedure TRecordsMap.DoOr(Right, Result: TRecordsMap);
begin

end;

procedure TRecordsMap.DoNot(Right, Result: TRecordsMap);
begin

end;

function TRecordsMap.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TRecordsMap.GetItem(Index: Integer): Integer;
begin
  Result := Integer(FList.Items[Index]);
end;

procedure TRecordsMap.SetItem(Index, Value: Integer);
begin
  FList.Items[Index] := Pointer(Value);
end;
{$ENDIF __SQL}

{ TDataProcessAlgo }

constructor TDataProcessAlgo.Create(AOwner: TObject);
begin
  FOwner := AOwner;
end;

destructor TDataProcessAlgo.Destroy;
begin
  inherited;
end;

procedure TDataProcessAlgo.DoEncodeProgress(Percent: Integer);
begin
  if Assigned(FOnEncodeProgress) then
    FOnEncodeProgress(FOwner, Percent);
end;

procedure TDataProcessAlgo.DoDecodeProgress(Percent: Integer);
begin
  if Assigned(FOnDecodeProgress) then
    FOnDecodeProgress(FOwner, Percent);
end;

{ TCompressAlgo }

procedure TCompressAlgo.SetLevel(Value: TCompressLevel);
begin
end;

function TCompressAlgo.GetLevel: TCompressLevel;
begin
  Result := clNormal;
end;

{ TEncryptAlgo }

procedure TEncryptAlgo.DoProgress(Current, Maximal: Integer; Encode: Boolean);
begin
  if Encode then
  begin
    if Maximal = 0 then
      DoEncodeProgress(0)
    else
      DoEncodeProgress(Round(Current / Maximal * 100));
  end
  else
  begin
    if Maximal = 0 then
      DoDecodeProgress(0)
    else
      DoDecodeProgress(Round(Current / Maximal * 100));
  end;
end;

procedure TEncryptAlgo.InternalCodeStream(Source, Dest: TMemoryStream;
  DataSize: Integer; Encode: Boolean);
const
  EncMaxBufSize = 1024 * 4;
type
  TCodeProc = procedure(const Source; var Dest; DataSize: Integer) of object;
var
  Buf: TRecordBuffer;
  SPos: Integer;
  DPos: Integer;
  Len: Integer;
  Proc: TCodeProc;
  Size: Integer;
begin
  if Source = nil then Exit;
  if Encode then Proc := EncodeBuffer else Proc := DecodeBuffer;
  if Dest = nil then Dest := Source;
  if DataSize < 0 then
  begin
    DataSize := Source.Size;
    Source.Position := 0;
  end;
  Buf := nil;
  Size := DataSize;
  DoProgress(0, Size, Encode);
  try
    Buf    := AllocMem(EncMaxBufSize);
    DPos   := Dest.Position;
    SPos   := Source.Position;
    while DataSize > 0 do
    begin
      Source.Position := SPos;
      Len := DataSize;
      if Len > EncMaxBufSize then Len := EncMaxBufSize;
      Len := Source.Read(Buf^, Len);
      SPos := Source.Position;
      if Len <= 0 then Break;
      Proc(Buf^, Buf^, Len);
      Dest.Position := DPos;
      Dest.Write(Buf^, Len);
      DPos := Dest.Position;
      Dec(DataSize, Len);
      DoProgress(Size - DataSize, Size, Encode);
    end;
  finally
    DoProgress(0, 0, Encode);
    ReallocMem(Buf, 0);
  end;
end;

procedure TEncryptAlgo.SetMode(Value: TEncryptMode);
begin
end;

function TEncryptAlgo.GetMode: TEncryptMode;
begin
  Result := FTinyDBDefaultEncMode;
end;

procedure TEncryptAlgo.Done;
begin
end;

procedure TEncryptAlgo.EncodeStream(Source, Dest: TMemoryStream; DataSize: Integer);
begin
  InternalCodeStream(Source, Dest, DataSize, True);
end;

procedure TEncryptAlgo.DecodeStream(Source, Dest: TMemoryStream; DataSize: Integer);
begin
  InternalCodeStream(Source, Dest, DataSize, False);
end;

{ TDataProcessMgr }

constructor TDataProcessMgr.Create(AOwner: TTinyDBFileIO);
begin
  FTinyDBFile := AOwner;
end;

destructor TDataProcessMgr.Destroy;
begin
  FreeAndNil(FDPObject);
  inherited;
end;

class function TDataProcessMgr.CheckAlgoRegistered(const AlgoName: string): Integer;
begin
  Result := -1;
end;

procedure TDataProcessMgr.EncodeStream(Source, Dest: TMemoryStream; DataSize: Integer);
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  FDPObject.EncodeStream(Source, Dest, DataSize);
end;

procedure TDataProcessMgr.DecodeStream(Source, Dest: TMemoryStream; DataSize: Integer);
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  FDPObject.DecodeStream(Source, Dest, DataSize);
end;

{ TCompressMgr }

function TCompressMgr.GetLevel: TCompressLevel;
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  Result := (FDPObject as TCompressAlgo).GetLevel;
end;

procedure TCompressMgr.SetLevel(const Value: TCompressLevel);
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  (FDPObject as TCompressAlgo).SetLevel(Value);
end;

class function TCompressMgr.CheckAlgoRegistered(const AlgoName: string): Integer;
begin
  Result := FCompressClassList.IndexOf(AlgoName);
  if Result = -1 then
    DatabaseErrorFmt(SCompressAlgNotFound, [AlgoName]);
end;

procedure TCompressMgr.SetAlgoName(const Value: string);
var
  I: Integer;
  CmpClass: TCompressAlgoClass;
  NewObj: Boolean;
begin
  I := CheckAlgoRegistered(Value);
  if I >= 0 then
  begin
    CmpClass := Pointer(FCompressClassList.Objects[I]);
    if FDPObject = nil then NewObj := True
    else if FDPObject.ClassType <> CmpClass then NewObj := True
    else NewObj := False;
    if NewObj then
    begin
      FDPObject.Free;
      FDPObject := CmpClass.Create(FTinyDBFile);
      if FTinyDBFile <> nil then
      begin
        FDPObject.OnEncodeProgress := FTinyDBFile.OnCompressProgressEvent;
        FDPObject.OnDecodeProgress := FTinyDBFile.OnUncompressProgressEvent;
      end;
    end;
  end;
end;

{ TEncryptMgr }

function TEncryptMgr.GetMode: TEncryptMode;
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  Result := (FDPObject as TEncryptAlgo).GetMode;
end;

procedure TEncryptMgr.SetMode(const Value: TEncryptMode);
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  (FDPObject as TEncryptAlgo).SetMode(Value);
end;

procedure TEncryptMgr.InitKey(const Key: string);
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  (FDPObject as TEncryptAlgo).InitKey(Key);
end;

procedure TEncryptMgr.Done;
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  (FDPObject as TEncryptAlgo).Done;
end;

class function TEncryptMgr.CheckAlgoRegistered(const AlgoName: string): Integer;
begin
  Result := FEncryptClassList.IndexOf(AlgoName);
  if Result = -1 then
    DatabaseErrorFmt(SEncryptAlgNotFound, [AlgoName]);
end;

procedure TEncryptMgr.SetAlgoName(const Value: string);
var
  I: Integer;
  EncClass: TEncryptAlgoClass;
  NewObj: Boolean;
begin
  I := CheckAlgoRegistered(Value);
  if I >= 0 then
  begin
    EncClass := Pointer(FEncryptClassList.Objects[I]);
    if FDPObject = nil then NewObj := True
    else if FDPObject.ClassType <> EncClass then NewObj := True
    else NewObj := False;
    if NewObj then
    begin
      FDPObject.Free;
      FDPObject := EncClass.Create(FTinyDBFile);
      if FTinyDBFile <> nil then
      begin
        FDPObject.OnEncodeProgress := FTinyDBFile.OnEncryptProgressEvent;
        FDPObject.OnDecodeProgress := FTinyDBFile.OnDecryptProgressEvent;
      end;
    end;
  end;
end;

procedure TEncryptMgr.DecodeStream(Source, Dest: TMemoryStream;
  DataSize: Integer);
begin
  inherited;
  Done;
end;

procedure TEncryptMgr.EncodeStream(Source, Dest: TMemoryStream;
  DataSize: Integer);
begin
  inherited;
  Done;
end;

procedure TEncryptMgr.EncodeBuffer(const Source; var Dest;
  DataSize: Integer);
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  (FDPObject as TEncryptAlgo).EncodeBuffer(Source, Dest, DataSize);
  Done;
end;

procedure TEncryptMgr.DecodeBuffer(const Source; var Dest;
  DataSize: Integer);
begin
  if not Assigned(FDPObject) then DatabaseError(SGeneralError);
  (FDPObject as TEncryptAlgo).DecodeBuffer(Source, Dest, DataSize);
  Done;
end;

{ TFieldBufferItem }

constructor TFieldBufferItem.Create;
begin
  inherited;
  FActive := True;
end;

destructor TFieldBufferItem.Destroy;
begin
  FreeBuffer;
  inherited;
end;

function TFieldBufferItem.GetAsString: string;
var
  TempTimeStamp: TTimeStamp;
begin
  try
    {$IFDEF UNICODE}
    if FFieldType = ftWideString then
      Result := PChar(FBuffer)
    else {$ENDIF} if FFieldType in StringFieldTypes then
      Result := string(PAnsiChar(FBuffer))
    else if FFieldType in BlobFieldTypes then
      Result := string(PAnsiChar(TMemoryStream(FBuffer).Memory))
    else if FFieldType in [ftInteger, ftAutoInc] then
      Result := IntToStr(PInteger(FBuffer)^)
    else if FFieldType in [ftWord] then
      Result := IntToStr(PWord(FBuffer)^)
    else if FFieldType in [ftLargeint] then
      Result := IntToStr(PLargeInt(FBuffer)^)
    else if FFieldType in [ftSmallint] then
      Result := IntToStr(PSmallInt(FBuffer)^)
    else if FFieldType in [ftFloat, ftCurrency] then
      Result := FloatToStr(PDouble(FBuffer)^)
    else if FFieldType in [ftBoolean] then
      if PWordBool(FBuffer)^ then Result := STextTrue
      else Result := STextFalse
    else if FFieldType in [ftDateTime] then
      Result := DateTimeToStr(TimeStampToDateTime(MSecsToTimeStamp(PDouble(FBuffer)^)))
    else if FFieldType in [ftDate] then
    begin
      TempTimeStamp.Date := PInteger(FBuffer)^;
      TempTimeStamp.Time := 0;
      Result := DateToStr(TimeStampToDateTime(TempTimeStamp));
    end
    else if FFieldType in [ftTime] then
    begin
      TempTimeStamp.Date := Trunc(Date);
      TempTimeStamp.Time := PInteger(FBuffer)^;
      Result := TimeToStr(TimeStampToDateTime(TempTimeStamp));
    end
    else
      Result := '';
  except
    Result := '';
  end;
end;

function TFieldBufferItem.GetAsAnsiString: AnsiString;
begin
  Result := AnsiString(GetAsString);
end;

function TFieldBufferItem.GetDataBuf: Pointer;
begin
  if FFieldType in BlobFieldTypes then
    Result := TMemoryStream(FBuffer).Memory
  else
    Result := FBuffer;
end;

procedure TFieldBufferItem.AllocBuffer;
begin
  FreeBuffer;
  if FFieldType in BlobFieldTypes then
    FBuffer := TMemoryStream.Create
  else
    FBuffer := AllocMem(FFieldSize);
  FMemAlloc := True;
end;

procedure TFieldBufferItem.FreeBuffer;
begin
  if FMemAlloc then
  begin
    if FFieldType in BlobFieldTypes then
      TMemoryStream(FBuffer).Free
    else
      FreeMem(FBuffer);
    FBuffer := nil;
    FMemAlloc := False;
  end;
end;

function TFieldBufferItem.IsBlob: Boolean;
begin
  Result := FFieldType in BlobFieldTypes;
end;

{ TFieldBuffers }

constructor TFieldBuffers.Create;
begin
  FItems := TList.Create;
end;

destructor TFieldBuffers.Destroy;
begin
  Clear;
  FreeAndNil(FItems);
  inherited;
end;

function TFieldBuffers.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TFieldBuffers.GetItem(Index: Integer): TFieldBufferItem;
begin
  Result := TFieldBufferItem(FItems[Index]);
end;

procedure TFieldBuffers.Add(FieldType: TFieldType; FieldSize: Integer);
var
  FieldBufferItem: TFieldBufferItem;
begin
  FieldBufferItem := TFieldBufferItem.Create;
  FieldBufferItem.FieldType := FieldType;
  FieldBufferItem.FieldSize := FieldSize;
  FieldBufferItem.AllocBuffer;
  FItems.Add(FieldBufferItem);
end;

procedure TFieldBuffers.Add(Buffer: Pointer; FieldType: TFieldType; FieldSize: Integer);
var
  FieldBufferItem: TFieldBufferItem;
begin
  FieldBufferItem := TFieldBufferItem.Create;
  if FieldType in BlobFieldTypes then
    FieldBufferItem.FBuffer := PMemoryStream(Buffer)^
  else
    FieldBufferItem.FBuffer := Buffer;
  FieldBufferItem.FieldType := FieldType;
  FieldBufferItem.FieldSize := FieldSize;
  FItems.Add(FieldBufferItem);
end;

procedure TFieldBuffers.Delete(Index: Integer);
begin
  TFieldBufferItem(FItems[Index]).Free;
  FItems.Delete(Index);
end;

procedure TFieldBuffers.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TFieldBufferItem(FItems[I]).Free;
  FItems.Clear;
end;

{ TTinyDBFileStream }

constructor TTinyDBFileStream.Create(const FileName: string; Mode: Word; AOffset: Integer);
begin
  inherited Create(FileName, Mode);
  FFlushed := True;
  SetOffset(AOffset);
end;

{$IFDEF COMPILER_6_UP}
constructor TTinyDBFileStream.Create(const FileName: string; Mode: Word; Rights: Cardinal; AOffset: Integer);
begin
  inherited Create(FileName, Mode, Rights);
  FFlushed := True;
  SetOffset(AOffset);
end;
{$ENDIF}

procedure TTinyDBFileStream.SetOffset(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FOffset <> Value then
  begin
    FOffset := Value;
{$IFDEF COMPILER_6_UP}
    Seek(0, soBeginning);
{$ELSE}
    Seek(0, soFromBeginning);
{$ENDIF}
  end;
end;

function TTinyDBFileStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FFlushed := False;
end;

{$IFDEF COMPILER_6_UP}
function TTinyDBFileStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := inherited Seek(Offset + FOffset, Origin);
end;
{$ELSE}
function TTinyDBFileStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := inherited Seek(Offset + FOffset, Origin);
end;
{$ENDIF}

procedure TTinyDBFileStream.Flush;
begin
  FlushFileBuffers(Handle);
  FFlushed := True;
end;

{ TTinyDBFileIO }

constructor TTinyDBFileIO.Create(AOwner: TTinyDatabase);
begin
  FDatabase := AOwner;
  if FDatabase <> nil then
  begin
    FMediumType := FDatabase.MediumType;
    FExclusive := FDatabase.Exclusive;
  end;
  FCompressMgr := TCompressMgr.Create(Self);
  FEncryptMgr := TEncryptMgr.Create(Self);
  InitializeCriticalSection(FDPCSect);
end;

destructor TTinyDBFileIO.Destroy;
begin
  Close;
  FreeAndNil(FCompressMgr);
  FreeAndNil(FEncryptMgr);
  DeleteCriticalSection(FDPCSect);
  inherited;
end;

function TTinyDBFileIO.GetIsOpen: Boolean;
begin
  Result := FDBStream <> nil;
end;

function TTinyDBFileIO.GetFlushed: Boolean;
begin
  if FDBStream is TTinyDBFileStream then
    Result := (FDBStream as TTinyDBFileStream).Flushed
  else
    Result := True;
end;

//-----------------------------------------------------------------------------
// Stream of data on the decoding (decryption and decompression)
// Encrypt: {Whether the decryption}
// Compress: {Does decompression}
//-----------------------------------------------------------------------------
procedure TTinyDBFileIO.DecodeMemoryStream(SrcStream, DstStream: TMemoryStream; Encrypt, Compress: Boolean);
var
  TempStream: TMemoryStream;
begin
  EnterCriticalSection(FDPCSect);
  try
    SrcStream.Position := 0;
    if SrcStream <> DstStream then DstStream.Clear;

    //  If you do not have any treatment
    if not Encrypt and not Compress then
    begin
      if SrcStream <> DstStream then DstStream.CopyFrom(SrcStream, 0);
    end;

    //  Decryption
    if Encrypt then
    begin
      FEncryptMgr.DecodeStream(SrcStream, DstStream, SrcStream.Size);
    end;

    //   Decompression
    if Compress then
    begin
      if Encrypt then
      begin
        TempStream := TMemoryStream.Create;
        TempStream.LoadFromStream(DstStream);
        DstStream.Clear;
      end
      else
        TempStream := SrcStream;
      TempStream.Position := 0;
      try
        FCompressMgr.DecodeStream(TempStream, DstStream, TempStream.Size);
      finally
        if TempStream <> SrcStream then
          TempStream.Free;
      end;
    end;
  finally
    LeaveCriticalSection(FDPCSect);
  end;
end;

//-----------------------------------------------------------------------------
// Buffer the data on the decoding (decryption)
// Encrypt:    {Whether the decryption}
// Note: SrcBuffer, DstBuffer format does not exist, but an ordinary block of memory
//-----------------------------------------------------------------------------
procedure TTinyDBFileIO.DecodeMemoryBuffer(SrcBuffer, DstBuffer: PAnsiChar; DataSize: Integer; Encrypt: Boolean);
begin
  EnterCriticalSection(FDPCSect);
  try
    if Encrypt then
    begin
      FEncryptMgr.DecodeBuffer(SrcBuffer^, DstBuffer^, DataSize);
    end
    else
    begin
      Move(SrcBuffer^, DstBuffer^, DataSize);
    end;
  finally
    LeaveCriticalSection(FDPCSect);
  end;
end;

//-----------------------------------------------------------------------------
// Stream data for encoding (compression and encryption)
// Encrypt:    {Encrypt}
// Compress:   {Is compressed}
//-----------------------------------------------------------------------------
procedure TTinyDBFileIO.EncodeMemoryStream(SrcStream, DstStream: TMemoryStream; Encrypt, Compress: Boolean);
var
  TempStream: TMemoryStream;
begin
  EnterCriticalSection(FDPCSect);
  try
    SrcStream.Position := 0;
    if SrcStream <> DstStream then DstStream.Clear;

    //If you do not have any treatment
    if not Encrypt and not Compress then
    begin
      if SrcStream <> DstStream then DstStream.CopyFrom(SrcStream, 0);
    end;

    //Compression
    if Compress then
    begin
      FCompressMgr.EncodeStream(SrcStream, DstStream, SrcStream.Size)
    end;

    //Encryption
    if Encrypt then
    begin
      if Compress then
      begin
        TempStream := TMemoryStream.Create;
        TempStream.LoadFromStream(DstStream);
        DstStream.Clear;
      end
      else
        TempStream := SrcStream;
      TempStream.Position := 0;
      try
        FEncryptMgr.EncodeStream(TempStream, DstStream, TempStream.Size);
      finally
        if TempStream <> SrcStream then
          TempStream.Free;
      end;
    end;
  finally
    LeaveCriticalSection(FDPCSect);
  end;
end;

//-----------------------------------------------------------------------------
// Of Buffer data is encoded (encrypted)
// Encrypt:    {Encrypt}
// Note: SrcBuffer, DstBuffer format does not exist, but an ordinary block of memory
//-----------------------------------------------------------------------------
procedure TTinyDBFileIO.EncodeMemoryBuffer(SrcBuffer, DstBuffer: PAnsiChar; DataSize: Integer; Encrypt: Boolean);
begin
  EnterCriticalSection(FDPCSect);
  try
    if Encrypt then
    begin
      FEncryptMgr.EncodeBuffer(SrcBuffer^, DstBuffer^, DataSize);
    end
    else
    begin
      Move(SrcBuffer^, DstBuffer^, DataSize);
    end;
  finally
    LeaveCriticalSection(FDPCSect);
  end;
end;

procedure TTinyDBFileIO.OnCompressProgressEvent(Sender: TObject; Percent: Integer);
begin
  if Assigned(FDatabase.FOnCompressProgress) then
    FDatabase.FOnCompressProgress(FDatabase, Percent);
end;

procedure TTinyDBFileIO.OnUncompressProgressEvent(Sender: TObject; Percent: Integer);
begin
  if Assigned(FDatabase.FOnUncompressProgress) then
    FDatabase.FOnUncompressProgress(FDatabase, Percent);
end;

procedure TTinyDBFileIO.OnEncryptProgressEvent(Sender: TObject; Percent: Integer);
begin
  if Assigned(FDatabase.FOnEncryptProgress) then
    FDatabase.FOnEncryptProgress(FDatabase, Percent);
end;

procedure TTinyDBFileIO.OnDecryptProgressEvent(Sender: TObject; Percent: Integer);
begin
  if Assigned(FDatabase.FOnDecryptProgress) then
    FDatabase.FOnDecryptProgress(FDatabase, Percent);
end;

function TTinyDBFileIO.CheckDupTableName(const TableName: string): Boolean;
var
  TableTab: TTableTab;
  TableHeader: TTableHeader;
  I: Integer;
begin
  Result := False;
  ReadTableTab(TableTab);
  for I := 0 to TableTab.TableCount - 1 do
  begin
    ReadTableHeader(I, TableHeader);
    {$IFDEF UNICODE}
    if SysUtils.AnsiCompareText(string(TableHeader.TableName), TableName) = 0 then
    {$ELSE}
    if StrIComp(TableHeader.TableName, PAnsiChar(TableName)) = 0 then
    {$ENDIF}
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TTinyDBFileIO.CheckDupIndexName(var TableHeader: TTableHeader; const IndexName: string): Boolean;
var
  I: Integer;
begin
  for I := 0 to TableHeader.IndexCount - 1 do
  begin
    {$IFDEF UNICODE}
    if SysUtils.AnsiCompareText(string(TableHeader.IndexHeader[I].IndexName), IndexName) = 0 then
    {$ELSE}
    if StrIComp(TableHeader.IndexHeader[I].IndexName, PAnsiChar(IndexName)) = 0 then
    {$ENDIF}
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

//-----------------------------------------------------------------------------
// Check if there are multiple Primary Index
// Return value: there are multiple returns True
//-----------------------------------------------------------------------------
function TTinyDBFileIO.CheckDupPrimaryIndex(var TableHeader: TTableHeader; IndexOptions: TTDIndexOptions): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (tiPrimary in IndexOptions) then
  begin
    for I := 0 to TableHeader.IndexCount - 1 do
    begin
      if tiPrimary in TableHeader.IndexHeader[I].IndexOptions then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;
end;

procedure TTinyDBFileIO.CheckValidFields(Fields: array of TFieldItem);
var
  I, J: Integer;
  AutoIncFieldCount: Integer;
begin
  if Length(Fields) >= tdbMaxField then
    DatabaseError(STooManyFields);
  if Length(Fields) = 0 then
    DatabaseError(SFieldNameExpected);

  for I := 0 to High(Fields) do
  begin
    if not (Fields[I].FieldType in TinyDBFieldTypes) then
      DatabaseErrorFmt(SBadFieldType, [Fields[I].FieldName]);
    if not IsValidDBName(string(Fields[I].FieldName)) then
      DatabaseErrorFmt(SInvalidFieldName, [Fields[I].FieldName]);
    if (Fields[I].FieldType in BlobFieldTypes) then
    begin
      if Fields[I].DataSize <> 0 then DatabaseError(SInvalidFieldSize);
    end
    else
    if (Fields[I].FieldType in StringFieldTypes) then
    begin
      if (Fields[I].DataSize <= 0) or (Fields[I].DataSize > tdbMaxTextFieldSize) then
        DatabaseError(SInvalidFieldSize);
    end;
  end;
  for I := 0 to High(Fields)-1 do
    for J := I+1 to High(Fields) do
    begin
      if SysUtils.AnsiCompareText(string(Fields[I].FieldName), string(Fields[J].FieldName)) = 0 then
        DatabaseErrorFmt(SDuplicateFieldName, [Fields[I].FieldName]);
    end;

  // Check whether it contains multiple AutoInc fields
  AutoIncFieldCount := 0;
  for I := 0 to High(Fields)-1 do
    if Fields[I].FieldType = ftAutoInc then Inc(AutoIncFieldCount);
  if AutoIncFieldCount > 1 then
    DatabaseError(SDuplicateAutoIncField);
end;

procedure TTinyDBFileIO.CheckValidIndexFields(FieldNames: array of string; IndexOptions: TTDIndexOptions; var TableHeader: TTableHeader);
var
  I, J: Integer;
begin
  if Length(FieldNames) = 0 then
    DatabaseError(SFieldNameExpected);
  for I := 0 to High(FieldNames)-1 do
    for J := I+1 to High(FieldNames) do
    begin
      {$IFDEF UNICODE}
      if SysUtils.AnsiCompareText(FieldNames[I], FieldNames[J]) = 0 then
      {$ELSE}
      if StrIComp(PAnsiChar(FieldNames[I]), PAnsiChar(FieldNames[J])) = 0 then
      {$ENDIF}
        DatabaseErrorFmt(SDuplicateFieldName, [FieldNames[I]]);
    end;
  for I := 0 to High(FieldNames) do
  begin
    if GetFieldIdxByName(TableHeader, FieldNames[I]) = -1 then
      DatabaseErrorFmt(SFieldNotFound, [FieldNames[I]]);
  end;
end;

function TTinyDBFileIO.GetFieldIdxByName(const TableHeader: TTableHeader; const FieldName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to TableHeader.FieldCount - 1 do
  begin
    {$IFDEF UNICODE}
    if SysUtils.AnsiCompareText(string(TableHeader.FieldTab[I].FieldName), FieldName) = 0 then
    {$ELSE}
    if StrIComp(TableHeader.FieldTab[I].FieldName, PAnsiChar(FieldName)) = 0 then
    {$ENDIF}
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TTinyDBFileIO.GetIndexIdxByName(const TableHeader: TTableHeader; const IndexName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to TableHeader.IndexCount - 1 do
  begin
    {$IFDEF UNICODE}
    if SysUtils.AnsiCompareText(string(TableHeader.IndexHeader[I].IndexName), IndexName) = 0 then
    {$ELSE}
    if StrIComp(TableHeader.IndexHeader[I].IndexName, PAnsiChar(IndexName)) = 0 then
    {$ENDIF}
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TTinyDBFileIO.GetTableIdxByName(const TableName: string): Integer;
var
  TableHeader: TTableHeader;
  I: Integer;
begin
  Result := -1;
  for I := 0 to FTableTab.TableCount - 1 do
  begin
    ReadTableHeader(I, TableHeader);
    {$IFDEF UNICODE}
    if SysUtils.AnsiCompareText(string(TableHeader.TableName), TableName) = 0 then
    {$ELSE}
    if StrIComp(TableHeader.TableName, PAnsiChar(TableName)) = 0 then
    {$ENDIF}
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TTinyDBFileIO.GetTempFileName: string;
var
  Buf: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, Buf);
  Windows.GetTempFileName(Buf, 'TDB', 0, Buf);
  Result := Buf;
end;

function TTinyDBFileIO.ReCreate(NewCompressBlob: Boolean; NewCompressLevel: TCompressLevel; const NewCompressAlgoName: string;
  NewEncrypt: Boolean; const NewEncAlgoName, OldPassword, NewPassword: string; NewCRC32: Boolean): Boolean;
var
  DstTinyDatabase: TTinyDataBase;
  SrcTinyTable, DstTinyTable: TTinyTable;
  DstFileName: string;
  RealNewEncAlgoName: string;
  Comments: string;
  ExtData: TExtData;
  TableHeader: TTableHeader;
  TableIdx, FieldIdx, IndexIdx, RecIdx: Integer;
  I: Integer;
  FieldNames: array of string;
  FieldItems: array of TFieldItem;
  SaveKeepConn: Boolean;
begin
  if (FStartOffset <> 0) or FReadOnly or FFileIsReadOnly then
  begin
    Result := False;
    Exit;
  end;

  SaveKeepConn := FDatabase.KeepConnection;
  FDatabase.KeepConnection := True;
  try
    DstTinyDatabase := TTinyDatabase.Create(nil);
    SrcTinyTable := TTinyTable.Create(nil);
    DstTinyTable := TTinyTable.Create(nil);
    try
      try
        DstFileName := GetTempFileName;
        // Encryption algorithm to adjust
        RealNewEncAlgoName := NewEncAlgoName;
        if NewEncrypt then
          if FEncryptClassList.IndexOf(NewEncAlgoName) = -1 then
            RealNewEncAlgoName := FTinyDBDefaultEncAlgo;
        // Construction Database
        DstTinyDatabase.CreateDatabase(DstFileName, NewCompressBlob, NewCompressLevel, NewCompressAlgoName,
          NewEncrypt, RealNewEncAlgoName, NewPassword, NewCRC32);
        DstTinyDatabase.DatabaseName := DstFileName;
        DstTinyDatabase.Password := NewPassword;
        DstTinyDatabase.Open;
        GetComments(Comments, OldPassword);
        GetExtData(@ExtData);
        DstTinyDatabase.SetComments(Comments);
        DstTinyDatabase.SetExtData(@ExtData, SizeOf(ExtData));

        SrcTinyTable.DatabaseName := FDatabase.DatabaseName;
        for TableIdx := 0 to FTableTab.TableCount - 1 do
        begin
          ReadTableHeader(TableIdx, TableHeader);
          SrcTinyTable.TableName := string(TableHeader.TableName);
          SrcTinyTable.Password := OldPassword;
          SrcTinyTable.Open;
          try
            // {Construction Table}
            SetLength(FieldItems, SrcTinyTable.FieldDefs.Count);
            for I := 0 to SrcTinyTable.FieldDefs.Count - 1 do
            begin
            FieldItems[I].FieldName := AnsiString(SrcTinyTable.FieldDefs[I].Name);
              FieldItems[I].FieldType := SrcTinyTable.FieldDefs[I].DataType;
              FieldItems[I].DataSize := SrcTinyTable.FieldDefs[I].Size;
            end;
            DstTinyDatabase.CreateTable(SrcTinyTable.TableName, FieldItems);
            // {Indexing}
            for IndexIdx := 0 to SrcTinyTable.FTableIO.IndexDefs.Count - 1 do
            begin
              if SrcTinyTable.FieldDefs[SrcTinyTable.FTableIO.IndexDefs[IndexIdx].FieldIdxes[0]].DataType = ftAutoInc then Continue;
              SetLength(FieldNames, Length(SrcTinyTable.FTableIO.IndexDefs[IndexIdx].FieldIdxes));
              for FieldIdx := 0 to High(SrcTinyTable.FTableIO.IndexDefs[IndexIdx].FieldIdxes) do
              begin
                I := SrcTinyTable.FTableIO.IndexDefs[IndexIdx].FieldIdxes[FieldIdx];
                FieldNames[FieldIdx] := SrcTinyTable.FieldDefs[I].Name;
              end;
              DstTinyDatabase.CreateIndex(SrcTinyTable.TableName, SrcTinyTable.FTableIO.IndexDefs[IndexIdx].Name,
                SrcTinyTable.FTableIO.IndexDefs[IndexIdx].Options, FieldNames);
            end;
          finally
            SrcTinyTable.Close;
          end;
        end;

        // {Copy records}
        DstTinyTable.DatabaseName := DstFileName;
        for TableIdx := 0 to FTableTab.TableCount - 1 do
        begin
          ReadTableHeader(TableIdx, TableHeader);
          SrcTinyTable.TableName := string(TableHeader.TableName);
          SrcTinyTable.Password := OldPassword;
          SrcTinyTable.Open;
          try
            DstTinyTable.TableName := string(TableHeader.TableName);
            DstTinyTable.Password := NewPassword;
            DstTinyTable.Open;
            try
              DstTinyTable.BeginUpdate;
              try
                for RecIdx := 0 to SrcTinyTable.RecordCount - 1 do
                begin
                  DoOperationProgressEvent(FDatabase, RecIdx + 1, SrcTinyTable.RecordCount);
                  DstTinyTable.Append;
                  for I := 0 to SrcTinyTable.Fields.Count - 1 do
                    DstTinyTable.Fields[I].Value := SrcTinyTable.Fields[I].Value;
                  try
                    DstTinyTable.Post;
                  except
                    //catch duplicate field error and etc.
                    //and step over to next record
                    on E: Exception do
                      {ShowMessage(E.Message)};
                  end;
                  SrcTinyTable.Next;
                end;
              finally
                DstTinyTable.EndUpdate;
              end;
            finally             
              DstTinyTable.Close;
            end;
          finally            
            SrcTinyTable.Close;
          end;
        end;
      except
        Result := False;
        Exit;
      end;
    finally
      DstTinyTable.Free;
      SrcTinyTable.Free;
      DstTinyDatabase.Free;
    end;

    Lock;
    try
      Close;
      try
        Result := CopyFile(PChar(DstFileName), PChar(FDatabaseName), False);
        DeleteFile(DstFileName);
      finally
        Open(FDatabaseName, FMediumType, FExclusive, FReadOnly, FStartOffset);
      end;
    finally
      Unlock;
    end;
  finally
    FDatabase.KeepConnection := SaveKeepConn;
  end;
end;

function TTinyDBFileIO.ImportDataset(iName: string; SrcTinyTable: TDataset): Boolean;
var
  DstTinyTable: TTinyTable;
  RecIdx: Integer;
  I: Integer;
  FieldItems: array of TFieldItem;
  SaveKeepConn: Boolean;
begin
  Result := False;
  if FDatabase = nil then Exit; //no session, no content
  if SrcTinyTable = nil then Exit; //no source container

  SaveKeepConn := FDatabase.KeepConnection;
  FDatabase.KeepConnection := True;
  try
    DstTinyTable := TTinyTable.Create(nil);
    try
      try
        if not FDatabase.Connected then
          FDatabase.Connected := True;
        if FDatabase.TableExists(IName) then
          FDatabase.DeleteTable(IName);
        //Table reconstruction
        SetLength(FieldItems, SrcTinyTable.FieldDefs.Count);
        for I := 0 to SrcTinyTable.FieldDefs.Count - 1 do
        begin
          FieldItems[I].FieldName := AnsiString(SrcTinyTable.FieldDefs[I].Name);
          FieldItems[I].FieldType := SrcTinyTable.FieldDefs[I].DataType;
          FieldItems[I].DataSize := SrcTinyTable.FieldDefs[I].Size;
        end;
        FDatabase.CreateTable(iName, FieldItems);
        //open destination "new" table
        DstTinyTable.DatabaseName := FDatabase.DatabaseName;
        DstTinyTable.Password := FDatabase.Password;
        DstTinyTable.TableName := iName;
        DstTinyTable.Open;
        if not DstTinyTable.CanAccess then
        begin
          ShowMessage('Password is incorrect, you cannot open the database.');
          Exit;
        end;
        DstTinyTable.BeginUpdate;
        try
          SrcTinyTable.First;
          RecIdx := 0;
          while not SrcTinyTable.Eof do
          begin
            DoOperationProgressEvent(FDatabase, RecIdx + 1, SrcTinyTable.RecordCount);
            DstTinyTable.Append;
            for I := 0 to SrcTinyTable.Fields.Count - 1 do
              DstTinyTable.Fields[I].Value := SrcTinyTable.Fields[I].Value;
            try
              DstTinyTable.Post;
            except
              //catch duplicate field error and etc.
              //and step over to next record
              on E: Exception do
                {ShowMessage(E.Message)};
            end;
            SrcTinyTable.Next; Inc(RecIdx);
          end;
        finally
          DstTinyTable.EndUpdate;
        end;
        SrcTinyTable.Close;
        DstTinyTable.Close;
        Result := True;
      except
        on E: Exception do
        begin
          ShowMessage(E.Message);
        Result := False;
        Exit;
        end;
      end;
    finally
      DstTinyTable.Free;
    end;
  finally
    FDatabase.KeepConnection := SaveKeepConn;
  end;
end;

function TTinyDBFileIO.ExportDataset(iName: string; DstTinyTable: TDataset; ReCreateStruct: Boolean = True): Boolean;
var
  SrcTinyTable: TTinyTable;
  RecIdx: Integer;
  I: Integer;
  fd: TFieldDef;
begin
  Result := False;
  if FDatabase = nil then Exit; //no session, no content
  if DstTinyTable = nil then Exit; //no destination
  
  if not FDatabase.Connected then Exit;
  SrcTinyTable := TTinyTable.Create(nil);
  try
    try
      if not FDatabase.TableExists(IName) then Exit; //nothing for export
      DstTinyTable.Open;
      SrcTinyTable.DatabaseName := FDatabase.DatabaseName;
      SrcTinyTable.Password := FDatabase.Password;
      SrcTinyTable.TableName := iName;
      //when is sure, structure need not recreate
      //but - as implicit - recreate 1:1 for security always
      if ReCreateStruct then
      begin
        SrcTinyTable.Open;
        if DstTinyTable.FieldDefs.Count > 0 then
          DstTinyTable.FieldDefs.Clear;
        for I := 0 to SrcTinyTable.FieldDefs.Count - 1 do
        begin
          fd := DstTinyTable.FieldDefs.AddFieldDef;
          fd.Name := SrcTinyTable.FieldDefs[I].Name;
          fd.DataType := SrcTinyTable.FieldDefs[I].DataType;
          fd.Size := SrcTinyTable.FieldDefs[I].Size;
        end;
        DstTinyTable.Close;
      end;
      //now poke datas into
      DstTinyTable.Open;
      SrcTinyTable.BeginUpdate;
      SrcTinyTable.First; RecIdx := 0;
      while not SrcTinyTable.Eof do
      begin
        DoOperationProgressEvent(FDatabase, RecIdx + 1, SrcTinyTable.RecordCount);
        DstTinyTable.Append;
        for I := 0 to SrcTinyTable.Fields.Count - 1 do
          DstTinyTable.Fields[I].Value := SrcTinyTable.Fields[I].Value;
        try
          DstTinyTable.Post;
        except
          //catch duplicate field error and etc.
          //and step over to next record
          on E: Exception do
            {ShowMessage(E.Message)};
        end;
        SrcTinyTable.Next; Inc(RecIdx);
      end;
      //** DstTinyTable.Close; no close, lost contect
      SrcTinyTable.EndUpdate;
      SrcTinyTable.Close;
      Result := True;
    except
      on E: Exception do
      begin
        ShowMessage(E.Message);
      Result := False;
      Exit;
      end;
    end;
  finally
    SrcTinyTable.Free;
  end;
end;

procedure TTinyDBFileIO.Open(const ADatabaseName: string; AMediumType: TTinyDBMediumType; AExclusive, AReadOnly: Boolean; AStartOffset: Integer);
var
  DBStream: TStream;
  OpenMode: Word;
begin
  if FDBStream = nil then
  begin
    FMediumType := AMediumType;
    FExclusive := AExclusive;
    FReadOnly := AReadOnly;
    FStartOffset := AStartOffset;
    case AMediumType of
      mtDisk:
        begin
          if not FileExists(ADatabaseName) then
            DatabaseErrorFmt(SFOpenError, [ADatabaseName]);
          if not CheckValidTinyDB(ADatabaseName, AStartOffset) then
            DatabaseErrorFmt(SInvalidDatabase, [ADatabaseName]);
          CheckTinyDBVersion(ADatabaseName, AStartOffset);

          FFileIsReadOnly := GetFileAttributes(PChar(ADatabaseName)) and FILE_ATTRIBUTE_READONLY > 0;
          if FFileIsReadOnly or FReadOnly then OpenMode := fmOpenRead
          else OpenMode := fmOpenReadWrite;
          if AExclusive then OpenMode := OpenMode or fmShareExclusive
          else OpenMode := OpenMode or fmShareDenyNone;
          FDBStream := TTinyDBFileStream.Create(ADatabaseName, OpenMode, AStartOffset);
        end;
      mtMemory:
        begin
          if not IsPointerStr(AnsiString(ADatabaseName)) then
            DatabaseErrorFmt(SInvalidDatabaseName, [ADatabaseName]);
          DBStream := StrToPointer(ADatabaseName);
          if not CheckValidTinyDB(DBStream) then
            DatabaseErrorFmt(SInvalidDatabase, [ADatabaseName]);
          CheckTinyDBVersion(DBStream);

          FDBStream := DBStream;
        end;
    end;
    FDatabaseName := ADatabaseName;
    InitDBOptions;
    InitTableTab;
  end;
end;

procedure TTinyDBFileIO.Close;
begin
  if FMediumType = mtDisk then
    FDBStream.Free;
  FDBStream := nil;
  // FDatabaseName := '';
end;

procedure TTinyDBFileIO.Flush;
begin
  if FDBStream is TTinyDBFileStream then
    (FDBStream as TTinyDBFileStream).Flush;
end;

procedure TTinyDBFileIO.InitDBOptions;
begin
  ReadDBOptions(FDBOptions);
  if FDBOptions.CompressBlob then
    FCompressMgr.SetAlgoName(string(FDBOptions.CompressAlgoName));
  if FDBOptions.Encrypt then
  begin
    FEncryptMgr.SetAlgoName(string(FDBOptions.EncryptAlgoName));
    FEncryptMgr.Mode := FDBOptions.EncryptMode;
  end;
end;

procedure TTinyDBFileIO.InitTableTab;
begin
  ReadTableTab(FTableTab);
end;

procedure TTinyDBFileIO.DoOperationProgressEvent(ADatabase: TTinyDatabase; Pos, Max: Integer);
begin
  if Assigned(ADatabase.FOnOperationProgress) then
  begin
    if Max = 0 then
      ADatabase.FOnOperationProgress(ADatabase, 0)
    else
      ADatabase.FOnOperationProgress(ADatabase, Round(Pos / Max * 100));
  end;
end;

function TTinyDBFileIO.SetPassword(const Value: string): Boolean;
begin
  Result := False;
  if not IsOpen then Exit;

  if FDBOptions.Encrypt then
  begin
    if Hash(FTinyDBCheckPwdHashClass, AnsiString(Value)) = AnsiString(FDBOptions.HashPassword) then
    begin
      FEncryptMgr.InitKey(Value);
      Result := True;
    end
    else
    begin
      Result := False;
    end;
  end
  else
    Result := True;
end;

procedure TTinyDBFileIO.Lock;
var
  LockWaitTime, LockRetryCount: Integer;
  RetryCount: Integer;
begin
  if not IsOpen then Exit;

  LockRetryCount := FDatabase.Session.LockRetryCount;
  LockWaitTime := FDatabase.Session.LockWaitTime;

  case FMediumType of
    mtMemory:
      begin
      end;
    mtDisk:
      begin
        RetryCount := 0;
        while not LockFile((FDBStream as TFileStream).Handle, 1, 0, 1, 0) do
        begin
          Inc(RetryCount);
          if (LockRetryCount <> 0) and (RetryCount > LockRetryCount) then
          begin
            DatabaseError(SWaitForUnlockTimeOut);
          end;
          Sleep(LockWaitTime);
        end;
      end;
  end;
end;

procedure TTinyDBFileIO.Unlock;
begin
  if not IsOpen then Exit;

  case FMediumType of
    mtMemory:
      begin
      end;
    mtDisk:
      begin
        UnlockFile((FDBStream as TFileStream).Handle, 1, 0, 1, 0);
      end;
  end;
end;

procedure TTinyDBFileIO.ReadBuffer(var Buffer; Position, Count: Longint);
begin
  FDBStream.Position := Position;
  FDBStream.Read(Buffer, Count);
end;

procedure TTinyDBFileIO.ReadDBVersion(var Dest: string);
var
  FileHeader: TFileHeader;
begin
  FDBStream.Position := 0;
  FDBStream.Read(FileHeader, SizeOf(FileHeader));
  Dest := string(FileHeader.FileFmtVer);
end;

procedure TTinyDBFileIO.ReadExtDataBlock(var Dest: TExtDataBlock);
begin
  FDBStream.Position := SizeOf(TFileHeader);
  FDBStream.Read(Dest, SizeOf(Dest));
end;

function TTinyDBFileIO.WriteExtDataBlock(var Dest: TExtDataBlock): Boolean;
begin
  FDBStream.Position := SizeOf(TFileHeader);
  Result := FDBStream.Write(Dest, SizeOf(Dest)) = SizeOf(Dest);
end;

procedure TTinyDBFileIO.ReadDBOptions(var Dest: TDBOptions);
begin
  FDBStream.Position := SizeOf(TFileHeader) + SizeOf(TExtDataBlock);
  FDBStream.Read(Dest, SizeOf(Dest));
end;

function TTinyDBFileIO.WriteDBOptions(var Dest: TDBOptions): Boolean;
begin
  FDBStream.Position := SizeOf(TFileHeader) + SizeOf(TExtDataBlock);
  Result := FDBStream.Write(Dest, SizeOf(Dest)) = SizeOf(Dest);
end;

procedure TTinyDBFileIO.ReadTableTab(var Dest: TTableTab);
begin
  FDBStream.Position := SizeOf(TFileHeader) + SizeOf(TExtDataBlock) + SizeOf(TDBOptions);
  FDBStream.Read(Dest, SizeOf(Dest));
end;

function TTinyDBFileIO.WriteTableTab(var Dest: TTableTab): Boolean;
begin
  FDBStream.Position := SizeOf(TFileHeader) + SizeOf(TExtDataBlock) + SizeOf(TDBOptions);
  Result := FDBStream.Write(Dest, SizeOf(Dest)) = SizeOf(Dest);
end;

procedure TTinyDBFileIO.ReadTableHeader(TableIdx: Integer; var Dest: TTableHeader);
begin
  FDBStream.Position := FTableTab.TableHeaderOffset[TableIdx];
  FDBStream.Read(Dest, SizeOf(Dest));
end;

function TTinyDBFileIO.WriteTableHeader(TableIdx: Integer; var Dest: TTableHeader): Boolean;
begin
  FDBStream.Position := FTableTab.TableHeaderOffset[TableIdx];
  Result := FDBStream.Write(Dest, SizeOf(Dest)) = SizeOf(Dest);
end;

class function TTinyDBFileIO.CheckValidTinyDB(ADBStream: TStream; StartOffset: Integer): Boolean;
var
  FileHeader: TFileHeader;
begin
  ADBStream.Position := StartOffset;
  ADBStream.Read(FileHeader, SizeOf(FileHeader));
  Result := FileHeader.SoftName = tdbSoftName;
end;

class function TTinyDBFileIO.CheckValidTinyDB(const FileName: string; StartOffset: Integer): Boolean;
var
  DBStream: TStream;
begin
  DBStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := CheckValidTinyDB(DBStream, StartOffset);
  finally
    DBStream.Free;
  end;
end;

class function TTinyDBFileIO.CheckTinyDBVersion(ADBStream: TStream; StartOffset: Integer): Boolean;
const
  tdbFileFmtVer100 = '1.00';
var
  FileHeader: TFileHeader;
begin
  Result := False;
  ADBStream.Position := StartOffset;
  ADBStream.Read(FileHeader, SizeOf(FileHeader));

  if FileHeader.FileFmtVer = tdbFileFmtVer100 then
    DatabaseError(SInvalidVersion100);

  if FileHeader.FileFmtVer > tdbFileFmtVer then
    DatabaseError(SInvalidVersionTooHigh)
  else
    Result := True;
end;

class function TTinyDBFileIO.CheckTinyDBVersion(const FileName: string; StartOffset: Integer): Boolean;
var
  DBStream: TStream;
begin
  DBStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    Result := CheckTinyDBVersion(DBStream, StartOffset);
  finally
    DBStream.Free;
  end;
end;

procedure TTinyDBFileIO.GetTableNames(List: TStrings);
var
  I: Integer;
  TableHeader: TTableHeader;
begin
  List.BeginUpdate;
  List.Clear;
  for I := 0 to FTableTab.TableCount - 1 do
  begin
    ReadTableHeader(I, TableHeader);
    List.Add(string(TableHeader.TableName));
  end;
  List.EndUpdate;
end;

procedure TTinyDBFileIO.GetFieldNames(const TableName: string; List: TStrings);
var
  I, J: Integer;
  TableHeader: TTableHeader;
begin
  List.BeginUpdate;
  List.Clear;
  for I := 0 to FTableTab.TableCount - 1 do
  begin
    ReadTableHeader(I, TableHeader);
    if SysUtils.AnsiCompareText(TableName, string(TableHeader.TableName)) = 0 then
    begin
      for J := 0 to TableHeader.FieldCount - 1 do
        List.Add(string(TableHeader.FieldTab[J].FieldName));
    end;
  end;
  List.EndUpdate;
end;

procedure TTinyDBFileIO.GetIndexNames(const TableName: string; List: TStrings);
var
  I, J: Integer;
  TableHeader: TTableHeader;
begin
  List.BeginUpdate;
  List.Clear;
  for I := 0 to FTableTab.TableCount - 1 do
  begin
    ReadTableHeader(I, TableHeader);
    if SysUtils.AnsiCompareText(TableName, string(TableHeader.TableName)) = 0 then
    begin
      for J := 0 to TableHeader.IndexCount - 1 do
        List.Add(string(TableHeader.IndexHeader[J].IndexName));
    end;
  end;
  List.EndUpdate;
end;

procedure TTinyDBFileIO.ReadFieldData(DstStream: TMemoryStream; RecTabItemOffset, DiskFieldOffset, FieldSize: Integer;
  IsBlob: Boolean; ShouldEncrypt, ShouldCompress: Boolean);
var
  RecTabItem: TRecordTabItem;
  FieldDataOffset: Integer;
begin
  FDBStream.Position := RecTabItemOffset;
  FDBStream.Read(RecTabItem, SizeOf(RecTabItem));
  FieldDataOffset := RecTabItem.DataOffset + DiskFieldOffset;
  ReadFieldData(DstStream, FieldDataOffset, FieldSize, IsBlob, ShouldEncrypt, ShouldCompress);
end;

procedure TTinyDBFileIO.ReadFieldData(DstStream: TMemoryStream; FieldDataOffset, FieldSize: Integer;
  IsBlob: Boolean; ShouldEncrypt, ShouldCompress: Boolean);
var
  RecBuf: array of AnsiChar;
  BlobOffset, BlobSize: Integer;
  TmpStream: TMemoryStream;
begin
  TmpStream := TMemoryStream.Create;
  try
    // Read the field data
    FDBStream.Position := FieldDataOffset;
    // If the BLOB field
    if IsBlob then
    begin
      SetLength(RecBuf, FieldSize);
      FDBStream.Read(RecBuf[0], FieldSize);
      BlobOffset := PBlobFieldHeader(@RecBuf[0])^.DataOffset;
      BlobSize := PBlobFieldHeader(@RecBuf[0])^.DataSize;
      FDBStream.Position := BlobOffset;
      TmpStream.SetSize(BlobSize);
      FDBStream.Read(TmpStream.Memory^, BlobSize);
      SetLength(RecBuf, 0);
    end
    else
    {when it isn't BLOB field}
    begin
      TmpStream.SetSize(FieldSize);
      FDBStream.Read(TmpStream.Memory^, FieldSize);
    end;

    // Decoding
    DecodeMemoryStream(TmpStream, DstStream, ShouldEncrypt, ShouldCompress);
  finally
    TmpStream.Free;
  end;
end;

//-----------------------------------------------------------------------------
// Read all of the RecTab to Items in and to each of the offset into BlockOffsets
//-----------------------------------------------------------------------------
procedure TTinyDBFileIO.ReadAllRecordTabItems(const TableHeader: TTableHeader;
  var Items: TRecordTabItems; var BlockOffsets: TIntegerAry);
var
  ReadCount, Count: Integer;
  NextOffset: Integer;
  BlockCount, BlockIdx: Integer;
begin
  FDBStream.Position := TableHeader.RecTabOffset;
  BlockCount := TableHeader.RecordTotal div tdbRecTabUnitNum;
  if TableHeader.RecordTotal mod tdbRecTabUnitNum > 0 then Inc(BlockCount);
  SetLength(BlockOffsets, BlockCount);
  SetLength(Items, TableHeader.RecordTotal);
  ReadCount := 0;
  BlockIdx := 0;
  while ReadCount < TableHeader.RecordTotal do
  begin
    BlockOffsets[BlockIdx] := FDBStream.Position;
    Inc(BlockIdx);
    Count := TableHeader.RecordTotal - ReadCount;
    if Count > tdbRecTabUnitNum then Count := tdbRecTabUnitNum;
    FDBStream.Read(NextOffset, SizeOf(Integer));
    FDBStream.Read(Items[ReadCount], SizeOf(TRecordTabItem)*Count);
    FDBStream.Position := NextOffset;
    Inc(ReadCount, Count);
  end;
end;

//-----------------------------------------------------------------------------
// Index No. IndexIdx read the index of all IndexTab to Items in and to each of the offset into BlockOffsets
//-----------------------------------------------------------------------------
procedure TTinyDBFileIO.ReadAllIndexTabItems(const TableHeader: TTableHeader; IndexIdx: Integer;
  var Items: TIndexTabItems; var BlockOffsets: TIntegerAry);
var
  ReadCount, Count: Integer;
  NextOffset: Integer;
  BlockCount, BlockIdx: Integer;
begin
  FDBStream.Position := TableHeader.IndexHeader[IndexIdx].IndexOffset;
  BlockCount := TableHeader.RecordTotal div tdbIdxTabUnitNum;
  if TableHeader.RecordTotal mod tdbIdxTabUnitNum > 0 then Inc(BlockCount);
  SetLength(BlockOffsets, BlockCount);
  SetLength(Items, TableHeader.RecordTotal);
  ReadCount := 0;
  BlockIdx := 0;
  while ReadCount < TableHeader.RecordTotal do
  begin
    BlockOffsets[BlockIdx] := FDBStream.Position;
    Inc(BlockIdx);
    Count := TableHeader.RecordTotal - ReadCount;
    if Count > tdbIdxTabUnitNum then Count := tdbIdxTabUnitNum;
    FDBStream.Read(NextOffset, SizeOf(Integer));
    FDBStream.Read(Items[ReadCount], SizeOf(TIndexTabItem)*Count);
    FDBStream.Position := NextOffset;
    Inc(ReadCount, Count);
  end;
end;

//-----------------------------------------------------------------------------
// To do in the database to delete tag
// RecordIdx: record number 0-based
//-----------------------------------------------------------------------------
procedure TTinyDBFileIO.WriteDeleteFlag(RecTabItemOffset: Integer);
var
  RecTabItem: TRecordTabItem;
  BakFilePos: Integer;
begin
  // Go to do mark the location of
  FDBStream.Position := RecTabItemOffset;
  BakFilePos := FDBStream.Position;
  FDBStream.Read(RecTabItem, SizeOf(RecTabItem));
  RecTabItem.DeleteFlag := True;
  FDBStream.Position := BakFilePos;
  FDBStream.Write(RecTabItem, SizeOf(RecTabItem));
end;

function TTinyDBFileIO.CreateDatabase(const DBFileName: string;
  CompressBlob: Boolean; CompressLevel: TCompressLevel; const CompressAlgoName: string;
  Encrypt: Boolean; const EncryptAlgoName, Password: string; CRC32: Boolean = False): Boolean;
var
  DBStream: TStream;
  FileHeader: TFileHeader;
  ExtDataBlock: TExtDataBlock;
  DBOptions: TDBOptions;
  TableTab: TTableTab;
begin
  Result := True;
  FillChar(FileHeader, SizeOf(FileHeader), 0);
  {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrCopy(PAnsiChar(@FileHeader.SoftName), tdbSoftName);
  {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrCopy(PAnsiChar(@FileHeader.FileFmtVer), tdbFileFmtVer);
  FillChar(ExtDataBlock, SizeOf(ExtDataBlock), 0);
  if Encrypt then
    EncryptBuffer(
      @ExtDataBlock.Comments,
      {$IFDEF UNICODE}Length{$ELSE}SizeOF{$ENDIF}(ExtDataBlock.Comments),
      EncryptAlgoName,
      FTinyDBDefaultEncMode,
      Password);
  FillChar(DBOptions, SizeOf(DBOptions), 0);
  RandomFillBuffer(DBOptions.RandomBuffer, SizeOf(DBOptions.RandomBuffer), 20, 122);

  DBOptions.CompressBlob := CompressBlob;
  DBOptions.CompressLevel := CompressLevel;
  {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(DBOptions.CompressAlgoName, PAnsiChar(AnsiString(CompressAlgoName)), tdbMaxAlgoNameChar);
  DBOptions.Encrypt := Encrypt;
  {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(DBOptions.EncryptAlgoName, PAnsiChar(AnsiString(EncryptAlgoName)), tdbMaxAlgoNameChar);
  DBOptions.EncryptMode := FTinyDBDefaultEncMode;
  {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(DBOptions.HashPassword, PAnsiChar(Hash(FTinyDBCheckPwdHashClass, AnsiString(Password))), tdbMaxHashPwdSize);
  DBOptions.CRC32 := CRC32;

  FillChar(TableTab, SizeOf(TableTab), 0);
  TableTab.TableCount := 0;

  try
    case FMediumType of
      mtDisk:
        DBStream := TFileStream.Create(DBFileName, fmCreate or fmShareDenyNone);
      mtMemory:
        begin
          if (DBFileName <> '') and (IsPointerStr(AnsiString(DBFileName))) then
            DBStream := StrToPointer(DBFileName);
        end
    else
      DBStream := nil;
    end;
    try
      DBStream.Write(FileHeader, SizeOf(FileHeader));
      DBStream.Write(ExtDataBlock, SizeOf(ExtDataBlock));
      DBStream.Write(DBOptions, SizeOf(DBOptions));
      DBStream.Write(TableTab, SizeOf(TableTab));
    finally
      if FMediumType = mtDisk then
        DBStream.Free;
    end;
  except
    Result := False;
  end;
end;

function TTinyDBFileIO.CreateTable(const TableName: string; Fields: array of TFieldItem): Boolean;
var
  TableHeader: TTableHeader;
  I, AutoIncFieldIdx: Integer;
  AutoIncFieldName: string;
begin
  if not IsValidDBName(TableName) then
    DatabaseErrorFmt(SInvalidTableName, [TableName]);
  try
    Lock;
    try
      CheckValidFields(Fields);

      AutoIncFieldIdx := -1;
      ReadTableTab(FTableTab);
      if FTableTab.TableCount >= tdbMaxTable then
        DatabaseError(STooManyTables);
      if CheckDupTableName(TableName) then
        DatabaseErrorFmt(SDuplicateTableName, [TableName]);
      Inc(FTableTab.TableCount);
      FTableTab.TableHeaderOffset[FTableTab.TableCount-1] := FDBStream.Size;
      WriteTableTab(FTableTab);

      FillChar(TableHeader, SizeOf(TableHeader), 0);
      {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(TableHeader.TableName, PAnsiChar(AnsiString(TableName)), tdbMaxTableNameChar);
      TableHeader.RecTabOffset := 0;
      TableHeader.RecordTotal := 0;
      TableHeader.AutoIncCounter := 0;
      TableHeader.FieldCount := Length(Fields);
      for I := 0 to High(Fields) do
      begin
        if Fields[I].FieldType = ftAutoInc then AutoIncFieldIdx := I;
        {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(TableHeader.FieldTab[I].FieldName, PAnsiChar(AnsiString(Fields[I].FieldName)), tdbMaxFieldNameChar);
        TableHeader.FieldTab[I].FieldType := Fields[I].FieldType;
        if Fields[I].FieldType in StringFieldTypes then
          TableHeader.FieldTab[I].FieldSize := Fields[I].DataSize
        else
          TableHeader.FieldTab[I].FieldSize := 0;
        TableHeader.FieldTab[I].DPMode := Fields[I].DPMode;
      end;
      TableHeader.IndexCount := 0;
      FDBStream.Seek(0, soFromEnd);
      I := SizeOf(TableHeader);
      Result := FDBStream.Write(TableHeader, I) = I;
    finally
      Unlock;
    end;

    if AutoIncFieldIdx <> -1 then
    begin
      AutoIncFieldName := string(Fields[AutoIncFieldIdx].FieldName);
      Result := CreateIndex(TableName, AutoIncFieldName, [tiPrimary], [AutoIncFieldName]);
      if not Result then DeleteTable(TableName);
    end;
  except
    Result := False;
  end;
end;

function TTinyDBFileIO.DeleteTable(const TableName: string): Boolean;
var
  TableHeader: TTableHeader;
  I, TableIdx: Integer;
begin
  if not IsValidDBName(TableName) then
    DatabaseErrorFmt(SInvalidTableName, [TableName]);
  Lock;
  try
    ReadTableTab(FTableTab);
    // Finding what you want to delete the table number
    TableIdx := GetTableIdxByName(TableName);
    if TableIdx <> -1 then
      ReadTableHeader(TableIdx, TableHeader);
    if TableIdx = -1 then
      DatabaseErrorFmt(STableNotFound, [TableName]);
    try
      // Remove the table offset information
      for I := TableIdx to FTableTab.TableCount - 2 do
        FTableTab.TableHeaderOffset[I] := FTableTab.TableHeaderOffset[I + 1];
      // Table 1 by the total number of
      Dec(FTableTab.TableCount);
      // Write back to the database
      Result := WriteTableTab(FTableTab);
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

function TTinyDBFileIO.CreateIndex(const TableName, IndexName: string; IndexOptions: TTDIndexOptions; FieldNames: array of string): Boolean;
var
  TableHeader: TTableHeader;
  I: Integer;
  TableIdx, IndexIdx, FieldIdx: Integer;
begin
  if not IsValidDBName(TableName) then
    DatabaseErrorFmt(SInvalidTableName, [TableName]);
  if not IsValidDBName(IndexName) then
    DatabaseErrorFmt(SInvalidIndexName, [IndexName]);

  Lock;
  try
    ReadTableTab(FTableTab);
    // TableName obtained the corresponding TableIdx
    TableIdx := GetTableIdxByName(TableName);
    if TableIdx <> -1 then
      ReadTableHeader(TableIdx, TableHeader);
    // Legal checks
    if TableIdx = -1 then
      DatabaseErrorFmt(STableNotFound, [TableName]);
    if TableHeader.IndexCount >= tdbMaxIndex then
      DatabaseError(STooManyIndexes);
    if CheckDupIndexName(TableHeader, IndexName) then
      DatabaseErrorFmt(SDuplicateIndexName, [IndexName]);
    if CheckDupPrimaryIndex(TableHeader, IndexOptions) then
      DatabaseError(SDuplicatePrimaryIndex);
    if TableHeader.RecordTotal > 0 then
      DatabaseError(SFailToCreateIndex);
    CheckValidIndexFields(FieldNames, IndexOptions, TableHeader);
    try
      // Changes TableHeader
      IndexIdx := TableHeader.IndexCount;
      Inc(TableHeader.IndexCount);
      {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(TableHeader.IndexHeader[IndexIdx].IndexName, PAnsiChar(AnsiString(IndexName)), tdbMaxIndexNameChar);
      TableHeader.IndexHeader[IndexIdx].IndexOptions := IndexOptions;
      for I := 0 to tdbMaxMultiIndexFields - 1 do
      begin
        if I <= High(FieldNames) then
          FieldIdx := GetFieldIdxByName(TableHeader, FieldNames[I])
        else
          FieldIdx := -1;
        TableHeader.IndexHeader[IndexIdx].FieldIdx[I] := FieldIdx;
      end;
      TableHeader.IndexHeader[IndexIdx].IndexOffset := DBStream.Size;
      TableHeader.IndexHeader[IndexIdx].StartIndex := 0;
      // Write-back TableHeader
      Result := WriteTableHeader(TableIdx, TableHeader);
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

function TTinyDBFileIO.DeleteIndex(const TableName, IndexName: string): Boolean;
var
  TableHeader: TTableHeader;
  I: Integer;
  TableIdx, IndexIdx: Integer;
begin
  if not IsValidDBName(TableName) then
    DatabaseErrorFmt(SInvalidTableName, [TableName]);
  if not IsValidDBName(IndexName) then
    DatabaseErrorFmt(SInvalidIndexName, [IndexName]);

  Lock;
  try
    ReadTableTab(FTableTab);
    // TableName obtained the corresponding TableIdx
    TableIdx := GetTableIdxByName(TableName);
    if TableIdx <> -1 then
      ReadTableHeader(TableIdx, TableHeader);
    // Legal checks
    if TableIdx = -1 then
      DatabaseErrorFmt(STableNotFound, [TableName]);

    // Obtain IndexName corresponding IndexIdx
    IndexIdx := GetIndexIdxByName(TableHeader, IndexName);

    // Legal checks
    if IndexIdx = -1 then
      DatabaseErrorFmt(SIndexNotFound, [IndexName]);
    try

      // Remove the index information
      for I := IndexIdx to TableHeader.IndexCount - 2 do
        TableHeader.IndexHeader[I] := TableHeader.IndexHeader[I + 1];

      // Index by a number of
      Dec(TableHeader.IndexCount);

      // Write-back TableHeader
      Result := WriteTableHeader(TableIdx, TableHeader);
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

function TTinyDBFileIO.RenameTable(const OldTableName, NewTableName: string): Boolean;
var
  TableHeader: TTableHeader;
  I, TableIdx: Integer;
begin
  if not IsValidDBName(NewTableName) then
    DatabaseErrorFmt(SInvalidTableName, [NewTableName]);

  Lock;
  try
    ReadTableTab(FTableTab);

    // Find the table number to be renamed
    TableIdx := GetTableIdxByName(OldTableName);
    if TableIdx <> -1 then
      ReadTableHeader(TableIdx, TableHeader);
    if TableIdx = -1 then
      DatabaseErrorFmt(STableNotFound, [OldTableName]);
    I := GetTableIdxByName(NewTableName);
    if (I <> -1) and (I <> TableIdx) then
      DatabaseErrorFmt(SDuplicateTableName, [NewTableName]);
    try
      // Write back to the database
      {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(TableHeader.TableName, PAnsiChar(AnsiString(NewTableName)), tdbMaxTableNameChar);
      Result := WriteTableHeader(TableIdx, TableHeader);
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

function TTinyDBFileIO.RenameField(const TableName, OldFieldName, NewFieldName: string): Boolean;
var
  TableHeader: TTableHeader;
  I, TableIdx, FieldIdx: Integer;
begin
  if not IsValidDBName(NewFieldName) then
    DatabaseErrorFmt(SInvalidFieldName, [NewFieldName]);

  Lock;
  try
    ReadTableTab(FTableTab);

    // Finding what you want to change the table number field names
    TableIdx := GetTableIdxByName(TableName);
    if TableIdx = -1 then
      DatabaseErrorFmt(STableNotFound, [TableName]);
    ReadTableHeader(TableIdx, TableHeader);

    // To modify the number of fields to obtain
    FieldIdx := GetFieldIdxByName(TableHeader, OldFieldName);
    if FieldIdx = -1 then
      DatabaseErrorFmt(SFieldNotFound, [OldFieldName]);
    I := GetFieldIdxByName(TableHeader, NewFieldName);
    if (I <> -1) and (I <> FieldIdx) then
      DatabaseErrorFmt(SDuplicateFieldName, [NewFieldName]);
    try
      // Write back to the database
      {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(TableHeader.FieldTab[FieldIdx].FieldName, PAnsiChar(AnsiString(NewFieldName)), tdbMaxFieldNameChar);
      Result := WriteTableHeader(TableIdx, TableHeader);
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

function TTinyDBFileIO.RenameIndex(const TableName, OldIndexName, NewIndexName: string): Boolean;
var
  TableHeader: TTableHeader;
  I, TableIdx, IndexIdx: Integer;
begin
  if not IsValidDBName(NewIndexName) then
    DatabaseErrorFmt(SInvalidIndexName, [NewIndexName]);

  Lock;
  try
    ReadTableTab(FTableTab);

    // Finding what you want to change the name of the table index number
    TableIdx := GetTableIdxByName(TableName);
    if TableIdx = -1 then
      DatabaseErrorFmt(STableNotFound, [TableName]);
    ReadTableHeader(TableIdx, TableHeader);

    // To obtain an index number to modify
    IndexIdx := GetIndexIdxByName(TableHeader, OldIndexName);
    if IndexIdx = -1 then
      DatabaseErrorFmt(SIndexNotFound, [OldIndexName]);
    I := GetIndexIdxByName(TableHeader, NewIndexName);
    if (I <> -1) and (I <> IndexIdx) then
      DatabaseErrorFmt(SDuplicateIndexName, [NewIndexName]);
    try
      // Write back to the database
      {$IFDEF DELPHI_18_UP}AnsiStrings.{$ENDIF}StrLCopy(TableHeader.IndexHeader[IndexIdx].IndexName, PAnsiChar(AnsiString(NewIndexName)), tdbMaxIndexNameChar);
      Result := WriteTableHeader(TableIdx, TableHeader);
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

function TTinyDBFileIO.Compact(const Password: string): Boolean;
begin
  Result := ReCreate(FDBOptions.CompressBlob, FDBOptions.CompressLevel, string(FDBOptions.CompressAlgoName),
    FDBOptions.Encrypt, string(FDBOptions.EncryptAlgoName), Password, Password, FDBOptions.CRC32);
end;

function TTinyDBFileIO.Repair(const Password: string): Boolean;
begin
  Result := ReCreate(FDBOptions.CompressBlob, FDBOptions.CompressLevel, string(FDBOptions.CompressAlgoName),
    FDBOptions.Encrypt, string(FDBOptions.EncryptAlgoName), Password, Password, FDBOptions.CRC32);
end;

function TTinyDBFileIO.ChangePassword(const OldPassword, NewPassword: string; Check: Boolean = True): Boolean;
begin
  Result := ReCreate(FDBOptions.CompressBlob, FDBOptions.CompressLevel, string(FDBOptions.CompressAlgoName),
    Check, string(FDBOptions.EncryptAlgoName), OldPassword, NewPassword, FDBOptions.CRC32);
end;

function TTinyDBFileIO.ChangeEncrypt(NewEncrypt: Boolean; const NewEncAlgo, OldPassword, NewPassword: string): Boolean;
begin
  Result := ReCreate(FDBOptions.CompressBlob, FDBOptions.CompressLevel, string(FDBOptions.CompressAlgoName),
    NewEncrypt, NewEncAlgo, OldPassword, NewPassword, FDBOptions.CRC32);
end;

function TTinyDBFileIO.SetComments(const Value: string; const Password: string): Boolean;
var
  ExtDataBlock: TExtDataBlock;
  {$IFDEF UNICODE}
  S: AnsiString;
  {$ENDIF}
begin
  Lock;
  try
    //Result := True;
    try
      ReadExtDataBlock(ExtDataBlock);
      if FDBOptions.Encrypt then
        DecryptBuffer(
          {fix}@ExtDataBlock.Comments,
          SizeOf(ExtDataBlock.Comments),
          string(FDBOptions.EncryptAlgoName),
          FTinyDBDefaultEncMode,
          Password);
      {$IFDEF UNICODE}
      FillChar(ExtDataBlock.Comments, SizeOf(ExtDataBlock.Comments), 0);
      S := Copy(AnsiString(Value), 1, tdbMaxCommentsChar);
      Move(S[1], ExtDataBlock.Comments, Length(S)); {only up to tdbMaxCommentsChar length}
      {$ELSE}
      StrLCopy(ExtDataBlock.Comments, PAnsiChar(Value), tdbMaxCommentsChar);
      {$ENDIF}
      if FDBOptions.Encrypt then
        EncryptBuffer(@ExtDataBlock.Comments,
          SizeOf(ExtDataBlock.Comments),
          string(FDBOptions.EncryptAlgoName),
          FTinyDBDefaultEncMode,
          Password);
      Result := WriteExtDataBlock(ExtDataBlock);
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

function TTinyDBFileIO.GetComments(var Value: string; const Password: string): Boolean;
var
  ExtDataBlock: TExtDataBlock;
begin
  Lock;
  try
    Result := True;
    try
      ReadExtDataBlock(ExtDataBlock);
      if FDBOptions.Encrypt then
        DecryptBuffer(
          {fix}@ExtDataBlock.Comments,
          SizeOf(ExtDataBlock.Comments),
          string(FDBOptions.EncryptAlgoName),
          FTinyDBDefaultEncMode,
          Password);
      Value := string(ExtDataBlock.Comments);
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

function TTinyDBFileIO.SetExtData(Buffer: PAnsiChar; Size: Integer): Boolean;
var
  ExtDataBlock: TExtDataBlock;
begin
  Lock;
  try
    //Result := True;
    try
      ReadExtDataBlock(ExtDataBlock);
      FillChar(ExtDataBlock.Data[0], SizeOf(ExtDataBlock.Data), 0);
      if Size > SizeOf(ExtDataBlock.Data) then
        Size := SizeOf(ExtDataBlock.Data);
      Move(Buffer^, ExtDataBlock.Data[0], Size);
      Result := WriteExtDataBlock(ExtDataBlock);
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

function TTinyDBFileIO.GetExtData(Buffer: PAnsiChar): Boolean;
var
  ExtDataBlock: TExtDataBlock;
begin
  Lock;
  try
    Result := True;
    try
      ReadExtDataBlock(ExtDataBlock);
      Move(ExtDataBlock.Data[0], Buffer^, SizeOf(ExtDataBlock.Data));
    except
      Result := False;
    end;
  finally
    Unlock;
  end;
end;

{ TTinyTableIO }

constructor TTinyTableIO.Create(AOwner: TTinyDatabase);
begin
  FDatabase := AOwner;
  FTableIdx := -1;
  FIndexDefs := TTinyIndexDefs.Create(AOwner);
  FFieldDefs := TTinyFieldDefs.Create(AOwner);
end;

destructor TTinyTableIO.Destroy;
begin
  Finalize;
  FreeAndNil(FIndexDefs);
  FreeAndNil(FFieldDefs);
  inherited;
end;

procedure TTinyTableIO.SetActive(Value: Boolean);
begin
  if Value <> Active then
  begin
    if Value then Open
    else Close;
  end;
end;

procedure TTinyTableIO.SetTableName(const Value: string);
begin
  FTableName := Value;
end;

function TTinyTableIO.GetActive: Boolean;
begin
  Result := FRefCount > 0;
end;

function TTinyTableIO.GetRecTabList(Index: Integer): TList;
begin
  Result := FRecTabLists[Index];
end;

function TTinyTableIO.GetTableIdxByName(const TableName: string): Integer;
var
  List: TStrings;
begin
  List := TStringList.Create;
  try
    FDatabase.GetTableNames(List);
    Result := List.IndexOf(TableName);
  finally
    List.Free;
  end;
end;

procedure TTinyTableIO.InitFieldDefs;
var
  I: Integer;
  FieldDefItem: TTinyFieldDef;
begin
  FFieldDefs.Clear;
  for I := 0 to FTableHeader.FieldCount - 1 do
  begin
    FieldDefItem := TTinyFieldDef(FFieldDefs.Add);
    FieldDefItem.Name := string(FTableHeader.FieldTab[I].FieldName);
    FieldDefItem.FFieldType := FTableHeader.FieldTab[I].FieldType;
    FieldDefItem.FFieldSize := FTableHeader.FieldTab[I].FieldSize;
    FieldDefItem.FDPMode := FTableHeader.FieldTab[I].DPMode;
  end;
end;

procedure TTinyTableIO.InitIndexDefs;
var
  I, K: Integer;
  IdxFieldCount: Integer;
  IndexDefItem: TTinyIndexDef;
begin
  FIndexDefs.Clear;
  for I := 0 to FTableHeader.IndexCount - 1 do
  begin
    IndexDefItem := TTinyIndexDef(FIndexDefs.Add);
    IndexDefItem.Name := string(FTableHeader.IndexHeader[I].IndexName);
    IndexDefItem.Options := FTableHeader.IndexHeader[I].IndexOptions;
    IdxFieldCount := 0;
    for K := 0 to tdbMaxMultiIndexFields - 1 do
    begin
      if FTableHeader.IndexHeader[I].FieldIdx[K] = -1 then Break;
      Inc(IdxFieldCount);
    end;
    SetLength(IndexDefItem.FFieldIdxes, IdxFieldCount);
    for K := 0 to tdbMaxMultiIndexFields - 1 do
    begin
      if FTableHeader.IndexHeader[I].FieldIdx[K] = -1 then Break;
      IndexDefItem.FFieldIdxes[K] := FTableHeader.IndexHeader[I].FieldIdx[K];
    end;
  end;
end;

procedure TTinyTableIO.InitRecTabList(ListIdx: Integer; ReadRecTabItems: Boolean);
var
  IndexTab: TIndexTabItems;
  MemRecTabItem: TMemRecTabItem;
  IndexOffset, RecordTotal: Integer;
  IndexIdx, Count, I: Integer;
begin
  if FRecTabLists[ListIdx] <> nil then Exit;

  if ReadRecTabItems then
    FDatabase.DBFileIO.ReadAllRecordTabItems(FTableHeader, FInitRecordTab, FRecTabBlockOffsets);

  RecordTotal := FTableHeader.RecordTotal;
  FRecTabLists[ListIdx] := TList.Create;
  IndexIdx := ListIdx - 1;
  // The physical order of records set initialization pointers
  if IndexIdx = -1 then
  begin
    for I := 0 to High(FInitRecordTab) do
    begin
      if not FInitRecordTab[I].DeleteFlag then
      begin
        MemRecTabItem.DataOffset := FInitRecordTab[I].DataOffset;
        MemRecTabItem.RecIndex := I;
        AddMemRecTabItem(FRecTabLists[ListIdx], MemRecTabItem);
      end;
    end;
  end
  else
  begin
    //Initialize index
    IndexOffset := FTableHeader.IndexHeader[IndexIdx].IndexOffset;
    if IndexOffset <> 0 then
    begin
      // The index data read into the
      FDatabase.DBFileIO.ReadAllIndexTabItems(FTableHeader, IndexIdx, IndexTab, FIdxTabBlockOffsets[IndexIdx]);
      // According to the order list and so finishing the FRecTabLists [ListIdx] in the
      Count := 0;
      I := FTableHeader.IndexHeader[IndexIdx].StartIndex;
      while (I <> -1) and (Count < RecordTotal) do
      begin
        // Remove the tag marked records do not read into the FIdxTabLists
        if not FInitRecordTab[I].DeleteFlag then
        begin
          MemRecTabItem.DataOffset := FInitRecordTab[I].DataOffset;
          MemRecTabItem.RecIndex := I;
          AddMemRecTabItem(FRecTabLists[ListIdx], MemRecTabItem);
        end;
        Inc(Count);
        I := IndexTab[I].Next;
      end;
    end;
  end;
end;

procedure TTinyTableIO.InitAllRecTabLists;
var
  ListIdx, IndexCount: Integer;
begin
  FDatabase.DBFileIO.ReadAllRecordTabItems(FTableHeader, FInitRecordTab, FRecTabBlockOffsets);

  IndexCount := FTableHeader.IndexCount;
  SetLength(FRecTabLists, IndexCount + 1);

  for ListIdx := 0 to High(FRecTabLists) do
  begin
    InitRecTabList(ListIdx, False);
  end;
end;

procedure TTinyTableIO.InitDiskRecInfo;
var
  I: Integer;
  FieldType: TFieldType;
begin
  // Initialization FDiskRecSize
  FDiskRecSize := 0;
  SetLength(FDiskFieldOffsets, FTableHeader.FieldCount);
  for I := 0 to FTableHeader.FieldCount - 1 do
  begin
    FDiskFieldOffsets[I] := FDiskRecSize;
    FieldType := FTableHeader.FieldTab[I].FieldType;
    if FieldType in BlobFieldTypes then
      Inc(FDiskRecSize, SizeOf(TBlobFieldHeader))
    else
      Inc(FDiskRecSize, GetFieldSize(FieldType, FTableHeader.FieldTab[I].FieldSize));
  end;
end;

procedure TTinyTableIO.InitAutoInc;
var
  I: Integer;
  FieldType: TFieldType;
begin
  FAutoIncFieldIdx := -1;
  for I := 0 to FTableHeader.FieldCount - 1 do
  begin
    FieldType := FTableHeader.FieldTab[I].FieldType;
    if FieldType = ftAutoInc then
    begin
      FAutoIncFieldIdx := I;
      Break;
    end;
  end;
end;

procedure TTinyTableIO.ClearMemRecTab(AList: TList);
var
  I: Integer;
  F: PMemRecTabItem;
begin
  if not Assigned(AList) then Exit;
  for I := 0 to AList.Count - 1 do
  begin
    F := AList.Items[I];
    Dispose(F);
    AList.Items[I] := nil;
  end;
  AList.Clear;
end;

procedure TTinyTableIO.AddMemRecTabItem(AList: TList; Value: TMemRecTabItem);
var
  MemRecTabItemPtr: PMemRecTabItem;
begin
  New(MemRecTabItemPtr);
  MemRecTabItemPtr^ := Value;
  AList.Add(MemRecTabItemPtr);
end;

procedure TTinyTableIO.InsertMemRecTabItem(AList: TList; Index: Integer; Value: TMemRecTabItem);
var
  MemRecTabItemPtr: PMemRecTabItem;
begin
  New(MemRecTabItemPtr);
  MemRecTabItemPtr^ := Value;
  AList.Insert(Index, MemRecTabItemPtr);
end;

procedure TTinyTableIO.DeleteMemRecTabItem(AList: TList; Index: Integer);
var
  F: PMemRecTabItem;
begin
  F := AList.Items[Index];
  Dispose(F);
  AList.Items[Index] := nil;
  AList.Delete(Index);
end;

function TTinyTableIO.GetMemRecTabItem(AList: TList; Index: Integer): TMemRecTabItem;
begin
  Result := PMemRecTabItem(AList.Items[Index])^;
end;

function TTinyTableIO.ShouldEncrypt(FieldIdx: Integer): Boolean;
begin
  Result := FDatabase.Encrypted and (FFieldDefs[FieldIdx].DPMode = fdDefault);
end;

function TTinyTableIO.ShouldCompress(FieldIdx: Integer): Boolean;
begin
  Result := FDatabase.Compressed and
    (FTableHeader.FieldTab[FieldIdx].FieldType in BlobFieldTypes) and
    (FFieldDefs[FieldIdx].DPMode = fdDefault);
end;

//-----------------------------------------------------------------------------
// To obtain the first ItemIdx record sheet items, offset
//-----------------------------------------------------------------------------
function TTinyTableIO.GetRecTabItemOffset(ItemIdx: Integer): Integer;
var
  BlockIdx: Integer;
begin
  BlockIdx := ItemIdx div tdbRecTabUnitNum;
  Result := FRecTabBlockOffsets[BlockIdx] + SizeOf(Integer) +
    SizeOf(TRecordTabItem)*(ItemIdx mod tdbRecTabUnitNum);
end;

//-----------------------------------------------------------------------------
// To obtain an index number is the index of IndexIdx all IndexTab first ItemIdx offset projects
//-----------------------------------------------------------------------------
function TTinyTableIO.GetIdxTabItemOffset(IndexIdx: Integer; ItemIdx: Integer): Integer;
var
  BlockIdx: Integer;
begin
  BlockIdx := ItemIdx div tdbIdxTabUnitNum;
  Result := FIdxTabBlockOffsets[IndexIdx][BlockIdx] + SizeOf(Integer) +
    SizeOf(TIndexTabItem)*(ItemIdx mod tdbIdxTabUnitNum);
end;

//-----------------------------------------------------------------------------
// Compare the size of the two field data
// Field is one of the data in FieldBuffer1 in the field in two of the data in FieldBuffer2
// FieldType: Field Types
// CaseInsensitive: not case-sensitive
// PartialCompare: Is it only a partial string comparison
// Return value:
// If Field1 is equal to or match the Field2 returned value = "0"
// If Field1> Field2 the return value is > "0"
// If Field1 <Field2 the return value is < "0"
//-----------------------------------------------------------------------------
function TTinyTableIO.CompFieldData(FieldBuffer1, FieldBuffer2: Pointer; FieldType: TFieldType;
  CaseInsensitive, PartialCompare: Boolean): Integer;
var
  ExprNodes: TExprNodes;
  LeftNode, RightNode, ResultNode: TExprNode;
  Options: TStrCompOptions;
begin
  ExprNodes := TExprNodes.Create(nil);
  try
    LeftNode := ExprNodes.NewNode(enConst, FieldType, 0, toNOTDEFINED, nil, nil);
    LeftNode.FData := FieldBuffer1;
    if PartialCompare and (FieldType in StringFieldTypes) then
      LeftNode.FPartialLength := Length(LeftNode.FData); // Pos('*', LeftNode.FData);   //modified 2003.3.22
    RightNode := ExprNodes.NewNode(enConst, FieldType, 0, toNOTDEFINED, nil, nil);
    RightNode.FData := FieldBuffer2;
    if PartialCompare and (FieldType in StringFieldTypes) then
      RightNode.FPartialLength := LeftNode.FPartialLength; //Pos('*', RightNode.FData);
    ResultNode := ExprNodes.NewNode(enOperator, ftBoolean, SizeOf(Boolean), toLT, LeftNode, RightNode);

    Options := [];
    if not PartialCompare then Include(Options, scNoPartialCompare);
    if CaseInsensitive then Include(Options, scCaseInsensitive);

    // If Field1 = Field2
    ResultNode.FOperator := toEQ;
    ResultNode.Calculate(Options);
    if ResultNode.AsBoolean then
    begin
      Result := 0;
      Exit;
    end;
    // If Field1 < Field2
    ResultNode.FOperator := toLT;
    ResultNode.Calculate(Options);
    if ResultNode.AsBoolean then
    begin
      Result := -1;
      Exit;
    end;
    // Otherwise, Field1 > Field2
    Result := 1;
  finally
    ExprNodes.Free;
  end;
end;

//-----------------------------------------------------------------------------
// According to specified data from the index to find the nearest location (dichotomy)
// FieldBuffers: Find data should be stored in advance in FieldBuffers
// RecTabList: To find the record set Note: must be a sorted recordset
// IndexIdx: index number (0-based), should be in the sort index RecTabList
// ResultState: store the search results state
//     0: to be the results of search to find the location of value = value
//     1: to be to find the value of the to find the location of the value of the results of
//    -1: To be to find the value of the location to find the value of the results of
//    -2: No Record
// EffFieldCount: Fields in the number of effective fields. The default is 0, that, in terms of actual number of fields in composite index terms
// PartialCompare: string partial matches
// Return value: RecTabList the record number (0-based), it returns -1 if there is no record of
//-----------------------------------------------------------------------------
function TTinyTableIO.SearchIndexedField(FieldBuffers: TFieldBuffers;
  RecTabList: TList; IndexIdx: Integer; var ResultState: Integer;
  EffFieldCount: Integer = 0; PartialCompare: Boolean = False): Integer;
var
  I, Hi, Lo, Mid, Pos: Integer;
  FieldIdx, CompRes: Integer;
  IndexOptions: TTDIndexOptions;
  MemRecTabItem: TMemRecTabItem;
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
  IndexOptions := FIndexDefs[IndexIdx].Options;
  Lo := 0;
  Hi := RecTabList.Count - 1;
  Pos := -1;
  ResultState := -2;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    MemRecTabItem := GetMemRecTabItem(RecTabList, Mid);
    CompRes := 0;
    for I := 0 to High(FIndexDefs[IndexIdx].FieldIdxes) do
    begin
      if (EffFieldCount <> 0) and (I >= EffFieldCount) then Break;
      FieldIdx := FIndexDefs[IndexIdx].FieldIdxes[I]; // ** Number of physical fields
      ReadFieldData(DataStream, MemRecTabItem.RecIndex, FieldIdx);
      CompRes := CompFieldData(FieldBuffers.Items[FieldIdx].DataBuf, DataStream.Memory,
        FieldBuffers.Items[FieldIdx].FieldType, tiCaseInsensitive in IndexOptions, PartialCompare);
      if CompRes <> 0 then Break;
    end;

    if tiDescending in IndexOptions then CompRes := - CompRes;

    Pos := Mid;
    if CompRes > 0 then
    begin
      Lo := Mid + 1;
      ResultState := 1;
    end
    else
    if CompRes < 0 then
    begin
      Hi := Mid - 1;
      ResultState := -1;
    end
    else
    begin
      ResultState := 0;
      Break;
    end;
  end;
  finally
    DataStream.Free;
  end;
  Result := Pos;
end;

//-----------------------------------------------------------------------------
// According to the index to find the boundaries of the data from the specified location (dichotomy)
// FieldBuffers: Find data should be stored in advance in FieldBuffers
// RecTabList: To find the record set Note: must be a sorted recordset
// IndexIdx: index number (0-based), should be in the sort index RecTabList
// LowBound: when the search for True Low-border, in order to False when the search for high-boundary
// ResultState: store the search results state, the definition of the same SearchIndexedField
// EffFieldCount: Fields in the number of effective fields. The default is 0, that, in terms of actual number of fields in composite index terms
// PartialCompare: string partial matches
// Return value: RecTabList the record number (0-based), it returns -1 if there is no record of
//-----------------------------------------------------------------------------
function TTinyTableIO.SearchIndexedFieldBound(FieldBuffers: TFieldBuffers;
  RecTabList: TList; IndexIdx: Integer; LowBound: Boolean; var ResultState: Integer;
  EffFieldCount: Integer = 0; PartialCompare: Boolean = False): Integer;
var
  I, Hi, Lo, Mid, Pos: Integer;
  MemRecTabItem: TMemRecTabItem;
  FieldIdx, CompRes: Integer;
  IndexOptions: TTDIndexOptions;
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  IndexOptions := FIndexDefs[IndexIdx].Options;
  Lo := 0;
  Hi := RecTabList.Count - 1;
  Pos := -1;
  ResultState := -2;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    MemRecTabItem := GetMemRecTabItem(RecTabList, Mid);
    CompRes := 0;
    for I := 0 to High(FIndexDefs[IndexIdx].FieldIdxes) do
    begin
      if (EffFieldCount <> 0) and (I >= EffFieldCount) then Break;
      FieldIdx := FIndexDefs[IndexIdx].FieldIdxes[I];
      ReadFieldData(DataStream, MemRecTabItem.RecIndex, FieldIdx);
      CompRes := CompFieldData(FieldBuffers.Items[FieldIdx].DataBuf, DataStream.Memory,
        FieldBuffers.Items[FieldIdx].FieldType, tiCaseInsensitive in IndexOptions, PartialCompare);
      if CompRes <> 0 then Break;
    end;

    if tiDescending in IndexOptions then CompRes := - CompRes;

    if ResultState <> 0 then Pos := Mid;
    if CompRes > 0 then
    begin
      Lo := Mid + 1;
      if ResultState <> 0 then ResultState := 1;
    end
    else
    if CompRes < 0 then
    begin
      Hi := Mid - 1;
      if ResultState <> 0 then ResultState := -1;
    end
    else
    begin
      if LowBound then Hi := Mid - 1
      else Lo := Mid + 1;
      Pos := Mid;
      ResultState := 0;
    end;
  end;
  DataStream.Free;
  Result := Pos;
end;

//-----------------------------------------------------------------------------
// According to the index position to strike a SubRangeStart
// FieldBuffers: Find data should be stored in advance in FieldBuffers
// RecTabList: To find the record set Note: must be a sorted recordset
// IndexIdx: index number (0-based), should be in the sort index RecTabList
// ResultState: store the search results state, the definition of the same SearchIndexedField
// EffFieldCount: FieldBuffers number of effective fields. The default is 0, that, in terms of actual number of fields in composite index terms
// Return value: the results obtained in the RecTabList the record number (0-based)
//-----------------------------------------------------------------------------
function TTinyTableIO.SearchRangeStart(FieldBuffers: TFieldBuffers; RecTabList: TList;
  IndexIdx: Integer; var ResultState: Integer; EffFieldCount: Integer = 0): Integer;
begin
  Result := SearchIndexedFieldBound(FieldBuffers, RecTabList, IndexIdx, True, ResultState, EffFieldCount);
  if ResultState = 1 then Inc(Result);
end;

//-----------------------------------------------------------------------------
// According to the index position to strike a SubRangeEnd
// FieldBuffers: Find data should be stored in advance in FieldBuffers
// RecTabList: To find the record set Note: must be a sorted recordset
// IndexIdx: index number (0-based), should be in the sort index RecTabList
// ResultState: store the search results state, the definition of the same SearchIndexedField
// EffFieldCount: FieldBuffers number of effective fields. The default is 0, that, in terms of actual number of fields in composite index terms
// Return value: the results obtained in the RecTabList the record number (0-based)
//-----------------------------------------------------------------------------
function TTinyTableIO.SearchRangeEnd(FieldBuffers: TFieldBuffers; RecTabList: TList;
  IndexIdx: Integer; var ResultState: Integer; EffFieldCount: Integer = 0): Integer;
begin
  Result := SearchIndexedFieldBound(FieldBuffers, RecTabList, IndexIdx, False, ResultState, EffFieldCount);
  if ResultState = -1 then Dec(Result);
end;

//-----------------------------------------------------------------------------
// Insertion point to strike the index
// FieldBuffers: Find data should be stored in advance in FieldBuffers
// IndexIdx: No. 0-based index
// ResultState: store the search results state, the definition of the same SearchIndexedField
// Return value: the location of the insertion point (0-based)
//-----------------------------------------------------------------------------
function TTinyTableIO.SearchInsertPos(FieldBuffers: TFieldBuffers; IndexIdx: Integer; var ResultState: Integer): Integer;
begin
  Result := SearchIndexedField(FieldBuffers, FRecTabLists[IndexIdx+1], IndexIdx, ResultState);
  if ResultState in [0, 1] then Inc(Result)
  else if ResultState = -2 then Result := 0;
end;

//-----------------------------------------------------------------------------
// Check if it contains the Primary Index
// Return value:
//   Does not return -1
//   With the index number is returned
//-----------------------------------------------------------------------------
function TTinyTableIO.CheckPrimaryFieldExists: Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FIndexDefs.Count - 1 do
  begin
    if tiPrimary in FIndexDefs[I].Options then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

//-----------------------------------------------------------------------------
// Add record checks Primary, Unique field uniqueness
// FieldBuffers: check the field value to be stored in advance in which the
// Return value: the legal (in line with the uniqueness) returns True
//-----------------------------------------------------------------------------
function TTinyTableIO.CheckUniqueFieldForAppend(FieldBuffers: TFieldBuffers): Boolean;
var
  I, Idx, ResultState: Integer;
  S: string;
  IndexOptions: TTDIndexOptions;
begin
  Result := True;
  for Idx := 0 to FIndexDefs.Count - 1 do
  begin
    IndexOptions := FIndexDefs[Idx].Options;
    if (tiPrimary in IndexOptions) or (tiUnique in IndexOptions) then
    begin
      SearchIndexedField(FieldBuffers, FRecTabLists[Idx+1], Idx, ResultState);
      if ResultState = 0 then
      begin
        Result := False;
        S := '';
        for I := 0 to Length(FIndexDefs[Idx].FieldIdxes) - 1 do
        begin
          if I > 0 then S := S + ',';
          S := S + FieldBuffers.Items[FIndexDefs[Idx].FieldIdxes[I]].AsString;
        end;
        DatabaseErrorFmt(SInvalidUniqueFieldValue, [S]);
      end;
    end;
  end;
end;

//-----------------------------------------------------------------------------
// Modified records checks Primary, Unique field uniqueness
// FieldBuffers: check the field value to be stored in advance in which the
// PhyRecordIdx: is modifying the physical record number of records
// Return value: the legal (in line with the uniqueness) returns True
//-----------------------------------------------------------------------------
function TTinyTableIO.CheckUniqueFieldForModify(FieldBuffers: TFieldBuffers; PhyRecordIdx: Integer): Boolean;
var
  I, Idx, RecIdx, ResultState: Integer;
  S: string;
  IndexOptions: TTDIndexOptions;
begin
  Result := True;
  for Idx := 0 to FIndexDefs.Count - 1 do
  begin
    IndexOptions := FIndexDefs[Idx].Options;
    if (tiPrimary in IndexOptions) or (tiUnique in IndexOptions) then
    begin
      RecIdx := SearchIndexedField(FieldBuffers, FRecTabLists[Idx+1], Idx, ResultState);
      if ResultState = 0 then
      begin
        ConvertRecordIdx(Idx, RecIdx, -1, RecIdx);
        if PhyRecordIdx <> RecIdx then
        begin
          Result := False;
          S := '';
          for I := 0 to Length(FIndexDefs[Idx].FieldIdxes) - 1 do
          begin
            if I > 0 then S := S + ',';
            S := S + FieldBuffers.Items[FIndexDefs[Idx].FieldIdxes[I]].AsString;
          end;
          DatabaseErrorFmt(SInvalidUniqueFieldValue, [S]);
        end;
      end;
    end;
  end;
end;

//-----------------------------------------------------------------------------
// The index number is SrcIndexIdx index into the record number SrcRecordIdx
// Index number of the index for the DstIndexIdx record number, the result stored in the DstRecordIdx
//-----------------------------------------------------------------------------
procedure TTinyTableIO.ConvertRecordIdx(SrcIndexIdx, SrcRecordIdx, DstIndexIdx: Integer; var DstRecordIdx: Integer);
var
  I, RecIdx: Integer;
  MemRecTabItem: TMemRecTabItem;
begin
  if (SrcRecordIdx < 0) or (SrcRecordIdx >= FRecTabLists[SrcIndexIdx+1].Count) then
  begin
    DstRecordIdx := -1;
    Exit;
  end;

  if SrcIndexIdx = DstIndexIdx then
  begin
    DstRecordIdx := SrcRecordIdx;
    Exit;
  end;

  MemRecTabItem := GetMemRecTabItem(FRecTabLists[SrcIndexIdx+1], SrcRecordIdx);
  RecIdx := MemRecTabItem.RecIndex;

  if DstIndexIdx = -1 then
  begin
    ConvertRecIdxForPhy(SrcIndexIdx, SrcRecordIdx, DstRecordIdx);
    Exit;
  end else
  begin
    for I := 0 to FRecTabLists[DstIndexIdx+1].Count - 1 do
    begin
      MemRecTabItem := GetMemRecTabItem(FRecTabLists[DstIndexIdx+1], I);
      if MemRecTabItem.RecIndex = RecIdx then
      begin
        DstRecordIdx := I;
        Exit;
      end;
    end;
  end;
  DstRecordIdx := -1;
end;

//-----------------------------------------------------------------------------
// Will SrcRecTabList the record number SrcRecordIdx converted
// DstRecTabList in the record number, the result stored in the DstRecordIdx
//-----------------------------------------------------------------------------
procedure TTinyTableIO.ConvertRecordIdx(SrcRecTabList: TList; SrcRecordIdx: Integer;
  DstRecTabList: TList; var DstRecordIdx: Integer);
var
  I, RecIdx: Integer;
  MemRecTabItem: TMemRecTabItem;
begin
  if (SrcRecordIdx < 0) or (SrcRecordIdx >= SrcRecTabList.Count) then
  begin
    DstRecordIdx := -1;
    Exit;
  end;

  if SrcRecTabList = DstRecTabList then
  begin
    DstRecordIdx := SrcRecordIdx;
    Exit;
  end;

  MemRecTabItem := GetMemRecTabItem(SrcRecTabList, SrcRecordIdx);
  RecIdx := MemRecTabItem.RecIndex;

  for I := 0 to DstRecTabList.Count - 1 do
  begin
    MemRecTabItem := GetMemRecTabItem(DstRecTabList, I);
    if MemRecTabItem.RecIndex = RecIdx then
    begin
      DstRecordIdx := I;
      Exit;
    end;
  end;
  DstRecordIdx := -1;
end;

//-----------------------------------------------------------------------------
// The index number is SrcIndexIdx index into the record number SrcRecordIdx
// FRecTabLists [0] in the corresponding record number, the result stored in the DstRecordIdx
//-----------------------------------------------------------------------------
procedure TTinyTableIO.ConvertRecIdxForPhy(SrcIndexIdx, SrcRecordIdx: Integer; var DstRecordIdx: Integer);
var
  Lo, Hi, Mid, Pos, RecIdx: Integer;
  MemRecTabItem: TMemRecTabItem;
begin
  if (SrcRecordIdx < 0) or (SrcRecordIdx >= FRecTabLists[SrcIndexIdx+1].Count) then
  begin
    DstRecordIdx := -1;
    Exit;
  end;

  MemRecTabItem := GetMemRecTabItem(FRecTabLists[SrcIndexIdx+1], SrcRecordIdx);
  RecIdx := MemRecTabItem.RecIndex;

  // FRecTabLists[0] in the RecIndex was ordered, so you can use the dichotomy
  Lo := 0;
  Hi := FRecTabLists[0].Count - 1;
  Pos := -1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    MemRecTabItem := GetMemRecTabItem(FRecTabLists[0], Mid);
    if RecIdx > MemRecTabItem.RecIndex then
    begin
      Lo := Mid + 1;
    end
    else
    if RecIdx < MemRecTabItem.RecIndex then
    begin
      Hi := Mid - 1;
    end
    else
    begin
      Pos := Mid;
      Break;
    end;
  end;
  DstRecordIdx := Pos;
end;

//-----------------------------------------------------------------------------
// Will be recorded in the record number set SrcRecTabList into SrcRecordIdx
// FRecTabLists[0] in the corresponding record number, the result stored in the DstRecordIdx
//-----------------------------------------------------------------------------
procedure TTinyTableIO.ConvertRecIdxForPhy(SrcRecTabList: TList; SrcRecordIdx: Integer; var DstRecordIdx: Integer);
var
  Lo, Hi, Mid, Pos, RecIdx: Integer;
  MemRecTabItem: TMemRecTabItem;
begin
  if (SrcRecordIdx < 0) or (SrcRecordIdx >= SrcRecTabList.Count) then
  begin
    DstRecordIdx := -1;
    Exit;
  end;

  MemRecTabItem := GetMemRecTabItem(SrcRecTabList, SrcRecordIdx);
  RecIdx := MemRecTabItem.RecIndex;

  // FRecTabLists[0] in the RecIndex was ordered, so you can use the dichotomy
  Lo := 0;
  Hi := FRecTabLists[0].Count - 1;
  Pos := -1;
  while Lo <= Hi do
  begin
    Mid := (Lo + Hi) div 2;
    MemRecTabItem := GetMemRecTabItem(FRecTabLists[0], Mid);
    if RecIdx > MemRecTabItem.RecIndex then
    begin
      Lo := Mid + 1;
    end
    else
    if RecIdx < MemRecTabItem.RecIndex then
    begin
      Hi := Mid - 1;
    end
    else
    begin
      Pos := Mid;
      Break;
    end;
  end;
  DstRecordIdx := Pos;
end;

//-----------------------------------------------------------------------------
// The index number is SrcIndexIdx index into the record number SrcRecordIdx
// RecTabList the corresponding record number, the result stored in the DstRecordIdx
//-----------------------------------------------------------------------------
procedure TTinyTableIO.ConvertRecIdxForCur(SrcIndexIdx, SrcRecordIdx: Integer;
  RecTabList: TList; var DstRecordIdx: Integer);
var
  I, RecIdx: Integer;
  MemRecTabItem: TMemRecTabItem;
begin
  if (SrcRecordIdx < 0) or (SrcRecordIdx >= FRecTabLists[SrcIndexIdx+1].Count) then
  begin
    DstRecordIdx := -1;
    Exit;
  end;

  if RecTabList = FRecTabLists[SrcIndexIdx + 1] then
  begin
    DstRecordIdx := SrcRecordIdx;
    Exit;
  end;

  MemRecTabItem := GetMemRecTabItem(FRecTabLists[SrcIndexIdx+1], SrcRecordIdx);
  RecIdx := MemRecTabItem.RecIndex;

  for I := 0 to RecTabList.Count - 1 do
  begin
    MemRecTabItem := GetMemRecTabItem(RecTabList, I);
    if MemRecTabItem.RecIndex = RecIdx then
    begin
      DstRecordIdx := I;
      Exit;
    end;
  end;
  DstRecordIdx := -1;
end;

//-----------------------------------------------------------------------------
// Add record to adjust database index
// RecDataOffset: new recorded data zone offset
// RecTotal: added before the total number of records, including records to delete tag
//-----------------------------------------------------------------------------
procedure TTinyTableIO.AdjustIndexesForAppend(FieldBuffers: TFieldBuffers;
  RecDataOffset, RecTotal: Integer; OnAdjustIndex: TOnAdjustIndexForAppendEvent);
var
  Idx, Pos, ResultState: Integer;
  IdxTabOffset, StartIndex: Integer;
  BakFilePos: Integer;
  IdxTabItem, IdxTabItemA: TIndexTabItem;
  MemRecTabItem: TMemRecTabItem;
  IndexTabAry: array of TIndexTabItem;
  NextBlockOffset: Integer;
  DBStream: TStream;
begin
  DBStream := FDatabase.DBFileIO.DBStream;

  MemRecTabItem.DataOffset := RecDataOffset;
  MemRecTabItem.RecIndex := RecTotal;
  OnAdjustIndex(-1, -1, MemRecTabItem);

  for Idx := 0 to High(FRecTabLists) - 1 do
  begin
    // The insertion point to strike
    Pos := SearchInsertPos(FieldBuffers, Idx, ResultState);

    if Assigned(OnAdjustIndex) then
    begin
      MemRecTabItem.DataOffset := RecDataOffset;
      MemRecTabItem.RecIndex := RecTotal;
      OnAdjustIndex(Idx, Pos, MemRecTabItem);
    end;

    // Adjustment FRecTabLists
    MemRecTabItem.DataOffset := RecDataOffset;
    MemRecTabItem.RecIndex := RecTotal;
    InsertMemRecTabItem(FRecTabLists[Idx+1], Pos, MemRecTabItem);

    // The index is written to disk database file in the -----
    IdxTabOffset := FTableHeader.IndexHeader[Idx].IndexOffset;

    // If you already have adjusted the number of records is an integer multiple of step, then ..
    if RecTotal mod tdbIdxTabUnitNum = 0 then
    begin
      // Construct a new index table block
      SetLength(IndexTabAry, tdbIdxTabUnitNum);
      FillChar(IndexTabAry[0], SizeOf(TIndexTabItem)*Length(IndexTabAry), 0);
      // The adjustment of the head of a block of index table Next Pointer
      NextBlockOffset := DBStream.Size;
      if Length(FIdxTabBlockOffsets[Idx]) > 0 then
      begin
        DBStream.Position := FIdxTabBlockOffsets[Idx][High(FIdxTabBlockOffsets[Idx])];
        DBStream.Write(NextBlockOffset, SizeOf(Integer));
      end;
      // If you had no records, then adjust the TableHeader
      if RecTotal = 0 then
      begin
        IdxTabOffset := DBStream.Size;
        FTableHeader.IndexHeader[Idx].IndexOffset := IdxTabOffset;
        FDatabase.DBFileIO.WriteTableHeader(TableIdx, FTableHeader);
      end;
      // Adjustment FIdxTabBlockOffsets [Idx]
      SetLength(FIdxTabBlockOffsets[Idx], Length(FIdxTabBlockOffsets[Idx]) + 1);
      FIdxTabBlockOffsets[Idx][High(FIdxTabBlockOffsets[Idx])] := NextBlockOffset;

      // The new index table to write a database block
      DBStream.Seek(0, soFromEnd);
      NextBlockOffset := 0;
      DBStream.Write(NextBlockOffset, SizeOf(Integer));
      DBStream.Write(IndexTabAry[0], SizeOf(TIndexTabItem)*Length(IndexTabAry));
    end;

    // If this is the first record
    if RecTotal = 0 then
    begin
      IdxTabItem.RecIndex := 0;
      IdxTabItem.Next := -1;
      DBStream.Position := IdxTabOffset + SizeOf(Integer);
      DBStream.Write(IdxTabItem, SizeOf(IdxTabItem));
    end
    else
    // If it is not the first record
    begin
      // If you insert a header in the list
      if Pos = 0 then
      begin
        // Construct a new IdxTabItem
        StartIndex := FTableHeader.IndexHeader[Idx].StartIndex;
        IdxTabItem.RecIndex := RecTotal;
        IdxTabItem.Next := StartIndex;
        // The new IdxTabItem written to the database
        DBStream.Position := GetIdxTabItemOffset(Idx, RecTotal);
        DBStream.Write(IdxTabItem, SizeOf(IdxTabItem));
        // Adjustment TableHeader
        FTableHeader.IndexHeader[Idx].StartIndex := RecTotal;
        FDatabase.DBFileIO.WriteTableHeader(TableIdx, FTableHeader);
      end
      else
      // If you do not insert a header in the list
      begin
        // The insertion point to read an index item IdxTabItemA
        MemRecTabItem := GetMemRecTabItem(FRecTabLists[Idx+1], Pos - 1);
        DBStream.Position := GetIdxTabItemOffset(Idx, MemRecTabItem.RecIndex);
        BakFilePos := DBStream.Position;
        DBStream.Read(IdxTabItemA, SizeOf(IdxTabItemA));
        // Write the new IdxTabItem
        IdxTabItem.RecIndex := RecTotal;
        IdxTabItem.Next := IdxTabItemA.Next;
        DBStream.Position := GetIdxTabItemOffset(Idx, RecTotal);
        DBStream.Write(IdxTabItem, SizeOf(IdxTabItem));
        // Will be adjusted IdxTabItemA written to the database
        IdxTabItemA.Next := RecTotal;
        DBStream.Position := BakFilePos;
        DBStream.Write(IdxTabItemA, SizeOf(IdxTabItem));
      end;
    end;
  end;
end;

//-----------------------------------------------------------------------------
// Modify the record to adjust database index
// EditPhyRecordIdx: Record has been modified the physical record number (that is, FRecTabLists[0] in the record number) 0-based
//-----------------------------------------------------------------------------
procedure TTinyTableIO.AdjustIndexesForModify(FieldBuffers: TFieldBuffers;
  EditPhyRecordIdx: Integer; OnAdjustIndex: TOnAdjustIndexForModifyEvent);
var
  Idx, Pos, FromPos, ToPos, ResultState: Integer;
  BakFilePos: Integer;
  EditRecordIdx: Integer;
  MemRecTabItem, OrgMemRecTabItem: TMemRecTabItem;
  IdxTabItem, IdxTabItemA: TIndexTabItem;
  IndexOptions: TTDIndexOptions;
  EditRecordIdxes: array[0..tdbMaxIndex-1] of Integer;
  DBStream: TStream;
begin
  DBStream := FDatabase.DBFileIO.DBStream;
  for Idx := 0 to High(FRecTabLists) - 1 do
    ConvertRecordIdx(-1, EditPhyRecordIdx, Idx, EditRecordIdxes[Idx]);

  for Idx := 0 to High(FRecTabLists) - 1 do
  begin
    IndexOptions := FIndexDefs[Idx].Options;

    EditRecordIdx := EditRecordIdxes[Idx];
    OrgMemRecTabItem := GetMemRecTabItem(FRecTabLists[Idx+1], EditRecordIdx);
    DeleteMemRecTabItem(FRecTabLists[Idx+1], EditRecordIdx);
    // The insertion point to strike
    Pos := SearchInsertPos(FieldBuffers, Idx, ResultState);
    FromPos := EditRecordIdx;
    if Pos <= EditRecordIdx then ToPos := Pos
    else ToPos := Pos + 1;
    // Restore FRecTabLists [Idx +1]
    InsertMemRecTabItem(FRecTabLists[Idx+1], EditRecordIdx, OrgMemRecTabItem);

    if FromPos = ToPos then Continue;

    if Assigned(OnAdjustIndex) then
      OnAdjustIndex(Idx, FromPos, ToPos);

    // Adjust the disk database file in the index data ----------------------------
    // Should first move the index item from the list to be separated (IdxTabItem)
    // If you want to move the index project is the first node in linked list
    if FromPos = 0 then
    begin
      DBStream.Position := GetIdxTabItemOffset(Idx, OrgMemRecTabItem.RecIndex);
      DBStream.Read(IdxTabItem, SizeOf(IdxTabItem));
      FTableHeader.IndexHeader[Idx].StartIndex := IdxTabItem.Next;
      FDatabase.DBFileIO.WriteTableHeader(TableIdx, FTableHeader);
    end
    else
    // If it is not the first node in list
    begin
      DBStream.Position := GetIdxTabItemOffset(Idx, OrgMemRecTabItem.RecIndex);
      DBStream.Read(IdxTabItem, SizeOf(IdxTabItem));
      MemRecTabItem := GetMemRecTabItem(FRecTabLists[Idx+1], FromPos - 1);
      DBStream.Position := GetIdxTabItemOffset(Idx, MemRecTabItem.RecIndex);
      BakFilePos := DBStream.Position;
      DBStream.Read(IdxTabItemA, SizeOf(IdxTabItemA));
      IdxTabItemA.Next := IdxTabItem.Next;
      DBStream.Position := BakFilePos;
      DBStream.Write(IdxTabItemA, SizeOf(IdxTabItemA));
    end;
    // Then separated list of index entries placed in the appropriate place
    // If you want to put into the position of the first node of list
    if ToPos = 0 then
    begin
      IdxTabItem.Next := FTableHeader.IndexHeader[Idx].StartIndex;
      FTableHeader.IndexHeader[Idx].StartIndex := OrgMemRecTabItem.RecIndex;
      DBStream.Position := GetIdxTabItemOffset(Idx, OrgMemRecTabItem.RecIndex);
      DBStream.Write(IdxTabItem, SizeOf(IdxTabItem));
      FDatabase.DBFileIO.WriteTableHeader(TableIdx, FTableHeader);
    end
    else
    // If you want to place the location is not the first node position list
    begin
      MemRecTabItem := GetMemRecTabItem(FRecTabLists[Idx+1], ToPos - 1);
      DBStream.Position := GetIdxTabItemOffset(Idx, MemRecTabItem.RecIndex);
      BakFilePos := DBStream.Position;
      DBStream.Read(IdxTabItemA, SizeOf(IdxTabItemA));
      IdxTabItem.Next := IdxTabItemA.Next;
      IdxTabItemA.Next := OrgMemRecTabItem.RecIndex;
      DBStream.Position := BakFilePos;
      DBStream.Write(IdxTabItemA, SizeOf(IdxTabItemA));
      DBStream.Position := GetIdxTabItemOffset(Idx, OrgMemRecTabItem.RecIndex);
      DBStream.Write(IdxTabItem, SizeOf(IdxTabItem));
    end;

    // Adjustment FRecTabLists[Idx +1]
    DeleteMemRecTabItem(FRecTabLists[Idx+1], FromPos);
    InsertMemRecTabItem(FRecTabLists[Idx+1], Pos, OrgMemRecTabItem);
  end;
end;

//-----------------------------------------------------------------------------
// Delete records to adjust database index
// DeletePhyRecordIdx: deleted records of the physical record number (that is, FRecTabLists [0] in the record number) 0-based
//-----------------------------------------------------------------------------
procedure TTinyTableIO.AdjustIndexesForDelete(DeletePhyRecordIdx: Integer);
var
  Idx: Integer;
  DeleteRecordIdxes: array[0..tdbMaxIndex-1] of Integer;
begin
  for Idx := 0 to High(FRecTabLists) - 1 do
    ConvertRecordIdx(-1, DeletePhyRecordIdx, Idx, DeleteRecordIdxes[Idx]);

  for Idx := 0 to High(FRecTabLists) - 1 do
    DeleteMemRecTabItem(FRecTabLists[Idx+1], DeleteRecordIdxes[Idx]);
end;

//-----------------------------------------------------------------------------
// Remove tags in the database to do
// PhyRecordIdx: Physical record number 0-based
//-----------------------------------------------------------------------------
procedure TTinyTableIO.WriteDeleteFlag(PhyRecordIdx: Integer);
var
  RecTabItemOffset: Integer;
begin
  RecTabItemOffset := GetRecTabItemOffset(GetMemRecTabItem(FRecTabLists[0], PhyRecordIdx).RecIndex);
  FDatabase.DBFileIO.WriteDeleteFlag(RecTabItemOffset);
end;

//-----------------------------------------------------------------------------
// Adjusted FieldBuffers, so that one of the String field is empty unused area character
//-----------------------------------------------------------------------------
procedure TTinyTableIO.AdjustStrFldInBuffer(FieldBuffers: TFieldBuffers);
var
  I, DataSize, Len: Integer;
  Buffer: PAnsiChar;
begin
  for I := 0 to FieldBuffers.Count - 1 do
  begin
    if FieldBuffers.Items[I].FieldType in StringFieldTypes then
    begin
      Buffer := FieldBuffers.Items[I].Buffer;
      DataSize := FieldBuffers.Items[I].FieldSize;
      Len := Length(Buffer);
      if Len >= DataSize then Len := DataSize - 1;
      FillChar(Buffer[Len], DataSize - Len, 0);
    end;
  end;
end;

procedure TTinyTableIO.ClearAllRecTabLists;
var
  I: Integer;
begin
  for I := 0 to High(FRecTabLists) do
  begin
    ClearMemRecTab(FRecTabLists[I]);
    FreeAndNil(FRecTabLists[I]);
  end;
  SetLength(FRecTabLists, 0);
end;

procedure TTinyTableIO.Initialize;
var
  IndexCount: Integer;
begin
  FDatabase.DBFileIO.Lock;
  try
    FTableIdx := GetTableIdxByName(FTableName);
    if FTableIdx = -1 then
      DatabaseErrorFmt(STableNotFound, [FTableName]);

    FDatabase.DBFileIO.ReadTableHeader(FTableIdx, FTableHeader);
    IndexCount := FTableHeader.IndexCount;
    SetLength(FIdxTabBlockOffsets, IndexCount);

    InitFieldDefs;
    InitIndexDefs;
    InitAllRecTabLists;
    InitDiskRecInfo;
    InitAutoInc;
  finally
    FDatabase.DBFileIO.Unlock;
  end;
end;

procedure TTinyTableIO.Finalize;
begin
  FFieldDefs.Clear;
  FIndexDefs.Clear;
  ClearAllRecTabLists;
end;

procedure TTinyTableIO.Open;
begin
  if FRefCount = 0 then Initialize;
  Inc(FRefCount);
end;

procedure TTinyTableIO.Close;
begin
  Dec(FRefCount);
  if FRefCount = 0 then Finalize;
  if FRefCount < 0 then FRefCount := 0;
end;

procedure TTinyTableIO.Refresh;
begin
  if Active then
  begin
    Finalize;
    Initialize;
  end;
end;

//-----------------------------------------------------------------------------
// Buffer data will be added to the database
// FieldBuffers: will want to add field data (includes all the physical fields)
// Flush: New exhausted whether the flush cache
//-----------------------------------------------------------------------------
procedure TTinyTableIO.AppendRecordData(FieldBuffers: TFieldBuffers; Flush: Boolean;
  OnAdjustIndex: TOnAdjustIndexForAppendEvent);
var
  RecordTab: TRecordTabItem;
  RecordTabAry: array of TRecordTabItem;
  TableHeaderOffset, RecTabOffset, RecTotal, DataOffset: Integer;
  BlobHeader: TBlobFieldHeader;
  BlobRestBuf: array[0..tdbBlobSizeUnitNum-1] of AnsiChar;
  BlobStream, DstBlobStream, DstBlobStream1: TMemoryStream;
  MemRecTabItem: TMemRecTabItem;
  NextRecTabBlockOffset: Integer;
  I, BlobDataPos, DstBlobSize: Integer;
  CRC32Value1, CRC32Value2: Longword;
  SrcDiskRecBuf, DstDiskRecBuf: PAnsiChar;
  FieldBuffer: Pointer;
  DBStream: TStream;
  {$IFDEF WIN64}
  A: array of Byte;
  P: Pointer;
  {$ENDIF}
begin
  FDatabase.DBFileIO.Lock;
  try
    DBStream := FDatabase.DBFileIO.DBStream;

    // Check Primary, Unique field uniqueness
    CheckUniqueFieldForAppend(FieldBuffers);

    // AutoInc make adjustments on the field
    if FAutoIncFieldIdx <> -1 then
    begin
      FieldBuffer := FieldBuffers.Items[FAutoIncFieldIdx].Buffer;
      if PInteger(FieldBuffer)^ = 0 then
      begin
        Inc(FTableHeader.AutoIncCounter);
        PInteger(FieldBuffer)^ := FTableHeader.AutoIncCounter;
      end
      else
      if PInteger(FieldBuffer)^ > FTableHeader.AutoIncCounter then
      begin
        FTableHeader.AutoIncCounter := PInteger(FieldBuffer)^;
      end;
    end;

    TableHeaderOffset := FDatabase.DBFileIO.TableTab.TableHeaderOffset[FTableIdx];
    RecTabOffset := FTableHeader.RecTabOffset;
    RecTotal := FTableHeader.RecordTotal;

    // If you do not have any record of
    if RecTotal = 0 then
    begin
      RecTabOffset := DBStream.Size;
      // Adjustment FRecTabBlockOffsets
      SetLength(FRecTabBlockOffsets, Length(FRecTabBlockOffsets) + 1);
      FRecTabBlockOffsets[High(FRecTabBlockOffsets)] := RecTabOffset;
      // ..
      SetLength(RecordTabAry, tdbRecTabUnitNum);
      FillChar(RecordTabAry[0], SizeOf(TRecordTabItem)*tdbRecTabUnitNum, 0);
      DataOffset := DBStream.Size + SizeOf(Integer) + SizeOf(TRecordTabItem)*tdbRecTabUnitNum;
      RecordTabAry[0].DataOffset := DataOffset;
      RecordTabAry[0].DeleteFlag := False;
      NextRecTabBlockOffset := 0;
      DBStream.Seek(0, soFromEnd);
      DBStream.Write(NextRecTabBlockOffset, SizeOf(Integer));
      DBStream.Write(RecordTabAry[0], SizeOf(TRecordTabItem)*tdbRecTabUnitNum);
    end
    else
    // If the table already exists in the record
    begin
      // If you already have adjusted the number of records is an integer multiple of step, then ..
      if RecTotal mod tdbRecTabUnitNum = 0 then
      begin
        // Construct a new log block
        SetLength(RecordTabAry, tdbRecTabUnitNum);
        FillChar(RecordTabAry[0], SizeOf(TRecordTabItem)*Length(RecordTabAry), 0);
        DataOffset := DBStream.Size + SizeOf(Integer) + SizeOf(TRecordTabItem)*Length(RecordTabAry);
        RecordTabAry[0].DataOffset := DataOffset;
        RecordTabAry[0].DeleteFlag := False;
        // The adjustment of a record form the first block of the Next pointer
        NextRecTabBlockOffset := DBStream.Size;
        DBStream.Position := FRecTabBlockOffsets[High(FRecTabBlockOffsets)];
        DBStream.Write(NextRecTabBlockOffset, SizeOf(Integer));
        // Adjustment FRecTabBlockOffsets
        SetLength(FRecTabBlockOffsets, Length(FRecTabBlockOffsets) + 1);
        FRecTabBlockOffsets[High(FRecTabBlockOffsets)] := NextRecTabBlockOffset;
        //  Added at the end of a record in the file block list
        DBStream.Seek(0, soFromEnd);
        NextRecTabBlockOffset := 0;
        DBStream.Write(NextRecTabBlockOffset, SizeOf(Integer));
        DBStream.Write(RecordTabAry[0], SizeOf(TRecordTabItem)*Length(RecordTabAry));
      end
      else
      // Otherwise, just directly modify the
      begin
        DBStream.Position := GetRecTabItemOffset(RecTotal);
        DataOffset := DBStream.Size;
        RecordTab.DataOffset := DataOffset;
        RecordTab.DeleteFlag := False;
        DBStream.Write(RecordTab, SizeOf(RecordTab));
      end;
    end;
    // Will be adjusted RecTabOffset, RecordTotal write back to the database
    Inc(RecTotal);
    DBStream.Position := TableHeaderOffset + SizeOf(TTableNameString);
    DBStream.Write(RecTabOffset, SizeOf(Integer));
    DBStream.Write(RecTotal, SizeOf(Integer));
    if FAutoIncFieldIdx <> -1 then DBStream.Write(FTableHeader.AutoIncCounter, SizeOf(Integer));
    // Adjustment FTableHeader
    FTableHeader.RecTabOffset := RecTabOffset;
    FTableHeader.RecordTotal := RecTotal;

    // Write the real data
    DstBlobStream := TMemoryStream.Create;
    DstBlobStream1 := TMemoryStream.Create;
    SrcDiskRecBuf := AllocMem(FDiskRecSize);
    DstDiskRecBuf := AllocMem(FDiskRecSize);
    try
      FillChar(BlobHeader, SizeOf(BlobHeader), 0);
      // Adjust the string field
      AdjustStrFldInBuffer(FieldBuffers);
      // The first to write a database to occupy the position of
      DBStream.Position := DataOffset;
      DBStream.Write(DstDiskRecBuf^, FDiskRecSize);
      for I := 0 to FTableHeader.FieldCount - 1 do
      begin
        FieldBuffer := FieldBuffers.Items[I].Buffer;
        // If it is indefinite length of the data
        if FieldBuffers.Items[I].IsBlob then
        begin
          BlobStream := TMemoryStream(FieldBuffer);
          // Data encoding (compression, encryption)
          FDatabase.DBFileIO.EncodeMemoryStream(BlobStream, DstBlobStream, ShouldEncrypt(I), ShouldCompress(I));
          DstBlobSize := DstBlobStream.Size;
          // Calculation BlobHeader
          BlobDataPos := DBStream.Position;
          BlobHeader.DataOffset := BlobDataPos;
          BlobHeader.DataSize := DstBlobSize;
          BlobHeader.AreaSize := DstBlobSize + (tdbBlobSizeUnitNum - DstBlobSize mod tdbBlobSizeUnitNum);
          // Written to the database will be BlobHeader
          Move(BlobHeader, DstDiskRecBuf[FDiskFieldOffsets[I]], SizeOf(BlobHeader));

          // Write BLOB data:
          //  If begun using CRC32 checkout
          if FDatabase.CRC32 then
          begin
            CRC32Value1 := 0;
            CRC32Value2 := 0;
            repeat
              if CRC32Value1 <> CRC32Value2 then
                FDatabase.DBFileIO.EncodeMemoryStream(BlobStream, DstBlobStream, ShouldEncrypt(I), ShouldCompress(I));
              DstBlobSize := DstBlobStream.Size;
              // Calculation of the original data Checksum Value
              {$IFDEF WIN64}
              SetLength(A, BlobStream.Size);
              Move(BlobStream.Memory^, A[0], BlobStream.Size);
              P := @A[0];
              CRC32Value1 := CheckSumCRC32(P, BlobStream.Size);
              {$ELSE}
              CRC32Value1 := CheckSumCRC32(BlobStream.Memory^, BlobStream.Size);
              {$ENDIF}
              // Write Blob data
              DBStream.Position := BlobDataPos;
              DBStream.Write(DstBlobStream.Memory^, DstBlobStream.Size);
              // Read Blob data
              DBStream.Position := BlobDataPos;
              DBStream.Read(DstBlobStream.Memory^, DstBlobStream.Size);
              // Decoding
              try
                FDatabase.DBFileIO.DecodeMemoryStream(DstBlobStream, DstBlobStream1, ShouldEncrypt(I), ShouldCompress(I));
              except
              end;
              // Calculate the decoded data Checksum Value
              {$IFDEF WIN64}
              SetLength(A, DstBlobStream1.Size);
              Move(DstBlobStream1.Memory^, A[0], DstBlobStream1.Size);
              P := @A[0];
              CRC32Value1 := CheckSumCRC32(P, DstBlobStream1.Size);
              {$ELSE}
              CRC32Value2 := CheckSumCRC32(DstBlobStream1.Memory^, DstBlobStream1.Size);
              {$ENDIF}
              // Checksum Value if the two are equal, then checkout is correct, out of circulation
            until CRC32Value1 = CRC32Value2;
          end
          else
          // If you can not afford to use CRC32 checkout
          begin
            // Blob data is written directly to
            DBStream.Write(DstBlobStream.Memory^, DstBlobStream.Size);
          end;
          // Fill Blob region, making Blob length tdbBlobSizeUnitNum an integer multiple of
          DBStream.Write(BlobRestbuf[0], tdbBlobSizeUnitNum - DstBlobSize mod tdbBlobSizeUnitNum);
        end
        else
        // Fixed-length data
        begin
          // Data coding (encryption)
          FDatabase.DBFileIO.EncodeMemoryBuffer(FieldBuffer, DstDiskRecBuf + FDiskFieldOffsets[I],
            FieldBuffers.Items[I].FieldSize, ShouldEncrypt(I));
        end;
      end;
      // Re-written to the database
      DBStream.Position := DataOffset;
      DBStream.Write(DstDiskRecBuf^, FDiskRecSize);
    finally
      // Release memory
      FreeMem(DstDiskRecBuf, FDiskRecSize);
      FreeMem(SrcDiskRecBuf, FDiskRecSize);
      DstBlobStream.Free;
      DstBlobStream1.Free;
    end;
    // Adjustment RecTabLists [0]
    MemRecTabItem.DataOffset := DataOffset;
    MemRecTabItem.RecIndex := RecTotal - 1;
    AddMemRecTabItem(FRecTabLists[0], MemRecTabItem);
    // Adjustment Index
    AdjustIndexesForAppend(FieldBuffers, DataOffset, RecTotal - 1, OnAdjustIndex);
    if Flush then FDatabase.DBFileIO.Flush;
  finally
    FDatabase.DBFileIO.Unlock;
  end;
end;

//-----------------------------------------------------------------------------
// Will FieldBuffers the data is written to the database record of the first PhyRecordIdx
// FieldBuffers: going to modify the field data (includes all the physical fields)
// PhyRecordIdx: Physical record number (that is, FRecTabLists [0] in the record number) 0-based
// Flush: changes exhausted it flush cache
//-----------------------------------------------------------------------------
procedure TTinyTableIO.ModifyRecordData(FieldBuffers: TFieldBuffers;
  PhyRecordIdx: Integer;  Flush: Boolean;
  OnAdjustIndex: TOnAdjustIndexForModifyEvent);
var
  I, DataOffset: Integer;
  BakPos, BlobDataPos: Integer;
  DstBlobSize: Integer;
  BlobHeader, NewBlobHeader: TBlobFieldHeader;
  BlobRestBuf: array[0..tdbBlobSizeUnitNum-1] of AnsiChar;
  BlobStream, DstBlobStream, DstBlobStream1: TMemoryStream;
  CRC32Value1, CRC32Value2: Longword;
  DiskRecBuf: PAnsiChar;
  DBStream: TStream;
  FieldBuffer: Pointer;
  {$IFDEF WIN64}
  A: array of Byte;
  P: Pointer;
  {$ENDIF}
begin
  FDatabase.DBFileIO.Lock;
  try
    DBStream := FDatabase.DBFileIO.DBStream;
    DstBlobStream := TMemoryStream.Create;
    DstBlobStream1 := TMemoryStream.Create;
    DiskRecBuf := AllocMem(FDiskRecSize);
    try
      // Check Primary, Unique field uniqueness
      CheckUniqueFieldForModify(FieldBuffers, PhyRecordIdx);

      FillChar(BlobRestBuf, SizeOf(BlobRestBuf), 0);
      FillChar(BlobHeader, SizeOf(BlobHeader), 0);
      DataOffset := GetMemRecTabItem(FRecTabLists[0], PhyRecordIdx).DataOffset;
      DBStream.Position := DataOffset;
      DBStream.Read(DiskRecBuf^, FDiskRecSize);

      for I := 0 to FieldBuffers.Count - 1 do
      begin
        if not FieldBuffers.Items[I].Active then Continue;

        FieldBuffer := FieldBuffers.Items[I].Buffer;
        // -- If it is indefinite length of the data --
        if FieldBuffers.Items[I].IsBlob then
        begin
          BlobStream := TMemoryStream(FieldBuffer);
          // Data encoding (compression, encryption)
          FDatabase.DBFileIO.EncodeMemoryStream(BlobStream, DstBlobStream, ShouldEncrypt(I), ShouldCompress(I));
          DstBlobSize := DstBlobStream.Size;
          // Read the original BlobHeader data
          BlobHeader := PBlobFieldHeader(DiskRecBuf + FDiskFieldOffsets[I])^;
          NewBlobHeader := BlobHeader;
          // if DstBlobSize <= BlobHeader.AreaSize then
          if DstBlobSize < BlobHeader.AreaSize then  // modified by haoxg 2004.11.14
          begin
            BlobDataPos := BlobHeader.DataOffset;
            NewBlobHeader.DataOffset := BlobDataPos;
            NewBlobHeader.DataSize := DstBlobSize;
            NewBlobHeader.AreaSize := BlobHeader.AreaSize;
          end
          else
          begin
            // If the Blob data in the file at the end of
            if BlobHeader.DataOffset + BlobHeader.AreaSize = DBStream.Size then
            begin
              BlobDataPos := BlobHeader.DataOffset;
              NewBlobHeader.DataOffset := BlobDataPos;
            end
            else
            // If the Blob data is not end of file
            begin
              BlobDataPos := DBStream.Size;
              NewBlobHeader.DataOffset := BlobDataPos;
            end;
            NewBlobHeader.DataSize := DstBlobSize;
            NewBlobHeader.AreaSize := DstBlobSize + (tdbBlobSizeUnitNum - DstBlobSize mod tdbBlobSizeUnitNum);
          end;
          // If begun using CRC32 checkout
          if FDatabase.CRC32 then
          begin
            CRC32Value1 := 0;
            CRC32Value2 := 0;
            repeat
              if CRC32Value1 <> CRC32Value2 then
                FDatabase.DBFileIO.EncodeMemoryStream(BlobStream, DstBlobStream, ShouldEncrypt(I), ShouldCompress(I));
              DstBlobSize := DstBlobStream.Size;
              // Calculation of the original data Checksum Value
              {$IFDEF WIN64}
              SetLength(A, BlobStream.Size);
              Move(BlobStream.Memory^, A[0], BlobStream.Size);
              P := @A[0];
              CRC32Value1 := CheckSumCRC32(P, BlobStream.Size);
              {$ELSE}
              CRC32Value1 := CheckSumCRC32(BlobStream.Memory^, BlobStream.Size);
              {$ENDIF}
              // Write Blob data
              DBStream.Position := BlobDataPos;
              DBStream.Write(DstBlobStream.Memory^, DstBlobStream.Size);
              // Read Blob data
              DBStream.Position := BlobDataPos;
              DBStream.Read(DstBlobStream.Memory^, DstBlobStream.Size);
              // Decoding
              try
                FDatabase.DBFileIO.DecodeMemoryStream(DstBlobStream, DstBlobStream1, ShouldEncrypt(I), ShouldCompress(I));
              except
              end;
              // Calculate the decoded data Checksum Value
              {$IFDEF WIN64}
              SetLength(A, DstBlobStream1.Size);
              Move(DstBlobStream1.Memory^, A[0], DstBlobStream1.Size);
              P := @A[0];
              CRC32Value1 := CheckSumCRC32(P, DstBlobStream1.Size);
              {$ELSE}
              CRC32Value2 := CheckSumCRC32(DstBlobStream1.Memory^, DstBlobStream1.Size);
              {$ENDIF}
              // Checksum Value if the two are equal, then checkout is correct, out of circulation
            until CRC32Value1 = CRC32Value2;
          end
          else
          // If you can not afford to use CRC32 checkout
          begin
            // Blob data is written directly to
            DBStream.Position := BlobDataPos;
            DBStream.Write(DstBlobStream.Memory^, DstBlobStream.Size);
          end;
          // Fill Blob region, making Blob length TDBlobSizeUnitNum an integer multiple of
          DBStream.Write(BlobRestBuf[0], tdbBlobSizeUnitNum - DstBlobSize mod tdbBlobSizeUnitNum);
          // Write the adjusted BlobHeader
          PBlobFieldHeader(DiskRecBuf + FDiskFieldOffsets[I])^ := NewBlobHeader;
        // ------------ If it is AutoInc field------------------
        end
        else
        if FieldBuffers.Items[I].FieldType = ftAutoInc then
        begin
          // Make changes
          if PInteger(FieldBuffer)^ <> 0 then
          begin
            // Data coding (encryption)
            FDatabase.DBFileIO.EncodeMemoryBuffer(FieldBuffer, DiskRecBuf + FDiskFieldOffsets[I],
              FieldBuffers.Items[I].FieldSize, ShouldEncrypt(I));
            // Adjustment AutoIncCounter
            if PInteger(FieldBuffer)^ > FTableHeader.AutoIncCounter then
            begin
              FTableHeader.AutoIncCounter := PInteger(FieldBuffer)^;
              BakPos := DBStream.Position;
              DBStream.Position := FDatabase.DBFileIO.TableTab.TableHeaderOffset[FTableIdx] + SizeOf(TTableNameString) + SizeOf(Integer) * 2;
              DBStream.Write(FTableHeader.AutoIncCounter, SizeOf(Integer));
              DBStream.Position := BakPos;
            end;
          end;
        end
        else
        //  -------------Other fixed-length data-----------------
        begin
          // Data coding (encryption)
          FDatabase.DBFileIO.EncodeMemoryBuffer(FieldBuffer, DiskRecBuf + FDiskFieldOffsets[I],
            FieldBuffers.Items[I].FieldSize, ShouldEncrypt(I));
        end;
      end;
      // Write to the database
      DBStream.Position := DataOffset;
      DBStream.Write(DiskRecBuf^, FDiskRecSize);
    finally
      // Release memory
      FreeMem(DiskRecBuf, FDiskRecSize);
      DstBlobStream.Free;
      DstBlobStream1.Free;
    end;
    // Adjustment Index
    AdjustIndexesForModify(FieldBuffers, PhyRecordIdx, OnAdjustIndex);
    if Flush then FDatabase.DBFileIO.Flush;
  finally
    FDatabase.DBFileIO.Unlock;
  end;
end;

//-----------------------------------------------------------------------------
// Delete records
// PhyRecordIdx: Physical record number (that is, FRecTabLists[0] in the record number) 0-based
// Flush: changes exhausted it flush cache
//-----------------------------------------------------------------------------
procedure TTinyTableIO.DeleteRecordData(PhyRecordIdx: Integer; Flush: Boolean);
begin
  FDatabase.DBFileIO.Lock;
  try
    // To do in the database to delete tag
    WriteDeleteFlag(PhyRecordIdx);
    // Adjustment Index
    AdjustIndexesForDelete(PhyRecordIdx);
    // Remove FRecTabLists [0] in the corresponding items
    DeleteMemRecTabItem(FRecTabLists[0], PhyRecordIdx);
    if Flush then FDatabase.DBFileIO.Flush;
  finally
    FDatabase.DBFileIO.Unlock;
  end;
end;

//-----------------------------------------------------------------------------
// Delete all records
//-----------------------------------------------------------------------------
procedure TTinyTableIO.DeleteAllRecords;
var
  I: Integer;
begin
  FDatabase.DBFileIO.Lock;
  try
    // Adjustment FTableHeader
    FTableHeader.RecTabOffset := 0;
    FTableHeader.RecordTotal := 0;
    FTableHeader.AutoIncCounter := 0;
    for I := 0 to tdbMaxIndex - 1 do
    begin
      FTableHeader.IndexHeader[I].StartIndex := 0;
      FTableHeader.IndexHeader[I].IndexOffset := 0;
    end;
    FDatabase.DBFileIO.WriteTableHeader(TableIdx, FTableHeader);
    FDatabase.DBFileIO.Flush;

    // Empty FRecTabLists
    for I := 0 to High(FRecTabLists) do
      ClearMemRecTab(FRecTabLists[I]);

    // Empty FRecTabBlockOffsets and FIdxTabBlockOffsets
    SetLength(FRecTabBlockOffsets, 0);
    for I := 0 to High(FIdxTabBlockOffsets) do
      SetLength(FIdxTabBlockOffsets[I], 0);
  finally
    FDatabase.DBFileIO.Unlock;
  end;
end;

//-----------------------------------------------------------------------------
// Read a record data in a field in
// DstStream: the resulting data
// DiskRecIndex: This RecordTab recorded in the file the next label (0-based)
// FieldIdx: Physical Field Number (0-based)
//-----------------------------------------------------------------------------
procedure TTinyTableIO.ReadFieldData(DstStream: TMemoryStream; DiskRecIndex, FieldIdx: Integer);
var
  FieldOffset, FieldSize: Integer;
  RecTabItemOffset: Integer;
  FieldType: TFieldType;
begin
  // Records in the table to take the first month RecTabItem offset RecIndex
  RecTabItemOffset := GetRecTabItemOffset(DiskRecIndex);
  FieldOffset := FDiskFieldOffsets[FieldIdx];
  FieldType := FTableHeader.FieldTab[FieldIdx].FieldType;
  if FieldType in BlobFieldTypes then
    FieldSize := SizeOf(TBlobFieldHeader)
  else
    FieldSize := GetFieldSize(FieldType, FTableHeader.FieldTab[FieldIdx].FieldSize);

  FDatabase.DBFileIO.ReadFieldData(DstStream, RecTabItemOffset, FieldOffset, FieldSize,
    FieldType in BlobFieldTypes, ShouldEncrypt(FieldIdx), ShouldCompress(FieldIdx));
end;

//-----------------------------------------------------------------------------
// Read the record data into the FieldBuffers in
// RecTabList: Record set for limited RecIndex
// RecIndex: This parameter RecTabList recorded in the next label (0-based)
// Note:
// FieldBuffer must first specify the good FieldType, FieldSize, Buffer, and more.
// If some fields do not read, can correspond to FieldBufferItem in the Active Set False.
//-----------------------------------------------------------------------------
procedure TTinyTableIO.ReadRecordData(FieldBuffers: TFieldBuffers; RecTabList: TList; RecordIdx: Integer);
var
  SrcDiskRecBuf: PAnsiChar;
  RecDataOffset: Integer;
  I, FldSize, DiskOfs: Integer;
  BlobStream: TMemoryStream;
begin
  FDatabase.DBFileIO.Lock;
  SrcDiskRecBuf := AllocMem(FDiskRecSize);
  try
    RecDataOffset := GetMemRecTabItem(RecTabList, RecordIdx).DataOffset;
    FDatabase.DBFileIO.ReadBuffer(SrcDiskRecBuf[0], RecDataOffset, FDiskRecSize);
    for I := 0 to FieldBuffers.Count - 1 do
    begin
      if FieldBuffers.Items[I].Active then
      begin
        DiskOfs := FDiskFieldOffsets[I];
        if not FieldBuffers.Items[I].IsBlob then
        begin
          FldSize := FieldBuffers.Items[I].FFieldSize;
          FDatabase.DBFileIO.DecodeMemoryBuffer(SrcDiskRecBuf + DiskOfs, FieldBuffers.Items[I].Buffer,
            FldSize, ShouldEncrypt(I) );
        end
        else
        begin
          // Blobstream remove from the Buffer
          BlobStream := TMemoryStream(FieldBuffers.Items[I].Buffer);
          // Initialization BlobStream
          (BlobStream as TOptimBlobStream).Init(RecDataOffset + DiskOfs, ShouldEncrypt(I), ShouldCompress(I));
        end;
      end;
    end;
  finally
    FreeMem(SrcDiskRecBuf);
    FDatabase.DBFileIO.Unlock;
  end;
end;

{ TTDEDataSet }

function TTDEDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
begin
  {$IFDEF UNICODE}
  if Length(Bookmark1) <> Length(Bookmark2) then
    if Length(Bookmark1) < Length(Bookmark2) then Result := -1 else Result := 1
  else
  if SysUtils.CompareMem(@Bookmark1[0], @Bookmark2[0], SizeOf(TBookmark)) then
    Result := 0
  else
  begin
    if Length(Bookmark1) = SizeOf(Integer) then
      if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then Result := -1 else Result := 1
    else
      Result := -1;
  end;
  {$ELSE}
  if (Bookmark1 = nil) and (Bookmark2 = nil) then Result := 0
  else if (Bookmark1 <> nil) and (Bookmark2 = nil) then Result := 1
  else if (Bookmark1 = nil) and (Bookmark2 <> nil) then Result := -1
  else if PInteger(Bookmark1)^ > PInteger(Bookmark2)^ then Result := 1
  else if PInteger(Bookmark1)^ < PInteger(Bookmark2)^ then Result := -1
  else Result := 0;
  {$ENDIF}
end;

constructor TTDEDataSet.Create(AOwner: TComponent);
begin
  inherited;
  ShowNagScreen(Self); // Show nag-screen.
  FDatabaseName := '';
  FMediumType := mtDisk;
  FCurRec := -1;
  FRecordSize := 0;
  FFilterParser := TFilterParser.Create(Self);
end;

destructor TTDEDataSet.Destroy;
begin
  FreeAndNil(FFilterParser);
  inherited;
end;

function TTDEDataSet.GetActiveRecBuf(var RecBuf: TRecordBuffer): Boolean;
begin
  case State of
    dsBrowse: if IsEmpty then RecBuf := nil else RecBuf := TRecordBuffer(ActiveBuffer);
    dsEdit, dsInsert: RecBuf := TRecordBuffer(ActiveBuffer);
    dsSetKey: RecBuf := FKeyBuffer;
    dsCalcFields: RecBuf := TRecordBuffer(CalcBuffer);
    dsFilter: RecBuf := FFilterBuffer;
    dsNewValue: RecBuf := TRecordBuffer(ActiveBuffer);
  else
    RecBuf := nil;
  end;
  Result := RecBuf <> nil;
end;

procedure TTDEDataSet.ActivateFilters;
begin
end;

procedure TTDEDataSet.DeactivateFilters;
begin
end;

procedure TTDEDataSet.ReadRecordData(Buffer: TRecordBuffer; RecordIdx: Integer);
begin
end;

function TTDEDataSet.GetFieldOffsetByFieldNo(FieldNo: Integer): Integer;
begin
  Result := FFieldOffsets[FieldNo - 1];
end;

procedure TTDEDataSet.ReadFieldData(DstStream: TMemoryStream; FieldDataOffset, FieldSize: Integer;
  IsBlob: Boolean; ShouldEncrypt, ShouldCompress: Boolean);
begin
  Database.DBFileIO.ReadFieldData(DstStream, FieldDataOffset, FieldSize, IsBlob, ShouldEncrypt, ShouldCompress);
end;

function TTDEDataSet.GetRecordCount: Longint;
begin
  Result := 0;
end;

procedure TTDEDataSet.SetMediumType(Value: TTinyDBMediumType);
var
  ADatabase: TTinyDatabase;
begin
  if FMediumType <> Value then
  begin
    CheckInactive;
    FMediumType := Value;
    ADatabase := DBSession.FindDatabase(FDatabaseName);
    if ADatabase <> nil then
      ADatabase.MediumType := FMediumType;
  end;
end;

procedure TTDEDataSet.SetPassword(const Value: string);
var
  ADatabase: TTinyDatabase;
begin
  ADatabase := OpenDatabase(False);
  if ADatabase <> nil then
    ADatabase.SetPassword(Value);
end;

procedure TTDEDataSet.SetCRC32(Value: Boolean);
var
  ADatabase: TTinyDatabase;
begin
  ADatabase := OpenDatabase(False);
  if ADatabase <> nil then
    ADatabase.CRC32 := Value;
end;

function TTDEDataSet.GetCRC32: Boolean;
begin
  Result := (Database <> nil) and Database.CRC32;
end;

function TTDEDataSet.GetCanAccess: Boolean;
begin
  Result := (Database <> nil) and Database.CanAccess;
end;

procedure TTDEDataSet.InitRecordSize;
var
  I: Integer;
begin
  // Initialization of FRecordSize
  FRecordSize := 0;
  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I].FieldNo > 0 then
    begin
      if Fields[I].IsBlob then
        Inc(FRecordSize, SizeOf(PMemoryStream))
      else
        Inc(FRecordSize, Fields[I].DataSize);
    end;
  end;

  // Initialization of FRecBufSize
  FRecBufSize := FRecordSize + CalcFieldsSize + SizeOf(TRecInfo);
end;

procedure TTDEDataSet.InitFieldOffsets;
var
  I, Offset, MaxNo: Integer;
begin
  Offset := 0;
  MaxNo := 0;
  for I := 0 to Fields.Count - 1 do
    if MaxNo < Fields[I].FieldNo then
      MaxNo := Fields[I].FieldNo;

  SetLength(FFieldOffsets, MaxNo);
  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I].FieldNo > 0 then
    begin
      FFieldOffsets[Fields[I].FieldNo - 1] := Offset;
      if Fields[I].IsBlob then
        Inc(Offset, SizeOf(PMemoryStream))
      else
        Inc(Offset, Fields[I].DataSize);
    end;
  end;
end;

function TTDEDataSet.FiltersAccept: Boolean;

  function FuncFilter: Boolean;
  begin
    Result := True;
    if Assigned(OnFilterRecord) then
      OnFilterRecord(Self, Result);
  end;

  function ExprFilter: Boolean;
  begin
    Result := True;
    if Filter <> '' then
      Result := FFilterParser.Calculate(TStrCompOptions(FilterOptions)) <> 0;
  end;

begin
  Result := FuncFilter;
  if Result then Result := ExprFilter;
end;

procedure TTDEDataSet.SetFilterData(const Text: string; Options: TFilterOptions);
var
  Changed: Boolean;
  SaveText: string;
begin
  Changed := False;
  SaveText := Filter;
  if Active then
  begin
    CheckBrowseMode;
    if Text <> '' then FFilterParser.Parse(Text);
    if (Filter <> Text) or (FilterOptions <> Options) then Changed := True;
  end;

  inherited SetFilterText(Text);
  inherited SetFilterOptions(Options);
  try
    if Changed then
      if Filtered then
      begin
        DeactivateFilters;
        ActivateFilters;
      end;
  except
    inherited SetFilterText(SaveText);
    raise;
  end;
end;

procedure TTDEDataSet.AllocKeyBuffers;
var
  KeyIndex: TTDKeyIndex;
begin
  try
    for KeyIndex := Low(TTDKeyIndex) to High(TTDKeyIndex) do
    begin
      FKeyBuffers[KeyIndex] := AllocRecordBuffer;
      InitKeyBuffer(KeyIndex);
    end;
  except
    FreeKeyBuffers;
    raise;
  end;
end;

procedure TTDEDataSet.FreeKeyBuffers;
var
  KeyIndex: TTDKeyIndex;
begin
  for KeyIndex := Low(TTDKeyIndex) to High(TTDKeyIndex) do
    FreeRecordBuffer(FKeyBuffers[KeyIndex]);
end;

procedure TTDEDataSet.InitKeyBuffer(KeyIndex: TTDKeyIndex);
begin
  InternalInitRecord(FKeyBuffers[KeyIndex]);
end;

//-----------------------------------------------------------------------------
// TDataSet calls this method to allocate the record buffer.  Here we use
// FRecBufSize which is equal to the size of the data plus the size of the
// TRecInfo structure.
//-----------------------------------------------------------------------------
function TTDEDataSet.AllocRecordBuffer: TRecordBuffer;
var
  I, FieldOffset: Integer;
  BlobStream: TMemoryStream;
begin
  Result := AllocMem(FRecBufSize);
  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I].IsBlob then
    begin
      FieldOffset := GetFieldOffsetByFieldNo(Fields[I].FieldNo);
      BlobStream := TOptimBlobStream.Create(Self);
      PMemoryStream(@Result[FieldOffset])^ := BlobStream;
    end;
  end;
end;

function TTDEDataSet.BufferToRecBuf(Buffer: TRecordBuffer): PRecInfo;
begin
  Result := PRecInfo(Buffer + FRecordSize + CalcFieldsSize);
end;

//-----------------------------------------------------------------------------
// Again, TDataSet calls this method to free the record buffer.
// Note: Make sure the value of FRecBufSize does not change before all
// allocated buffers are freed.
//-----------------------------------------------------------------------------
procedure TTDEDataSet.FreeRecordBuffer(var Buffer: TRecordBuffer);
var
  I, FieldOffset: Integer;
  BlobStream: TMemoryStream;
begin
  if Buffer = nil then Exit;
  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I].IsBlob then
    begin
      FieldOffset := GetFieldOffsetByFieldNo(Fields[I].FieldNo);
      BlobStream := PMemoryStream(@Buffer[FieldOffset])^;
      if Assigned(BlobStream) then
      begin
        BlobStream.Free;
      end;
    end;
  end;
  FreeMem(Buffer, FRecBufSize);
  Buffer := nil;
end;

procedure TTDEDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  Move(BufferToRecBuf(Buffer).Bookmark, Data^, SizeOf(Integer));
  //PInteger(Data)^ := PRecInfo(Buffer + FRecordSize + CalcFieldsSize).Bookmark;
end;
{$IFDEF DELPHI_17_UP}
procedure TTDEDataSet.GetBookmarkData(Buffer: TRecordBuffer; Data: TBookmark);
begin
  Move(BufferToRecBuf(Buffer).Bookmark, PInteger(Data)^, SizeOf(Integer));
  //PInteger(Data)^ := PRecInfo(Buffer + FRecordSize + CalcFieldsSize).Bookmark;
end;
{$ENDIF}
{$IFDEF DELPHI_18_UP}
procedure TTDEDataSet.GetBookmarkData(Buffer: TRecBuf; Data: TBookmark);
begin
  PInteger(Data)^ := PRecInfo(Buffer + FRecordSize + CalcFieldsSize).Bookmark;
end;
{$ENDIF}
//-----------------------------------------------------------------------------
// Bookmark flags are used to indicate if a particular record is the first
// or last record in the dataset.  This is necessary for "crack" handling.
// If the bookmark flag is bfBOF or bfEOF then the bookmark is not actually
// used; InternalFirst, or InternalLast are called instead by TDataSet.
//-----------------------------------------------------------------------------
function TTDEDataSet.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := BufferToRecBuf(Buffer).BookmarkFlag;
end;

//-----------------------------------------------------------------------------
// This method returns the size of just the data in the record buffer.
// Do not confuse this with RecBufSize which also includes any additonal
// structures stored in the record buffer (such as TRecInfo).
//-----------------------------------------------------------------------------
function TTDEDataSet.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

//-----------------------------------------------------------------------------
// This routine is called to initialize a record buffer.  In this sample,
// we fill the buffer with zero values, but we might have code to initialize
// default values or do other things as well.
//-----------------------------------------------------------------------------
procedure TTDEDataSet.InternalInitRecord(Buffer: TRecordBuffer);
var
  I, FieldOffset: Integer;
  BlobStream: TMemoryStream;
  TempDateTime: TDateTime;
  TempTimeStamp: TTimeStamp;
  TempDouble: Double;
  TempInteger: Integer;
begin
  for I := 0 to Fields.Count - 1 do
  begin
    if Fields[I].FieldNo > 0 then
    begin
      FieldOffset := GetFieldOffsetByFieldNo(Fields[I].FieldNo);
      if Fields[I].IsBlob then
      begin
        BlobStream := PMemoryStream(@Buffer[FieldOffset])^;
        BlobStream.Clear;
      end
      else
      if Fields[I].DataType = ftDateTime then
      begin
        TempDateTime := 0; //Now;
        TempTimeStamp := DateTimeToTimeStamp(TempDateTime);
        TempDouble := TimeStampToMSecs(TempTimeStamp);
        Move(TempDouble, Buffer[FieldOffset], Fields[I].DataSize);
      end
      else
      if Fields[I].DataType = ftDate then
      begin
        TempInteger := DateTimeToTimeStamp(SysUtils.Date).Date;
        Move(TempInteger, Buffer[FieldOffset], Fields[I].DataSize);
      end
      else
      if Fields[I].DataType = ftTime then
      begin
        TempInteger := DateTimeToTimeStamp(SysUtils.Time).Time;
        Move(TempInteger, Buffer[FieldOffset], Fields[I].DataSize);
      end
      else
      begin
        FillChar(Buffer[FieldOffset], Fields[I].DataSize, 0);
      end;
    end
    else {fkCalculated, fkLookup}
    begin
      FieldOffset := FRecordSize + Fields[I].Offset;
      FillChar(Buffer[FieldOffset], 1, 0);
      if Fields[I].IsBlob then
      begin
        BlobStream := PMemoryStream(@Buffer[FieldOffset + 1])^;
        BlobStream.Clear;
      end
      else
      begin
        FillChar(Buffer[FieldOffset + 1], Fields[I].DataSize, 0);
      end;
    end;
  end;
end;

//-----------------------------------------------------------------------------
// This method is called by TDataSet.First.  Crack behavior is required.
// That is we must position to a special place *before* the first record.
// Otherwise, we will actually end up on the second record after Resync
// is called.
//-----------------------------------------------------------------------------
procedure TTDEDataSet.InternalFirst;
begin
  FCurRec := -1;
end;

//-----------------------------------------------------------------------------
// Again, we position to the crack *after* the last record here.
//-----------------------------------------------------------------------------
procedure TTDEDataSet.InternalLast;
begin
  FCurRec := RecordCount;
end;

//-----------------------------------------------------------------------------
// This is the exception handler which is called if an exception is raised
// while the component is being stream in or streamed out.  In most cases this
// should be implemented useing the application exception handler as follows. }
//-----------------------------------------------------------------------------
procedure TTDEDataSet.InternalHandleException;
begin
  Application.HandleException(Self);
end;

//-----------------------------------------------------------------------------
// This function does the same thing as InternalGotoBookmark, but it takes
// a record buffer as a parameter instead
//-----------------------------------------------------------------------------
procedure TTDEDataSet.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  InternalGotoBookmark(@BufferToRecBuf(Buffer).Bookmark);
end;

{$IFDEF DELPHI_18_UP}
procedure TTDEDataSet.InternalSetToRecord(Buffer: TRecBuf);
begin
  InternalGotoBookmark(@PRecInfo(Buffer + FRecordSize + CalcFieldsSize).Bookmark);
end;
{$ENDIF}

procedure TTDEDataSet.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  BufferToRecBuf(Buffer).BookmarkFlag := Value;
end;

procedure TTDEDataSet.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  BufferToRecBuf(Buffer).Bookmark := PInteger(Data)^;
end;

procedure TTDEDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RecBuf: TRecordBuffer;
  FieldOffset: Integer;
begin
  if not GetActiveRecBuf(RecBuf) then Exit;

  if Field.FieldNo > 0 then
  begin
    if Buffer <> nil then
    begin
      FieldOffset := GetFieldOffsetByFieldNo(Field.FieldNo);
      if Field.IsBlob then
      begin
        DatabaseError(SGeneralError);
      end
      else
      begin
        Move(Buffer^, RecBuf[FieldOffset], Field.DataSize);
      end;
      DataEvent(deFieldChange, Longint(Field));
    end;
  end
  else {fkCalculated, fkLookup}
  begin
    Inc(RecBuf, FRecordSize + Field.Offset);
    Boolean(RecBuf[0]) := LongBool(Buffer);
    if Boolean(RecBuf[0]) then
      Move(Buffer^, RecBuf[1], Field.DataSize);
  end;
  if (State <> dsCalcFields) and (State <> dsFilter) then
    DataEvent(deFieldChange, Longint(Field));
end;

{$IFDEF DELPHI_17_UP}
procedure TTDEDataSet.SetFieldData(Field: TField; Buffer: TValueBuffer);
begin
  Self.SetFieldData(Field, Pointer(Buffer));
end;
{$ENDIF}

//-----------------------------------------------------------------------------
// This property is used while opening the dataset.
// It indicates if data is available even though the
// current state is still dsInActive.
//-----------------------------------------------------------------------------
function TTDEDataSet.IsCursorOpen: Boolean;
begin
  Result := False;
end;

procedure TTDEDataSet.DataConvert(Field: TField; Source, Dest: Pointer; ToNative: Boolean);

  { DateTime Conversions }

  function NativeToDateTime(DataType: TFieldType; Data: TDateTimeRec): TDateTime;
  var
    TimeStamp: TTimeStamp;
  begin
    case DataType of
      ftDate:
        begin
          TimeStamp.Time := 0;
          TimeStamp.Date := Data.Date;
        end;
      ftTime:
        begin
          TimeStamp.Time := Data.Time;
          TimeStamp.Date := DateDelta;
        end;
    else
      try
        TimeStamp := MSecsToTimeStamp(Data.DateTime);
      except
        TimeStamp.Time := 0;
        TimeStamp.Date := 0;
      end;
    end;
    try
      Result := TimeStampToDateTime(TimeStamp);
    except
      Result := 0;
    end;
  end;

  function DateTimeToNative(DataType: TFieldType; Data: TDateTime): TDateTimeRec;
  var
    TimeStamp: TTimeStamp;
  begin
    TimeStamp := DateTimeToTimeStamp(Data);
    case DataType of
      ftDate: Result.Date := TimeStamp.Date;
      ftTime: Result.Time := TimeStamp.Time;
    else
      Result.DateTime := TimeStampToMSecs(TimeStamp);
    end;
  end;

begin
  case Field.DataType of
    ftDate, ftTime, ftDateTime:
      if ToNative then
        TDateTimeRec(Dest^) := DateTimeToNative(Field.DataType, TDateTime(Source^))
      else
        TDateTime(Dest^) := NativeToDateTime(Field.DataType, TDateTimeRec(Source^));
  else
    inherited;
  end;
end;

function TTDEDataSet.GetRecNo: Longint;
begin
  UpdateCursorPos;
  if (FCurRec = -1) and (RecordCount > 0) then
    Result := 1
  else
    Result := FCurRec + 1;
end;

procedure TTDEDataSet.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if (Value >= 0) and (Value <= RecordCount) and (Value <> RecNo) then
  begin
    DoBeforeScroll;
    FCurRec := Value - 1;
    Resync([]);
    DoAfterScroll;
  end;
end;

function TTDEDataSet.GetCanModify: Boolean;
begin
  Result := FCanModify;
end;

procedure TTDEDataSet.SetFiltered(Value: Boolean);
begin
  if Active then
  begin
    CheckBrowseMode;
    if Filtered <> Value then
    begin
      if Value then ActivateFilters
      else DeactivateFilters;
      inherited SetFiltered(Value);
    end;
    First;
  end
  else
    inherited SetFiltered(Value);
end;

procedure TTDEDataSet.SetFilterOptions(Value: TFilterOptions);
begin
  SetFilterData(Filter, Value);
end;

procedure TTDEDataSet.SetFilterText(const Value: string);
begin
  SetFilterData(Value, FilterOptions);
end;

procedure TTDEDataSet.DoAfterOpen;
begin
  if Filtered then ActivateFilters;
  inherited;
end;

function TTDEDataSet.FindRecord(Restart, GoForward: Boolean): Boolean;
var
  RecIdx, Step, StartIdx, EndIdx: Integer;
  SaveCurRec: Integer;
  Accept: Boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  SetFound(False);
  UpdateCursorPos;
  CursorPosChanged;

  if GoForward then
  begin
    Step := 1;
    if Restart then StartIdx := 0
    else StartIdx := FCurRec + 1;
    EndIdx := RecordCount - 1;
  end
  else
  begin
    Step := -1;
    if Restart then StartIdx := RecordCount - 1
    else StartIdx := FCurRec - 1;
    EndIdx := 0;
  end;

  if Filter <> '' then FFilterParser.Parse(Filter);
  SaveCurRec := FCurRec;
  SetTempState(dsFilter);
  try
    Accept := False;
    RecIdx := StartIdx;
    while (GoForward and (RecIdx <= EndIdx)) or
      (not GoForward and (RecIdx >= EndIdx))
    do
    begin
      FCurRec := RecIdx;
      FFilterBuffer := TRecordBuffer(ActiveBuffer);
      ReadRecordData(FFilterBuffer, FCurRec);
      Accept := FiltersAccept;
      if Accept then Break;
      Inc(RecIdx, Step);
    end;
  finally
    RestoreState(dsBrowse);
  end;

  if Accept then
  begin
    SetFound(True);
  end
  else
  begin
    FCurRec := SaveCurRec;
  end;
  Resync([rmExact, rmCenter]);
  Result := Found;
  if Result then DoAfterScroll;
end;

function TTDEDataSet.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TTinyBlobStream.Create(Field as TBlobField, Mode);
end;

//-----------------------------------------------------------------------------
// Remove the field values from the RecordBuffer
// Return:
//   True: success, and the value is not empty
//   False: failure, or the value is null
//-----------------------------------------------------------------------------
function TTDEDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RecBuf: TRecordBuffer;
  FieldOffset: Integer;
begin
  Result := GetActiveRecBuf(RecBuf);
  if not Result then Exit;

  if Field.FieldNo > 0 then
  begin
    if Buffer <> nil then
    begin
      FieldOffset := GetFieldOffsetByFieldNo(Field.FieldNo);
      if Field.IsBlob then
      begin
        DatabaseError(SGeneralError);
      end
      else
      begin
        Move(RecBuf[FieldOffset], Buffer^, Field.DataSize);
      end;
      Result := True;
    end;
  end
  else {fkCalculated, fkLookup}
  begin
    FieldOffset := FRecordSize + Field.Offset;
    Result := Boolean(RecBuf[FieldOffset]);
    if Result and (Buffer <> nil) then
      Move((RecBuf + FieldOffset + 1)^, Buffer^, Field.DataSize);
  end;
end;

{$IFDEF DELPHI_17_UP}
function TTDEDataSet.GetFieldData(Field: TField; {$IFDEF DELPHI_18_UP}var {$ENDIF}Buffer: TValueBuffer): Boolean;
var
  RecBuf: TRecordBuffer;
  FieldOffset: Integer;
begin
  Result := GetActiveRecBuf(RecBuf);
  if not Result then Exit;

  if Field.FieldNo > 0 then
  begin
    if Buffer <> nil then
    begin
      FieldOffset := GetFieldOffsetByFieldNo(Field.FieldNo);
      if Field.IsBlob then
      begin
        DatabaseError(SGeneralError);
      end
      else
      begin
        Move(RecBuf[FieldOffset], Buffer[0], Field.DataSize);
      end;
      Result := True;
    end;
  end
  else {fkCalculated, fkLookup}
  begin
    FieldOffset := FRecordSize + Field.Offset;
    Result := Boolean(RecBuf[FieldOffset]);
    if Result and (Buffer <> nil) then
      Move((RecBuf + FieldOffset + 1)^, Buffer[0], Field.DataSize);
  end;
end;
{$ENDIF}

{ TTDBDataSet }

constructor TTDBDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TTinyDatabase then
  begin
    DatabaseName := (AOwner as TTinyDatabase).DatabaseName;
    SessionName := (AOwner as TTinyDatabase).SessionName;
  end;
end;

procedure TTDBDataSet.CheckDBSessionName;
var
  S: TTinySession;
  Database: TTinyDatabase;
begin
  if (SessionName <> '') and (DatabaseName <> '') then
  begin
    S := Sessions.FindSession(SessionName);
    if Assigned(S) and not Assigned(S.DoFindDatabase(DatabaseName, Self)) then
    begin
      Database := DefaultSession.DoFindDatabase(DatabaseName, Self);
      if Assigned(Database) then Database.CheckSessionName(True);
    end;
  end;
end;

procedure TTDBDataSet.OpenCursor(InfoQuery: Boolean);
begin
  CheckDBSessionName;
  FDatabase := OpenDatabase(True);
  FDatabase.RegisterClient(Self);
  FDatabase.CheckCanAccess;
  inherited;
end;

procedure TTDBDataSet.CloseCursor;
begin
  inherited;
  if FDatabase <> nil then
  begin
    FDatabase.UnregisterClient(Self);
    FDatabase.Session.CloseDatabase(FDatabase);
    FDatabase := nil;
  end;
end;

function TTDBDataSet.OpenDatabase(IncRef: Boolean): TTinyDatabase;
begin
  with Sessions.List[FSessionName] do
    Result := DoOpenDatabase(FDatabasename, Self.Owner, Self, IncRef);
end;

procedure TTDBDataSet.CloseDatabase(Database: TTinyDatabase);
begin
  if Assigned(Database) then
    Database.Session.CloseDatabase(Database);
end;

procedure TTDBDataSet.Disconnect;
begin
  Close;
end;

function TTDBDataSet.GetDBSession: TTinySession;
begin
  if (FDatabase <> nil) then
    Result := FDatabase.Session
  else
    Result := Sessions.FindSession(SessionName);
  if Result = nil then Result := DefaultSession;
end;

procedure TTDBDataSet.SetDatabaseName(const Value: string);
begin
  if csReading in ComponentState then
    FDatabaseName := Value
  else if FDatabaseName <> Value then
  begin
    CheckInactive;
    if FDatabase <> nil then DatabaseError(SDatabaseOpen, Self);
    FDatabaseName := Value;
    DataEvent(dePropertyChange, 0);
  end;
end;

procedure TTDBDataSet.SetSessionName(const Value: string);
begin
  CheckInactive;
  FSessionName := Value;
  DataEvent(dePropertyChange, 0);
end;

{ TTinyTable }

constructor TTinyTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTableName := '';
  FIndexName := '';
  FIndexIdx := -1;
  FUpdateCount := 0;
  FIndexDefs := TIndexDefs.Create(Self);
  FMasterLink := TMasterDataLink.Create(Self);
  FMasterLink.OnMasterChange := MasterChanged;
  FMasterLink.OnMasterDisable := MasterDisabled;
end;

destructor TTinyTable.Destroy;
begin
  FreeAndNil(FIndexDefs);
  FreeAndNil(FMasterLink);
  inherited Destroy;
end;

function TTinyTable.GetRecordCount: Integer;
begin
  if FRecTabList = nil then
    Result := 0
  else
    Result := FRecTabList.Count;
end;

function TTinyTable.GetCanModify: Boolean;
begin
  Result := inherited GetCanModify and not ReadOnly and not Database.ReadOnly and not Database.DBFileIO.FileIsReadOnly;
end;

function TTinyTable.GetDataSource: TDataSource;
begin
  Result := FMasterLink.DataSource;
end;

procedure TTinyTable.SetDatabaseName(const Value: string);
begin
  inherited;
  FTableName := '';
  FIndexName := '';
end;

procedure TTinyTable.DoAfterOpen;
begin
  CheckMasterRange;
  inherited;
end;

procedure TTinyTable.SetTableName(const Value: string);
begin
  if FTableName <> Value then
  begin
    CheckInactive;
    FTableName := Value;
  end;
end;

procedure TTinyTable.SetIndexName(const Value: string);
begin
  if Value = '' then
  begin
    FIndexName := Value;
    FIndexIdx := -1;
  end
  else
  begin
    FIndexName := Value;
  end;
  if Active then
  begin
    CheckBrowseMode;
    FIndexIdx := FTableIO.IndexDefs.IndexOf(FIndexName);
    if FIndexIdx = -1 then
      FIndexIdx := FTableIO.CheckPrimaryFieldExists;
    SwitchToIndex(FIndexIdx);
    CheckMasterRange;
    First;
  end;
end;

procedure TTinyTable.SetReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TTinyTable.SetMasterFields(const Value: string);
var
  SaveValue: string;
begin
  SaveValue := FMasterLink.FieldNames;
  try
    FMasterLink.FieldNames := Value;
  except
    FMasterLink.FieldNames := SaveValue;
    raise;
  end;
end;

procedure TTinyTable.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then DatabaseError(SCircularDataLink, Self);
  FMasterLink.DataSource := Value;
end;

function TTinyTable.GetTableIdx: Integer;
begin
  if FTableIO <> nil then
    Result := FTableIO.TableIdx
  else
    Result := -1;
end;

function TTinyTable.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

procedure TTinyTable.InitIndexDefs;
var
  I, J: Integer;
  IndexName, Fields: string;
  Options: TIndexOptions;
  List: TStrings;
begin
  List := TStringList.Create;
  FIndexDefs.Clear;
  for I := 0 to FTableIO.IndexDefs.Count - 1 do
  begin
    IndexName := FTableIO.IndexDefs[I].Name;
    List.Clear;
    for J := 0 to High(FTableIO.IndexDefs[I].FieldIdxes) do
      List.Add(FTableIO.FieldDefs[FTableIO.IndexDefs[I].FieldIdxes[J]].Name);
    Fields := List.CommaText;
    Options := [];
    if tiPrimary in FTableIO.IndexDefs[I].Options then Include(Options, ixPrimary);
    if tiUnique in FTableIO.IndexDefs[I].Options then Include(Options, ixUnique);
    if tiDescending in FTableIO.IndexDefs[I].Options then Include(Options, ixDescending);
    if tiCaseInsensitive in FTableIO.IndexDefs[I].Options then Include(Options, ixCaseInsensitive);

    FIndexDefs.Add(IndexName, Fields, Options);
  end;
  List.Free;
end;

procedure TTinyTable.InitCurRecordTab;
begin
  FIndexIdx := FTableIO.IndexDefs.IndexOf(FIndexName);
  if FIndexIdx = -1 then
    FIndexIdx := FTableIO.CheckPrimaryFieldExists;
  SwitchToIndex(FIndexIdx);
end;

procedure TTinyTable.ClearMemRecTab(AList: TList);
var
  I: Integer;
  F: PMemRecTabItem;
begin
  if not Assigned(AList) then Exit;
  for I := 0 to AList.Count - 1 do
  begin
    F := AList.Items[I];
    Dispose(F);
    AList.Items[I] := nil;
  end;
  AList.Clear;
end;

procedure TTinyTable.AddMemRecTabItem(AList: TList; Value: TMemRecTabItem);
var
  MemRecTabItemPtr: PMemRecTabItem;
begin
  New(MemRecTabItemPtr);
  MemRecTabItemPtr^ := Value;
  AList.Add(MemRecTabItemPtr);
end;

procedure TTinyTable.InsertMemRecTabItem(AList: TList; Index: Integer; Value: TMemRecTabItem);
var
  MemRecTabItemPtr: PMemRecTabItem;
begin
  New(MemRecTabItemPtr);
  MemRecTabItemPtr^ := Value;
  AList.Insert(Index, MemRecTabItemPtr);
end;

procedure TTinyTable.DeleteMemRecTabItem(AList: TList; Index: Integer);
var
  F: PMemRecTabItem;
begin
  F := AList.Items[Index];
  Dispose(F);
  AList.Items[Index] := nil;
  AList.Delete(Index);
end;

function TTinyTable.GetMemRecTabItem(AList: TList; Index: Integer): TMemRecTabItem;
begin
  Result := PMemRecTabItem(AList.Items[Index])^;
end;

//-----------------------------------------------------------------------------
// Switch the index
// IndexIdx: No. 0-based index
//-----------------------------------------------------------------------------
procedure TTinyTable.SwitchToIndex(IndexIdx: Integer);
var
  I: Integer;
  MemRecTabItemPtr: PMemRecTabItem;
begin
  FTableIO.InitRecTabList(IndexIdx + 1);

  ClearMemRecTab(FRecTabList);
  for I := 0 to FTableIO.RecTabLists[IndexIdx + 1].Count - 1 do
  begin
    New(MemRecTabItemPtr);
    MemRecTabItemPtr^ := PMemRecTabItem(FTableIO.RecTabLists[IndexIdx + 1].Items[I])^;
    FRecTabList.Add(MemRecTabItemPtr);
  end;

  FCanModify := True;
end;

//-----------------------------------------------------------------------------
// Read the record data into the Buffer in the
// RecordIdx: centralized records in the current record number 0-based
// Note: Buffer storage format of the data in accordance with the definition of TDataSet.ActiveBuffer
// Non-Blob field in sequence, Blob field instead of using PMemoryStream
//-----------------------------------------------------------------------------
procedure TTinyTable.ReadRecordData(Buffer: TRecordBuffer; RecordIdx: Integer);
var
  FieldBuffers: TFieldBuffers;
begin
  if (RecordIdx < 0) or (RecordIdx >= RecordCount) then Exit;

  FieldBuffers := TFieldBuffers.Create;
  RecordBufferToFieldBuffers(Buffer, FieldBuffers);
  FTableIO.ReadRecordData(FieldBuffers, FRecTabList, RecordIdx);
  FieldBuffers.Free;
end;

//-----------------------------------------------------------------------------
// AppendRecordData callback function
//-----------------------------------------------------------------------------
procedure TTinyTable.OnAdjustIndexForAppend(IndexIdx, InsertPos: Integer; MemRecTabItem: TMemRecTabItem);
var
  Pos, ResultState: Integer;
begin
  if not Filtered and not FSetRanged then
  begin
    if FIndexIdx = IndexIdx then
    begin
      if FIndexIdx = -1 then
      begin
        AddMemRecTabItem(FRecTabList, MemRecTabItem);
      end
      else
      begin
        // Adjustment FRecTabList
        InsertMemRecTabItem(FRecTabList, InsertPos, MemRecTabItem);
        // Adjustment FCurRec
        FCurRec := InsertPos;
      end;
    end;
  end
  else
  begin
    if FIndexIdx = IndexIdx then
    begin
      if FIndexIdx = -1 then
      begin
        AddMemRecTabItem(FRecTabList, MemRecTabItem);
      end
      else
      begin
        // To find the location should be inserted
        Pos := SearchInsertPos(IndexIdx, ResultState);
        // Adjustment FRecTabList
        InsertMemRecTabItem(FRecTabList, Pos, MemRecTabItem);
        // Adjustment FCurRec
        FCurRec := Pos;
      end;
    end;
  end;
end;

//-----------------------------------------------------------------------------
// Buffer data will be added to the database
// Note: Buffer data format defined in the same ReadRecordData
//-----------------------------------------------------------------------------
procedure TTinyTable.AppendRecordData(Buffer: TRecordBuffer);
var
  FieldBuffers: TFieldBuffers;
begin
  FieldBuffers := TFieldBuffers.Create;
  try
    RecordBufferToFieldBuffers(Buffer, FieldBuffers);
    FTableIO.AppendRecordData(FieldBuffers, (FUpdateCount = 0) and Database.FlushCacheAlways, OnAdjustIndexForAppend);
  finally
    FieldBuffers.Free;
  end;
end;

//-----------------------------------------------------------------------------
// ModifyRecordData callback function
//-----------------------------------------------------------------------------
procedure TTinyTable.OnAdjustIndexForModify(IndexIdx, FromRecIdx, ToRecIdx: Integer);
var
  MemRecTabItem: TMemRecTabItem;
  Pos, ResultState: Integer;
begin
  if not Filtered and not FSetRanged then
  begin
    // If this is the current index
    if FIndexIdx = IndexIdx then
    begin
      // Because in FRecTabList Insert before you do Delete, so ToRecIdx need to be adjusted.
      if ToRecIdx > FromRecIdx then Dec(ToRecIdx);
      // Adjustment FRecTabList
      MemRecTabItem := GetMemRecTabItem(FRecTabList, FromRecIdx);
      DeleteMemRecTabItem(FRecTabList, FromRecIdx);
      InsertMemRecTabItem(FRecTabList, ToRecIdx, MemRecTabItem);
      // Adjustment FCurRec
      FCurRec := ToRecIdx;
    end;
  end
  else
  begin
    // If this is the current index
    if FIndexIdx = IndexIdx then
    begin
      // Adjustment FRecTabList
      MemRecTabItem := GetMemRecTabItem(FRecTabList, FCurRec);
      DeleteMemRecTabItem(FRecTabList, FCurRec);
      // To find the location should be inserted
      Pos := SearchInsertPos(IndexIdx, ResultState);
      InsertMemRecTabItem(FRecTabList, Pos, MemRecTabItem);
      // Adjustment FCurRec
      FCurRec := Pos;
    end;
  end;
end;

//-----------------------------------------------------------------------------
// Buffer data will be written to the database record of the first RecordIdx
// RecordIdx: the current record centralized record number 0-based
// Note: Buffer data format defined in the same ReadRecordData
//-----------------------------------------------------------------------------
procedure TTinyTable.ModifyRecordData(Buffer: TRecordBuffer; RecordIdx: Integer);
var
  FieldBuffers: TFieldBuffers;
  PhyRecordIdx: Integer;
begin
  if (RecordIdx < 0) or (RecordIdx >= RecordCount) then Exit;
  FTableIO.ConvertRecIdxForPhy(FRecTabList, RecordIdx, PhyRecordIdx);

  FieldBuffers := TFieldBuffers.Create;
  try
    RecordBufferToFieldBuffers(Buffer, FieldBuffers);
    FTableIO.ModifyRecordData(FieldBuffers, PhyRecordIdx, (FUpdateCount = 0) and Database.FlushCacheAlways, OnAdjustIndexForModify);
  finally
    FieldBuffers.Free;
  end;
end;

//-----------------------------------------------------------------------------
// Delete records
// RecordIdx: the current record centralized record number 0-based
//-----------------------------------------------------------------------------
procedure TTinyTable.DeleteRecordData(RecordIdx: Integer);
var
  PhyRecordIdx: Integer;
begin
  if (RecordIdx < 0) or (RecordIdx >= RecordCount) then Exit;

  FTableIO.ConvertRecIdxForPhy(FRecTabList, RecordIdx, PhyRecordIdx);
  FTableIO.DeleteRecordData(PhyRecordIdx, (FUpdateCount = 0) and Database.FlushCacheAlways);
  // Remove FRecTabList the corresponding items
  DeleteMemRecTabItem(FRecTabList, RecordIdx);
end;

//-----------------------------------------------------------------------------
// Delete all records
//-----------------------------------------------------------------------------
procedure TTinyTable.DeleteAllRecords;
begin
  FTableIO.DeleteAllRecords;
  ClearMemRecTab(FRecTabList);
  FCurRec := -1;
end;

//-----------------------------------------------------------------------------
// According to specified data from the index to find the nearest location (dichotomy)
// RecTabList: To find the record set Note: must be a sorted recordset
// IndexIdx: No. 0-based index
// ResultState: store the search results state
//     0: to be the results of search to find the location of value = value
//     1: to be to find the value of the to find the location of the value of the results of
//    -1: To be to find the value of the location to find the value of the results of
//    -2: No Record
// EffFieldCount: Fields in the number of effective fields. The default is 0, that, in terms of actual number of fields in composite index terms
// PartialCompare: string partial matches
// Return value: RecTabList the record number (0-based), it returns -1 if there is no record of
// Note: The search for data should be stored in advance in the Fields of
//-----------------------------------------------------------------------------
function TTinyTable.SearchIndexedField(RecTabList: TList; IndexIdx: Integer;
  var ResultState: Integer; EffFieldCount: Integer; PartialCompare: Boolean): Integer;
var
  FieldBuffers: TFieldBuffers;
  RecBuf: TRecordBuffer;
  OK: Boolean;
begin
  FieldBuffers := TFieldBuffers.Create;
  try
    try
      OK := GetActiveRecBuf(RecBuf);
      if OK then
      begin
        RecordBufferToFieldBuffers(RecBuf, FieldBuffers);
        Result := FTableIO.SearchIndexedField(FieldBuffers, RecTabList, IndexIdx, ResultState, EffFieldCount, PartialCompare);
      end
      else
        Result := -1;
    except
      Result := -1;
    end;
  finally
    FieldBuffers.Free;
  end;
end;

//-----------------------------------------------------------------------------
// According to the index to find the boundaries of the data from the specified location (dichotomy)
// RecTabList: To find the record set Note: must be a sorted recordset
// IndexIdx: No. 0-based index
// LowBound: when the search for True Low-border, in order to False when the search for high-boundary
// ResultState: store the search results state, the definition of the same SearchIndexedField
// EffFieldCount: Fields in the number of effective fields. The default is 0, that, in terms of actual number of fields in composite index terms
// PartialCompare: string partial matches
// Return value: RecTabList the record number (0-based), it returns -1 if there is no record of
// Note:
// 1. To find data should be stored in advance in the Fields of
//-----------------------------------------------------------------------------
function TTinyTable.SearchIndexedFieldBound(RecTabList: TList; IndexIdx: Integer;
  LowBound: Boolean; var ResultState: Integer; EffFieldCount: Integer; PartialCompare: Boolean): Integer;
var
  FieldBuffers: TFieldBuffers;
  RecBuf: TRecordBuffer;
  OK: Boolean;
begin
  FieldBuffers := TFieldBuffers.Create;
  try
    try
      OK := GetActiveRecBuf(RecBuf);
      if OK then
      begin
    RecordBufferToFieldBuffers(RecBuf, FieldBuffers);
    Result := FTableIO.SearchIndexedFieldBound(FieldBuffers, RecTabList, IndexIdx, LowBound, ResultState, EffFieldCount, PartialCompare);
      end
      else
        Result := -1;
    except
      Result := -1;
    end;
  finally
    FieldBuffers.Free;
  end;
end;

//-----------------------------------------------------------------------------
// According to the index position to strike a SubRangeStart
// RecTabList: To find the record set Note: must be a sorted recordset
// IndexIdx: index number (0-based), should be in the sort index RecTabList
// ResultState: store the search results state, the definition of the same SearchIndexedField
// EffFieldCount: Fields in the number of effective fields. The default is 0, that, in terms of actual number of fields in composite index terms
// Return value: the results obtained in the RecTabList the record number (0-based)
// Note: 1. To find the record data should be stored in advance in the Fields of
//-----------------------------------------------------------------------------
function TTinyTable.SearchRangeStart(RecTabList: TList; IndexIdx: Integer;
  var ResultState: Integer; EffFieldCount: Integer): Integer;
var
  SaveState: TDataSetState;
  SaveKeyBuffer: TRecordBuffer;
begin
  SaveState := SetTempState(dsSetKey);
  SaveKeyBuffer := FKeyBuffer;
  FKeyBuffer := FKeyBuffers[tkRangeStart];
  try
    Result := SearchIndexedFieldBound(RecTabList, IndexIdx, True, ResultState, EffFieldCount);
    if ResultState = 1 then Inc(Result);
  finally
    RestoreState(SaveState);
    FKeyBuffer := SaveKeyBuffer;
  end;
end;

//-----------------------------------------------------------------------------
// According to the index position to strike a RangeEnd
// RecTabList: To find the record set Note: must be a sorted recordset
// IndexIdx: index number (0-based), should be in the sort index RecTabList
// ResultState: store the search results state, the definition of the same SearchIndexedField
// EffFieldCount: Fields in the number of effective fields. The default is 0, that, in terms of actual number of fields in composite index terms
// Return value: the results obtained in the RecTabList the record number (0-based)
// Note: 1. To find the record data should be stored in advance in the Fields of
//-----------------------------------------------------------------------------
function TTinyTable.SearchRangeEnd(RecTabList: TList; IndexIdx: Integer;
  var ResultState: Integer; EffFieldCount: Integer): Integer;
var
  SaveState: TDataSetState;
  SaveKeyBuffer: TRecordBuffer;
begin
  SaveState := SetTempState(dsSetKey);
  SaveKeyBuffer := FKeyBuffer;
  FKeyBuffer := FKeyBuffers[tkRangeEnd];
  try
    Result := SearchIndexedFieldBound(RecTabList, IndexIdx, False, ResultState, EffFieldCount);
    if ResultState = -1 then Dec(Result);
  finally
    RestoreState(SaveState);
    FKeyBuffer := SaveKeyBuffer;
  end;
end;

//-----------------------------------------------------------------------------
// Find the index records
// RecTabList: To find the record set Note: must be a sorted recordset
// IndexIdx: index number (0-based), should be in the sort index RecTabList
// EffFieldCount: Fields in the number of effective fields. The default is 0, that, in terms of actual number of fields in composite index terms
// GotoKey and GotoNearest to use this function
//-----------------------------------------------------------------------------
function TTinyTable.SearchKey(RecTabList: TList; IndexIdx: Integer;
  EffFieldCount: Integer; Nearest: Boolean): Boolean;
var
  Pos, ResultState, CurRec: Integer;
  SaveState: TDataSetState;
begin
  Result := False;
  if IndexIdx = -1 then Exit;

  SaveState := SetTempState(dsSetKey);
  try
    Pos := SearchIndexedField(RecTabList, IndexIdx, ResultState, EffFieldCount, True);
    FTableIO.ConvertRecordIdx(RecTabList, Pos, FRecTabList, CurRec);

    if ResultState = -2 then // No records, return False
    begin
      Result := False;
    end
    else
    if (ResultState = 0) and (CurRec <> -1) then // Found
    begin
      FCurRec := CurRec;
      Result := True;
    end
    else // Similar records found
    begin
      if Nearest then
        if CurRec <> -1 then
          FCurRec := CurRec;
      Result := Nearest;
    end;
  finally
    RestoreState(SaveState);
  end;
end;

function TTinyTable.SearchInsertPos(IndexIdx: Integer; var ResultState: Integer): Integer;
begin
  Result := SearchIndexedField(FRecTabList, IndexIdx, ResultState);
  if ResultState in [0, 1] then Inc(Result)
  else if ResultState = -2 then Result := 0;
end;

//-----------------------------------------------------------------------------
// FindKey, SetRange to use this process
//-----------------------------------------------------------------------------
procedure TTinyTable.SetKeyFields(KeyIndex: TTDKeyIndex; const Values: array of const);
begin
  if FIndexIdx = -1 then DatabaseError(SNoFieldIndexes, Self);
  SetKeyFields(FIndexIdx, KeyIndex, Values);
end;

procedure TTinyTable.SetKeyFields(IndexIdx: Integer; KeyIndex: TTDKeyIndex; const Values: array of const);
var
  I, FieldIdx: Integer;
  SaveState: TDataSetState;
begin
  SaveState := SetTempState(dsSetKey);
  try
    InitKeyBuffer(KeyIndex);
    FKeyBuffer := FKeyBuffers[KeyIndex];
    for I := 0 to High(FTableIO.IndexDefs[IndexIdx].FieldIdxes) do
    begin
      FieldIdx := FTableIO.IndexDefs[IndexIdx].FieldIdxes[I];
      if I <= High(Values) then
      begin
        Fields[FieldIdx].AssignValue(Values[I]);
      end
      else
        Fields[FieldIdx].Clear;
    end;
  finally
    RestoreState(SaveState);
  end;
end;

function TTinyTable.LocateRecord(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions; SyncCursor: Boolean): Boolean;

  function SearchOrderly(AFields: TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF}): Integer;
  var
    DataStream: TMemoryStream;
    RecIdx, FieldIdx, CompRes, I: Integer;
    MemRecTabItem: TMemRecTabItem;
    FieldType: TFieldType;
    RecBuf: TRecordBuffer;
  begin
    Result := -1;
    DataStream := TMemoryStream.Create;
    try
      GetActiveRecBuf(RecBuf);
      for RecIdx := 0 to RecordCount - 1 do
      begin
        MemRecTabItem := GetMemRecTabItem(FRecTabList, RecIdx);
        CompRes := 0;
        for I := 0 to AFields.Count - 1 do
        begin
          FieldIdx := TField(AFields[I]).FieldNo - 1;
          FieldType := TField(AFields[I]).DataType;
          FTableIO.ReadFieldData(DataStream, MemRecTabItem.RecIndex, FieldIdx);
          CompRes := FTableIO.CompFieldData(RecBuf + FFieldOffsets[FieldIdx],
            DataStream.Memory, FieldType, loCaseInsensitive in Options, loPartialKey in Options);
          if CompRes <> 0 then Break;
        end;
        if CompRes = 0 then
        begin
          Result := RecIdx;
          Break;
        end;
      end;
    finally
      DataStream.Free;
    end;
  end;

  function SearchByIndex(AFields: TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF}; IndexIdx: Integer): Integer;
  var
    ResultState: Integer;
    RecIdx, DstRecIdx: Integer;
  begin
    Result := -1;
    RecIdx := SearchIndexedField(FTableIO.RecTabLists[IndexIdx+1], IndexIdx, ResultState, 0, loPartialKey in Options);
    if (RecIdx <> -1) and (ResultState = 0) then
    begin
      FTableIO.ConvertRecIdxForCur(IndexIdx, RecIdx, FRecTabList, DstRecIdx);
      if (DstRecIdx >= 0) and (DstRecIdx < RecordCount) then
        Result := DstRecIdx;
    end;
  end;

var
  I, FieldCount, RecIdx, IndexIdx: Integer;
  Buffer: TRecordBuffer;
  Fields: TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF};
begin
  CheckBrowseMode;
  CursorPosChanged;

  Buffer := TRecordBuffer(TempBuffer);
  Fields := TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF}.Create;
  SetTempState(dsFilter);
  FFilterBuffer := Buffer;
  try
    GetFieldList(Fields, KeyFields);

    FieldCount := Fields.Count;
    if FieldCount = 1 then
    begin
      if VarIsArray(KeyValues) then
        TField(Fields.First).Value := KeyValues[0]
      else
        TField(Fields.First).Value := KeyValues;
    end
    else
      for I := 0 to FieldCount - 1 do
        TField(Fields[I]).Value := KeyValues[I];

    IndexIdx := MapsToIndexForSearch(Fields, loCaseInsensitive in Options);
    if IndexIdx = -1 then
      RecIdx := SearchOrderly(Fields)
    else
      RecIdx := SearchByIndex(Fields, IndexIdx);
    Result := RecIdx <> -1;
    if Result then
    begin
      ReadRecordData(Buffer, RecIdx);
      if SyncCursor then FCurRec := RecIdx;
    end;

  finally
    RestoreState(dsBrowse);
    Fields.Free;
  end;
end;

//-----------------------------------------------------------------------------
// Check the Fields is about to find whether, and an index matching
// Return: If the match is found, it returns the index number (0-based), did not return -1
//-----------------------------------------------------------------------------
function TTinyTable.MapsToIndexForSearch(Fields: TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF}; CaseInsensitive: Boolean): Integer;
var
  I, J: Integer;
  HasStr, Ok: Boolean;
begin
  Result := -1;
  HasStr := False;
  for I := 0 to Fields.Count - 1 do
  begin
    HasStr := TField(Fields[I]).DataType in [ftString, ftFixedChar, ftWideString];
    if HasStr then Break;
  end;
  for I := 0 to FTableIO.IndexDefs.Count - 1 do
  begin
    Ok := True;
    if not HasStr or (CaseInsensitive = (tiCaseInsensitive in FTableIO.IndexDefs[I].Options)) then
    begin
      if Fields.Count = Length(FTableIO.IndexDefs[I].FieldIdxes) then
      begin
        for J := 0 to High(FTableIO.IndexDefs[I].FieldIdxes) do
        begin
          if TField(Fields[J]).FieldNo - 1 <> FTableIO.IndexDefs[I].FieldIdxes[J] then
          begin
            Ok := False;
            Break;
          end;
        end;
      end
      else
        Ok := False;
    end
    else
      Ok := False;
    if Ok then
    begin
      Result := I;
      Break;
    end;
  end;
end;

//-----------------------------------------------------------------------------
// Check Filter can match the index, in order to optimize the processing
//-----------------------------------------------------------------------------
function TTinyTable.CheckFilterMapsToIndex: Boolean;
var
  Node: TExprNode;
  I, FieldIdx: Integer;
  Exists: Boolean;
begin
  Result := True;

  if Assigned(OnFilterRecord) then
  begin
    Result := False;
    Exit;
  end;

  Node := FFilterParser.FExprNodes.FNodes;
  while Node <> nil do
  begin
    if Node.FKind = enField then
    begin
      FieldIdx := FTableIO.FieldDefs.IndexOf(string(Node.FData));
      if FieldIdx = -1 then
      begin
        Result := False;
        Break;
      end
      else
      begin
        Exists := False;
        for I := 0 to FTableIO.IndexDefs.Count - 1 do
          if (Length(FTableIO.IndexDefs[I].FFieldIdxes) = 1) and
            (FTableIO.IndexDefs[I].FFieldIdxes[0] = FieldIdx)
          then
          begin
            Exists := True;
            Break;
          end;
        if Exists = False then
        begin
          Result := False;
          Break;
        end;
      end;
    end;
    if Node.FOperator in [toLIKE] then
    begin
      Result := False;
      Break;
    end;
    Node := Node.FNext;
  end;
end;

procedure TTinyTable.MasterChanged(Sender: TObject);
begin
  SetLinkRange(TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF}(FMasterLink.Fields));
end;

procedure TTinyTable.MasterDisabled(Sender: TObject);
begin
  CancelRange;
end;

procedure TTinyTable.SetLinkRange(MasterFields: TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF});

  function GetIndexField(Index: Integer): TField;
  var
    I: Integer;
  begin
    I := FTableIO.IndexDefs[FIndexIdx].FieldIdxes[Index];
    Result := Fields[I];
  end;

var
  SaveState: TDataSetState;
  StartIdx, EndIdx: Integer;
  I, ResultState: Integer;
  RecTabList: TList;
begin
  if FIndexIdx = -1 then Exit;
  if Filtered then Filtered := False;
  // if FSetRanged then CancelRange;

  // Set the scope of pre-initialization of the value of Fields
  CheckBrowseMode;
  SaveState := SetTempState(dsSetKey);
  try
    FKeyBuffer := FKeyBuffers[tkRangeStart];
    InitKeyBuffer(tkRangeStart);
    for I := 0 to MasterFields.Count - 1 do
      GetIndexField(I).Assign(TField(MasterFields[I]));

    FKeyBuffer := FKeyBuffers[tkRangeEnd];
    InitKeyBuffer(tkRangeEnd);
    for I := 0 to MasterFields.Count - 1 do
      GetIndexField(I).Assign(TField(MasterFields[I]));
  finally
    RestoreState(SaveState);
  end;

  // Setting Range
  CheckBrowseMode;
  FEffFieldCount := MasterFields.Count;
  StartIdx := SearchRangeStart(FTableIO.RecTabLists[FIndexIdx+1], FIndexIdx, ResultState, FEffFieldCount);
  if ResultState = -2 then Exit;
  EndIdx := SearchRangeEnd(FTableIO.RecTabLists[FIndexIdx+1], FIndexIdx, ResultState, FEffFieldCount);
  if ResultState = -2 then Exit;

  RecTabList := TList.Create;
  for I := StartIdx to EndIdx do
    AddMemRecTabItem(RecTabList, GetMemRecTabItem(FTableIO.RecTabLists[FIndexIdx+1], I));

  ClearMemRecTab(FRecTabList);
  for I := 0 to RecTabList.Count - 1 do
    AddMemRecTabItem(FRecTabList, GetMemRecTabItem(RecTabList, I));
  ClearMemRecTab(RecTabList);
  RecTabList.Free;

  FSetRanged := True;
  //FCanModify := False;
  First;
end;

procedure TTinyTable.CheckMasterRange;
begin
  if FMasterLink.Active and (FMasterLink.Fields.Count > 0) then
  begin
    SetLinkRange(TList{$IFDEF DELPHI_17_UP}<TField>{$ENDIF}(FMasterLink.Fields));
  end;
end;

//-----------------------------------------------------------------------------
// RecordBuffer format to FieldBuffers conversion
// Just let the FieldBuffers pointer RecordBuffer offset in each field office, but not copy the data.
//-----------------------------------------------------------------------------
procedure TTinyTable.RecordBufferToFieldBuffers(RecordBuffer: TRecordBuffer; FieldBuffers: TFieldBuffers);
var
  I: Integer;
  Field: TField;
begin
  FieldBuffers.Clear;
  for I := 0 to FTableIO.FieldDefs.Count - 1 do
  begin
    Field := FieldByNumber(I + 1);
    if Field = nil then
    begin
      FieldBuffers.Add(nil, ftUnknown, 0);
      FieldBuffers.Items[FieldBuffers.Count - 1].Active := False;
    end
    else
    begin
      FieldBuffers.Add(RecordBuffer + FFieldOffsets[I], Field.DataType, Field.DataSize);
    end;
  end;
end;

function TTinyTable.FieldDefsStored: Boolean;
begin
  Result := FieldDefs.Count > 0;
end;

function TTinyTable.IndexDefsStored: Boolean;
begin
  Result := IndexDefs.Count > 0;
end;

procedure TTinyTable.ActivateFilters;
var
  I: Integer;
  Accept: Boolean;
  RecTabList: TList;
begin
  RecTabList := TList.Create;
  SetTempState(dsFilter);
  if DBSession.SQLHourGlass then Screen.Cursor := crSQLWait;
  try
    if Filter <> '' then FFilterParser.Parse(Filter);

    FFilterMapsToIndex := CheckFilterMapsToIndex;
    //if FFilterMapsToIndex then
    //begin
      //showmessage('yes, maps to index.');
    //  FiltersAccept;
    //end else
    //begin
      FFilterBuffer := TRecordBuffer(ActiveBuffer);
      for I := 0 to RecordCount - 1 do
      begin
        FCurRec := I;
        ReadRecordData(FFilterBuffer, FCurRec);
        Accept := FiltersAccept;
        if Accept then
          AddMemRecTabItem(RecTabList, GetMemRecTabItem(FRecTabList, I));
        if Assigned(FOnFilterProgress) then
          FOnFilterProgress(Self, Trunc(I/RecordCount*100));
      end;
      if FRecTabList.Count <> RecTabList.Count then
      begin
        ClearMemRecTab(FRecTabList);
        for I := 0 to RecTabList.Count - 1 do
          AddMemRecTabItem(FRecTabList, GetMemRecTabItem(RecTabList, I));
      end;
    //end;
  finally
    Screen.Cursor := crDefault;
    RestoreState(dsBrowse);
    ClearMemRecTab(RecTabList);
    RecTabList.Free;
    FCurRec := -1;
    First;
    //FCanModify := False;
  end;
end;

procedure TTinyTable.DeactivateFilters;
begin
  InitCurRecordTab;
  FCurRec := -1;
  First;
  FCanModify := True;
end;

procedure TTinyTable.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TTinyTable.EndUpdate;
begin
  Dec(FUpdateCount);
end;

//-----------------------------------------------------------------------------
// This method is called by TDataSet.Open and also when FieldDefs need to
// be updated (usually by the DataSet designer).  Everything which is
// allocated or initialized in this method should also be freed or
// uninitialized in the InternalClose method.
//-----------------------------------------------------------------------------
procedure TTinyTable.InternalOpen;
begin
  FTableIO := Database.TableIOByName(FTableName);
  if FTableIO = nil then
    DatabaseErrorFmt(STableNotFound, [FTableName], Self);
  FTableIO.Open;

  FRecTabList := TList.Create;
  FUpdateCount := 0;
  FCurRec := -1;

  BookmarkSize := SizeOf(Integer);
  FCanModify := True;
  InternalInitFieldDefs;
  if DefaultFields then CreateFields;
  BindFields(True);

  InitIndexDefs;
  InitCurRecordTab;
  InitRecordSize;
  InitFieldOffsets;
  AllocKeyBuffers;
end;

procedure TTinyTable.InternalClose;
begin
  if FTableIO <> nil then FTableIO.Close;
  ClearMemRecTab(FRecTabList);
  FreeAndNil(FRecTabList);
  FreeKeyBuffers;

  { Destroy the TField components if no persistent fields }
  if DefaultFields then DestroyFields;
  { Reset these internal flags }
  FCurRec := -1;
  FCanModify := False;
end;

//-----------------------------------------------------------------------------
// For this simple example we just create one FieldDef, but a more complete
// TDataSet implementation would create multiple FieldDefs based on the
// actual data.
//-----------------------------------------------------------------------------
procedure TTinyTable.InternalInitFieldDefs;
var
  I: Integer;
  FieldType: TFieldType;
  FieldSize: Integer;
begin
  FieldDefs.Clear;

  for I := 0 to FTableIO.FieldDefs.Count -1  do
  begin
    FieldType := FTableIO.FieldDefs[I].FieldType;
    if FieldType in StringFieldTypes then
      FieldSize := FTableIO.FieldDefs[I].FieldSize
    else
      FieldSize := 0;
    FieldDefs.Add(FTableIO.FieldDefs[I].Name,
                  FieldType,
                  FieldSize,
                  False);
  end;
end;

// Bookmarks

//-----------------------------------------------------------------------------
// In this sample the bookmarks are stored in the Object property of the
// TStringList holding the data.  Positioning to a bookmark just requires
// finding the offset of the bookmark in the TStrings.Objects and using that
// value as the new current record pointer.
//-----------------------------------------------------------------------------
procedure TTinyTable.InternalGotoBookmark(Bookmark: Pointer);
var
  I, Index: Integer;
begin
  Index := -1;
  for I := 0 to FRecTabList.Count - 1 do
  begin
    if PInteger(Bookmark)^ = GetMemRecTabItem(FRecTabList, I).RecIndex then
    begin
      Index := I;
      Break;
    end;
  end;
  if Index <> -1 then
    FCurRec := Index
  else
    DatabaseError(SBookmarkNotFound);
end;

//-----------------------------------------------------------------------------
// This multi-purpose function does 3 jobs.  It retrieves data for either
// the current, the prior, or the next record.  It must return the status
// (TGetResult), and raise an exception if DoCheck is True.
//-----------------------------------------------------------------------------
function TTinyTable.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
begin
  if RecordCount < 1 then
    Result := grEOF
  else
  begin
    Result := grOK;
    case GetMode of
      gmNext:
        if FCurRec >= RecordCount - 1 then
          Result := grEOF
        else
          Inc(FCurRec);
      gmPrior:
        if FCurRec <= 0 then
          Result := grBOF
        else
          Dec(FCurRec);
      gmCurrent:
        if (FCurRec < 0) or (FCurRec >= RecordCount) then
          Result := grError;
    end;
    if Result = grOK then
    begin
      ReadRecordData(Buffer, FCurRec);
      with BufferToRecBuf(Buffer)^ do
      begin
        BookmarkFlag := bfCurrent;
        Bookmark := GetMemRecTabItem(FRecTabList, FCurRec).RecIndex;
      end;
      GetCalcFields(Buffer);
    end
    else
      if (Result = grError) and DoCheck then DatabaseError(SNoRecords);
  end;
end;

procedure TTinyTable.SetKeyBuffer(KeyIndex: TTDKeyIndex; Clear: Boolean);
begin
  CheckBrowseMode;
  FKeyBuffer := FKeyBuffers[KeyIndex];
  if Clear then InitKeyBuffer(KeyIndex);
  SetState(dsSetKey);
  DataEvent(deDataSetChange, 0);
end;

procedure TTinyTable.InternalRefresh;
begin
  InitCurRecordTab;
  if FSetRanged then ApplyRange;
  if Filtered then ActivateFilters;
end;

//-----------------------------------------------------------------------------
// This method is called by TDataSet.Post.  Most implmentations would write
// the changes directly to the associated datasource, but here we simply set
// a flag to write the changes when we close the dateset.
//-----------------------------------------------------------------------------
procedure TTinyTable.InternalPost;
var
  RecBuf: TRecordBuffer;
begin
  if GetActiveRecBuf(RecBuf) then
  begin
    //if FUpdateCount = 0 then InitCurRecordTab;
    if State = dsEdit then
    begin //edit
      ModifyRecordData(RecBuf, FCurRec);
    end
    else
    begin //insert or append
      AppendRecordData(RecBuf);
    end;
  end;
end;

//-----------------------------------------------------------------------------
// This method is similar to InternalPost above, but the operation is always
// an insert or append and takes a pointer to a record buffer as well.
//-----------------------------------------------------------------------------
procedure TTinyTable.InternalAddRecord(Buffer: Pointer; Append: Boolean);
begin
  if Append then InternalLast;
  AppendRecordData(Buffer);
end;

{$IFDEF DELPHI_17_UP}
procedure TTinyTable.InternalAddRecord(Buffer: TRecordBuffer; Append: Boolean);
begin
  Self.InternalAddRecord(Pointer(Buffer), Append);
end;
{$ENDIF}

{$IFDEF DELPHI_18_UP}
procedure TTinyTable.InternalAddRecord(Buffer: TRecBuf; Append: Boolean);
begin
  Self.InternalAddRecord(Pointer(Buffer), Append);
end;
{$ENDIF}

//-----------------------------------------------------------------------------
// This method is called by TDataSet.Delete to delete the current record
//-----------------------------------------------------------------------------
procedure TTinyTable.InternalDelete;
begin
  DeleteRecordData(FCurRec);
  if FCurRec >= RecordCount then
    Dec(FCurRec);
end;

//-----------------------------------------------------------------------------
// This property is used while opening the dataset.
// It indicates if data is available even though the
// current state is still dsInActive.
//-----------------------------------------------------------------------------
function TTinyTable.IsCursorOpen: Boolean;
begin
  Result := Assigned(FRecTabList);
end;

procedure TTinyTable.Post;
begin
  inherited Post;
  //When state is dsSetKey, calling CheckBrowseMode will run to here:
  if State = dsSetKey then
  begin
    DataEvent(deCheckBrowseMode, 0);
    SetState(dsBrowse);
    DataEvent(deDataSetChange, 0);
  end;
end;

function TTinyTable.BookmarkValid(Bookmark: TBookmark): Boolean;
var
  I, Index: Integer;
begin
  Result := IsCursorOpen;
  if not Result then Exit;

  Index := -1;
  if Bookmark <> nil then
  for I := 0 to FRecTabList.Count - 1 do
    if PInteger(Bookmark)^ = GetMemRecTabItem(FRecTabList, I).RecIndex then
    begin
      Index := I;
      Break;
    end;
  Result := (Index <> -1);
end;

procedure TTinyTable.SetKey;
begin
  SetKeyBuffer(tkLookup, True);
  FEffFieldCount := 0;
end;

procedure TTinyTable.EditKey;
begin
  SetKeyBuffer(tkLookup, False);
  FEffFieldCount := 0;
end;

function TTinyTable.GotoKey: Boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;

  Result := SearchKey(FRecTabList, FIndexIdx, FEffFieldCount, False);

  if Result then Resync([rmExact, rmCenter]);
  if Result then DoAfterScroll;
end;

function TTinyTable.GotoKey(const IndexName: string): Boolean;
var
  IndexIdx: Integer;
begin
  IndexIdx := FTableIO.IndexDefs.IndexOf(IndexName);
  if IndexIdx = -1 then
    DatabaseErrorFmt(SInvalidIndexName, [IndexName]);

  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;

  Result := SearchKey(FTableIO.RecTabLists[IndexIdx+1], IndexIdx, FEffFieldCount, False);

  if Result then Resync([rmExact, rmCenter]);
  if Result then DoAfterScroll;
end;

procedure TTinyTable.GotoNearest;
var
  Result: Boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;

  Result := SearchKey(FRecTabList, FIndexIdx, FEffFieldCount, True);

  Resync([rmExact, rmCenter]);
  if Result then DoAfterScroll;
end;

procedure TTinyTable.GotoNearest(const IndexName: string);
var
  IndexIdx: Integer;
  Result: Boolean;
begin
  IndexIdx := FTableIO.IndexDefs.IndexOf(IndexName);
  if IndexIdx = -1 then
    DatabaseErrorFmt(SInvalidIndexName, [IndexName]);

  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;

  Result := SearchKey(FTableIO.RecTabLists[IndexIdx+1], IndexIdx, FEffFieldCount, True);

  Resync([rmExact, rmCenter]);
  if Result then DoAfterScroll;
end;

function TTinyTable.FindKey(const KeyValues: array of const): Boolean;
begin
  CheckBrowseMode;
  FEffFieldCount := Length(KeyValues);
  SetKeyFields(tkLookup, KeyValues);
  Result := GotoKey;
end;

function TTinyTable.FindKey(const IndexName: string; const KeyValues: array of const): Boolean;
var
  IndexIdx: Integer;
begin
  IndexIdx := FTableIO.IndexDefs.IndexOf(IndexName);
  if IndexIdx = -1 then
    DatabaseErrorFmt(SInvalidIndexName, [IndexName]);

  CheckBrowseMode;
  FEffFieldCount := Length(KeyValues);
  SetKeyFields(IndexIdx, tkLookup, KeyValues);
  Result := GotoKey(IndexName);
end;

procedure TTinyTable.FindNearest(const KeyValues: array of const);
begin
  CheckBrowseMode;
  FEffFieldCount := Length(KeyValues);
  SetKeyFields(tkLookup, KeyValues);
  GotoNearest;
end;

procedure TTinyTable.FindNearest(const IndexName: string; const KeyValues: array of const);
var
  IndexIdx: Integer;
begin
  IndexIdx := FTableIO.IndexDefs.IndexOf(IndexName);
  if IndexIdx = -1 then
    DatabaseErrorFmt(SInvalidIndexName, [IndexName]);

  CheckBrowseMode;
  FEffFieldCount := Length(KeyValues);
  SetKeyFields(IndexIdx, tkLookup, KeyValues);
  GotoNearest(IndexName);
end;

procedure TTinyTable.SetRangeStart;
begin
  SetKeyBuffer(tkRangeStart, True);
  FEffFieldCount := 0;
end;

procedure TTinyTable.SetRangeEnd;
begin
  SetKeyBuffer(tkRangeEnd, True);
  FEffFieldCount := 0;
end;

procedure TTinyTable.EditRangeStart;
begin
  SetKeyBuffer(tkRangeStart, False);
  FEffFieldCount := 0;
end;

procedure TTinyTable.EditRangeEnd;
begin
  SetKeyBuffer(tkRangeEnd, False);
  FEffFieldCount := 0;
end;

procedure TTinyTable.ApplyRange;
var
  StartIdx, EndIdx: Integer;
  I, ResultState: Integer;
  RecTabList: TList;
begin
  CheckBrowseMode;
  if FIndexIdx = -1 then DatabaseError(SNoFieldIndexes, Self);
  if RecordCount = 0 then Exit;

  StartIdx := SearchRangeStart(FRecTabList, FIndexIdx, ResultState, FEffFieldCount);
  if ResultState = -2 then Exit;
  EndIdx := SearchRangeEnd(FRecTabList, FIndexIdx, ResultState, FEffFieldCount);
  if ResultState = -2 then Exit;

  RecTabList := TList.Create;
  for I := StartIdx to EndIdx do
    AddMemRecTabItem(RecTabList, GetMemRecTabItem(FRecTabList, I));
  ClearMemRecTab(FRecTabList);
  for I := 0 to RecTabList.Count - 1 do
    AddMemRecTabItem(FRecTabList, GetMemRecTabItem(RecTabList, I));
  ClearMemRecTab(RecTabList);
  RecTabList.Free;

  //FCanModify := False;
  First;
  FSetRanged := True;
end;

procedure TTinyTable.ApplyRange(const IndexName: string);
var
  IndexIdx: Integer;
  StartIdx, EndIdx: Integer;
  I, J, RecIndex, ResultState: Integer;
  RecTabList: TList;
begin
  IndexIdx := FTableIO.IndexDefs.IndexOf(IndexName);
  if IndexIdx = -1 then
    DatabaseErrorFmt(SInvalidIndexName, [IndexName]);

  CheckBrowseMode;
  if RecordCount = 0 then Exit;

  StartIdx := SearchRangeStart(FTableIO.RecTabLists[IndexIdx+1], IndexIdx, ResultState, FEffFieldCount);
  if ResultState = -2 then Exit;
  EndIdx := SearchRangeEnd(FTableIO.RecTabLists[IndexIdx+1], IndexIdx, ResultState, FEffFieldCount);
  if ResultState = -2 then Exit;

  RecTabList := TList.Create;
  for I := 0 to FRecTabList.Count - 1 do
  begin
    RecIndex := GetMemRecTabItem(FRecTabList, I).RecIndex;
    for J := StartIdx to EndIdx do
      if RecIndex = GetMemRecTabItem(FTableIO.RecTabLists[IndexIdx+1], J).RecIndex then
      begin
        AddMemRecTabItem(RecTabList, GetMemRecTabItem(FRecTabList, I));
        Break;
      end;
  end;

  ClearMemRecTab(FRecTabList);
  for I := 0 to RecTabList.Count - 1 do
    AddMemRecTabItem(FRecTabList, GetMemRecTabItem(RecTabList, I));
  ClearMemRecTab(RecTabList);
  RecTabList.Free;

  //FCanModify := False;
  First;
end;

procedure TTinyTable.SetRange(const StartValues, EndValues: array of const);
begin
  CheckBrowseMode;
  FEffFieldCount := Min(Length(StartValues), Length(EndValues));
  SetKeyFields(tkRangeStart, StartValues);
  SetKeyFields(tkRangeEnd, EndValues);
  ApplyRange;
end;

procedure TTinyTable.SetRange(const IndexName: string; const StartValues, EndValues: array of const);
var
  IndexIdx: Integer;
begin
  IndexIdx := FTableIO.IndexDefs.IndexOf(IndexName);
  if IndexIdx = -1 then
    DatabaseErrorFmt(SInvalidIndexName, [IndexName]);

  CheckBrowseMode;
  FEffFieldCount := Min(Length(StartValues), Length(EndValues));
  SetKeyFields(IndexIdx, tkRangeStart, StartValues);
  SetKeyFields(IndexIdx, tkRangeEnd, EndValues);
  ApplyRange(IndexName);
end;

procedure TTinyTable.CancelRange;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  InitCurRecordTab;
  Resync([]);
  First;
  FCanModify := True;
  if Filtered then ActivateFilters;
  FSetRanged := False;
  FEffFieldCount := 0;
end;

function TTinyTable.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, True);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TTinyTable.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: String): Variant;
begin
  Result := Null;
  if LocateRecord(KeyFields, KeyValues, [], False) then
  begin
    SetTempState(dsCalcFields);
    try
      CalculateFields(TempBuffer);
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

procedure TTinyTable.EmptyTable;
begin
  if Active then
  begin
    CheckBrowseMode;
    DeleteAllRecords;
    ClearBuffers;
    DataEvent(deDataSetChange, 0);
  end
  else
  begin
    DeleteAllRecords;
  end;
end;

procedure TTinyTable.CreateTable;
var
  ADatabase: TTinyDatabase;
  FieldItems: array of TFieldItem;
  IndexFieldNames: array of string;
  TempList: TStrings;
  IndexName: string;
  IndexOptions: TTDIndexOptions;
  I, J: Integer;
  IndexExists: Boolean;
begin
  ADatabase := OpenDatabase(False);
  if ADatabase <> nil then
  begin
    SetLength(FieldItems, FieldDefs.Count);
    for I := 0 to FieldDefs.Count - 1 do
    begin
      FieldItems[I].FieldName := AnsiString(FieldDefs[I].Name);
      FieldItems[I].FieldType := FieldDefs[I].DataType;
      FieldItems[I].DataSize := FieldDefs[I].Size;
      FieldItems[I].DPMode := fdDefault;
    end;
    ADatabase.CreateTable(TableName, FieldItems);

    for I := 0 to IndexDefs.Count - 1 do
    begin
      IndexName := IndexDefs[I].Name;
      IndexOptions := [];
      if ixPrimary in IndexDefs[I].Options then Include(IndexOptions, tiPrimary);
      if ixUnique in IndexDefs[I].Options then Include(IndexOptions, tiUnique);
      if ixDescending in IndexDefs[I].Options then Include(IndexOptions, tiDescending);
      if ixCaseInsensitive in IndexDefs[I].Options then Include(IndexOptions, tiCaseInsensitive);

      TempList := TStringList.Create;
      TempList.CommaText := IndexDefs[I].Fields;
      SetLength(IndexFieldNames, TempList.Count);
      for J := 0 to TempList.Count - 1 do
        IndexFieldNames[J] := TempList[J];
      TempList.Free;

      IndexExists := tiPrimary in IndexOptions;
      if not IndexExists then
        ADatabase.CreateIndex(TableName, IndexName, IndexOptions, IndexFieldNames);
    end;
  end;
end;

{$IFDEF __SQL}

{ TTinyQuery }

constructor TTinyQuery.Create(AOwner: TComponent);
begin
  inherited;
  FSQL := TStringList.Create;
  FSQLParser := TSQLParser.Create(Self);
end;

destructor TTinyQuery.Destroy;
begin
  FreeAndNil(FSQLParser);
  FreeAndNil(FSQL);
  inherited;
end;

procedure TTinyQuery.ExecSQL;
begin
  FSQLParser.Parse(SQL.Text);
  FSQLParser.Execute;
end;

procedure TTinyQuery.SetQuery(Value: TStrings);
begin
  if SQL.Text <> Value.Text then
  begin
    SQL.BeginUpdate;
    try
      SQL.Assign(Value);
    finally
      SQL.EndUpdate;
    end;
  end;
end;

function TTinyQuery.GetRowsAffected: Integer;
begin
  Result := FSQLParser.RowsAffected;
end;

procedure TTinyQuery.InternalOpen;
begin

end;

procedure TTinyQuery.InternalClose;
begin

end;

{$ENDIF __SQL}

{ TTinyDatabase }

constructor TTinyDatabase.Create(AOwner: TComponent);
begin
  inherited;
  FDataSets := TList.Create;
  if FSession = nil then
    if AOwner is TTinySession then
      FSession := TTinySession(AOwner)
    else
      FSession := DefaultSession;
  SessionName := FSession.SessionName;
  FSession.AddDatabase(Self);
  FTableDefs := TTinyTableDefs.Create(Self);
  FKeepConnection := False;
  FAutoFlushInterval := tdbDefAutoFlushInterval;  // 60 sec.
  FAutoFlushTimer := TTimer.Create(nil);
  FAutoFlushTimer.Enabled := False;
  FAutoFlushTimer.OnTimer := AutoFlushTimer;
end;

destructor TTinyDatabase.Destroy;
begin
  SetConnected(False);
  Destroying;
  if FSession <> nil then
    FSession.RemoveDatabase(Self);
  FreeAndNil(FDataSets);
  FreeAndNil(FTableDefs);
  FreeAndNil(FAutoFlushTimer);
  inherited;
end;

procedure TTinyDatabase.Open;
begin
  SetConnected(True);
end;

procedure TTinyDatabase.Close;
begin
  SetConnected(False);
end;

procedure TTinyDatabase.CloseDataSets;
begin
  while DataSetCount <> 0 do TTDBDataSet(DataSets[DataSetCount-1]).Disconnect;
end;

procedure TTinyDatabase.FlushCache;
begin
  if FDBFileIO <> nil then FDBFileIO.Flush;
end;

procedure TTinyDatabase.DoConnect;
begin
  CheckDatabaseName;
  CheckSessionName(True);
  if FDBFileIO = nil then
    FDBFileIO := TTinyDBFileIO.Create(Self);
  try
    FDBFileIO.Open(GetDBFileName, FMediumType, FExclusive, FReadOnly, FStartOffset);
  except
    FDBFileIO.Close;
    FreeAndNil(FDBFileIO);
    raise;
  end;
  FCanAccess := not FDBFileIO.FDBOptions.Encrypt;
  if not FCanAccess and FPasswordModified then
    FCanAccess := FDBFileIO.SetPassword(FPassword);
  InitTableDefs;
  InitTableIOs;
end;

procedure TTinyDatabase.DoDisconnect;
begin
  if FDBFileIO <> nil then
  begin
    FDBFileIO.Close;
    FreeAndNil(FDBFileIO);
    Session.DBNotification(dbClose, Self);
    CloseDataSets;
    FRefCount := 0;
    FCanAccess := False;
    FPassword := '';
    FPasswordModified := False;
    FreeTableIOs;
    FTableDefs.Clear;
  end;
  FCanAccess := False;
end;

procedure TTinyDatabase.CheckCanAccess;
var
  TempPassword: string;
  I: Integer;
begin
  if not FCanAccess then
  begin
    // check passwords from session
    if Session.FPasswords.Count > 0 then
    begin
      for I := 0 to Session.FPasswords.Count - 1 do
      begin
        Password := Session.FPasswords[I];
        if FCanAccess then Break;
      end;
    end;

    if not FCanAccess then
    begin
      if not FPasswordModified then
      begin
        if ShowLoginDialog(GetDBFileName, TempPassword) then
        begin
          Password := TempPassword;
          if not FCanAccess then DatabaseError(SAccessDenied);
        end
        else
          Abort;
      end
      else
      begin
        Password := FPassword;
        if not FCanAccess then DatabaseError(SAccessDenied);
      end;
    end;
  end;
end;

procedure TTinyDatabase.CheckReadOnly;
begin
  if FReadOnly or ((FDBFileIO <> nil) and (FDBFileIO.FileIsReadOnly)) then
    DatabaseError(SDatabaseReadOnly);
end;

function TTinyDatabase.GetDataSet(Index: Integer): TTDEDataSet;
begin
  Result := FDataSets[Index];
end;

function TTinyDatabase.GetDataSetCount: Integer;
begin
  Result := FDataSets.Count;
end;

procedure TTinyDatabase.RegisterClient(Client: TObject; Event: TConnectChangeEvent = nil);
begin
  if Client is TTDBDataSet then
    FDataSets.Add(Client);
end;

procedure TTinyDatabase.UnRegisterClient(Client: TObject);
begin
  if Client is TTDBDataSet then
    FDataSets.Remove(Client);
end;

procedure TTinyDatabase.SendConnectEvent(Connecting: Boolean);
var
  I: Integer;
begin
  for I := 0 to FDataSets.Count - 1 do
    TTDBDataSet(FDataSets[I]).DataEvent(deConnectChange, Integer(Connecting));
end;

function TTinyDatabase.GetConnected: Boolean;
begin
  Result := (FDBFileIO <> nil) and (FDBFileIO.IsOpen);
end;

function TTinyDatabase.GetEncrypted: Boolean;
begin
  if FDBFileIO = nil then DatabaseError(SDatabaseClosed, Self);
  Result := FDBFileIO.FDBOptions.Encrypt;
end;

function TTinyDatabase.GetEncryptAlgoName: string;
begin
  if FDBFileIO = nil then DatabaseError(SDatabaseClosed, Self);
  Result := string(FDBFileIO.FDBOptions.EncryptAlgoName);
end;

function TTinyDatabase.GetCompressed: Boolean;
begin
  if FDBFileIO = nil then DatabaseError(SDatabaseClosed, Self);
  Result := FDBFileIO.FDBOptions.CompressBlob;
end;

function TTinyDatabase.GetCompressLevel: TCompressLevel;
begin
  if FDBFileIO = nil then DatabaseError(SDatabaseClosed, Self);
  Result := FDBFileIO.FDBOptions.CompressLevel;
end;

function TTinyDatabase.GetCompressAlgoName: string;
begin
  if FDBFileIO = nil then DatabaseError(SDatabaseClosed, Self);
  Result := string(FDBFileIO.FDBOptions.CompressAlgoName);
end;

function TTinyDatabase.GetCRC32: Boolean;
begin
  if FDBFileIO = nil then DatabaseError(SDatabaseClosed, Self);
  Result := FDBFileIO.FDBOptions.CRC32;
end;

function TTinyDatabase.GetTableIOs(Index: Integer): TTinyTableIO;
begin
  Result := TTinyTableIO(FTableIOs[Index]);
end;

function TTinyDatabase.GetFileSize: Integer;
begin
  if FDBFileIO = nil then DatabaseError(SDatabaseClosed, Self);
  Result := FDBFileIO.DBStream.Size;
end;

function TTinyDatabase.GetFileDate: TDateTime;
var
  FileDate: Integer;
begin
  if FDBFileIO = nil then DatabaseError(SDatabaseClosed, Self);
  if FMediumType = mtDisk then
  begin
    FileDate := FileGetDate((FDBFileIO.FDBStream as TFileStream).Handle);
    Result := FileDateToDateTime(FileDate);
  end
  else
    Result := Now;
end;

function TTinyDatabase.GetFileIsReadOnly: Boolean;
begin
  Result := DBFileIO.FileIsReadOnly;
end;

function TTinyDatabase.TableIOByName(const Name: string): TTinyTableIO;
var
  I: Integer;
begin
  for I := 0 to FTableIOs.Count - 1 do
    if SysUtils.AnsiCompareText(Name, TTinyTableIO(FTableIOs[I]).TableName) = 0 then
    begin
      Result := TTinyTableIO(FTableIOs[I]);
      Exit;
    end;
  Result := nil;
end;

procedure TTinyDatabase.SetDatabaseName(const Value: string);
begin
  if FDatabaseName <> Value then
  begin
    CheckInactive;
    ValidateName(Value);
    FDatabaseName := Value;
  end;
end;

procedure TTinyDatabase.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    CheckInactive;
    FFileName := Value;
  end;
end;

procedure TTinyDatabase.SetMediumType(const Value: TTinyDBMediumType);
var
  I: Integer;
begin
  if FMediumType <> Value then
  begin
    CheckInactive;
    FMediumType := Value;
    for I := 0 to FDataSets.Count - 1 do
      if TObject(FDataSets[I]) is TTDEDataSet then
        TTDEDataSet(FDataSets[I]).MediumType := FMediumType;
  end;
end;

procedure TTinyDatabase.SetExclusive(const Value: Boolean);
begin
  CheckInactive;
  FExclusive := Value;
end;

procedure TTinyDatabase.SetReadOnly(const Value: Boolean);
begin
  CheckInactive;
  FReadOnly := Value;
end;

procedure TTinyDatabase.SetKeepConnection(const Value: Boolean);
begin
  if FKeepConnection <> Value then
  begin
    FKeepConnection := Value;
    if not Value and (FRefCount = 0) then Close;
  end;
end;

procedure TTinyDatabase.SetSessionName(const Value: string);
begin
  if csReading in ComponentState then
    FSessionName := Value
  else
  begin
    CheckInactive;
    if FSessionName <> Value then
    begin
      FSessionName := Value;
      CheckSessionName(False);
    end;
  end;
end;

procedure TTinyDatabase.SetConnected(const Value: Boolean);
begin
  if (csReading in ComponentState) and Value then
    FStreamedConnected := True
  else
  begin
    if Value = GetConnected then Exit;
    if Value then
    begin
      if Assigned(BeforeConnect) then BeforeConnect(Self);
      DoConnect;
      SendConnectEvent(True);
      if Assigned(AfterConnect) then AfterConnect(Self);
    end
    else
    begin
      if Assigned(BeforeDisconnect) then BeforeDisconnect(Self);
      SendConnectEvent(False);
      DoDisconnect;
      if Assigned(AfterDisconnect) then AfterDisconnect(Self);
    end;
  end;
end;

procedure TTinyDatabase.SetPassword(Value: string);
begin
  FPasswordModified := True;
  FPassword := Value;
  if Connected then
    FCanAccess := FDBFileIO.SetPassword(Value);
end;

procedure TTinyDatabase.SetCRC32(Value: Boolean);
begin
  if FDBFileIO = nil then DatabaseError(SDatabaseClosed, Self);
  FDBFileIO.FDBOptions.CRC32 := Value;
end;

procedure TTinyDatabase.SetAutoFlush(Value: Boolean);
begin
  if FAutoFlush <> Value then
  begin
    FAutoFlush := Value;
    if Value then
      FAutoFlushTimer.Interval := FAutoFlushInterval;
    FAutoFlushTimer.Enabled := Value;
  end;
end;

procedure TTinyDatabase.SetAutoFlushInterval(Value: Integer);
begin
  if FAutoFlushInterval <> Value then
  begin
    FAutoFlushInterval := Value;
    if FAutoFlushTimer.Enabled then
      FAutoFlushTimer.Interval := FAutoFlushInterval;
  end;
end;

procedure TTinyDatabase.SetStartOffset(Value: Integer);
begin
  if Value < 0 then Value := 0;
  FStartOffset := Value;
end;

function TTinyDatabase.CreateLoginDialog(const ADatabaseName: string): TForm;
var
  BackPanel: TPanel;
begin
  Result := TTinyDBLoginForm.CreateNew(Application);
  with Result do
  begin
    BiDiMode := Application.BiDiMode;
    BorderStyle := bsDialog;
    Canvas.Font := Font;
    Width := 281;
    Height := 154;
    Position := poScreenCenter;
    Scaled := False;
    Caption := 'Database Login';
    BackPanel := TPanel.Create(Result);
    with BackPanel do
    begin
      Name := 'BackPanel';
      Parent := Result;
      Caption := '';
      BevelInner := bvRaised;
      BevelOuter := bvLowered;
      SetBounds(8, 8, Result.ClientWidth - 16, 75);
    end;
    with TLabel.Create(Result) do
    begin
      Name := 'DatabaseLabel';
      Parent := BackPanel;
      Caption := 'Database:';
      BiDiMode := Result.BiDiMode;
      Left := 12;
      Top := 15;
    end;
    with TLabel.Create(Result) do
    begin
      Name := 'PasswordLabel';
      Parent := BackPanel;
      Caption := 'Password:';
      BiDiMode := Result.BiDiMode;
      Left := 12;
      Top := 45;
    end;
    with TEdit.Create(Result) do
    begin
      Name := 'DatabaseEdit';
      Parent := BackPanel;
      BiDiMode := Result.BiDiMode;
      SetBounds(86, 12, BackPanel.ClientWidth - 86 - 12, 21);
      ReadOnly := True;
      Color := clBtnFace;
      TabStop := False;
      Text := ADatabaseName;
    end;
    with TEdit.Create(Result) do
    begin
      Name := 'PasswordEdit';
      Parent := BackPanel;
      BiDiMode := Result.BiDiMode;
      SetBounds(86, 42, BackPanel.ClientWidth - 86 - 12, 21);
      PasswordChar := '*';
      TabOrder := 0;
      Text := '';
    end;
    with TButton.Create(Result) do
    begin
      Name := 'OkButton';
      Parent := Result;
      Caption := '&OK';
      Default := True;
      ModalResult := mrOk;
      Left := 109;
      Top := 94;
    end;
    with TButton.Create(Result) do
    begin
      Name := 'CancelButton';
      Parent := Result;
      Caption := '&Cancel';
      Cancel := True;
      ModalResult := mrCancel;
      Left := 191;
      Top := 94;
    end;
  end;
end;

function TTinyDatabase.ShowLoginDialog(const ADatabaseName: string; var APassword: string): Boolean;
begin
  with CreateLoginDialog(ADatabaseName) as TTinyDBLoginForm do
  try
    Result := ShowModal = mrOk;
    if Result then
      APassword := (FindComponent('PasswordEdit') as TEdit).Text;
  finally
    Free;
  end;
end;

function TTinyDatabase.GetDBFileName: string;
begin
  if FFileName <> '' then
    Result := FFileName
  else
    Result := FDatabaseName;
end;

procedure TTinyDatabase.CheckSessionName(Required: Boolean);
var
  NewSession: TTinySession;
begin
  if Required then
    NewSession := Sessions.List[FSessionName]
  else
    NewSession := Sessions.FindSession(FSessionName);
  if (NewSession <> nil) and (NewSession <> FSession) then
  begin
    if (FSession <> nil) then FSession.RemoveDatabase(Self);
    FSession := NewSession;
    FSession.FreeNotification(Self);
    FSession.AddDatabase(Self);
    try
      ValidateName(FDatabaseName);
    except
      FDatabaseName := '';
      raise;
    end;
  end;
  if Required then FSession.Active := True;
end;

procedure TTinyDatabase.CheckInactive;
begin
  if FDBFileIO <> nil then
    if csDesigning in ComponentState then
      Close
    else
      DatabaseError(SDatabaseOpen, Self);
end;

procedure TTinyDatabase.CheckDatabaseName;
begin
  if (FDatabaseName = '') and not FTemporary then
    DatabaseError(SDatabaseNameMissing, Self);
end;

procedure TTinyDatabase.InitTableIOs;
var
  I: Integer;
  TableIO: TTinyTableIO;
  TableNames: TStringList;
begin
  FreeTableIOs;
  FTableIOs := TList.Create;
  TableNames := TStringList.Create;
  try
    GetTableNames(TableNames);
    for I := 0 to TableNames.Count - 1 do
    begin
      TableIO := TTinyTableIO.Create(Self);
      TableIO.TableName := TableNames[I];
      FTableIOs.Add(TableIO);
    end;
  finally
    TableNames.Free;
  end;
end;

procedure TTinyDatabase.FreeTableIOs;
var
  I: Integer;
begin
  if FTableIOs <> nil then
  begin
    for I := 0 to FTableIOs.Count - 1 do
      TTinyTableIO(FTableIOs[I]).Free;
    FTableIOs.Clear;
    FreeAndNil(FTableIOs);
  end;
end;

procedure TTinyDatabase.AddTableIO(const TableName: string);
var
  TableIO: TTinyTableIO;
begin
  TableIO := TTinyTableIO.Create(Self);
  TableIO.TableName := TableName;
  FTableIOs.Add(TableIO);
end;

procedure TTinyDatabase.DeleteTableIO(const TableName: string);
var
  I: Integer;
begin
  for I := 0 to FTableIOs.Count - 1 do
    if SysUtils.AnsiCompareText(TTinyTableIO(FTableIOs[I]).TableName, TableName) = 0 then
    begin
      TTinyTableIO(FTableIOs[I]).Free;
      FTableIOs.Delete(I);
      Break;
    end;
end;

procedure TTinyDatabase.RenameTableIO(const OldTableName, NewTableName: string);
var
  I: Integer;
begin
  for I := 0 to FTableIOs.Count - 1 do
    if SysUtils.AnsiCompareText(TTinyTableIO(FTableIOs[I]).TableName, OldTableName) = 0 then
    begin
      TTinyTableIO(FTableIOs[I]).TableName := NewTableName;
      Break;
    end;
end;

procedure TTinyDatabase.RefreshAllTableIOs;
var
  I: Integer;
begin
  if FTableIOs <> nil then
  begin
    for I := 0 to FTableIOs.Count - 1 do
      TTinyTableIO(FTableIOs[I]).Refresh;
  end;
end;

procedure TTinyDatabase.RefreshAllDataSets;
var
  I: Integer;
begin
  for I := 0 to DataSetCount - 1 do
    DataSets[I].Refresh;
end;

procedure TTinyDatabase.InitTableDefs;
var
  I: Integer;
  List: TStrings;
begin
  List := TStringList.Create;
  try
    FTableDefs.Clear;
    GetTableNames(List);
    for I := 0 to List.Count - 1 do
      with TTinyTableDef(FTableDefs.Add) do
      begin
        Name := List[I];
        TableIdx := I;
      end;
  finally
    List.Free;
  end;
end;

procedure TTinyDatabase.AutoFlushTimer(Sender: TObject);
begin
  if FDBFileIO <> nil then
  begin
    if not FDBFileIO.Flushed then
      FlushCache;
  end;
end;

procedure TTinyDatabase.Loaded;
begin
  inherited Loaded;
  try
    if FStreamedConnected then SetConnected(True);
  except
    on E: Exception do
      if csDesigning in ComponentState then
        ShowException(E, ExceptAddr)
      else
        raise;
  end;
  if not StreamedConnected then CheckSessionName(False);
end;

procedure TTinyDatabase.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FSession) and
    (FSession <> DefaultSession)
  then
  begin
    Close;
    SessionName := '';
  end;
end;

procedure TTinyDatabase.ValidateName(const Name: string);
var
  Database: TTinyDatabase;
begin
  if (Name <> '') and (FSession <> nil) then
  begin
    Database := FSession.FindDatabase(Name);
    if (Database <> nil) and (Database <> Self) and
      not (Database.HandleShared and HandleShared)
    then
    begin
      if not Database.Temporary or (Database.FRefCount <> 0) then
        DatabaseErrorFmt(SDuplicateDatabaseName, [Name]);
      Database.Free;
    end;
  end;
end;

procedure TTinyDatabase.GetTableNames(List: TStrings);
begin
  if Connected then
    DBFileIO.GetTableNames(List);
end;

procedure TTinyDatabase.GetFieldNames(const TableName: string; List: TStrings);
begin
  if Connected then
    DBFileIO.GetFieldNames(TableName, List);
end;

procedure TTinyDatabase.GetIndexNames(const TableName: string; List: TStrings);
begin
  if Connected then
    DBFileIO.GetIndexNames(TableName, List);
end;

function TTinyDatabase.TableExists(const TableName: string): Boolean;
var
  Tables: TStrings;
begin
  Tables := TStringList.Create;
  try
    try
      GetTableNames(Tables);
    except
    end;
    Result := (Tables.IndexOf(TableName) <> -1);
  finally
    Tables.Free;
  end;
end;

class function TTinyDatabase.GetCompressAlgoNames(List: TStrings): Integer;
begin
  List.Assign(FCompressClassList);
  Result := List.Count;
end;

class function TTinyDatabase.GetEncryptAlgoNames(List: TStrings): Integer;
begin
  List.Assign(FEncryptClassList);
  Result := List.Count;
end;

class function TTinyDatabase.IsTinyDBFile(const FileName: string): Boolean;
begin
  try
    Result := TTinyDBFileIO.CheckValidTinyDB(FileName);
  except
    Result := False;
  end;
end;

function TTinyDatabase.CreateDatabase(const DBFileName: string): Boolean;
begin
  Result := CreateDatabase(DBFileName, False, clNormal, '', False, '', '', False);
end;

function TTinyDatabase.CreateDatabase(const DBFileName: string;
  CompressBlob: Boolean; CompressLevel: TCompressLevel; const CompressAlgoName: string;
  Encrypt: Boolean; const EncryptAlgoName, Password: string; CRC32: Boolean = False): Boolean;
var
  TempDBFile: TTinyDBFileIO;
begin
  TempDBFile := TTinyDBFileIO.Create(Self);
  try
    Result := TempDBFile.CreateDatabase(DBFileName, CompressBlob, CompressLevel,
      CompressAlgoName, Encrypt, EncryptAlgoName, Password, CRC32);
  finally
    TempDBFile.Free;
  end;
end;

function TTinyDatabase.CreateDatabase(const DBFileName: string;
  const Compressed: Boolean): Boolean;
begin
  if Compressed then
    Result := CreateDatabase(DBFileName, Compressed, clMaximum, 'ZLIB', False, '', '', True)
  else
    Result := CreateDatabase(DBFileName);
end;

{-------------------------------------------------------------------------------
  Procedure: TTinyDatabase.CreateDatabase
  Author:    jaro.benes
  Created:   2015.01.15
  Arguments:
  Result:    TMemoryStream
  Purpose:   Create database in memory to method by on-fly
-------------------------------------------------------------------------------}
function TTinyDatabase.CreateDatabase(): TMemoryStream;
begin
  Result := TMemoryStream.Create;
  try
    FMediumType := mtMemory;
    FDatabaseName := PointerToStr(Result);
    CreateDatabase(PointerToStr(Result));
  except
    FreeAndNil(Result);
  end;
end;

function TTinyDatabase.CreateTable(const TableName: string; Fields: array of TFieldItem): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.CreateTable(TableName, Fields);
  if Result then
  begin
    InitTableDefs;
    AddTableIO(TableName);
  end;
end;

function TTinyDatabase.DeleteTable(const TableName: string): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.DeleteTable(TableName);
  if Result then
  begin
    InitTableDefs;
    DeleteTableIO(TableName);
  end;
end;

function TTinyDatabase.CreateIndex(const TableName, IndexName: string; IndexOptions: TTDIndexOptions; FieldNames: array of string): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.CreateIndex(TableName, IndexName, IndexOptions, FieldNames);
end;

function TTinyDatabase.DeleteIndex(const TableName, IndexName: string): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.DeleteIndex(TableName, IndexName);
end;

function TTinyDatabase.RenameTable(const OldTableName, NewTableName: string): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.RenameTable(OldTableName, NewTableName);
  if Result then
  begin
    InitTableDefs;
    RenameTableIO(OldTableName, NewTableName);
  end;
end;

function TTinyDatabase.RenameField(const TableName, OldFieldName, NewFieldName: string): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.RenameField(TableName, OldFieldName, NewFieldName);
end;

function TTinyDatabase.RenameIndex(const TableName, OldIndexName, NewIndexName: string): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.RenameIndex(TableName, OldIndexName, NewIndexName);
end;

function TTinyDatabase.Compact: Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.Compact(FPassword);
  if Result then
  begin
    RefreshAllTableIOs;
    RefreshAllDataSets;
  end;
end;

function TTinyDatabase.Repair: Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.Repair(FPassword);
  if Result then
  begin
    RefreshAllTableIOs;
    RefreshAllDataSets;
  end;
end;

function TTinyDatabase.ChangePassword(const NewPassword: string; Check: Boolean = True): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.ChangePassword(FPassword, NewPassword, Check);
  if Result then
  begin
    Password := NewPassword;
    RefreshAllTableIOs;
    RefreshAllDataSets;
  end;
end;

function TTinyDatabase.ChangeEncrypt(NewEncrypt: Boolean; const NewEncAlgo, NewPassword: string): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.ChangeEncrypt(NewEncrypt, NewEncAlgo, FPassword, NewPassword);
  if Result then
  begin
    Password := NewPassword;
    RefreshAllTableIOs;
    RefreshAllDataSets;
  end;
end;

function TTinyDatabase.SetComments(const Value: string): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.SetComments(Value, FPassword);
end;

function TTinyDatabase.GetComments(var Value: string): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  Result := FDBFileIO.GetComments(Value, FPassword);
end;

function TTinyDatabase.SetExtData(Buffer: PAnsiChar; Size: Integer): Boolean;
begin
  if not Connected then Open;
  CheckCanAccess;
  CheckReadOnly;
  Result := FDBFileIO.SetExtData(Buffer, Size);
end;

function TTinyDatabase.GetExtData(Buffer: PAnsiChar): Boolean;
begin
  if not Connected then Open;
  // Here, CheckCanAccess is not needed.
  Result := FDBFileIO.GetExtData(Buffer);
end;

{ TTinySession }

constructor TTinySession.Create(AOwner: TComponent);
begin
  ValidateAutoSession(AOwner, False);
  inherited Create(AOwner);
  FDatabases := TList.Create;
  FKeepConnections := False;
  FSQLHourGlass := True;
  FLockRetryCount := tdbDefaultLockRetryCount;
  FLockWaitTime := tdbDefaultLockWaitTime;
  FPasswords := TStringList.Create;
  Sessions.AddSession(Self);
end;

destructor TTinySession.Destroy;
begin
  SetActive(False);
  if Assigned(Sessions) then
    if Assigned(Sessions.FSessions) then
      Sessions.FSessions.Remove(Self);
  FreeAndNil(FPasswords);
  FreeAndNil(FDatabases); //change - it was move from behind "inherited destroy"
  inherited Destroy;
end;

procedure TTinySession.Open;
begin
  SetActive(True);
end;

procedure TTinySession.Close;
begin
  SetActive(False);
end;

procedure TTinySession.Loaded;
begin
  inherited Loaded;
  try
    if AutoSessionName then SetSessionNames;
    if FStreamedActive then SetActive(True);
  except
    if csDesigning in ComponentState then
      Application.HandleException(Self)
    else
      raise;
  end;
end;

procedure TTinySession.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AutoSessionName and (Operation = opInsert) then
    if AComponent is TTDBDataSet then
      TTDBDataSet(AComponent).FSessionName := Self.SessionName
    else if AComponent is TTinyDatabase then
      TTinyDatabase(AComponent).FSession := Self;
end;

procedure TTinySession.SetName(const NewName: TComponentName);
begin
  inherited SetName(NewName);
  if FAutoSessionName then UpdateAutoSessionName;
end;

function TTinySession.OpenDatabase(const DatabaseName: string): TTinyDatabase;
begin
  Result := DoOpenDatabase(DatabaseName, nil, nil, True);
end;

procedure TTinySession.CloseDatabase(Database: TTinyDatabase);
begin
  with Database do
  begin
    if FRefCount <> 0 then Dec(FRefCount);
    if (FRefCount = 0) and not KeepConnection then
      if not Temporary then
        Close
      else
        if not (csDestroying in ComponentState) then
          Free;
  end;
end;

function TTinySession.FindDatabase(const DatabaseName: string): TTinyDatabase;
var
  I: Integer;
begin
  for I := 0 to FDatabases.Count - 1 do
  begin
    Result := FDatabases[I];
    if ((Result.DatabaseName <> '') or Result.Temporary) and
      (SysUtils.AnsiCompareText(Result.DatabaseName, DatabaseName) = 0)
    then
      Exit;
  end;
  Result := nil;
end;

procedure TTinySession.DropConnections;
var
  I: Integer;
begin
  for I := FDatabases.Count - 1 downto 0 do
    with TTinyDatabase(FDatabases[I]) do
      if Temporary and (FRefCount = 0) then
        Free;
end;

procedure TTinySession.GetDatabaseNames(List: TStrings);
var
  I: Integer;
begin
  for I := 0 to FDatabases.Count - 1 do
    with TTinyDatabase(FDatabases[I]) do
      List.Add(DatabaseName);
end;

procedure TTinySession.GetTableNames(const DatabaseName: string; List: TStrings);
var
  Database: TTinyDatabase;
begin
  List.BeginUpdate;
  try
    List.Clear;
    Database := OpenDatabase(DatabaseName);
    try
      Database.GetTableNames(List);
    finally
      CloseDatabase(Database);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TTinySession.GetFieldNames(const DatabaseName, TableName: string; List: TStrings);
var
  Database: TTinyDatabase;
begin
  List.BeginUpdate;
  try
    List.Clear;
    Database := OpenDatabase(DatabaseName);
    try
      Database.GetFieldNames(TableName, List);
    finally
      CloseDatabase(Database);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TTinySession.GetIndexNames(const DatabaseName, TableName: string; List: TStrings);
var
  Database: TTinyDatabase;
begin
  List.BeginUpdate;
  try
    List.Clear;
    Database := OpenDatabase(DatabaseName);
    try
      Database.GetIndexNames(TableName, List);
    finally
      CloseDatabase(Database);
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TTinySession.AddPassword(const Password: string);
begin
  LockSession;
  try
    if GetPasswordIndex(Password) = -1 then
      FPasswords.Add(Password);
  finally
    UnlockSession;
  end;
end;

procedure TTinySession.RemovePassword(const Password: string);
var
  I: Integer;
begin
  LockSession;
  try
    I := GetPasswordIndex(Password);
    if I <> -1 then FPasswords.Delete(I);
  finally
    UnlockSession;
  end;
end;

procedure TTinySession.RemoveAllPasswords;
begin
  LockSession;
  try
    FPasswords.Clear;
  finally
    UnlockSession;
  end;
end;

procedure TTinySession.CheckInactive;
begin
  if Active then
    DatabaseError(SSessionActive, Self);
end;

function TTinySession.GetActive: Boolean;
begin
  Result := FActive;
end;

function TTinySession.GetDatabase(Index: Integer): TTinyDatabase;
begin
  Result := FDatabases[Index];
end;

function TTinySession.GetDatabaseCount: Integer;
begin
  Result := FDatabases.Count;
end;

procedure TTinySession.SetActive(Value: Boolean);
begin
  if csReading in ComponentState then
    FStreamedActive := Value
  else
    if Active <> Value then
      StartSession(Value);
end;

procedure TTinySession.SetAutoSessionName(Value: Boolean);
begin
  if Value <> FAutoSessionName then
  begin
    if Value then
    begin
      CheckInActive;
      ValidateAutoSession(Owner, True);
      FSessionNumber := -1;
      EnterCriticalSection(FSessionCSect);
      try
        with Sessions do
        begin
          FSessionNumber := FSessionNumbers.OpenBit;
          FSessionNumbers[FSessionNumber] := True;
        end;
      finally
        LeaveCriticalSection(FSessionCSect);
      end;
      UpdateAutoSessionName;
    end
    else
    begin
      if FSessionNumber > -1 then
      begin
        EnterCriticalSection(FSessionCSect);
        try
          Sessions.FSessionNumbers[FSessionNumber] := False;
        finally
          LeaveCriticalSection(FSessionCSect);
        end;
      end;
    end;
    FAutoSessionName := Value;
  end;
end;

procedure TTinySession.SetSessionName(const Value: string);
var
  Ses: TTinySession;
begin
  if FAutoSessionName and not FUpdatingAutoSessionName then
    DatabaseError(SAutoSessionActive, Self);
  CheckInActive;
  if Value <> '' then
  begin
    Ses := Sessions.FindSession(Value);
    if not ((Ses = nil) or (Ses = Self)) then
      DatabaseErrorFmt(SDuplicateSessionName, [Value], Self);
  end;
  FSessionName := Value
end;

procedure TTinySession.SetSessionNames;
var
  I: Integer;
  Component: TComponent;
begin
  if Owner <> nil then
    for I := 0 to Owner.ComponentCount - 1 do
    begin
      Component := Owner.Components[I];
      if (Component is TTDBDataSet) and
        (SysUtils.AnsiCompareText(TTDBDataSet(Component).SessionName, Self.SessionName) <> 0)
      then
        TTDBDataSet(Component).SessionName := Self.SessionName
      else
      if (Component is TTinyDataBase) and
        (SysUtils.AnsiCompareText(TTinyDataBase(Component).SessionName, Self.SessionName) <> 0)
      then
        TTinyDataBase(Component).SessionName := Self.SessionName
    end;
end;

procedure TTinySession.SetLockRetryCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> FLockRetryCount then
    FLockRetryCount := Value;
end;

procedure TTinySession.SetLockWaitTime(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value <> FLockWaitTime then
    FLockWaitTime := Value;
end;

function TTinySession.SessionNameStored: Boolean;
begin
  Result := not FAutoSessionName;
end;

procedure TTinySession.ValidateAutoSession(AOwner: TComponent; AllSessions: Boolean);
var
  I: Integer;
  Component: TComponent;
begin
  if AOwner <> nil then
    for I := 0 to AOwner.ComponentCount - 1 do
    begin
      Component := AOwner.Components[I];
      if (Component <> Self) and (Component is TTinySession) then
        if AllSessions then DatabaseError(SAutoSessionExclusive, Self)
        else if TTinySession(Component).AutoSessionName then
          DatabaseErrorFmt(SAutoSessionExists, [Component.Name]);
    end;
end;

function TTinySession.DoFindDatabase(const DatabaseName: string; AOwner: TComponent): TTinyDatabase;
var
  I: Integer;
begin
  if AOwner <> nil then
    for I := 0 to FDatabases.Count - 1 do
    begin
      Result := FDatabases[I];
      if (Result.Owner = AOwner) and (Result.HandleShared) and
        (SysUtils.AnsiCompareText(Result.DatabaseName, DatabaseName) = 0)
      then
        Exit;
    end;
  Result := FindDatabase(DatabaseName);
end;

function TTinySession.DoOpenDatabase(const DatabaseName: string;
  AOwner: TComponent; ADataSet: TTDBDataSet; IncRef: Boolean): TTinyDatabase;
var
  TempDatabase: TTinyDatabase;
begin
  //Result := nil;
  LockSession;
  try
    TempDatabase := nil;
    try
      Result := DoFindDatabase(DatabaseName, AOwner);
      if Result = nil then
      begin
        TempDatabase := TTinyDatabase.Create(Self);
        if ADataSet <> nil then
          TempDatabase.MediumType := (ADataSet as TTDEDataSet).MediumType;
        TempDatabase.DatabaseName := DatabaseName;
        TempDatabase.KeepConnection := FKeepConnections;
        TempDatabase.Temporary := True;
        Result := TempDatabase;
      end;
      Result.Open;
      if IncRef then Inc(Result.FRefCount);
    except
      TempDatabase.Free;
      raise;
    end;
  finally
    UnLockSession;
  end;
end;

procedure TTinySession.AddDatabase(Value: TTinyDatabase);
begin
  FDatabases.Add(Value);
  DBNotification(dbAdd, Value);
end;

procedure TTinySession.RemoveDatabase(Value: TTinyDatabase);
begin
  if Assigned(FDatabases) then FDatabases.Remove(Value);
  DBNotification(dbRemove, Value);
end;

procedure TTinySession.DBNotification(DBEvent: TTinyDatabaseEvent; const Param);
begin
  if Assigned(FOnDBNotify) then FOnDBNotify(DBEvent, Param);
end;

procedure TTinySession.LockSession;
begin
  if FLockCount = 0 then
  begin
    EnterCriticalSection(FSessionCSect);
    Inc(FLockCount);
    if not Active then SetActive(True);
  end
  else
    Inc(FLockCount);
end;

procedure TTinySession.UnlockSession;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    LeaveCriticalSection(FSessionCSect);
end;

procedure TTinySession.StartSession(Value: Boolean);
var
  I: Integer;
begin
  EnterCriticalSection(FSessionCSect);
  try
    if Value then
    begin
      if Assigned(FOnStartup) then FOnStartup(Self);
      if FSessionName = '' then DatabaseError(SSessionNameMissing, Self);
      if (DefaultSession <> Self) then DefaultSession.Active := True;
    end
    else
    begin
      for I := FDatabases.Count - 1 downto 0 do
        with TTinyDatabase(FDatabases[I]) do
          if Temporary then
            Free
          else
            Close;
    end;
    FActive := Value;
  finally
    LeaveCriticalSection(FSessionCSect);
  end;
end;

procedure TTinySession.UpdateAutoSessionName;
begin
  FUpdatingAutoSessionName := True;
  try
    SessionName := Format('%s_%d', [Name, FSessionNumber + 1]);
  finally
    FUpdatingAutoSessionName := False;
  end;
  SetSessionNames;
end;

function TTinySession.GetPasswordIndex(const Password: string): Integer;
var
  I: Integer;
begin
  for I := 0 to FPasswords.Count - 1 do
    if FPasswords[I] = Password then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

{ TTinySessionList }

constructor TTinySessionList.Create;
begin
  inherited Create;
  FSessions := TThreadList.Create;
  FSessionNumbers := TBits.Create;
  InitializeCriticalSection(FSessionCSect);
end;

destructor TTinySessionList.Destroy;
begin
  CloseAll;
  DeleteCriticalSection(FSessionCSect);
  FreeAndNil(FSessionNumbers);
  FreeAndNil(FSessions);
  inherited Destroy;
end;

procedure TTinySessionList.AddSession(ASession: TTinySession);
var
  List: TList;
begin
  List := FSessions.LockList;
  try
    if List.Count = 0 then ASession.FDefault := True;
    List.Add(ASession);
  finally
    FSessions.UnlockList;
  end;
end;

procedure TTinySessionList.CloseAll;
var
  I: Integer;
  List: TList;
begin
  List := FSessions.LockList;
  try
    for I := List.Count-1 downto 0 do
      TTinySession(List[I]).Free;
  finally
    FSessions.UnlockList;
  end;
end;

function TTinySessionList.GetCount: Integer;
var
  List: TList;
begin
  List := FSessions.LockList;
  try
    Result := List.Count;
  finally
    FSessions.UnlockList;
  end;
end;

function TTinySessionList.GetSession(Index: Integer): TTinySession;
var
  List: TList;
begin
  List := FSessions.LockList;
  try
    Result := TTinySession(List[Index]);
  finally
    FSessions.UnlockList;
  end;
end;

function TTinySessionList.GetSessionByName(const SessionName: string): TTinySession;
begin
  if SessionName = '' then
    Result := Session
  else
    Result := FindSession(SessionName);
  if Result = nil then
    DatabaseErrorFmt(SInvalidSessionName, [SessionName]);
end;

function TTinySessionList.FindSession(const SessionName: string): TTinySession;
var
  I: Integer;
  List: TList;
begin
  if SessionName = '' then
    Result := Session
  else
  begin
    List := FSessions.LockList;
    try
      for I := 0 to List.Count - 1 do
      begin
        Result := List[I];
        if SysUtils.AnsiCompareText(Result.SessionName, SessionName) = 0 then Exit;
      end;
      Result := nil;
    finally
      FSessions.UnlockList;
    end;
  end;
end;

procedure TTinySessionList.GetSessionNames(List: TStrings);
var
  I: Integer;
  SList: TList;
begin
  List.BeginUpdate;
  try
    List.Clear;
    SList := FSessions.LockList;
    try
      for I := 0 to SList.Count - 1 do
        with TTinySession(SList[I]) do
          List.Add(SessionName);
    finally
      FSessions.UnlockList;
    end;
  finally
    List.EndUpdate;
  end;
end;

function TTinySessionList.OpenSession(const SessionName: string): TTinySession;
begin
  Result := FindSession(SessionName);
  if Result = nil then
  begin
    Result := TTinySession.Create(nil);
    Result.SessionName := SessionName;
  end;
  Result.SetActive(True);
end;

{ TTinyDBLoginForm }

constructor TTinyDBLoginForm.CreateNew(AOwner: TComponent);
var
  NonClientMetrics: TNonClientMetrics;
begin
  inherited CreateNew(AOwner);
  NonClientMetrics.cbSize := SizeOf(NonClientMetrics);
  if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then
    Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
end;

initialization
  Sessions := TTinySessionList.Create;
  Session := TTinySession.Create(nil);
  Session.SessionName := 'Default'; { Do not localize }

finalization
  FreeAndNil(FCompressClassList);
  FreeAndNil(FEncryptClassList);
  FreeAndNil(Sessions);
end.