{*********************************************************}
{* BASE: Base Class Definitions                          *}
{* Programming: Krzysztof Winnicki                       *}
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

{$I fsdefine.inc}
{$IFDEF DCC6OrLater}
{$G+}
{$ENDIF}

{ Uncomment the following define to enable memory pool tracing. }
{.$DEFINE MemPoolTrace}

{ Uncomment the following to have memory obtained directly via GetMem,
  FreeMem, and ReallocMem instead of the FF memory pools. This aids leak
  detection using CodeWatch. }
{.$DEFINE MemCheck}

{$DEFINE UseEventPool}
Unit fsllbase;

Interface

Uses
  Dialogs,
  Windows,
  Messages,
  SysUtils,
  ShellApi,
  Classes,
  fsfunInterp,
  fsconst;

{$R fsllcnst.res}
{$R fsdbcnst.res}

{$IFDEF CBuilder3}
(*$HPPEMIT '' *)
(*$HPPEMIT '#pragma warn -hid' *)
(*$HPPEMIT '' *)
{$ENDIF}

{$IFDEF CBuilder5}
(*$HPPEMIT '' *)
(*$HPPEMIT '#ifndef DELPHITHREAD' *)
(*$HPPEMIT '#define DELPHITHREAD __declspec(thread)' *)
(*$HPPEMIT '#endif' *)
(*$HPPEMIT '' *)
{$ENDIF}

{===FSSQL Version Number===}
{ Version number is used to determine whether or not a client can properly
  work with a server.  The client supplies its version number to the
  server and the server decides whether or not the client is compatible.

  Reasons for incompatibility:

  1. The server's version number is less than the client's.
 }
Const
  fsVersionNumber: Longint = 1109;
  fsVersionBeta: Byte = 2;
  fsBuildYear: Word = 2006;
  fsBuildMonth: Byte = 2;
  fsBuildDay: Byte = 22;
  fsBuildHour: Byte = 12;
  fsBuildMinute: Byte = 0;
  fsSignSignatueStream = $FF;
  {$IFDEF IsNoVariantInt64}
Const
  VT_DECIMAL = 14;
  VT_E80 = 15;
  {$ENDIF}
Const
  MinSingle = 1.5E-45;
  MaxSingle = 3.4E+38;
  MinDouble = 5.0E-324;
  MaxDouble = 1.7E+308;
  MinExtended = 3.4E-4932;
  MaxExtended = 1.1E+4932;
  MinCurrency = -922337203685477.5808E+0;
  MaxCurrency = 922337203685477.5807E+0;

  MinByte = Low(Byte);
  MaxByte = High(Byte);
  MinWord = Low(Word);
  MaxWord = High(Word);
  MinLongWord = Low(Longword);
  MaxLongWord = High(Longword);
  MinShortInt = Low(Shortint);
  MaxShortInt = High(Shortint);
  MinSmallInt = Low(Smallint);
  MaxSmallInt = High(Smallint);
  MinLongInt = Low(Longint);
  MaxLongInt = High(Longint);
  MaxInt64 = High(Int64);
  MinInt64 = Low(Int64);
  MinInteger = Low(Integer);
  MaxInteger = High(Integer);
  MinCardinal = Low(Cardinal);
  MaxCardinal = High(Cardinal);

  BitsPerByte = 8;
  BitsPerWord = 16;
  BitsPerLongWord = 32;
  BytesPerCardinal = Sizeof(Cardinal);
  BitsPerCardinal = BytesPerCardinal * 8;
Const
  fscl_INFINITE = High(DWORD); {!!.06}
  fscl_MaxIndexes = 256; {maximum number of indexes per table}
  fscl_MaxIndexFlds = 32; {maximum count of fields in a composite key}
  fscl_MaxKeyLength = 1024; {maximum length of a key}
  fscl_FixedBookmarkSize = 24; {size of fixed part of a bookmark (ie, without key value)}
  fscl_MaxBookmarkSize = fscl_FixedBookmarkSize + fscl_MaxKeyLength;
  {maximum size of a bookmark}
  fscl_MaxBLOBLength = 2147483647; {maximum BLOB length(i.e., 2^31)}
  fscl_GeneralNameSize = 31; {count of chars in a (general) name}
  fscl_NetNameSize = 31; {count of chars in a network name}
  fscl_NetAddressSize = 63; {count of chars in a network address}
  fscl_UserNameSize = 31; {count of chars in a user/client name}
  fscl_ServerNameSize = 15; {count of chars in a server name}
  fscl_DescriptionSize = 96; //96; {count of chars in a description}
  fscl_DescSize = 63; //63; {count of chars in a description}
  fscl_DisplaySize = 63; //63; {count of chars in a description}
  fscl_TableNameSize = 31; {count of chars in a table name}

  fscl_PasswdSize = 32; {count of chars in a password}
  fscl_UserSignSize = 20; {count of chars in a user signature}
  fscl_PasswdBlancSize = 16; {count of chars in a password blanc}

  fscl_FileName = 31; {count of chars in a filename (no drive/path/ext)}
  fscl_Extension = 3; {count of chars in an extension}
  fscl_Path = 219; {count of chars in a directory path (excl final \)}
  fscl_MaxPictureLength = 175; {count of chars in a picture}
  fscl_MaxVCheckLength = 256; {count of bytes in a validity check value}
  fscl_MaxBlocks = 2147483647; {maximum number of blocks (i.e., 2^31)}
  fscl_MaxRecords = 2147483647; {maximum number of records (i.e., 2^31)}
  fscl_MinRecordLength = 8; {Minimum logical record length for the data
  dictionary.  We have a minimum because
  we must have this many bytes to hold the
  offset to the next deleted record.  This
  value does not include the leading
  deleted flag byte in the physical
  record.  }
  fscl_MaxBlockedThreads = 50; {maximum number of threads that may be
  waiting on read or write access to a
  data structure protected by an instance
  of TfsReadWritePortal}
  fscl_InitialListSize = 64; {Initial capacity of a TFSNormalList. }
  fscl_1KB = 1024; {One kilobyte. } {!!.06}
  fscl_1MB = 1024 * 1024; {One megabyte. }
  fscl_64MB = 64 * fscl_1MB; {64 megabytes. }
  fscl_64k = 64 * 1024; {64 kbytes. }
  fscl_InitialSemCount = 250; {Initial # of semaphores in sem pool. }
  fscl_RetainSemCount = 2500; {# of semaphores to retain when flush sem pool. } {!!.01}
  fscl_PortalTimeout = 5000; {# milliseconds for a BeginRead or BeginWrite
  timeout. }
  {$IFDEF UseEventPool}
  fscl_InitialEventCount = 250; {Initial # of events in event pool.}
  fscl_RetainEventCount = 2500; {# of events to retain when flush event pool. } {!!.01}
  {$ENDIF}

  {file-size constants}
  fscl_FourGigabytes = $FFFFFFFE;
  fscl_TwoGigabytes = $7FFFFFFF;
  fscl_MaxHDFloppy = $163E00;

  {Transaction constants}
  fscl_TrImplicit = True;
  fscl_TrExplicit = False;

  fscl_CollectionFrequency = 180000;
  { Default garbage collection to every 3 minutes. }

  fscl_TempStorageSize = 20;
  { Default temporary storage size to 20 MB.}

{===Extra 'primary' types===}
Type
  PffLongint = ^Longint; {pointer to a Longint}
  {$IFNDEF DCC4OrLater}
  PShortInt = ^Shortint; {pointer to a shortint}
  {$ENDIF}
  PffDateTime = ^TDateTime; {pointer to a TDateTime; required
  because we use PDateTime but it
  occurs only in D5+ or BCB4+ }
  TffWord16 = Word; {16-bit unsigned integer}
  TffWord32 = Type DWORD; {32-bit unsigned integer}
  TfsInt64 = Type Int64;
  PffWord32 = ^TffWord32; {pointer to a 32-bit unsigned integer}
  PffByteArray = ^TffByteArray; {General array of bytes}
  TffByteArray = Array[0..65531] Of Byte;
  PffWordArray = ^TffWordArray; {General array of Int}
  TffWordArray = Array[0..32767] Of Word;
  PffIntArray = ^TffIntArray; {General array of Int}
  TffIntArray = Array[0..16383] Of Integer;
  PffDoubleArray = ^TffDoubleArray; {General array of Int}
  TffDoubleArray = Array[0..8191] Of Double;

  PffCharArray = ^TffCharArray; {For debugging purposes. }
  TffCharArray = Array[0..65531] Of AnsiChar;
  PffBLOBArray = ^TffBLOBArray;
  TffBLOBArray = Array[0..pred(fscl_MaxBLOBLength)] Of Byte;
  TfsVarMsgField = Array[0..1] Of Byte; {Variably sized field (for messages)}
  PffLongintArray = ^TffLongintArray; {General array of long integers}
  TffLongintArray = Array[0..16382] Of Longint;
  TffShStr = String[255]; {a length-byte string}
  PffShStr = ^TffShStr; {pointer to a length-byte string}
  TffResult = Longint; {FSSQL&I result error code}
  TffMemSize = Integer; {type for size of memory to alloc/free}
  TffPicture = String[fscl_MaxPictureLength];
  {picture mask}
  TffVCheckValue = Array[0..pred(fscl_MaxVCheckLength)] Of Byte;
  {a validity check}
  PffInt64 = ^TffInt64; {pointer to a TffInt64}
  TffInt64 = Record {64-bit integer for Delphi 3}
    iLow: TffWord32;
    iHigh: TffWord32;
  End;

  TInfoGetSetPosition = (imMoveBy, imApprox, imPosition);
  // extra info for record (getnext, getprev, getrec)
  // Data + eFlagCollInfo + VarChar + TfsExtraRecInfo
  pfsExtraRecInfo = ^TfsExtraRecInfo;
  TfsExtraRecInfo = Record
    eFlagRowInfo: Byte;
    eRecID: Int64;
    eRecVersion: Int64;
    eRefNr: TffInt64;
    eRecNo: TffWord32;
    eIsLocked: Boolean;
    eFlagCollInfoSize: Word; // count coll [eFlagCollInfo * eFlagCollInfoSize]
    eSizeVarChar: Longint; // future
    eRes: Array[1..11] Of Byte;
  End; // 48 byte

  PffBlock = ^TffBlock;
  TffBlock = Array[0..65535] Of Byte; { A block may be 4k, 8k, 16k, 32k, or 64k
  in size. }

  TffBlockSize = (ffbs4k, ffbs8k, ffbs16k, ffbs32k, ffbs64k);
  TffBlockSizes = Set Of TffBlockSize;

  { The following types are used to improve parameter integrity. }
{Begin !!.10}
  TffBaseID = Type TffWord32;
  TffClientID = Type Int64;
  TffCursorID = Type TffBaseID;
  TffDatabaseID = Type TffBaseID;
  TffSessionID = Type TffBaseID;
  TffSqlStmtID = Type TffBaseID;
  TffTransID = Type TffBaseID;
  {End !!.10}

  {===Important constants===}
Const
  fsc_BlockHeaderSizeData = 32; {was defined in FFSRBASE}
  {file extensions (must NOT include period)}
  fsc_ExtForData: String[fscl_Extension] = 'FSD'; {extension for main table file}
  fsc_ExtForTrans: String[fscl_Extension] = 'FS$'; {extension for Transaction file}
  fsc_ExtForSQL: String[fscl_Extension] = 'SQL'; {extension for SQL text files}
  fsc_NoClientID: TffClientID = 0; { Represents no clientID specified }

  {===component notification constants===}
Const
  ffn_Insert = $01;
  ffn_Remove = $02;
  ffn_Activate = $03;
  ffn_Deactivate = $04;
  ffn_Destroy = $05;
  ffn_OwnerChanged = $06;
  ffn_ConnectionLost = $0A;

  {===Misc constants===}
Const
  ffcCRLF = #13#10;
  fsc_W32NoValue = $FFFFFFFF;

  fsTableEmptyFlags = 0;
  fsTableReadOnly = 1;
  fsTableDontRestructure = 2;
  fsTableDontReindex = 4;
  fsTableDontChangeAutoInc = 8;
  fsTableDontChangePassword = 16;
  fsTableDontInsertRecord = 32;
  fsTableDontModifyRecord = 64;
  fsTableDontDeleteRecord = 128;
  fsTableDontChangeMaxRecords = 256;
  fsTableDontEmptyTable = 512;
  fsTableDontProtectRow = 1024;

  {===Enumeration types===}
Type
  TfsTransIsolation = (tiSerializable, tiRepeatableRead); //, tiSerializable);
  TfsDataBaseRecLocking = (tlOptimisticNoWait, tlOptimisticWait, tlPessimisticNoWait, tlPessimisticWait);
  TfsUserRecLocking = (tluDatabase, tluPessimisticNoWait, tluPessimisticWait, tluOptimisticNoWait, tluOptimisticWait);
  TfsUnionType = (utNone, utNormal, utAll);

  TffOpenMode = ({Open modes for opening databases, tables}
    omReadOnly, {..read only mode}
    omReadWrite); {..read/write mode}

  TffShareMode = ({Share modes for opening databases, tables}
    smExclusive, {..exclusive, no sharing}
    smShared, {..allows others to Read or Write} {!!.06}
    smShareRead); {..allows others to Read only} {!!.06}

  TffLockType = ({Types of lock...}
    ffltNoLock, {..no lock at all}
    ffltReadLock, {..read lock (not for record locks)}
    ffltWriteLock); {..write lock}

  TTopDirection = (tdNone, tdTop, tdCenter, tdDown, tdTopCenter, tdTopCenterDown, tdTopDown, tdCenterDown);

  TffSearchKeyAction = ({Key search actions...}
    skaEqual, {..exactly equal to supplied key}
    skaEqualCrack, {..equal to supplied key or on crack before
    next key}
    skaGreater, {..greater than supplied key}
    skaGreaterEqual); {..greater than or equal to supplied key}

Type
  TfsFieldType = ({Field types for the data dictionary}
    fstBoolean, {..8-bit boolean flag}
    fstSingleChar, {..8-bit character}
    fstSingleWideChar, {..16-bit character (UNICODE)}
    fstUInt8, {..byte (8-bit unsigned integer)}
    fstUInt16, {..16-bit unsigned integer (aka word)}
    fstUInt32, {..32-bit unsigned integer}
    fstInt8, {..8-bit signed integer}
    fstInt16, {..16-bit signed integer}
    fstInt32, {..32-bit signed integer}
    fstInt64, {..64 int64 type (8 bytes signed integer)}
    fstAutoInc32, {..32-bit unsigned integer; auto incrementing}
    fstAutoInc64, {..64-bit signed integer; auto incrementing}
    fstSingle, {..IEEE single (4 bytes)}
    fstDouble, {..IEEE double (8 bytes)}
    fstExtended, {..IEEE extended (10 bytes)}
    fstCurrency, {.. Delphi Currency (8 bytes)}
    fstDate, {..SysTools date type (4 bytes)}
    fstTime, {..SysTools time type (4 bytes)}
    fstDateTime, {..Delphi date/time type (8 bytes)}
    fstBLOB, {..variable length BLOB field - general binary data}
    fstBLOBMemo, {..variable length BLOB field - text memo}
    fstBLOBGraphic, {..variable length BLOB field - graphics object}

    fstClob, //not yet
    fstWideClob, //not yet
    fstBlobFile, //not yet
    { NOTE: The SQL engine uses fstInterval to represent an
      Interval field type. We do not yet expose this field type
      to the outside world. }
    fstInterval,

    {..reserved enumeration elements - DO NOT USE}
    fstReserved1,
    fstReserved2, fstReserved3, fstReserved4,
    fstReserved5, fstReserved6, fstReserved7, fstReserved8,
    fstReserved9,
    fstReserved10, fstReserved11, fstReserved12,
    fstReserved13, fstReserved14, fstReserved15,
    fstBcd, {binary coded decimal precision 63} //not yet

    //fstDateTimeArray,
    fstArrayUInt16,
    fstArrayInt32,
    fstArrayDouble,

    fstRecVersion, {signed int64 inc recversion if any update}

    fstArrayUInt8, {..array of bytes}

    fstShortString, {..length byte string}
    fstVarNullString, {Var ..null-terminated Ansi string} //not yet
    fstNullString, {..null-terminated Ansi string}
    fstVarWideString, {Var..null-terminated string of wide chars} //not yet
    fstWideString); {..null-terminated string of wide chars}
  //fstUnicode);   {.. Unicode chars - UTF}  //not yet
  //fstVarUnicode  {Var.. Unicode chars - UTF}  //not yet

  TfsFieldTypes = Set Of TfsFieldType;
  TffBLOBCopyMode = (ffbcmNoCopy, ffbcmCopyFull, ffbcmCreateLink);

  {$IFDEF IsNoVariantInt64}
  PDecimal = ^TDecimal;
  tagDEC = Packed Record
    wReserved: Word;
    Case Integer Of
      0: (scale, sign: Byte; Hi32: Longint;
        Case Integer Of
          0: (Lo32, Mid32: Longint);
          1: (Lo64: LONGLONG));
      2: (Ext80: Extended);
      1: (signscale: Word);
  End;
  TDecimal = tagDEC;
  DECIMAL = TDecimal;
  {$ENDIF}

Const
  FieldDataTypes: Array[TfsFieldType] Of String[14] = (
    'Boolean',
    'SingleChar',
    'SingleWideChar',
    'UInt8',
    'UInt16',
    'UInt32',
    'Int8',
    'Int16',
    'Int32',
    'Int64',
    'AutoInc32',
    'AutoInc64',
    'Single',
    'Double',
    'Extended',
    'Currency',
    'Date',
    'Time',
    'DateTime',
    'Blob',
    'BlobMemo',
    'BlobGraphic',
    'Clob',
    'WideClob',
    'BlobFile',
    'Interval',

    'Reserved1',
    'Reserved2',
    'Reserved3',
    'Reserved4',
    'Reserved5',
    'Reserved6',
    'Reserved7',
    'Reserved8',
    'Reserved9',
    'Reserved10',
    'Reserved11',
    'Reserved12',
    'Reserved13',
    'Reserved14',
    'Reserved15',
    'BCD',
    'ArrayUInt16',
    'ArrayInt32',
    'ArrayDouble',
    'RecVersion',

    'ArrayUInt8',
    'ShortString',
    'VarNullString',
    'NullString',
    'VarWideString',
    'WideString');
  //'Unicode');
  //VarNullAnsiString  - memo
  //VarWideString      - widememo

Const
  ffcLastBLOBType = fstBLOBGraphic;
  {the last BLOB type, all BLOB types fall
  between fftBLOB and this one}

Type
  TffFileType = ({File types for the data dictionary}
    ftBaseFile, {..base file: at least data & dictionary}
    ftIndexFile, {..index file}
    ftBLOBFile, {..BLOB file}
    ftSystem); {future - systemfile}

Type
  TffFileName = String[fscl_FileName]; {File name type (no drive/path/extension)}
  TffExtension = String[fscl_Extension]; {Extension identifier type}
  TffFileNameExt = String[succ(fscl_FileName + fscl_Extension)];
  {File name + extension type}
  TffFullFileName = String[255]; {Expanded file name (inc drive/path}
  TffPath = String[fscl_Path]; {Complete directory path (excl final \)}
  TffMaxPathZ = Array[0..pred(MAX_PATH)] Of AnsiChar;
  {Null-terminated path&file name type}

  TffName = String[fscl_GeneralNameSize]; {A general name type}
  {Begin !!.03}
  {$IFDEF IsDelphi}
  TffNetName = String[fscl_NetNameSize]; {a network name type}
  TffNetAddress = String[fscl_NetAddressSize]; {a network address type}
  {$ELSE}
  TffNetName = String; {a network name type}
  TffNetAddress = String; {a network address type}
  TffNetNameShr = String[fscl_NetNameSize]; {a network name type - for requests}
  TffNetAddressShr = String[fscl_NetAddressSize]; {a network address type - for requests}
  {$ENDIF}
  {End !!.03}
  TfsTableName = String[fscl_TableNameSize]; {Table name type}

  TffStringZ = Array[0..255] Of AnsiChar; {For converting ShortStrings to StringZs}

  { !!.06 - Following type moved from FFNETMSG }
  {===Network message enums===}
Type
  TffNetMsgDataType = ({Types of network message data...}
    nmdByteArray, {..it's an array of bytes}
    nmdStream); {..it's a stream (TStream descendant)}

Type
  TfsConfigFileHeader = Record
    Sign: String[10];
    Version: Integer;
    Encrypt: Boolean;
    DateCreate: TDateTime;
    Size: Integer;
  End;

Type
  TffDirItemType = ({types of items a directory can contain}
    ditFile, {..file}
    ditDirectory, {..directory}
    ditVolumeID); {..VolumeID}
  TffDirItemTypeSet = Set Of TffDirItemType;

  TffDirItemAttr = ({attributes of directory items}
    diaNormal, {..normal}
    diaReadOnly, {..readonly}
    diaHidden, {..hidden}
    diaSystem, {..system}
    diaArchive); {..not backed up}
  TffDirItemAttrSet = Set Of TffDirItemAttr;

  TffSearchRec = Packed Record {FSSQL&I directory search record}
    srTime: TffWord32; {..timestamp}
    srSize: TffWord32; {..size (low 32 bits)}
    srSizeHigh: TffWord32; {..size (high 32 bits, generally 0)}
    srType: TffDirItemType; {..type}
    srAttr: TffDirItemAttrSet; {..attributes}
    srName: TffFileNameExt; {..name, including extension}
    srHandle: THandle; {..internal use only}
    srData: TWin32FindData; {..internal use only}
    srFindType: TffDirItemTypeSet; {..internal use only}
    srFindAttr: TffDirItemAttrSet; {..internal use only}
  End;

Const
  diaAnyAttr: TffDirItemAttrSet =
  [diaNormal, diaReadOnly, diaHidden, diaSystem, diaArchive];

  {===FSSQL data dictionary descriptors===}
Type
  TffDictItemName = String[fscl_GeneralNameSize]; {Field/Index name type}
  TffDictDescription = String[fscl_DescriptionSize]; {Field/Index description type}
  TffDictItemDisplay = String[fscl_DisplaySize];
  TffDictItemDesc = String[fscl_DescSize];
  TDefaultUpdate = (duNormal, duIFNULL, duALWAYS);

  PffVCheckDescriptor = ^TffVCheckDescriptor;
  TffVCheckDescriptor = Packed Record {Validity check descriptor}
    vdHasMinVal: boolean; {..true if the field has a minimum value}
    vdHasMaxVal: boolean; {..true if the field has a maximum value}
    vdHasDefVal: boolean; {..true if the field has a default value}
    vdFiller: Byte;
    vdMinVal: TffVCheckValue; {..the field's minimum value}
    vdMaxVal: TffVCheckValue; {..the field's maximum value}
    vdDefVal: TffVCheckValue; {..the field's default value}
    vdPicture: TffPicture; {..the field's picture clause}
    vdres: Byte;
    vdres1: Byte; // Not yet
  End;

  TDataCompLevel = (blNone, blFastest, blDefault, blMax);
  TfsReserved = Packed Record
    fdResByt: Array[0..11] Of Byte;
    fdResStr: Array[0..31] Of Char;
  End;
  TfsUserSortType = (ustNone, ustAnsi, ustUnicode);
  TfsIndexType = (itComposite, itExpression, itUdfDll, itSysRef, itSysVersion);

  PffFieldDescriptor = ^TffFieldDescriptor;
  TffFieldDescriptor = Packed Record {Field descriptor}
    fdNumber: Longint; {..number of field in record (zero based)}
    fdName: TffDictItemName; {..name of field}
    fdDesc: TffDictItemDisplay; {..display of field}
    fdDescription: TffDictDescription; {..description of field}
    fdDisplayMask: TffDictItemDisplay;
    fdEditMask: TffDictItemDisplay;
    fdUnits: Longint; {..number of characters/digits etc}
    fdDecPl: Longint; {..number of decimal places}
    fdOffset: Longint; {..offset of field in record}
    fdLength: Longint; {..length of field in bytes}
    fdVCheck: PffVCheckDescriptor; {..validity check (if nil, there is none)}
    fdType: TfsFieldType; {..type of field}
    fdRequired: boolean; {..true, if field must have a value to be stored}
    fdBlobLevelComp: TDataCompLevel;
    fdReadOnly: boolean;
    fdVisible: boolean;
    fdRound: TRound;
    fdIsCalculated: boolean;
    fdxxx1: Word;
    fdEmptyAsNull: boolean;
    fdOEMCodePage: Integer;
    fdCodePage: Integer;
    fdLocale: Integer;
    fdDefaultUpdate: TDefaultUpdate;
    fdReserved: TfsReserved;
    fdFiller: Array[0..1] Of Byte;
  End;

  TffFieldList = Array[0..pred(fscl_MaxIndexFlds)] Of Longint;
  {List of field numbers in an index}
  TffFieldIHList = Array[0..pred(fscl_MaxIndexFlds)] Of TffDictItemName;
  {List of extension functions used to build/compare an index}

  PffIndexDescriptor = ^TffIndexDescriptor;
  TffIndexDescriptor = Packed Record {Index descriptor}
    idNumber: Longint; {..number of index (zero based)}
    idName: TffDictItemName; {..name of index}
    idDesc: TffDictItemDesc; {..description of index}
    idFile: Longint; {..number of file containing index}
    idKeyLen: Longint; {..length of key in bytes}
    idCount: Longint; {..number of fields in composite index, or}
    {  -1 for user defined index}
    idIndexType: TfsIndexType;
    idFields: TffFieldList; {..field numbers for composite index}
    idFieldsAscDesc: TffFieldList; {..field numbers for AscDesc index}
    idFieldsSize: TffFieldList; {..field size for compare}
    idFieldsCase: TffFieldList;
    idFieldsNullTop: TffFieldList;
    idFieldsFlags: TffFieldList;
    idFieldIHlprs: TffFieldIHList; {..index helpers used to build/compare
    a composite index}
    idDups: boolean; {..0=no duplicate keys, 1=dups allowed}
    // for expression and userindex dll - udf
    idNoCase: boolean;
    idAscend: boolean;
  End;

  PffFileDescriptor = ^TffFileDescriptor;
  TffFileDescriptor = Packed Record {File descriptor}
    fdNumber: Longint; {..number of file (zero based)}
    fdDesc: TffDictItemDesc; {..description of file}
    fdExtension: TffExtension; {..extension for file}
    fdBlockSize: Longint; {..block size for file}
    fdType: TffFileType; {..type of file}
  End;

  PffAliasDescriptor = ^TffAliasDescriptor;
  TffAliasDescriptor = Packed Record {Database Alias descriptor}
    adAlias: TffName; {..alias name}
    adPath: TffPath; {..directory path for database}
  End;

  PffTableDescriptor = ^TfsTableDescriptor;
  TfsTableDescriptor = Packed Record
    tdTableName: TfsTableName;
    tdExt: TffExtension;
    tdSizeLo: TffWord32;
    tdSizeHi: TffWord32;
    tdTimeStamp: TffWord32;
  End;

  {===FSSQL&I information types===}
Type
  PffRebuildStatus = ^TffRebuildStatus;
  TffRebuildStatus = Packed Record {Rebuild operation status info}
    rsStartTime: DWord; {..start time (tick count from server)} {!!.10}
    rsSnapshotTime: DWord; {..snapshot time (tick count from server)} {!!.10}
    rsTotalRecs: Longint; {..total count of records to read}
    rsRecsRead: Longint; {..count of records read}
    rsRecsWritten: Longint; {..count of records written}
    rsPercentDone: Longint; {..RecsRead*100/TotalRecs}
    rsErrorCode: TffResult; {..error result for process}
    rsFinished: boolean; {..process has finished}
  End;

  PffRecordInfo = ^TffRecordInfo;
  TffRecordInfo = Packed Record {Information block for data records}
    riRecLength: Longint; {..record length}
    riRecCount: Longword; {..number of active records}
    riDelRecCount: Longword; {..number of deleted records}
    riRecsPerBlock: Longint; {..number of records in each block}
  End;

  PffIndexInfo = ^TffIndexInfo;
  TffIndexInfo = Packed Record {Information block for an index}
    iiKeyCount: Longint; {..number of keys}
    iiPageCount: Longint; {..number of B-Tree pages}
    iiMaxKeysPerNode: Longint; {..maximum number of keys per node page}
    iiMaxKeysPerLeaf: Longint; {..maximum number of keys per leaf page}
    iiKeyLength: Word; {..length of a key in bytes}
    iiAllowDups: boolean; {..duplicate keys allowed}
    iiKeysAreRefs: boolean; {..keys are reference numbers}
    iiBTreeHeight: Integer; {..height of the b-tree}
  End;

  PfsServerStatistics = ^TfsServerStatistics; {begin !!.10}
  TfsServerStatistics = Packed Record {Server statistics info}
    ssName: TffNetName;
    ssVersion: Longint;
    ssState: ShortString;
    ssClientCount: TffWord32;
    ssSessionCount: TffWord32;
    ssOpenDatabasesCount: TffWord32;
    ssOpenTablesCount: TffWord32;
    ssOpenCursorsCount: TffWord32;
    ssRamUsed: TffWord32;
    ssMaxRam: TffWord32;
    ssUpTimeSecs: DWord;
    ssCmdHandlerCount: Integer;
  End;

  PfsCommandHandlerStatistics = ^TfsCommandHandlerStatistics;
  TfsCommandHandlerStatistics = Packed Record {stats for command handler}
    csTransportCount: Integer;
  End;

  PfsTransportStatisticsInfo = ^TfsTransportStatistics;
  TfsTransportStatistics = Packed Record {stats related to a transport}
    tsName: TffNetName;
    tsState: ShortString;
    tsAddress: TffNetAddress;
    tsClientCount: TffWord32;
    tsMessageCount: TffWord32;
    tsMessagesPerSec: Double;
  End; {end !!.10}

  {===Notify event declarations===}
Type
  TfsNetIdle = Procedure(Sender: TObject);

Type

  { Delphi's memory management is not suitable for a 24x7 database server.  It
    will eat up memory and eventually crash.  To avoid this problem, we
    override certain VCL classes so that we can have the VCL classes use our
    own memory manager.  The new classes are listed below. }

  TfsPadlock = Class; { forward declaration }

  {===FSSQL&I TFSSpecObject class===}
    { All FF classes that would normally inherit from TObject must inherit
      from this class instead. }
  TFSSpecObject = Class(TObject)
    {Begin !!.03}
    {$IFDEF FF_DEBUG_THREADS}
  Protected {private}
    ffoMethodLock: Integer;
    ffoCurrentThreadID: Cardinal;
    ffoThreadLockCount: Integer;
  Protected
    Procedure ThreadEnter;
    Procedure ThreadExit;
  Public
    {$ENDIF}
    {End !!.03}
    Class Function NewInstance: TObject; Override;
    Procedure FreeInstance; Override;
  End;

  {===FSSQL&I TfsVCLList class===}
    { All FF classes using instances of TList should use this class instead. }
  TfsVCLList = Class(TList)
    Class Function NewInstance: TObject; Override;
    Procedure FreeInstance; Override;
  End;

  {===FSSQL&I TFSSpecPersis class===}
    { All FF classes that would normally inherit from TPersistent must inherit
      from this class instead. }
  TFSSpecPersis = Class(TPersistent)
    {Begin !!.03}
    {$IFDEF FF_DEBUG_THREADS}
  Protected {private}
    ffpMethodLock: Integer;
    ffpCurrentThreadID: Cardinal;
    ffpThreadLockCount: Integer;
  Protected
    Procedure ThreadEnter;
    Procedure ThreadExit;
  Public
    {$ENDIF}
    {End !!.03}
    Class Function NewInstance: TObject; Override;
    Procedure FreeInstance; Override;
  End;

  {===FSSQL&I TfsThread class===}
    { All FF classes that would normally inherit from TThread must inherit
      from this class instead.  Our reason for doing so is that Delphi's
      memory management is not suitable for a 24x7 database server.  It will
      eat up memory and eventually crash.  This class allocates its own memory.}
  TfsThread = Class(TThread)
    Class Function NewInstance: TObject; Override;
    Procedure FreeInstance; Override;
  Protected
    Procedure DoTerminate; Override;
    { Note: We override DoTerminate because the standard TThread.DoTerminate
      will block when it calls Synchronize if the thread was not created
      in the main thread of the application. }
{Begin !!.02}
  Public
    Procedure WaitForEx(Const Timeout: Longint);
    {End !!.02}
  End;

  {===Multithread support===}
    { Use TFSNormalEvent in those situations where Object A must wait for Object B to
      tell it something has happened.  For example, a TffRequest must wait for
      a reply to be received by the sending thread of a TffLegacyTransport. }
  TFSNormalEvent = Class(TFSSpecObject)
  Private
    ffeEvent: THandle; { the actual event object }
  Protected
  Public
    Constructor Create;

    Destructor Destroy; Override;

    Procedure WaitFor(Const timeOut: TffWord32);
    {-Call this method when an object must wait for this event to be
      signalled.  Timeout is the number of milliseconds the thread should
      wait for the event.  If timeOut is <= 0 then the thread will wait
      until the event is signalled otherwise it waits the specified
      number of milliseconds.  Raises an exception if the wait times out
      or a failure occurs. }

    Function WaitForQuietly(Const timeOut: TffWord32): DWORD;
    {-This method is just like the WaitFor method except that it returns
      an error code instead of raising an exception if a failure occurs.
      Possible return values:
      WAIT_ABANDONED - See MS SDK help for WaitForSingleObject. It is much
                       more mind-twisting than should be documented here.
      WAIT_OBJECT_0 - The event was signalled.
      WAIT_TIMEOUT - The timeout interval elapsed without the event being
                     signaled. }

    Procedure SignalEvent;
    {-Call this method when the event is to be set/raised/signalled.
      This releases a thread that called WaitFor. }

    Property Handle: THandle Read ffeEvent;
    {-Returns the events handle. }

  End;

  { Use TfsReadWritePortal to protect a data structure accessible by multiple
    threads.  This class allows multiple readers or one writer through the
    portal at a time.  It provides the best performance for multithreaded
    access to a data structure.

    When a thread wants to read the data structure, it must call BeginRead.
    It must then call EndRead when it has finished reading.

    When a thread wants to write to the data structure, it must call BeginWrite.
    It must then call EndWrite when it has finished writing.

    If a thread given write access needs to read the protected data structure
    then BeginRead automatically grants read access.

    Calls to BeginWrite are reference counted.  A thread granted write access
    may call BeginWrite multiple times but each call to BeginWrite must
    have a corresponding call to EndWrite.
  }

  TfsReadWritePortal = Class(TFSSpecObject)
  Private
    rwpBlockedReaders: THandle; { semaphore used to release blocked readers }
    rwpBlockedWriters: THandle; { semaphore used to release blocked writers }
    rwpGate: TfsPadlock; { critical section allowing single-threaded
    access to internal data structures }
    rwpActiveReaders: Integer; { the number of threads given read access }
    rwpActiveWriter: boolean; { if True then a thread has been granted
    write access; all other readers and writers
    are blocked }
    rwpActiveWriterID: TffWord32; { the threadID of the thread granted write
    access }
    rwpWaitingReaders: Integer; { the number of threads waiting for read
    access }
    rwpWaitingWriters: Integer; { the number of threads waiting for write
    access }
    rwpWriterReadCount: Integer; { the number of times the active writer has
    called BeginRead }
    rwpWriterWriteCount: Integer; { the number of times the active writer has
    called BeginWrite }
  Protected
  Public
    Constructor Create;
    {-Use this method to create an instance of TfsReadWritePortal.
      maxBlockedThreads is the maximum number of reader or writer threads
      that may wait for access to the protected data structure. }
    Destructor Destroy; Override;
    Procedure BeginRead;
    {-Call this method when a thread wants to start reading the protected
      data structure.  BeginRead will not return until the thread has been
      granted read access. Each occurrence of BeginRead must have a
      corresponding call to EndRead. }
    Procedure BeginWrite;
    {-Call this method when a thread wants to start writing the protected
      data structure.  BeginWrite will not return until the thread has
      been granted write access.  Each occurrence of BeginWrite must have a
      corresponding call to EndWrite. }
    Procedure EndRead;
    {-Call this method when a thread has finished reading the protected
      data structure. }
    Procedure EndWrite;
    {-Call this method when a thread has finished writing to the
      protected data structure. }
  End;

  { TfsPadlock allows only one reader or writer at a time.
    This class is obsolete and should be phased out. }
  TfsPadlock = Class {*NOT* class (TFSSpecObject)}
  Protected {public}
    plCount: Integer;
    plCritSect: TRTLCriticalSection;
  Protected
    Function GetLocked: boolean;
  Public
    Constructor Create;
    {-Create a multithread padlock}
    Destructor Destroy; Override;
    {-Free a multithread padlock}
    Procedure Lock;
    Procedure Unlock;
    Property Locked: boolean Read GetLocked;
  End;

  {===FSSQL&I List and List Item classes===}
Type
  TfsListState = (lsNormal, lsClearing);

  TfsListFindType = ({How to find an item in a list}
    ftFromID, {..from the item's ID}
    ftFromIndex); {..from the index of the item}

  TFSNormalList = Class;

  TFSSpecListItem = Class(TFSSpecObject)
  Protected {private}
    ffliList: TFSNormalList;
    ffliFreeOnRemove: boolean;
    ffliState: TfsListState;
    ffliMaintainLinks: boolean;
    { If True then track what lists contain this item. }

  Protected
    Function GetRefCount: Integer;
    Procedure ffliAddListLink(L: TFSNormalList);
    Procedure ffliBreakListLink(L: TFSNormalList);
    Procedure ffliSetMaintainLinks(Const Value: Boolean); {!!.11}
  Public
    Constructor Create;
    {-create the list item}
    Destructor Destroy; Override;
    {-destroy the list item; if the item is attached to any lists,
      it removes itself from those lists as well}

    Function Compare(aKey: pointer): Integer; Virtual; Abstract;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise}
    Function Key: pointer; Virtual; Abstract;
    {-return a pointer to this item's key}
    Property FreeOnRemove: boolean
      Read ffliFreeOnRemove Write ffliFreeOnRemove;
    {-if true, when item is removed from one list, it removes
      itself from all lists (and hence would be freed)}
    Property MaintainLinks: boolean
      Read ffliMaintainLinks Write ffliSetMaintainLinks;
    {-If True then track which lists contain this list item.
      Note that if you set this property after adding the item
      to one or more lists then it will already have a list
      of links to those lists.  So set it as soon as the item
      is created or pay the consequences. }
    Property ReferenceCount: Integer
      Read GetRefCount;
    {-the number of lists referencing this item}
  End;

  PfsListItemArray = ^TfsListItemArray;
  TfsListItemArray =
    Array[0..pred(MaxInt Div sizeof(TFSSpecListItem))] Of TFSSpecListItem;

  TfsStrListItem = Class(TFSSpecListItem)
  Protected {private}
    sliKey: PffShStr;
    sliExtraData: pointer;
  Protected
  Public
    Constructor Create(Const aKey: TffShStr);
    {-create the list item; aKey is its access/sort key}
    Destructor Destroy; Override;
    {-destroy the list item}

    Function KeyAsStr: TffShStr;
    {-return this item's key as a string (for convenience)}

    Function Compare(aKey: pointer): Integer; Override;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise}
    Function Key: pointer; Override;
    {-return a pointer to this item's key: it'll be a pointer to a
      shortstring}

    Property ExtraData: pointer
      Read sliExtraData Write sliExtraData;
  End;

  TfsUCStrListItem = Class(TfsStrListItem)
  Protected {private}
  Protected
  Public
    Function Compare(aKey: pointer): Integer; Override;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise; case insensitive compare}
  End;

  TfsI64ListItem = Class(TFSSpecListItem)
  Protected {private}
    iliKey: TffInt64;
    iliExtraData: Pointer;
  Public
    Constructor Create(Const aKey: TffInt64);
    {-create the list item; aKey is its access/sort key}
    Function KeyValue: TffInt64;
    {-return this item's ket as a TffInt64 (for convenience)}
    Function Compare(aKey: pointer): Integer; Override;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise}
    Function Key: pointer; Override;
    {-return a pointer to this item's key: it'll be a pointer to a
      TffInt64}
    Property ExtraData: Pointer
      Read iliExtraData Write iliExtraData;
    {-The additional data item attached to the list item.}
  End;

  TfsIntListItem = Class(TFSSpecListItem)
  Protected {private}
    iliKey: Longint;
    iliExtraData: pointer;
  Protected
  Public
    Constructor Create(Const aKey: Longint);
    {-create the list item; aKey is its access/sort key}
    Function KeyAsInt: Longint;
    {-return this item's key as a Longint (for convenience)}
    Function Compare(aKey: pointer): Integer; Override;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise}
    Function Key: pointer; Override;
    {-return a pointer to this item's key: it'll be a pointer to a
      Longint}
    Property ExtraData: pointer
      Read iliExtraData Write iliExtraData;
    {-The additional data item attached to the list item.}
  End;

  TFSSpecW32ListItem = Class(TFSSpecListItem)
  Protected {private}
    wliKey: TffWord32;
    wliExtraData: pointer;
    wliExtraData2: Longint;
  Protected
  Public
    Constructor Create(Const aKey: TffWord32);
    {-create the list item; aKey is its access/sort key}
    Function KeyValue: TffWord32;
    {-return this item's key as a TffWord32 (for convenience)}
    Function Compare(aKey: pointer): Integer; Override;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise}
    Function Key: pointer; Override;
    {-return a pointer to this item's key: it'll be a pointer to a
      Longint}
    Function KeyAsInt: TffWord32;
    {-return this item's key as a TffWord32 (for convenience)}
    Property ExtraData: pointer
      Read wliExtraData Write wliExtraData;
    {-An additional data item attached to the list item.}

    Property ExtraData2: Longint
      Read wliExtraData2 Write wliExtraData2;
    {-An additional data item attached to the list item.}
  End;

  TfsSelfListItem = Class(TfsIntListItem)
  Protected {private}
  Protected
  Public
    Constructor Create;
    {-create the list item; Key is the Self pointer as integer}
  End;

  TFSNormalList = Class(TFSSpecObject) {!!.01}
  Protected {private}
    fflCapacity: Longint;
    fflCount: Longint;
    fflList: PfsListItemArray;
    fflSorted: boolean;
    fflPortal: TfsReadWritePortal; {!!.02}
    fflState: TfsListState;
  Protected
    Procedure fflGrow;
    Function GetCapacity: Longint;
    Function GetCount: Longint;
    Function GetItem(Const aInx: Longint): TFSSpecListItem;
    Procedure SetCapacity(Const C: Longint);
    Procedure SetCount(Const C: Longint);
    Procedure SetItem(Const aInx: Longint; Item: TFSSpecListItem);
    Procedure SetSorted(S: boolean);

    Procedure fflDeleteAtPrim(aInx: Longint);
    {-Removes an item from the list and frees the item if its reference
      count is zero. }
    Function fflIndexPrim(Const aKey): Longint;
    Procedure fflRemoveAtPrim(aInx: Longint);
    {-Removes an item from the list but does not free the item. }

    Procedure InternalDelete(Const aKey); {!!.02}
  Public
    Constructor Create;
    {-create the list}
    Destructor Destroy; Override;
    {-destroy the list}
//      procedure Assign(Source : TPersistent); override;              {Deleted !!.01}
    {-assign another list's data to this one}
    Procedure Delete(Const aKey);
    {-Remove an item from the list, search for it.  Note this method
      will free the item if the item's reference count is zero.}
    Procedure DeleteAt(aInx: Longint);
    {-Remove an item from the list using its index.  Note this method
      will free the item if the item's reference count is zero.}
    Procedure Empty;
    {-empty the list of items}
    Function Exists(Const aKey): boolean;
    {-return true if the list has an item with the given key}
    Function GetInsertionPoint(aItem: TFSSpecListItem): Longint;
    {-Returns the index into which the item would be inserted. }
    Function Insert(aItem: TFSSpecListItem): boolean;
    {-insert an item in key sequence; return true on success}
    Function InsertPrim(aItem: TFSSpecListItem): Longint;
    {-insert an item in key sequence; return index or -1}
    Function IsEmpty: boolean;
    {-return true if the list is empty}
    Function Index(Const aKey): Longint;
    {-calculate the index of an item with the given key}

    Procedure Remove(Const aKey);
    {-Use this method to remove an item from the list without freeing
      the item. }
    Procedure RemoveAt(aInx: Longint);
    {-Use this method to remove an item at the specified position.  The
      item is not freed after it is removed from the list. }

    Property Capacity: Longint
      {-the total capacity of the list}
    Read GetCapacity Write SetCapacity;

    Property Count: Longint
      {-the number of items in the list}
    Read GetCount Write SetCount;

    Property Items[Const aInx: Longint]: TFSSpecListItem
    {-the list of items}
    Read GetItem Write SetItem;
    Default;

    Property Sorted: boolean
      {-true (by default) if the list is sorted; cannot set true if
list contains items}
    Read fflSorted Write SetSorted;
  End;

  { This class is a threadsafe version of TFSNormalList.  This class allows multiple
    threads to have read access or one thread to have write access (i.e.,
    multiple read, exclusive write).  A thread is granted write access only if
    there are no reading threads or writing threads.

    Threads desiring thread-safe access to the list must do the following:

    1. For read access, call BeginRead.  The thread will be blocked until
       it obtains read access.  Once the thread has finished, it must call
       EndRead.

    2. For write access, call BeginWrite.  The thread will be blocked until
       all existing readers and writers have finished.  Once the thread has
       finished, it must call EndWrite.

    For example:

      with FList.BeginWrite do
        try
          // do something
        finally
          EndWrite;
        end;

    This is a dangerous class to use in that outside objects are responsible
    for calling BeginRead, etc.  The outside code could be written such that
    it does not or such that it fails to call EndRead/EndWrite.

    However, this implementation was chosen so that only the appropriate
    amount of locking is performed.  For example, if something needs to read
    through a list of 100 items then we do not want to ask for read access
    100 times.  Instead, BeginRead is called once.
  }
  TFSSpecThreadList = Class(TFSNormalList)
  Protected {private}
    //      FPortal    : TfsReadWritePortal;                               {Deleted !!.02}
  Public

    Constructor Create; Virtual;

    Destructor Destroy; Override;

    Function BeginRead: TFSSpecThreadList;
    {-A thread must call this method to gain read access to the list.
      Returns the instance of TFSSpecThreadList as a convenience. }

    Function BeginWrite: TFSSpecThreadList;
    {-A thread must call this method to gain write access to the list.
      Returns the instance of TFSSpecThreadList as a convenience.}

    Procedure EndRead;
    {-A thread must call this method when it no longer needs read access
      to the list.  If it does not call this method, all writers will
      be perpetually blocked. }

    Procedure EndWrite;
    {-A thread must call this method when it no longer needs write access
      to the list.  If it does not call this method, all readers and writers
      will be perpetualy blocked. }
  End;

  TFSSpecStringList = Class(TFSSpecPersis)
  Protected {private}
    slCaseSensitive: boolean;
    slList: TFSNormalList;
  Protected
    Function GetCapacity: Longint;
    Function GetCount: Longint;
    Function GetObj(aInx: Longint): TObject;
    Function GetSorted: boolean;
    Function GetStr(aInx: Longint): TffShStr;
    Function GetValue(Const aName: TffShStr): TffShStr;
    Procedure SetCapacity(C: Longint);
    Procedure SetCaseSensitive(CS: boolean);
    Procedure SetObj(aInx: Longint; Const aObj: TObject);
    Procedure SetStr(aInx: Longint; Const aStr: TffShStr);
    Procedure SetSorted(S: boolean);
    Procedure SetValue(Const aName, aStr: TffShStr);

  Public
    Constructor Create;
    {-create the list}
    Destructor Destroy; Override;
    {-destroy the list}
    Procedure Assign(Source: TPersistent); Override;
    {-assign another list's string data to this one}
    Procedure AssignTo(Dest: TPersistent); Override;
    {-assign this string list's data to another one}
    Procedure Delete(Const aStr: TffShStr);
    {-remove a string from the list, search for it}
    Procedure DeleteAt(aInx: Longint);
    {-remove a string from the list using its index}
    Procedure Empty;
    {-empty the list of strings}
    Function Exists(Const aStr: TffShStr): boolean;
    {-return true if the list has an item with the given string}
    Function Index(Const aStr: TffShStr): Longint;
    {-calculate the index of an item with the given string}
    Function IndexOfName(Const aName: TffShStr): Longint;
    {-return the index of the name part of a string which is of
      the form Name=Value}
    Function Insert(Const aStr: TffShStr): boolean;
    {-insert an item in string sequence; return true on success}
    Function InsertPrim(Const aStr: TffShStr): Longint;
    {-insert an item in string sequence; return index or -1}
    Function IsEmpty: boolean;
    {-return true if the list is empty}

    Property Capacity: Longint
      {-the total capacity of the list}
    Read GetCapacity Write SetCapacity;

    Property CaseSensitive: boolean
      Read slCaseSensitive Write SetCaseSensitive;
    {-whether string compares are case sensitive or not; cannot
      set true if the list contains items}

    Property Count: Longint
      {-the number of strings in the list}
    Read GetCount;

    Property Strings[aInx: Longint]: TffShStr
    {-the list of strings}
    Read GetStr Write SetStr;
    Default;

    Property Objects[aInx: Longint]: TObject
    {-the list of objects associated with strings}
    Read GetObj Write SetObj;

    Property Sorted: boolean
      {-true (by default) if the list is sorted; cannot set true if
list contains items}
    Read GetSorted Write SetSorted;

    Property Values[Const aName: TffShStr]: TffShStr
    {-returns a string value given a string keyword.  Assumes the
      list of strings consists of "keyword=value" pairs. }
    Read GetValue Write SetValue;
  End;

  { The following types are used by TfsPointerList to store a list of pointers. }
  PfsPointerArray = ^TfsPointerArray;
  TfsPointerArray =
    Array[0..pred(MaxInt Div sizeof(Pointer))] Of Pointer;

  { This is an unsorted list type dealing only with pointers.  Note that it is
    the responsibility of the application to free the memory referenced by the
    pointer. }
  TfsPointerList = Class(TFSSpecPersis)
  Protected {private}
    plCapacity: Longint;
    plCount: Longint;
    plList: PfsPointerArray;
  Protected

    Function AppendPrim(aPtr: Pointer): Longint;
    Procedure fflGrow;
    Function GetCapacity: Longint;
    Function GetCount: Longint;
    Function GetPointer(aInx: Longint): Pointer;
    Function GetInternalAddress: Pointer;
    Procedure SetCapacity(Const C: Longint);
    Procedure SetCount(Const C: Longint);
    Procedure SetPointer(aInx: Longint; aPtr: Pointer);

    Procedure fflRemoveAtPrim(aInx: Longint);
    {-Removes an item from the list but does not free the item. }

  Public
    Constructor Create;
    {-create the list}
    Destructor Destroy; Override;
    {-destroy the list}
    Procedure Assign(Source: TPersistent); Override;
    {-assign another list's data to this one}
    Function Append(aPtr: Pointer): boolean;
    {-append an item to the list; return true on success}
    Procedure Empty;
    {-Empty the list of pointers.  Note that the application is
      responsible for freeing the memory referenced by the pointers. }
    Function IsEmpty: boolean;
    {-return true if the list is empty}

    Procedure RemoveAt(aInx: Longint);
    {-Use this method to remove the pointer at the specified position. }

    Property Capacity: Longint
      {-the total capacity of the list}
    Read GetCapacity Write SetCapacity;

    Property Count: Longint
      {-the number of items in the list}
    Read GetCount Write SetCount;

    Property InternalAddress: pointer Read GetInternalAddress;
    {-Returns a pointer to the internal list of pointers.  Be careful with
      this.  It is to be used only when necessary. }

    Property List: PfsPointerArray Read plList;
    {-Provides direct access to the internal list of pointers.  Use this
      only if you know what you are doing. }

    Property Pointers[aInx: Longint]: Pointer
    {-the list of items}
    Read GetPointer Write SetPointer; Default;
  End;

  { The following types are used by TFSSpecList to store a list of handles. }
  PfsHandleArray = ^TfsHandleArray;
  TfsHandleArray =
    Array[0..pred(MaxInt Div sizeof(THandle))] Of THandle;

  { This is an unsorted list type dealing only with THandles.  It is used by
    TfsSemaphorePool, TfsMutexPool & TFSEvent. }
  TFSSpecList = Class(TFSSpecPersis)
  Protected {private}
    FCapacity: Longint;
    FCount: Longint;
    FList: PfsHandleArray;
  Protected

    Function AppendPrim(aHandle: THandle): Longint;
    Procedure fflGrow;
    Function GetCapacity: Longint;
    Function GetCount: Longint;
    Function GetHandle(aInx: Longint): THandle;
    Function GetInternalAddress: pointer;
    Procedure SetCapacity(Const C: Longint);
    Procedure SetCount(Const C: Longint);

    Procedure fflDeleteAtPrim(aInx: Longint);
    {-Removes an item from the list and frees the item if its reference
      count is zero. }
    Procedure fflRemoveAtPrim(aInx: Longint);
    {-Removes an item from the list but does not free the item. }

  Public
    Constructor Create;
    {-create the list}
    Destructor Destroy; Override;
    {-destroy the list}
    Procedure Assign(Source: TPersistent); Override;
    {-assign another list's data to this one}
    Procedure DeleteAt(aInx: Longint);
    {-Remove an item from the list using its index.  Note this method
      will close the handle. }
    Procedure Empty;
    {-empty the list of items}
    Function Append(aHandle: THandle): boolean;
    {-append an item to the list; return true on success}
    Function IsEmpty: boolean;
    {-return true if the list is empty}

    Procedure RemoveAll;
    {-Removes all handles from the list without closing any of the
      handles. }

    Procedure RemoveAt(aInx: Longint);
    {-Use this method to remove an item at the specified position.  The
      handle is not closed after it is removed from the list. }

    Property Capacity: Longint
      {-the total capacity of the list}
    Read GetCapacity Write SetCapacity;

    Property Count: Longint
      {-the number of items in the list}
    Read GetCount Write SetCount;

    Property InternalAddress: pointer Read GetInternalAddress;
    {-Returns a pointer to the internal list of handles.  Be careful with
      this.  It is to be used only when necessary. }

    Property Handles[aInx: Longint]: THandle
    {-the list of items}
    Read GetHandle; Default;
  End;

  { This is a thread-safe string list class.  It handles read/write access issues
    identical to TFSSpecThreadList. }
  TFSSpecThreadStringList = Class(TFSSpecStringList)
  Protected
    tslPortal: TfsReadWritePortal;
  Public

    Constructor Create;

    Destructor Destroy; Override;

    Function BeginRead: TFSSpecThreadStringList;
    {-A thread must call this method to gain read access to the list.
      Returns the instance of TFSSpecThreadList as a convenience. }

    Function BeginWrite: TFSSpecThreadStringList;
    {-A thread must call this method to gain write access to the list.
      Returns the instance of TFSSpecThreadList as a convenience. }

    Procedure EndRead;
    {-A thread must call this method when it no longer needs read access
      to the list.  If it does not call this method, all writers will
      be perpetually blocked. }

    Procedure EndWrite;
    {-A thread must call this method when it no longer needs write access
      to the list.  If it does not call this method, all readers and writers
      will be perpetualy blocked. }

  End;

  TFSSpecQueue = Class(TFSSpecObject)
  Protected
    ffqList: TFSNormalList;

    Function GetCount: Longint;

    Function GetItem(aInx: Longint): TFSSpecListItem;

  Public

    Constructor Create;

    Destructor Destroy; Override;

    Procedure Delete(Const aKey);
    { Remove an item from the queue based upon its key. }

    Function Dequeue: TFSSpecListItem;
    {-Returns the first item inserted into the queue or nil if the queue
      is empty.  The item is automatically removed from the queue. }

    Procedure Enqueue(anItem: TFSSpecListItem);
    {-Add an item to the queue. }

    Function IsEmpty: boolean;
    {-Returns True if the queue is empty. }

    Property Count: Longint Read GetCount;
    {-Returns the number of items in the queue. }

    Property Items[aInx: Longint]: TFSSpecListItem Read GetItem; Default;
    {-The list of queued items.  Items[0] is the first item in the
      queue. }

  End;

  TfsThreadQueue = Class(TFSSpecQueue)
  Protected
    fftqPortal: TfsReadWritePortal;
  Public
    Constructor Create;

    Destructor Destroy; Override;

    Function BeginRead: TfsThreadQueue;
    {-A thread must call this method to gain read access to the queue.
      Returns the instance of TfsThreadQueue as a convenience. }

    Function BeginWrite: TfsThreadQueue;
    {-A thread must call this method to gain write access to the queue.
      Returns the instance of TfsThreadQueue as a convenience. }

    Procedure EndRead;
    {-A thread must call this method when it no longer needs read access
      to the queue.  If it does not call this method, all writers will
      be perpetually blocked. }

    Procedure EndWrite;
    {-A thread must call this method when it no longer needs write access
      to the queue.  If it does not call this method, all readers and writers
      will be perpetualy blocked. }

  End;

  {===Semaphore Pool===}
Type
  TfsSemaphorePool = Class
  Protected
    spList: TFSSpecList;
    spRetainCount: Integer;
    spPadLock: TfsPadlock;
  Public
    Constructor Create(Const initialCount, retainCount: Integer);
    Destructor Destroy; Override;
    Procedure Flush;
    Function Get: THandle;
    Procedure GetTwo(Var aHandle1, aHandle2: THandle); {!!.06}
    Procedure Put(Const aHandle: THandle);
  End;

  {===Mutex Pool===}
Type
  TfsMutexPool = Class
  Protected
    mpList: TFSSpecList;
    mpRetainCount: Integer;
    mpPadLock: TfsPadlock;
  Public
    Constructor Create(Const initialCount, retainCount: Integer);
    Destructor Destroy; Override;
    Procedure Flush;
    Function Get: THandle;
    Procedure Put(Const aHandle: THandle);
  End;

  {$IFDEF UseEventPool}
  {===Event Pool===}
Type
  TFSEvent = Class
  Protected
    epList: TFSSpecList;
    epRetainCount: Integer;
    epPadLock: TfsPadlock;
  Public
    Constructor Create(Const InitialCount, RetainCount: Integer);
    Destructor Destroy; Override;
    Procedure Flush;
    Function Get: THandle;
    Procedure Put(Const aHandle: THandle);
  End;
  {$ENDIF}

  {===Memory Pool===}
Type
  { This type defines the format of the information at the head of each
    block allocated by a memory pool. }
  PfsMemBlockInfo = ^TfsMemBlockInfo;
  TfsMemBlockInfo = Packed Record
    NextBlock: pointer;
    UsageCounter: Longint;
  End;
  TfsMemoryPool = Class
    { A memory pool is a heap manager for managing allocations and
      deallocations of items on the heap which all have the same size.  This
      class helps reduce heap fragmentation when lots of small allocations
      (interspersed with frees) are made on the heap.

      In practice, an application will have multiple memory pools to support
      allocation of items of varying size.

      When new memory is needed, a memory pool requests a slightly larger
      than 64k block from the Delphi memory manager.  The memory pool's
      block format is as follows:

      1. The first 4 bytes of a block are used as a pointer to the next block
         previously allocated by the memory pool.  The memory pool maintains
         a chain of blocks.  When the memory pool is freed, it walks through
         and deallocates the blocks.  The very last block in the chain will
         have these 4 bytes set to nil.

      2. The second 4 bytes of a block implement a usage counter.  As mentioned
         above, a block will be subdivided into one or more items with one
         item being handed out to each request for memory.  The usage counter
         tracks the actual number of items handed out.  The usage counter is
         incremented when an item is allocated (i.e., handed out).  The usage
         counter is decremented when an item is deallocated (i.e., handed
         back).

         The memory pool's RemoveUnusedBlocks method gets rid of blocks having
         their usage counter set to zero.

      3. The remaining bytes of the block are subdivided into items of the
         size supported by the pool.  However, each item includes an extra
         2 bytes which serve as an offset back to the block's usage counter.

         For example, if the memory pool is created to support items that are
         32 bytes in size then the 32k block will be subdivided into
         65536 div (32 bytes + 2 bytes) = 1,927 items.  As mentioned above, the
         first 2 bytes of each item provide an offset back to the block's usage
         counter.  This is required so that when an item is deallocated, the
         block's usage counter may be decremented.

         The next 4 bytes of the item are used to include the item in a chain
         of free items.  When the block is initialized, the memory pool
         walks through the items forming a chain as it goes.  The first item
         in the block has this 4 bytes set to nil.  The second item has the
         4 bytes pointing back to the first item.  The third item has the
         4 bytes pointing back to the second item, and so on until the last
         item of the block.

         This chaining makes it very quick to allocate a new item.  The
         memory pool maintains a pointer to the first free item (regardless
         of block).  When the item is allocated, the memory pool updates the
         head of this chain to point to the item referenced by the
         newly-allocated item. }
  Protected {private}
    FItemSize: TffMemSize;
    FItemsInBlock: Integer;
    FBlockSize: Integer;
    FFirstBlock: PfsMemBlockInfo;
    FFreeList: pointer;
    {-Points to the next available item in a chain of items that  The free
      list is updated as items are freed and removed. }

    mpPadlock: TfsPadlock;
  Protected
    Procedure mpAddBlock;
    Procedure mpCleanFreeList(Const BlockStart: pointer);
    {-When a block is removed from memory, this routine is used to remove
      the block's items from the free list. }
  Public
    Constructor Create(ItemSize: TffMemSize; ItemsInBlock: Integer);
    {-Create a pool of items. Each item has size ItemSize;
      ItemsInBlock defines how many items are allocated at once
      from the Delphi heap manager.  If ItemSize * ItemsInBlock > 64k
      then ItemsInBlock will be reduced such that it fits within 64k. }
    Destructor Destroy; Override;
    {-Free all blocks in the memory pool; destroy the object; all
      non-freed allocations from the pool will be invalid after
      this point}
    Function Alloc: pointer;
    {-Allocate a new item from the pool, return its address}
    Function BlockCount: Longint;
    {-Return the number of blocks owned by the memory pool. }
    Function BlockUsageCount(Const BlockIndex: Longint): Longint;
    {-Retrieves the usage count for a specific block.  BlockIndex identifies
      the block whose usage count is to be retrieved and is base 0.
      Returns -1 if the specified block could not be found. }
    Procedure Dispose(Var P);
    {-Return an item to the pool for reuse; set the pointer to nil}
    Function RemoveUnusedBlocks: Integer;
    {-Use this method to have the memory pool free its unused blocks.
      Returns the number of blocks freed. }

    Property BlockSize: Integer Read FBlockSize;
    { The total size of a block in the memory pool. }

    Property ItemsInBlock: Integer Read FItemsInBlock;
    { The number of items into which a block is subdivided. }

    Property ItemSize: TffMemSize Read FItemSize;
    { The size of each item within the block. }
  End;

  {===FSSQL&I TFSSpecComp class===}
    { All FF classes that would normally inherit from TComponent must inherit
      from this class instead. }
  TFSSpecComp = Class(TComponent)
    {$IFDEF IsDelphi} {!!.03}
    Class Function NewInstance: TObject; Override;
    Procedure FreeInstance; Override;
    {$ENDIF} {!!.03}
    {Begin !!.03}
    {$IFDEF FF_DEBUG_THREADS}
  Protected {private}
    ffcMethodLock: Integer;
    ffcCurrentThreadID: Cardinal;
    ffcThreadLockCount: Integer;
  Protected
    Procedure ThreadEnter;
    Procedure ThreadExit;
  Public
    {$ENDIF}
    {End !!.03}
  Protected
    fcDependentList: TFSNormalList; {!!.11}
    fcLock: TfsPadlock; {!!.11}
    fcDestroying: Boolean;
    Function GetVersion: String;
    Procedure SetVersion(Const Value: String);
  Public
    Constructor Create(AOwner: TComponent); Override;
    Destructor Destroy; Override;

    Procedure FFAddDependent(ADependent: TFSSpecComp); Virtual; {!!.11}
    Procedure FFNotification(Const AOp: Byte; AFrom: TFSSpecComp);
    Procedure FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
      Const AData: TffWord32); Virtual;
    Procedure FFRemoveDependent(ADependent: TFSSpecComp); Virtual; {!!.11}
    Procedure FFNotifyDependents(Const AOp: Byte); Virtual; {!!.05}
    Procedure FFNotifyDependentsEx(Const AOp: Byte; Const AData: TffWord32);
  Published
    Property Version: String
      Read GetVersion
      Write SetVersion
      Stored False;
  End;

  {===Timer declarations===}
Type
  TfsTimer = Packed Record
    trStart: DWord; {!!.10}
    trExpire: DWord; {!!.10}
    trWrapped: boolean;
    trForEver: boolean;
  End;

Const
  fsc_TimerInfinite = 0; {!!.06}
  fsc_TimerMaxExpiry = 3600 * 1000;

Procedure SetTimer(Var T: TfsTimer; Time: DWord); {!!.10}
{-Set a timer to expire in Time milliseconds. 1 <= Time <= 30000.}
Function HasTimerExpired(Const T: TfsTimer): boolean;
{-Return true if the timer has expired}

{===Comparison declarations===}
Function FFCmpB(a, b: Byte): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b unsigned 8-bit}
Function FFCmpDW(Const a, b: TffWord32): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b unsigned 32-bit}
Function FFCmpI(a, b: Integer): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed integers}
Function FFCmpI16(a, b: Smallint): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed 16-bit}
Function FFCmpI32(a, b: Longint): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed 32-bit}
Function FFCmpI8(a, b: Shortint): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed 8-bit}
Function FFCmpW(a, b: TffWord16): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b unsigned 16-bit}
Function FFCmpBytes(Const a, b: PffByteArray; MaxLen: Integer): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b byte arrays}
{ At most MaxLen bytes are compared}
Function FFCmpShStr(Const a, b: TffShStr; MaxLen: Byte): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b short strings}
{ At most MaxLen characters are compared}
Function FFCmpShStrUC(Const a, b: TffShStr; MaxLen: Byte): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b short strings, case insensitive}
{ At most MaxLen characters are compared}
Function FFCmpI64(Const a, b: TffInt64): Integer;
{-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed TffWord32}

{===TffInt64 Operations===}
Procedure ffCloneI64(Var aDest: TffInt64; Const aSrc: TffInt64);
{-clone a variable of type TffInt64}
Procedure ffInitI64(Var I: TffInt64);
{-initialize a variable of type TffInt64}
Procedure ffShiftI64L(Const I: TffInt64; Const Bits: Byte; Var Result: TffInt64);
{-shift a TffInt64 to the left Bits spaces}
Procedure ffShiftI64R(Const I: TffInt64; Const Bits: Byte; Var Result: TffInt64);
{-shift a TffInt64 to the right Bits spaces}
Procedure ffI64MinusI64(Const a, b: TffInt64; Var Result: TffInt64);
{-subtract a TffInt64 from a TffInt64}
Procedure ffI64MinusInt(Const aI64: TffInt64; Const aInt: TffWord32; Var Result: TffInt64);
{-subtract an integer from a TffInt64}
Function ffI64ModInt(Const aI64: TffInt64; Const aInt: TffWord32): Integer;
{-remainder of aI64 divided by aInt}
Procedure ffI64DivInt(Const aI64: TffInt64; Const aInt: TffWord32; Var Result: TffInt64);
{-divide a TffInt64 by an integer}
Procedure ffI64MultInt(Const aI64: TffInt64; Const aInt: TffWord32; Var Result: TffInt64);
{-Multiply a TffInt64 by an integer}
Procedure ffI64AddInt(Const aI64: TffInt64; Const aInt: TffWord32; Var Result: TffInt64);
{-add an integer to a TffInt64}
Function ffI64ToInt(Const aI64: TffInt64): TffWord32;
{-convert a TffInt64 to an integer}
Function ffI64ToStr(Const aI64: TffInt64): String;
{-convert a TffInt64 to a string}
Procedure ffIntToI64(Const aInt: TffWord32; Var Result: TffInt64);
{-convert an integer to a TffInt64}
Function ffI64IsZero(Const aI64: TffInt64): boolean;
{-If the specified Int64 is zero then return True. }
Function ffI64IsNonZero(Const aI64: TffInt64): boolean;

{===Minimum/maximum declarations===}
Function FFMinDW(a, b: TffWord32): TffWord32;
{-calculate the (signed) minimum of two long integers}
Function FFMaxDW(a, b: TffWord32): TffWord32;
{-calculate the (signed) maximum of two long integers}
Function FFMinI(a, b: Integer): Integer;
{-calculate the (signed) minimum of two integers}
Function FFMaxI(a, b: Integer): Integer;
{-calculate the (signed) maximum of two integers}
Function FFMinL(a, b: Longint): Longint;
{-calculate the (signed) minimum of two long integers}
Function FFMaxL(a, b: Longint): Longint;
{-calculate the (signed) maximum of two long integers}
Function FFMinI64(a, b: TffInt64): TffInt64;
{-calculate the (signed) minimum of two TffInt64s}
Function FFMaxI64(a, b: TffInt64): TffInt64;
{-calculate the (signed) maximum of two TffInt64s}

{===Calculate value declarations===}
Function FFCheckDescend(aAscend: boolean; a: Integer): Integer;
{-if aAscend is false, -a is returned, if true a is returned}
Function FFForceInRange(a, aLow, aHigh: Longint): Longint;
{-Force a to be in the range aLow..aHigh inclusive}
{ NOTE: no checks are made to see that aLow < aHigh}
Function FFForceNonZero(a, b: Integer): Integer;
{-if first integer is non-zero return it, else return second}

{===Memory allocation, etc===}
Procedure FFFreeMem(Var P; Size: TffMemSize);
{-deallocate memory allocated by FFGetMem}
Procedure FFGetMem(Var P; Size: TffMemSize);
{-like GetMem, but uses memory pools}
Procedure FFGetZeroMem(Var P; Size: TffMemSize);
{-like GetMem, but allocated memory is zeroed out}
Procedure FFReallocMem(Var P; OldSize, NewSize: Integer);
{-deallocates OldSize bytes for P then allocates aNewSize bytes
  for P. }

{===String routines===}
Function FsCommaizeChL(L: Longint; Ch: AnsiChar): AnsiString;
{-Convert a long integer to a string with Ch in comma positions}
Procedure FFShStrConcat(Var Dest: TffShStr; Const Src: TffShStr);
Procedure FFShStrAddChar(Var Dest: TffShStr; C: AnsiChar);
Function FFShStrAlloc(Const S: TffShStr): PffShStr;
Procedure FFShStrFree(Var P: PffShStr);
Function FFShStrRepChar(C: AnsiChar; N: Integer): TffShStr;
Function FFShStrUpper(Const S: TffShStr): TffShStr;
Function FFShStrUpperAnsi(Const S: TffShStr): TffShStr;
Function FFStrAlloc(aSize: Integer): PAnsiChar;
Function FFStrAllocCopy(S: PAnsiChar): PAnsiChar;
Procedure FFStrDispose(S: PAnsiChar);
Function FFStrNew(Const S: TffShStr): PAnsiChar;
Function FFStrPas(S: PAnsiChar): TffShStr;
Function FFStrPasLimit(S: PAnsiChar; MaxCharCount: Integer): TffShStr;
Function FFStrPCopy(Dest: PAnsiChar; Const S: TffShStr): PAnsiChar;
Function FFStrPCopyLimit(Dest: PAnsiChar; Const S: TffShStr;
  MaxCharCount: Integer): PAnsiChar;
Procedure FFShStrSplit(S: TffShStr; Const SplitChars: TffShStr;
  Var Left, Right: TffShStr);
{-Returns in Left and Right the substrings of S that exist to the left
  and right of any occurrence of any character given in SplitChars (see
  implementation) }
Procedure FFStrTrim(P: PAnsiChar);
{-Trim leading and trailing blanks from P}
Function FFStrTrimR(S: PAnsiChar): PAnsiChar;
{-Return a string with trailing white space removed}
Function FFShStrTrim(Const S: TffShStr): TffShStr;
Function FFShStrTrimL(Const S: TffShStr): TffShStr;
Function FFShStrTrimR(Const S: TffShStr): TffShStr;
Function FFShStrTrimWhite(Const S: TffShStr): TffShStr;
Function FFShStrTrimWhiteL(Const S: TffShStr): TffShStr;
Function FFShStrTrimWhiteR(Const S: TffShStr): TffShStr;
Function FFTrim(Const S: String): String;
Function FFTrimL(Const S: String): String;
Function FFTrimR(Const S: String): String;
Function FFTrimWhite(Const S: String): String;
Function FFTrimWhiteL(Const S: String): String;
Function FFTrimWhiteR(Const S: String): String;
Function FFOmitMisc(Const S: String): String;
{-Omit whitespace and punctuation characters from a string. }
Function FFAnsiCompareText(Const S1, S2: String): Integer; {!!.10}
{-Includes an extra failsafe comparison option if SafeAnsiCompare
  is defined }
Function FFAnsiStrIComp(S1, S2: PChar): Integer; {!!.10}
{-Includes an extra failsafe comparison option if SafeAnsiCompare
  is defined }
Function FFAnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer; {!!.10}
{-Includes an extra failsafe comparison option if SafeAnsiCompare
  is defined }

{===Wide-String routines===}
Function FFCharToWideChar(Ch: AnsiChar): WideChar;
{-Copies an ANSI character to a UNICODE wide character}

Function FFWideCharToChar(WC: WideChar): AnsiChar;
{-Copies a UNICODE wide char to an ANSI character}

Function FFShStrLToWideStr(S: TffShStr; WS: PWideChar; MaxLen: Longint): PWideChar;
{-Copies a short string to a null-terminated UNICODE wide string}

Function FFWideStrLToShStr(WS: PWideChar; MaxLen: Longint): TffShStr;
{-Copies a null-terminated UNICODE wide string to a short string}

Function FFNullStrLToWideStr(ZStr: PAnsiChar; WS: PWideChar; MaxLen: Longint): PWideChar;
{-Copies a null-terminated ANSI string to a null-terminated UNICODE wide string}

Function FFWideStrLToNullStr(WS: PWideChar; ZStr: PAnsiChar; MaxLen: Longint): PAnsiChar;
{-Copies a null-terminated UNICODE wide string to a null-terminated ANSI string}

Function FFWideStrLToWideStr(aSourceValue, aTargetValue: PWideChar; MaxLength: Longint): PWideChar;
{-Copies a null-terminated UNICODE wide string to another null-terminated UNICODE string}

{===File and Path name routines===}
Function FFDirectoryExists(Const Path: TffPath): boolean;
{-Returns true if the directory given by PN exists}
Function FFExpandFileName(Const FN: TffFullFileName): TffFullFileName;
{-Merges the filename with the current drive/directory to give a
  fully expanded file name; . and .. references are removed}
Function FFExtractExtension(Const PFN: TffFullFileName): TffExtension;
{-Extracts the file name extension from the path/file name PFN}
Function FFExtractFileName(Const PFN: TffFullFileName): TffFileName;
{-Strips the path and extension from the path/file name PFN}
Function FFExtractPath(Const PFN: TffFullFileName): TffPath;
{-Extracts the path from the path/file name PFN (excluding final \)}
Function FFExtractTableName(Const PFN: TffFullFileName): TfsTableName;
{-Strips the path and extension from the path/file name PFN to give a table name}
Function FFFileExists(Const PFN: TffFullFileName): boolean;
{-Return true if the file exists; wildcards are not allowed: if any
  are found, returns false}
Procedure FFFindClose(Var SR: TffSearchRec);
Function FFFindFirst(Const PFN: TffFullFileName;
  ItemType: TffDirItemTypeSet;
  Attr: TffDirItemAttrSet;
  Var SR: TffSearchRec): Integer;
Function FFFindNext(Var SR: TffSearchRec): Integer;
{-Directory 'find file' routines, in 32-bit they use shortstrings
  instead}
Function FFForceExtension(Const PFN: TffFullFileName;
  Const Ext: TffExtension): TffFullFileName;
{-Forces the path/file name PFN to have a given extension Ext}
Function FFGetCurDir: TffPath;
{-Returns the current directory (in 16-bit, on the current drive)}
Function FFGetDirList(Const Path: TffPath; FileSpec: TffFileNameExt): TFSSpecStringList;
{-Reads a directory with a given file spec, creates a string list to
  hold each file+ext encountered (the caller must free the list)}
Function FFGetEXEName: TffFullFileName;
{-Retrieves the full expanded file name of the calling program}
Function FFHasExtension(Const PFN: TffFullFileName; Var DotPos: Integer): boolean;
{-Returns true and the period position if the given path/file name
  has an extension}
Function FFMakeFileNameExt(Const FileName: TffFileName;
  Const Ext: TffExtension): TffFileNameExt;
{-Concatenate a file name with extension}
Function FFMakeFullFileName(Const Path: TffPath;
  Const FileName: TffFileNameExt): TffFullFileName;
{-Prepend a path to a file name with extension}
Function FFSetCurDir(Path: TffPath): boolean;
{-Set the current directory}

{===BitSet routines===}
Procedure FFClearAllBits(BitSet: PffByteArray; BitCount: Integer);
{-Clear all bits in a bit set}
Procedure FFClearBit(BitSet: PffByteArray; Bit: Integer);
{-Clear a bit in a bit set}
Function FFIsBitSet(BitSet: PffByteArray; Bit: Integer): boolean;
{-Return whether a bit is set}

Function GetFlags(Value: Int64; Bit: Int64): Boolean;
Procedure SetFlags(Var Value: Int64; Bit: Int64; Checked: boolean);

Procedure FFSetAllBits(BitSet: PffByteArray; BitCount: Integer);
{-Clear a bit set, ie set all bits off}
Procedure FFSetBit(BitSet: PffByteArray; Bit: Integer);
{-Set all bits in a bit set}

{===Verification routines===}
Function FFVerifyBlockSize(BlockSize: Longint): boolean;
{-Verify BlockSize to be 4K, 8K, 16K or 32K}
Function FFVerifyKeyLength(KeyLen: Word): boolean;
{-Verify length of key to be between 1 and 1024}
Function FFVerifyExtension(Const Ext: TffExtension): boolean;
{-Validates a string to contain a valid extension; allowed: a-z, 0-9
  and _}
Function FFVerifyFileName(Const FileName: TffFileName): boolean;
{-Validates a string to contain a valid filename (no drive, path or
  extension allowed); in 16-bit the length must be 8 or less; in
  32-bit it must be 31 characters or less; allowed: a-z, 0-9 and _}
Function FFVerifyServerName(aName: TffNetAddress): Boolean;
{-Validates a string to contain a valid server name; must be 15
  chars or less; valid chars are A-Z, a-z, 0-9, or space }

{===WWW Interfaces===}
Procedure ShellToWWW;
{-Shell out to TurboPower WWW site}
Procedure ShellToEMail;
{-Shell to e-mail to TurboPower tech support}

{===Mutex & Semaphore pools===}
Var
  ValidPhysicalConversions: Array[TfsFieldType, TfsFieldType] Of Boolean;
  FFSemPool: TfsSemaphorePool;
  { FF uses a lot of semaphores for managing threadsafe lists & queues.
    It takes a lot of time to create semaphores so we store unused
    semaphores in a pool until they are needed. }

  {$IFDEF UseEventPool}
  FFEventPool: TFSEvent;
  {$ENDIF}
  { FF uses a lot of semaphores for managing access to threadsafe lists
    & queues.  It takes a lot  of time to create events so we store unused
    events in a pool until they are needed. }

{===Utility routines===}
Function FFByteAsHex(Dest: PAnsiChar; B: Byte): PAnsiChar;
Function FFMapBlockSize(Const aBlockSize: Longint): TffBlockSize;
Function FFPointerAsHex(Dest: PAnsiChar; P: pointer): PAnsiChar;
Procedure FFFlushMemPools; {!!.01}
{== File-related utility routines ====================================}
Function PreGetDiskFreeSpaceEx(Directory: PChar;
  Var FreeAvailable,
  TotalSpace: TLargeInteger;
  TotalFree: PLargeInteger)
  : Bool; stdcall;

Function FFGetDiskFreeSpace(Const aDirectory: String): Int64;
{ Returns the amount of free space on the specified drive & directory,
  in kilobytes. }

Var
  FFLLGetDiskFreeSpaceEx: Function(Directory: PChar;
    Var FreeAvailable,
    TotalSpace: TLargeInteger;
    TotalFree: PLargeInteger)
  : Bool Stdcall;

  {$IFDEF MemPoolTrace}
Var
  Log: System.Text;
  {$ENDIF}

Implementation

Uses
  {$IFDEF FF_DEBUG_THREADS}
  JclSynch,
  {$ENDIF}
  fsllexcp;

{===Timer routines===================================================}

Procedure SetTimer(Var T: TfsTimer; Time: DWord); {!!.10}
Begin
  With T Do
    Begin
      If (Time = fsc_TimerInfinite) Then
        Begin
          trForEver := True;
          trStart := 0;
          trExpire := 0;
          trWrapped := False;
        End
      Else
        Begin
          trForEver := False;
          Time := FFForceInRange(Time, 1, fsc_TimerMaxExpiry);
          trStart := GetTickCount;
          trExpire := trStart + Time;
          trWrapped := FFCmpDW(trStart, trExpire) < 0;
        End;
    End;
End;
{--------}

Function HasTimerExpired(Const T: TfsTimer): boolean;
Asm
  push ebx
  xor ebx, ebx
  cmp [eax].TfsTimer.trForEver, 0
  jne @@Exit
  push eax
  call GetTickCount
  pop edx
  mov ecx, [edx].TfsTimer.trExpire
  mov edx, [edx].TfsTimer.trStart
  cmp edx, ecx
  jbe @@StartLEExpire
@@StartGEExpire:
  cmp eax, edx
  jae @@Exit
  cmp eax, ecx
  jae @@Expired
  jmp @@Exit
@@StartLEExpire:
  cmp eax, ecx
  jae @@Expired
  cmp eax, edx
  jae @@Exit
@@Expired:
  inc ebx
@@Exit:
  mov eax, ebx
  pop ebx
End;
{====================================================================}

{===Utility routines=================================================}

Function FFByteAsHex(Dest: PAnsiChar; B: Byte): PAnsiChar;
Const
  HexChars: Array[0..15] Of AnsiChar = '0123456789abcdef';
Begin
  If (Dest <> Nil) Then
    Begin
      Dest[0] := HexChars[B Shr 4];
      Dest[1] := HexChars[B And $F];
      Dest[2] := #0;
    End;
  Result := Dest;
End;
{--------}

Function FFMapBlockSize(Const aBlockSize: Longint): TffBlockSize;
Begin
  Case aBlockSize Of
    4 * 1024: Result := ffbs4k;
    8 * 1024: Result := ffbs8k;
    16 * 1024: Result := ffbs16k;
    32 * 1024: Result := ffbs32k;
    64 * 1024: Result := ffbs64k;
    Else
      Result := ffbs4k
  End; { case }
End;
{--------}

Function FFPointerAsHex(Dest: PAnsiChar; P: pointer): PAnsiChar;
Var
  L: Longint;
Begin
  Result := Dest;
  If (Dest <> Nil) Then
    Begin
      L := Longint(P);
      FFByteAsHex(Dest, L Shr 24);
      inc(Dest, 2);
      FFByteAsHex(Dest, (L Shr 16) And $FF);
      inc(Dest, 2);
      FFByteAsHex(Dest, (L Shr 8) And $FF);
      inc(Dest, 2);
      FFByteAsHex(Dest, L And $FF);
    End;
End;
{====================================================================}

{===Integer comparison declarations==================================}

Function FFCmpB(a, b: Byte): Integer;
Asm
  xor ecx, ecx
  cmp al, dl
  ja @@GT
  je @@EQ
@@LT:
  dec ecx
  dec ecx
@@GT:
  inc ecx
@@EQ:
  mov eax, ecx
End;
{--------}

Function FFCmpDW(Const a, b: TffWord32): Integer;
Asm
  xor ecx, ecx
  cmp eax, edx
  ja @@GT
  je @@EQ
@@LT:
  dec ecx
  dec ecx
@@GT:
  inc ecx
@@EQ:
  mov eax, ecx
End;
{--------}

Function FFCmpI(a, b: Integer): Integer;
Asm
  xor ecx, ecx
  cmp eax, edx
  jg @@GT
  je @@EQ
@@LT:
  dec ecx
  dec ecx
@@GT:
  inc ecx
@@EQ:
  mov eax, ecx
End;
{--------}

Function FFCmpI16(a, b: Smallint): Integer;
Asm
  xor ecx, ecx
  cmp ax, dx
  jg @@GT
  je @@EQ
@@LT:
  dec ecx
  dec ecx
@@GT:
  inc ecx
@@EQ:
  mov eax, ecx
End;
{--------}

Function FFCmpI8(a, b: Shortint): Integer;
Asm
  xor ecx, ecx
  cmp al, dl
  jg @@GT
  je @@EQ
@@LT:
  dec ecx
  dec ecx
@@GT:
  inc ecx
@@EQ:
  mov eax, ecx
End;
{--------}

Function FFCmpI32(a, b: Longint): Integer;
Asm
  xor ecx, ecx
  cmp eax, edx
  jg @@GT
  je @@EQ
@@LT:
  dec ecx
  dec ecx
@@GT:
  inc ecx
@@EQ:
  mov eax, ecx
End;
{--------}

Function FFCmpW(a, b: TffWord16): Integer;
Asm
  xor ecx, ecx
  cmp ax, dx
  ja @@GT
  je @@EQ
@@LT:
  dec ecx
  dec ecx
@@GT:
  inc ecx
@@EQ:
  mov eax, ecx
End;
{--------}

Function FFCmpBytes(Const a, b: PffByteArray; MaxLen: Integer): Integer;
Asm
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  xor eax, eax
  or ecx, ecx
  jz @@Equal
  repe cmpsb
  jb @@Exit
  je @@Equal
  inc eax
@@Equal:
  inc eax
@@Exit:
  dec eax
  pop edi
  pop esi
End;
{--------}

Function FFCmpShStr(Const a, b: TffShStr; MaxLen: Byte): Integer;
Asm
  push esi
  push edi
  mov esi, eax
  mov edi, edx
  movzx ecx, cl
  mov ch, cl
  xor eax, eax
  mov dl, [esi]
  inc esi
  mov dh, [edi]
  inc edi
  cmp cl, dl
  jbe @@Check2ndLength
  mov cl, dl
@@Check2ndLength:
  cmp cl, dh
  jbe @@CalcSigLengths
  mov cl, dh
@@CalcSigLengths:
  cmp dl, ch
  jbe @@Calc2ndSigLength
  mov dl, ch
@@Calc2ndSigLength:
  cmp dh, ch
  jbe @@CompareStrings
  mov dh, ch
@@CompareStrings:
  movzx ecx, cl
  or ecx, ecx
  jz @@CompareLengths
  repe cmpsb
  jb @@Exit
  ja @@GT
@@CompareLengths:
  cmp dl, dh
  je @@Equal
  jb @@Exit
@@GT:
  inc eax
@@Equal:
  inc eax
@@Exit:
  dec eax
  pop edi
  pop esi
End;
{--------}

Function FFCmpShStrUC(Const a, b: TffShStr; MaxLen: Byte): Integer;
Asm
  push esi
  push edi
  push ebx
  mov esi, eax
  mov edi, edx
  movzx ecx, cl
  mov ch, cl
  xor eax, eax
  mov dl, [esi]
  inc esi
  mov dh, [edi]
  inc edi
  cmp cl, dl
  jbe @@Check2ndLength
  mov cl, dl
@@Check2ndLength:
  cmp cl, dh
  jbe @@CalcSigLengths
  mov cl, dh
@@CalcSigLengths:
  cmp dl, ch
  jbe @@Calc2ndSigLength
  mov dl, ch
@@Calc2ndSigLength:
  cmp dh, ch
  jbe @@CompareStrings
  mov dh, ch
@@CompareStrings:
  movzx ecx, cl
  or ecx, ecx
  jz @@CompareLengths
@@NextChars:
  mov bl, [esi]
  cmp bl, 'a'
  jb @@OtherChar
  cmp bl, 'z'
  ja @@OtherChar
  sub bl, 'a'-'A'
@@OtherChar:
  mov bh, [edi]
  cmp bh, 'a'
  jb @@CompareChars
  cmp bh, 'z'
  ja @@CompareChars
  sub bh, 'a'-'A'
@@CompareChars:
  cmp bl, bh
  jb @@Exit
  ja @@GT
  inc esi
  inc edi
  dec ecx
  jnz @@NextChars
@@CompareLengths:
  cmp dl, dh
  je @@Equal
  jb @@Exit
@@GT:
  inc eax
@@Equal:
  inc eax
@@Exit:
  dec eax
  pop ebx
  pop edi
  pop esi
End;
{--------}

Procedure ffCloneI64(Var aDest: TffInt64; Const aSrc: TffInt64);
Begin
  aDest.iLow := aSrc.iLow;
  aDest.iHigh := aSrc.iHigh;
End;
{--------}

Procedure ffInitI64(Var I: TffInt64);
Begin
  I.iLow := 0;
  I.iHigh := 0;
End;
{--------}

Function FFCmpI64(Const a, b: TffInt64): Integer; {!!.06 - Rewritten}
Begin
  If (a.iHigh = b.iHigh) Then
    Result := FFCmpDW(a.iLow, b.iLow)
  Else
    Result := FFCmpDW(a.iHigh, b.iHigh);
End; {!!.06 - End rewritten}
{--------}

Procedure ffShiftI64L(Const I: TffInt64;
  Const Bits: Byte;
  Var Result: TffInt64);
Asm
  push ebx
  push edi
  mov ebx, [eax]
  mov edi, [eax+4]
  or dl, dl
  je @@Exit
@@LOOP:
  shl ebx, 1
  rcl edi, 1
  dec dl
  jnz @@LOOP
@@EXIT:
  mov [ecx], ebx
  mov [ecx+4], edi
  pop edi
  pop ebx
End;
{--------}

Procedure ffShiftI64R(Const I: TffInt64; Const Bits: Byte; Var Result: TffInt64);
Asm
  push ebx
  push edi
  mov ebx, [eax]
  mov edi, [eax+4]
  or dl, dl
  je @@Exit
@@LOOP:
  shr edi, 1
  rcr ebx, 1
//  rcr edi, 1
  dec dl
  jnz @@LOOP
@@EXIT:
  mov [ecx], ebx
  mov [ecx+4], edi
  pop edi
  pop ebx
End;
{--------}

Procedure ffI64MinusI64(Const a, b: TffInt64; Var Result: TffInt64);
Asm
  push ebx
  push edi
  mov ebx, eax
  mov edi, edx
  mov eax,[ebx]
  mov edx,[ebx+4]
  sub eax,[edi]
  sbb edx,[edi+4]
  mov [ecx], eax
  mov [ecx+4], edx
  pop edi
  pop ebx
End;
{--------}

Procedure ffI64MinusInt(Const aI64: TffInt64; Const aInt: TffWord32; Var Result: TffInt64);
Asm
  push edi
  mov edi, edx
  mov edx, [eax+4]
  mov eax, [eax]
  sub eax, edi
  sbb edx,  0
  mov [ecx], eax
  mov [ecx+4], edx
  pop edi
End;
{--------}

Function ffI64ModInt(Const aI64: TffInt64; Const aInt: TffWord32): Integer;
Var
  Quotient: TffInt64;
  QSum: TffInt64;
Begin
  Quotient.iLow := 0;
  Quotient.iHigh := 0;
  QSum.iLow := 0;
  QSum.iHigh := 0;
  {how many time will aInt go into aI64?}
  ffI64DivInt(aI64, aInt, Quotient);
  {multiply Quotient by aInt to see what it (QSum) equals}
  ffI64MultInt(Quotient, aInt, QSum);
  {mod equals (aI64 minus QSum)}
  ffI64MinusI64(aI64, QSum, QSum);

  Result := QSum.iLow;
End;
{--------}

Procedure ffI64DivInt(Const aI64: TffInt64; Const aInt: TffWord32; Var Result: TffInt64);
{This procedure was originally intended to divide a 64-bit word by a
 64-bit word.  Since we are now dividing a 64-bit word by a 32-bit word,
 we are forcing the divisor's high word to a 0.  This is an area for
 improvement}
Asm
  push ebp
  push ebx
  push esi
  push edi
  push ecx       {push ecx to the stack before we trash the address}

  mov  ebx, edx  //move divisor low word to ebx
  mov  ecx, 0        {we are forcing the divosor high word to zero because our divisor in only 4 bytes}
  mov  edx, [eax+4]    //move the dividend low word to edx
  mov  eax, [eax]


{if the low word of the dividend (i.e., aI64) is zero or
 the divisor low word is 0
 then we can do a quick division. }
  or   edx, edx
  jz   @ffI64DivInt_Quick
  or   ebx, ebx
  jz   @ffI64DivInt_Quick

{ Slow division starts here}
@ffI64DivInt_Slow:
  mov  ebp, ecx
  mov  ecx, 64
  xor  edi, edi
  xor  esi, esi

@ffI64DivInt_xLoop:
  shl  eax, 1
  rcl  edx, 1
  rcl  esi, 1
  rcl  edi, 1
  cmp  edi, ebp
  jb   @ffI64DivInt_NoSub
  ja   @ffI64DivInt_Subtract
  cmp  esi, ebx
  jb   @ffI64DivInt_NoSub

@ffI64DivInt_Subtract:
  sub  esi, ebx
  sbb  edi, ebp
  inc  eax

@ffI64DivInt_NoSub:
  loop @ffI64DivInt_xLoop
  jmp  @ffI64DivInt_Finish

{ Quick division starts here}
{ - either the dividend's low word or divisor low word is 0}
@ffI64DivInt_Quick:
  div  ebx
  xor  edx, edx

@ffI64DivInt_Finish:
// fill result, ecx = low word, ecx+4 = high word
  pop  ecx
  mov  [ecx].TffInt64.iHigh, edx
  mov  [ecx].TffInt64.iLow,  eax
  pop  edi
  pop  esi
  pop  ebx
  pop  ebp
End;
{--------}

Procedure ffI64MultInt(Const aI64: TffInt64; Const aInt: TffWord32; Var Result: TffInt64);
Asm
  push ebx
  push edi

  mov ebx, eax         // set [ebx] to aI64
  mov edi, edx         // set edi to aInt

  mov eax, [ebx+4]     // get top DWORD of aI64
  mul edi              // multiply by aInt
  push eax             // save bottom DWORD of result
  mov eax, [ebx]       // get bottom DWORD of aI64
  mul edi              // multiply by aInt

  pop ebx              // pop bottom part of upper result
  add edx, ebx         // add to top part of lower result

  mov [ecx], eax       // save result
  mov [ecx+4], edx

  pop edi
  pop ebx
End;
{--------}

Procedure ffI64AddInt(Const aI64: TffInt64; Const aInt: TffWord32; Var Result: TffInt64);
Asm
 push ebx
  mov ebx, [eax].TffInt64.iLow
  add ebx, edx
  mov [ecx].TffInt64.iLow, ebx
  mov ebx, [eax].TffInt64.iHigh
  adc ebx, 0
  mov [ecx].TffInt64.iHigh, ebx
  pop ebx
End;
{--------}

Function ffI64toInt(Const aI64: TffInt64): TffWord32;
Begin
  {What should we do if aI64 larger than DWord?
   - D5 doesn't do anything}
  Result := aI64.iLow;
End;
{--------}

Function ffI64ToStr(Const aI64: TffInt64): String;
Begin
  Result := IntToStr(aI64.iHigh) + IntToStr(aI64.iLow);
End;
{--------}

Procedure ffIntToI64(Const aInt: TffWord32; Var Result: TffInt64);
Begin
  Result.iLow := aInt;
  Result.iHigh := 0;
End;
{--------}

Function ffI64IsZero(Const aI64: TffInt64): boolean;
Begin
  Result := ((aI64.iHigh = 0) And (aI64.iLow = 0));
End;

Function ffI64IsNonZero(Const aI64: TffInt64): boolean;
Begin
  Result := ((aI64.iHigh <> 0) Or (aI64.iLow <> 0));
End;
{====================================================================}

{===Minimum/maximum routines=========================================}

Function FFMinDW(a, b: TffWord32): TffWord32;
Asm
  cmp eax, edx
  jbe @@Exit
  mov eax, edx
@@Exit:
End;
{--------}

Function FFMaxDW(a, b: TffWord32): TffWord32;
Asm
  cmp eax, edx
  jae @@Exit
  mov eax, edx
@@Exit:
End;
{--------}

Function FFMinI(a, b: Integer): Integer;
Asm
  cmp eax, edx
  jle @@Exit
  mov eax, edx
@@Exit:
End;
{--------}

Function FFMaxI(a, b: Integer): Integer;
Asm
  cmp eax, edx
  jge @@Exit
  mov eax, edx
@@Exit:
End;
{--------}

Function FFMinL(a, b: Longint): Longint;
Asm
  cmp eax, edx
  jle @@Exit
  mov eax, edx
@@Exit:
End;
{--------}

Function FFMaxL(a, b: Longint): Longint;
Asm
  cmp eax, edx
  jge @@Exit
  mov eax, edx
@@Exit:
End;
{--------}

Function FFMinI64(a, b: TffInt64): TffInt64;
Begin
  If FFCmpI64(a, b) <= 0 Then
    Result := a
  Else
    Result := b;
End;
{--------}

Function FFMaxI64(a, b: TffInt64): TffInt64;
Begin
  If FFCmpI64(a, b) >= 0 Then
    Result := a
  Else
    Result := b;
End;
{====================================================================}

{====================================================================}

Function FFCheckDescend(aAscend: boolean; a: Integer): Integer;
  Register;
Asm
  or al, al
  jnz @@Exit
  neg edx
@@Exit:
  mov eax, edx
End;
{--------}

Function FFForceInRange(a, aLow, aHigh: Longint): Longint;
  Register;
Asm
  cmp eax, edx
  jg @@CheckHigh
  mov eax, edx
  jmp @@Exit
@@CheckHigh:
  cmp eax, ecx
  jl @@Exit
  mov eax, ecx
@@Exit:
End;
{--------}

Function FFForceNonZero(a, b: Integer): Integer;
  Register;
Asm
  or eax, eax
  jnz @@Exit
  mov eax, edx
@@Exit:
End;
{====================================================================}

{===Memory allocation, etc===========================================}
Var

  FFMemPools: Array[0..91] Of TfsMemoryPool;
  { Array of memory pools used to replace Delphi's heap manager.
    Pools 0..31 handle object sizes in 32-byte increments.
    For example:
      Pool[0] is used to allocate objects <= 32 bytes in size
      Pool[1] for objects between 33 and 64 bytes in size
      on up to Pool[31] for objects between 993 and 1024 bytes in size.
      The maximum size handled by Pools 0..31 can be calculated as
        succ[<pool index>] * 32

    Pools 32..91 handle object sizes in 256-byte increments after the
      1024 byte boundary.
    For example:
      Pool[32] for objects between 1025 and 1280 bytes in size
      Pool[33] for objects between 1281 and 1536 bytes in size
      on up to Pool[91] for objects between 16129 and 16384 bytes in size
      The maximum size handled by Pools 32..91 can be calculated as
        1024 + (<pool index> - 31 * 256) }
{--------}

Function CalcPoolIndex(Size: TffMemSize): Integer;
Begin
  If (Size <= 1024) Then
    Result := (Size - 1) Div 32 {ie, 0..31}
  Else
    Result := ((Size - 1) Div 256) - 4 + 32; {ie, 32..91}
End;
{--------}

Procedure FFFreeMem(Var P; Size: TffMemSize);
{$IFNDEF MemCheck}
Var
  Pt: pointer;
  Inx: Integer;
  {$ENDIF}
Begin
  {$IFDEF MemCheck}
  FreeMem(pointer(P), Size);
  {$ELSE}
  Pt := pointer(P);
  If (Pt <> Nil) Then
    Begin
      If (Size <= 16 * 1024) Then
        Begin
          Inx := CalcPoolIndex(Size);
          FFMemPools[Inx].Dispose(Pt);
        End
      Else
        FreeMem(Pt, Size);
    End;
  {$ENDIF}
End;
{--------}

Procedure FFGetMem(Var P; Size: TffMemSize);
{$IFNDEF MemCheck}
Var
  Pt: pointer Absolute P;
  Inx: Integer;
  {$ENDIF}
Begin
  {$IFDEF MemCheck}
  GetMem(pointer(P), Size);
  {$ELSE}
  If (Size <= 16 * 1024) Then
    Begin
      Inx := CalcPoolIndex(Size);
      Pt := FFMemPools[Inx].Alloc;
    End
  Else
    GetMem(Pt, Size);
  {$ENDIF}
End;
{--------}

Procedure FFGetZeroMem(Var P; Size: TffMemSize);
Var
  Pt: pointer Absolute P;
Begin
  FFGetMem(Pt, Size);
  FillChar(Pt^, Size, 0);
End;
{--------}

Procedure FFReallocMem(Var P; OldSize, NewSize: Integer);
{$IFNDEF MemCheck}
Var
  Pt: Pointer Absolute P;
  P2: Pointer;
  OldInx, NewInx: Integer;
  {$ENDIF}
Begin
  {$IFDEF MemCheck}
  ReallocMem(pointer(P), NewSize);
  {$ELSE}
  If Pt = Nil Then
    FFGetMem(P, NewSize)
  Else If NewSize = 0 Then
    Begin
      FFFreeMem(P, OldSize);
      Pt := Nil;
    End
  Else If (OldSize > 16 * 1024) And (NewSize > 16 * 1024) Then
    ReAllocMem(Pt, NewSize)
  Else
    Begin
      OldInx := CalcPoolIndex(OldSize);
      NewInx := CalcPoolIndex(NewSize);
      If OldInx <> NewInx Then
        Begin
          If NewInx <= 91 Then
            P2 := FFMemPools[NewInx].Alloc
          Else
            GetMem(P2, NewSize);
          If NewSize < OldSize Then {!!.02}
            Move(Pt^, P2^, NewSize) {!!.02}
          Else {!!.02}
            Move(Pt^, P2^, OldSize); {!!.02}
          If OldInx <= 91 Then
            FFMemPools[OldInx].Dispose(Pt)
          Else
            FreeMem(Pt);
          Pointer(P) := P2;
        End;
    End;
  {$ENDIF}
End;
{Begin !!.01}
{--------}

Procedure FFFlushMemPools;
Var
  anInx: Integer;
Begin
  For anInx := 0 To 91 Do
    FFMemPools[anInx].RemoveUnusedBlocks;
End;
{End !!.01}

{===String routines==================================================}
Const
  EmptyShStr: Array[0..1] Of AnsiChar = #0#0;

  {--------}

Function FsCommaizeChL(L: Longint; Ch: AnsiChar): AnsiString;
{-Convert a long integer to a string with Ch in comma positions}
Var
  Temp: String;
  NumCommas, I, Len: Cardinal;
  Neg: Boolean;
Begin
  SetLength(Temp, 1);
  Temp[1] := Ch;
  If L < 0 Then
    Begin
      Neg := True;
      L := Abs(L);
    End
  Else
    Neg := False;
  Result := IntToStr(L);
  Len := Length(Result);
  NumCommas := (Pred(Len)) Div 3;
  For I := 1 To NumCommas Do
    System.Insert(Temp, Result, Succ(Len - (I * 3)));
  If Neg Then
    System.Insert('-', Result, 1);
End;
{--------}

Procedure FFShStrConcat(Var Dest: TffShStr; Const Src: TffShStr);
Begin
  Move(Src[1], Dest[succ(length(Dest))], length(Src));
  inc(Dest[0], length(Src));
End;
{--------}

Procedure FFShStrAddChar(Var Dest: TffShStr; C: AnsiChar);
Begin
  inc(Dest[0]);
  Dest[length(Dest)] := C;
End;
{--------}

Function FFShStrAlloc(Const S: TffShStr): PffShStr;
Begin
  If (S = '') Then
    Result := PffShStr(@EmptyShStr)
  Else
    Begin
      {save room for length byte and terminating #0}
      FFGetMem(Result, length(S) + 2);
      Result^ := S;
      Result^[succ(length(S))] := #0;
    End;
End;
{--------}

Procedure FFShStrFree(Var P: PffShStr);
Begin
  If (P <> Nil) And (P <> PffShStr(@EmptyShStr)) Then
    FFFreeMem(P, length(P^) + 2);
  P := Nil;
End;
{--------}

Procedure FFShStrSplit(S: TffShStr; Const SplitChars: TffShStr;
  Var Left, Right: TffShStr);
{-This procedure locates the first occurrence in S of any of the
  characters listed in SplitChars and returns the substring to the
  left of the split char (exclusive) in Left and the substring to the
  right of the split char (exclusive) in Right.  If none of the chars
  given in SplitChar exist in S, then Left = S and Right = ''. }
Var
  I: Integer;
Begin
  Left := S;
  Right := '';
  For I := 1 To Length(S) Do
    Begin
      If Pos(SplitChars, Copy(S, I, 1)) <> 0 Then
        Begin
          Left := Copy(S, 1, I - 1);
          Right := Copy(S, I + 1, 255);
          Break;
        End;
    End;
End;
{--------}

Function StrStDeletePrim(P: PAnsiChar; Pos, Count: Cardinal): PAnsiChar; Register;
Asm
  push   eax             {save because we will be changing them}
  push   edi
  push   esi
  push   ebx

  mov    ebx, ecx        {move Count to BX}
  mov    esi, eax        {move P to ESI and EDI}
  mov    edi, eax

  xor    eax, eax        {null}
  or     ecx, -1
  cld
  repne  scasb           {find null terminator}
  not    ecx             {calc length}
  jecxz  @@ExitPoint

  sub    ecx, ebx        {subtract Count}
  sub    ecx, edx        {subtract Pos}
  jns    @@L1

  mov    edi,esi         {delete everything after Pos}
  add    edi,edx
  stosb
  jmp    @@ExitPoint

@@L1:
  mov    edi,esi
  add    edi,edx         {point to position to adjust}
  mov    esi,edi
  add    esi,ebx         {point past string to delete in src}
  inc    ecx             {one more to include null terminator}
  rep    movsb           {adjust the string}

@@ExitPoint:

  pop    ebx            {restore registers}
  pop    esi
  pop    edi
  pop    eax
End;
{--------}

Procedure FFStrTrim(P: PAnsiChar);
{-Trim leading and trailing blanks from P}
Var
  I: Integer;
  PT: PAnsiChar;
Begin
  I := StrLen(P);
  If I = 0 Then
    Exit;

  {delete trailing spaces}
  Dec(I);
  While (I >= 0) And (P[I] = ' ') Do
    Begin
      P[I] := #0;
      Dec(I);
    End;

  {delete leading spaces}
  I := 0;
  PT := P;
  While PT^ = ' ' Do
    Begin
      Inc(I);
      Inc(PT);
    End;
  If I > 0 Then
    StrStDeletePrim(P, 0, I);
End;

Function FFStrTrimR(S: PAnsiChar): PAnsiChar; Register;
Asm
   cld
   push   edi
   mov    edx, eax
   mov    edi, eax

   or     ecx, -1
   xor    al, al
   repne  scasb
   not    ecx
   dec    ecx
   jecxz  @@ExitPoint

   dec    edi

@@1:
   dec    edi
   cmp    byte ptr [edi],' '
   jbe    @@1
   mov    byte ptr [edi+1],00h
@@ExitPoint:
   mov    eax, edx
   pop    edi
End;
{--------}

Function FFShStrTrim(Const S: TffShStr): TffShStr;
Var
  StartCh: Integer;
  EndCh: Integer;
  LenS: Integer;
Begin
  LenS := length(S);
  StartCh := 1;
  While (StartCh <= LenS) And (S[StartCh] = ' ') Do
    inc(StartCh);
  If (StartCh > LenS) Then
    Result := ''
  Else
    Begin
      EndCh := LenS;
      While (EndCh > 0) And (S[EndCh] = ' ') Do
        dec(EndCh);
      Result := Copy(S, StartCh, succ(EndCh - StartCh));
    End;
End;
{--------}

Function FFShStrTrimL(Const S: TffShStr): TffShStr;
Var
  StartCh: Integer;
  LenS: Integer;
Begin
  LenS := length(S);
  StartCh := 1;
  While (StartCh <= LenS) And (S[StartCh] = ' ') Do
    inc(StartCh);
  If (StartCh > LenS) Then
    Result := ''
  Else
    Result := Copy(S, StartCh, succ(LenS - StartCh));
End;
{--------}

Function FFShStrTrimR(Const S: TffShStr): TffShStr;
Begin
  Result := S;
  While (length(Result) > 0) And (Result[length(Result)] = ' ') Do
    dec(Result[0]);
End;
{--------}

Function FFShStrTrimWhite(Const S: TffShStr): TffShStr;
Var
  StartCh: Integer;
  EndCh: Integer;
  LenS: Integer;
Begin
  LenS := length(S);
  StartCh := 1;
  While (StartCh <= LenS) And (S[StartCh] <= ' ') Do
    inc(StartCh);
  If (StartCh > LenS) Then
    Result := ''
  Else
    Begin
      EndCh := LenS;
      While (EndCh > 0) And (S[EndCh] <= ' ') Do
        dec(EndCh);
      Result := Copy(S, StartCh, succ(EndCh - StartCh));
    End;
End;
{--------}

Function FFShStrTrimWhiteL(Const S: TffShStr): TffShStr;
Var
  StartCh: Integer;
  LenS: Integer;
Begin
  LenS := length(S);
  StartCh := 1;
  While (StartCh <= LenS) And (S[StartCh] <= ' ') Do
    inc(StartCh);
  If (StartCh > LenS) Then
    Result := ''
  Else
    Result := Copy(S, StartCh, succ(LenS - StartCh));
End;
{--------}

Function FFShStrTrimWhiteR(Const S: TffShStr): TffShStr;
Begin
  Result := S;
  While (length(Result) > 0) And (Result[length(Result)] <= ' ') Do
    dec(Result[0]);
End;
{--------}

Function FFShStrRepChar(C: AnsiChar; N: Integer): TffShStr;
Var
  i: Integer;
Begin
  If (N < 0) Then
    N := 0
  Else If (N > 255) Then
    N := 255;
  Result[0] := AnsiChar(N);
  For i := 1 To N Do
    Result[i] := C;
End;
{--------}

Function FFShStrUpper(Const S: TffShStr): TffShStr;
Var
  i: Integer;
Begin
  Result[0] := S[0];
  For i := 1 To length(S) Do
    Result[i] := upcase(S[i]);
End;
{--------}

Function FFShStrUpperAnsi(Const S: TffShStr): TffShStr;
Begin
  Result := S;
  CharUpperBuff(@Result[1], length(Result));
End;
{--------}

Function FFStrAlloc(aSize: Integer): PAnsiChar;
Begin
  inc(aSize, sizeof(Longint));
  FFGetMem(Result, aSize);
  PLongInt(Result)^ := aSize;
  inc(Result, sizeof(Longint));
  Result[0] := #0;
End;
{--------}

Function FFStrAllocCopy(S: PAnsiChar): PAnsiChar;
Var
  Len: Integer;
  Size: Longint;
Begin
  Len := StrLen(S);
  If (Len = 0) Then
    Result := Nil
  Else
    Begin
      Size := succ(Len) + sizeof(Longint);
      FFGetMem(Result, Size);
      PLongInt(Result)^ := Size;
      inc(Result, sizeof(Longint));
      StrCopy(Result, S);
    End;
End;
{--------}

Procedure FFStrDispose(S: PAnsiChar);
Begin
  If (S <> Nil) Then
    Begin
      dec(S, sizeof(Longint));
      FFFreeMem(S, PLongint(S)^);
    End;
End;
{--------}

Function FFStrNew(Const S: TffShStr): PAnsiChar;
Var
  Len: Integer;
  Size: Longint;
Begin
  Len := length(S);
  If (Len = 0) Then
    Result := Nil
  Else
    Begin
      Size := succ(Len) + sizeof(Longint);
      FFGetMem(Result, Size);
      PLongInt(Result)^ := Size;
      inc(Result, sizeof(Longint));
      Move(S[1], Result^, Len);
      Result[Len] := #0;
    End;
End;
{--------}

Function FFStrPas(S: PAnsiChar): TffShStr;
Var
  Len: Integer;
Begin
  If (S = Nil) Then
    Result := ''
  Else
    Begin
      Len := FFMinI(StrLen(S), 255);
      Move(S[0], Result[1], Len);
      Result[0] := AnsiChar(Len);
    End;
End;
{--------}

Function FFStrPasLimit(S: PAnsiChar; MaxCharCount: Integer): TffShStr;
Var
  Len: Integer;
Begin
  Len := FFMinI(StrLen(S), MaxCharCount);
  Move(S[0], Result[1], Len);
  Result[0] := AnsiChar(Len);
End;
{--------}

Function FFStrPCopy(Dest: PAnsiChar; Const S: TffShStr): PAnsiChar;
Begin
  Result := Dest;
  If (Dest <> Nil) Then
    Begin
      Move(S[1], Dest[0], length(S));
      Dest[length(S)] := #0;
    End;
End;
{--------}

Function FFStrPCopyLimit(Dest: PAnsiChar; Const S: TffShStr;
  MaxCharCount: Integer): PAnsiChar;
Var
  Len: Integer;
Begin
  Result := Dest;
  If (Dest <> Nil) Then
    Begin
      Len := FFMinI(MaxCharCount, length(S));
      Move(S[1], Dest[0], Len);
      Dest[Len] := #0;
    End;
End;
{--------}

Function FFTrim(Const S: String): String;
Var
  StartCh: Integer;
  EndCh: Integer;
  LenS: Integer;
Begin
  LenS := length(S);
  StartCh := 1;
  While (StartCh <= LenS) And (S[StartCh] = ' ') Do
    inc(StartCh);
  If (StartCh > LenS) Then
    Result := ''
  Else
    Begin
      EndCh := LenS;
      While (EndCh > 0) And (S[EndCh] = ' ') Do
        dec(EndCh);
      Result := Copy(S, StartCh, succ(EndCh - StartCh));
    End;
End;
{--------}

Function FFTrimL(Const S: String): String;
Var
  StartCh: Integer;
  LenS: Integer;
Begin
  LenS := length(S);
  StartCh := 1;
  While (StartCh <= LenS) And (S[StartCh] = ' ') Do
    inc(StartCh);
  If (StartCh > LenS) Then
    Result := ''
  Else
    Result := Copy(S, StartCh, succ(LenS - StartCh));
End;
{--------}

Function FFTrimR(Const S: String): String;
Var
  EndCh: Integer;
Begin
  EndCh := length(S);
  While (EndCh > 0) And (S[EndCh] = ' ') Do
    dec(EndCh);
  If (EndCh > 0) Then
    Result := Copy(S, 1, EndCh)
  Else
    Result := '';
End;
{--------}

Function FFTrimWhite(Const S: String): String;
Var
  StartCh: Integer;
  EndCh: Integer;
  LenS: Integer;
Begin
  LenS := length(S);
  StartCh := 1;
  While (StartCh <= LenS) And (S[StartCh] <= ' ') Do
    inc(StartCh);
  If (StartCh > LenS) Then
    Result := ''
  Else
    Begin
      EndCh := LenS;
      While (EndCh > 0) And (S[EndCh] <= ' ') Do
        dec(EndCh);
      Result := Copy(S, StartCh, succ(EndCh - StartCh));
    End;
End;
{--------}

Function FFTrimWhiteL(Const S: String): String;
Var
  StartCh: Integer;
  LenS: Integer;
Begin
  LenS := length(S);
  StartCh := 1;
  While (StartCh <= LenS) And (S[StartCh] <= ' ') Do
    inc(StartCh);
  If (StartCh > LenS) Then
    Result := ''
  Else
    Result := Copy(S, StartCh, succ(LenS - StartCh));
End;
{--------}

Function FFTrimWhiteR(Const S: String): String;
Var
  EndCh: Integer;
Begin
  EndCh := length(S);
  While (EndCh > 0) And (S[EndCh] <= ' ') Do
    dec(EndCh);
  If (EndCh > 0) Then
    Result := Copy(S, 1, EndCh)
  Else
    Result := '';
End;
{--------}

Function FFOmitMisc(Const S: String): String;
Var
  CurCh: Integer;
  LenS: Integer;
Begin
  Result := '';
  LenS := length(S);
  CurCh := 1;
  While (CurCh <= LenS) Do
    Begin
      If S[CurCh] In ['0'..'9', 'A'..'Z', 'a'..'z'] Then
        Result := Result + S[CurCh];
      inc(CurCh);
    End;
End;
{--------}

Function FFAnsiCompareText(Const S1, S2: String): Integer; {!!.10}
Begin
  {$IFDEF SafeAnsiCompare}
  Result := AnsiCompareText(AnsiLowerCase(S1), AnsiLowerCase(S2));
  {$ELSE}
  Result := AnsiCompareText(S1, S2);
  {$ENDIF}
End;
{--------}

Function FFAnsiStrIComp(S1, S2: PChar): Integer; {!!.10}
Begin
  {$IFDEF SafeAnsiCompare}
  Result := AnsiStrIComp(AnsiStrLower(S1), AnsiStrLower(S2));
  {$ELSE}
  Result := AnsiStrIComp(S1, S2);
  {$ENDIF}
End;
{--------}

Function FFAnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer; {!!.10}
Begin
  {$IFDEF SafeAnsiCompare}
  Result := AnsiStrLIComp(AnsiStrLower(S1), AnsiStrLower(S2), MaxLen);
  {$ELSE}
  Result := AnsiStrLIComp(S1, S2, MaxLen);
  {$ENDIF}
End;
{====================================================================}

{===Wide-String Routines=============================================}

Function FFCharToWideChar(Ch: AnsiChar): WideChar;
Begin
  Result := WideChar(Ord(Ch));
End;

Function FFWideCharToChar(WC: WideChar): AnsiChar;
Begin
  If WC >= #256 Then
    WC := #0;
  Result := AnsiChar(Ord(WC));
End;

Function FFShStrLToWideStr(S: TffShStr; WS: PWideChar; MaxLen: Longint): PWideChar;
Begin
  WS[MultiByteToWideChar(0, 0, @S[1], MaxLen, WS, MaxLen + 1)] := #0;
  Result := WS;
End;

Function FFWideStrLToShStr(WS: PWideChar; MaxLen: Longint): TffShStr;
Begin
  Result := WideCharLenToString(WS, MaxLen);
End;

Function FFNullStrLToWideStr(ZStr: PAnsiChar; WS: PWideChar; MaxLen: Longint): PWideChar;
Begin
  WS[MultiByteToWideChar(0, 0, ZStr, MaxLen, WS, MaxLen)] := #0;
  Result := WS;
End;

Function FFWideStrLToNullStr(WS: PWideChar; ZStr: PAnsiChar; MaxLen: Longint): PAnsiChar;
Begin
  ZStr[WideCharToMultiByte(0, 0, WS, MaxLen, ZStr, MaxLen, Nil, Nil)] := #0;
  Result := ZStr;
End;

Function FFWideStrLToWideStr(aSourceValue, aTargetValue: PWideChar; MaxLength: Longint): PWideChar;
Begin
  { Assumption: MaxLength is really # units multiplied by 2, which is how
    a Wide String's length is stored in the table's data dictionary. }
  Move(aSourceValue^, aTargetValue^, MaxLength);
  aTargetValue[MaxLength Div 2] := #0;
  Result := aTargetValue;
End;
{=============
=======================================================}

{===File and Path name routines======================================}
{===Helpers===}
Const
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_PLATFORM OFF}
  {$ENDIF}
  faNotNormal = faReadOnly Or faHidden Or faSysFile Or faArchive;
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}
  {--------}

Procedure SearchRecConvertPrim(Var SR: TffSearchRec);
Type
  LH = Packed Record L, H: Word;
  End;
Var
  LocalFileTime: TFileTime;
Begin
  With SR Do
    Begin
      srName := FFStrPasLimit(srData.cFileName, pred(sizeof(srName)));
      FileTimeToLocalFileTime(srData.ftLastWriteTime, LocalFileTime);
      FileTimeToDosDateTime(LocalFileTime, LH(srTime).H, LH(srTime).L);
      srSize := srData.nFileSizeLow;
      srSizeHigh := srData.nFileSizeHigh;
      If ((srData.dwFileAttributes And faDirectory) <> 0) Then
        srType := ditDirectory
          {$IFDEF DCC6OrLater}
        {$WARN SYMBOL_DEPRECATED OFF}
        {$WARN SYMBOL_PLATFORM   OFF}
        {$ENDIF}
      Else If ((srData.dwFileAttributes And faVolumeID) <> 0) Then
        {$IFDEF DCC6OrLater}
        {$WARN SYMBOL_PLATFORM   ON}
        {$WARN SYMBOL_DEPRECATED ON}
        {$ENDIF}
        srType := ditVolumeID
      Else
        srType := ditFile;
      srAttr := [];
      {$IFDEF DCC6OrLater}
      {$WARN SYMBOL_PLATFORM OFF}
      {$ENDIF}
      If ((srData.dwFileAttributes And faHidden) <> 0) Then
        include(srAttr, diaHidden);
      If ((srData.dwFileAttributes And faReadOnly) <> 0) Then
        include(srAttr, diaReadOnly);
      If ((srData.dwFileAttributes And faSysFile) <> 0) Then
        include(srAttr, diaSystem);
      If ((srData.dwFileAttributes And faArchive) <> 0) Then
        include(srAttr, diaArchive);
      If ((srData.dwFileAttributes And faNotNormal) = 0) Then
        include(srAttr, diaNormal);
      {$IFDEF DCC6OrLater}
      {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}
    End;
End;
{--------}

Function TypeAndAttrMatch(OSAttr: TffWord32;
  aType: TffDirItemTypeSet;
  aAttr: TffDirItemAttrSet): boolean;
Begin
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_PLATFORM   OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$ENDIF}
  Result := ((ditFile In aType) And ((OSAttr And (faDirectory Or faVolumeID)) = 0)) Or
    ((ditDirectory In aType) And ((OSAttr And faDirectory) <> 0)) Or
    ((ditVolumeID In aType) And ((OSAttr And faVolumeID) <> 0));
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_DEPRECATED ON}
  {$WARN SYMBOL_PLATFORM   ON}
  {$ENDIF}

  If Not Result Then
    Exit;
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_PLATFORM OFF}
  {$ENDIF}
  Result := ((diaReadOnly In aAttr) And ((OSAttr And faReadOnly) <> 0)) Or
    ((diaHidden In aAttr) And ((OSAttr And faHidden) <> 0)) Or
    ((diaSystem In aAttr) And ((OSAttr And faSysFile) <> 0)) Or
    ((diaArchive In aAttr) And ((OSAttr And faArchive) <> 0)) Or
    ((diaNormal In aAttr) And ((OSAttr And faNotNormal) = 0));
  {$IFDEF DCC6OrLater}
  {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}

End;
{--------}

Procedure ExtractHelper(Const PFN: TffFullFileName;
  Var DotPos: Integer;
  Var SlashPos: Integer);
Var
  i: Integer;
Begin
  {Note: if there is no period, DotPos is returned as one greater than
         the length of the full file name. If there is no slash
         SlashPos is returned as zero}
  DotPos := 0;
  SlashPos := 0;
  i := length(PFN);
  While (i > 0) And ((DotPos = 0) Or (SlashPos = 0)) Do
    Begin
      If (PFN[i] = '.') Then
        Begin
          If (DotPos = 0) Then
            DotPos := i;
        End
      Else If (PFN[i] = '\') Then
        Begin
          SlashPos := i;
          If (DotPos = 0) Then
            DotPos := succ(length(PFN));
        End;
      dec(i);
    End;
  If (DotPos = 0) Then
    DotPos := succ(length(PFN));
End;
{--------}

Function ValidFileNameHelper(Const S: TffShStr; MaxLen: Integer): boolean;
Const
  UnacceptableChars: Set Of AnsiChar =
  ['"', '*', '.', '/', ':', '<', '>', '?', '\', '|'];
Var
  i: Integer;
  LenS: Integer;
Begin
  Result := False;
  LenS := length(S);
  If (0 < LenS) And (LenS <= MaxLen) Then
    Begin
      For i := 1 To LenS Do
        If (S[i] In UnacceptableChars) Then
          Exit;
      Result := True;
    End;
End;
{===end Helpers===}

Function FFDirectoryExists(Const Path: TffPath): boolean;
Var
  Attr: TffWord32;
  PathZ: TffStringZ;
Begin
  Result := False;
  {we don't support wildcards}
  If (Pos('*', Path) <> 0) Or (Pos('?', Path) <> 0) Then
    Exit;
  Attr := GetFileAttributes(FFStrPCopy(PathZ, Path));
  If (Attr <> TffWord32(-1)) And ((Attr And FILE_ATTRIBUTE_DIRECTORY) <> 0) Then
    Result := True;
End;
{--------}

Function FFExpandFileName(Const FN: TffFullFileName): TffFullFileName;
Var
  FNZ: TffMaxPathZ;
  EFNZ: TffMaxPathZ;
  FileNamePos: PAnsiChar;
Begin
  GetFullPathName(FFStrPCopy(FNZ, FN), sizeof(EFNZ), EFNZ, FileNamePos);
  Result := FFStrPasLimit(EFNZ, pred(sizeof(TffFullFileName)));
End;
{--------}

Function FFExtractExtension(Const PFN: TffFullFileName): TffExtension;
Var
  DotPos: Integer;
  SlashPos: Integer;
Begin
  ExtractHelper(PFN, DotPos, SlashPos);
  If (DotPos >= length(PFN)) Then
    Result := ''
  Else
    Result := Copy(PFN, succ(DotPos), (length(PFN) - DotPos));
End;
{--------}

Function FFExtractFileName(Const PFN: TffFullFileName): TffFileName;
Var
  DotPos: Integer;
  SlashPos: Integer;
Begin
  ExtractHelper(PFN, DotPos, SlashPos);
  Result := Copy(PFN, succ(SlashPos), FFMinI(pred(DotPos - SlashPos), fscl_FileName));
End;
{--------}

Function FFExtractPath(Const PFN: TffFullFileName): TffPath;
Var
  DotPos: Integer;
  SlashPos: Integer;
Begin
  ExtractHelper(PFN, DotPos, SlashPos);
  If (SlashPos = 0) Then
    Result := ''
  Else
    Result := Copy(PFN, 1, FFMinI(pred(SlashPos), fscl_Path));
End;
{--------}

Function FFExtractTableName(Const PFN: TffFullFileName): TfsTableName;

Var
  DotPos: Integer;
  SlashPos: Integer;
Begin
  ExtractHelper(PFN, DotPos, SlashPos);
  Result := Copy(PFN, succ(SlashPos), FFMinI(pred(DotPos - SlashPos), fscl_TableNameSize));
End;
{--------}

Function FFFileExists(Const PFN: TffFullFileName): boolean;
Var
  SR: TffSearchRec;
Begin
  If (Pos('*', PFN) <> 0) Or (Pos('?', PFN) <> 0) Then
    Result := False
  Else If (FFFindFirst(PFN, [ditFile], diaAnyAttr, SR) = 0) Then
    Begin
      Result := True;
      FFFindClose(SR);
    End
  Else
    Result := False;
End;
{--------}

Procedure FFFindClose(Var SR: TffSearchRec);
Begin
  If (SR.srHandle <> INVALID_HANDLE_VALUE) Then
    Begin
      Windows.FindClose(SR.srHandle);
      SR.srHandle := INVALID_HANDLE_VALUE;
    End;
End;
{--------}

Function FFFindFirst(Const PFN: TffFullFileName;
  ItemType: TffDirItemTypeSet;
  Attr: TffDirItemAttrSet;
  Var SR: TffSearchRec): Integer;
Var
  PathZ: TffStringZ;
  GotAnError: boolean;
Begin
  FillChar(SR, sizeof(SR), 0);
  SR.srFindType := ItemType;
  SR.srFindAttr := Attr;
  SR.srHandle := Windows.FindFirstFile(FFStrPCopy(PathZ, PFN), SR.srData);
  If (SR.srHandle = INVALID_HANDLE_VALUE) Then
    Result := GetLastError
  Else
    Begin
      GotAnError := False;
      While (Not GotAnError) And
        (Not TypeAndAttrMatch(SR.srData.dwFileAttributes, SR.srFindType, SR.srFindAttr)) Do
        If Not Windows.FindNextFile(SR.srHandle, SR.srData) Then
          GotAnError := True;
      If GotAnError Then
        Begin
          Windows.FindClose(SR.srHandle);
          Result := GetLastError;
        End
      Else
        Begin
          Result := 0;
          SearchRecConvertPrim(SR);
        End;
    End;
End;
{--------}

Function FFFindNext(Var SR: TffSearchRec): Integer;
Var
  GotAnError: boolean;
Begin
  If Windows.FindNextFile(SR.srHandle, SR.srData) Then
    Begin
      GotAnError := False;
      While (Not GotAnError) And
        (Not TypeAndAttrMatch(SR.srData.dwFileAttributes, SR.srFindType, SR.srFindAttr)) Do
        If Not Windows.FindNextFile(SR.srHandle, SR.srData) Then
          GotAnError := True;
      If GotAnError Then
        Begin
          Result := GetLastError;
        End
      Else
        Begin
          Result := 0;
          SearchRecConvertPrim(SR);
        End;
    End
  Else
    Result := GetLastError;
End;
{--------}

Function FFForceExtension(Const PFN: TffFullFileName;
  Const Ext: TffExtension): TffFullFileName;
Var
  DotPos: Integer;
Begin
  Result := PFN;
  If FFHasExtension(PFN, DotPos) Then
    If (Ext = '') Then
      SetLength(Result, pred(DotPos))
    Else
      Begin
        SetLength(Result, DotPos + length(Ext));
        Move(Ext[1], Result[succ(DotPos)], length(Ext));
      End
  Else If (PFN <> '') And (Ext <> '') Then
    Begin
      FFShStrAddChar(Result, '.');
      FFShStrConcat(Result, Ext);
    End;
End;
{--------}

Function FFGetCurDir: TffPath;
Var
  CurDirZ: TffMaxPathZ;
  Len: Integer;
Begin
  Len := GetCurrentDirectory(sizeof(CurDirZ), CurDirZ);
  If (Len = 0) Then
    Result := ''
  Else
    Result := FFStrPasLimit(CurDirZ, 255);
End;
{--------}

Function FFGetDirList(Const Path: TffPath; FileSpec: TffFileNameExt): TFSSpecStringList;
Var
  FullSearchPath: TffFullFileName;
  ErrorCode: Integer;
  SR: TffSearchRec;
Begin
  Result := TFSSpecStringList.Create;
  Try
    Result.Capacity := 32; {to avoid too many reallocs}
    Result.CaseSensitive := False;
    FullSearchPath := FFMakeFullFileName(Path, FileSpec);
    ErrorCode := FFFindFirst(FullSearchPath, [ditFile], diaAnyAttr, SR);
    While (ErrorCode = 0) Do
      Begin
        Result.Insert(SR.srName);
        ErrorCode := FFFindNext(SR);
      End;
    FFFindClose(SR);
  Except
    Result.Free;
    Raise;
  End;
End;
{--------}

Function FFGetEXEName: TffFullFileName;
Begin
  Result := FFExpandFileName(ParamStr(0));
End;
{--------}

Function FFHasExtension(Const PFN: TffFullFileName; Var DotPos: Integer): boolean;
Var
  i: Integer;
Begin
  Result := False;
  DotPos := 0;
  For i := length(PFN) Downto 1 Do
    If (PFN[i] = '.') Then
      Begin
        DotPos := i;
        Result := True;
        Exit;
      End
    Else If (PFN[i] = '\') Then
      Exit;
End;
{--------}

Function FFMakeFileNameExt(Const FileName: TffFileName;
  Const Ext: TffExtension): TffFileNameExt;
Begin
  Result := FileName;
  FFShStrAddChar(Result, '.');
  FFShStrConcat(Result, Ext);
End;
{--------}

Function FFMakeFullFileName(Const Path: TffPath;
  Const FileName: TffFileNameExt): TffFullFileName;
Begin
  Result := Path;
  If (Result[length(Result)] <> '\') Then
    FFShStrAddChar(Result, '\');
  FFShStrConcat(Result, FileName);
End;
{--------}

Function FFSetCurDir(Path: TffPath): boolean;
Var
  DirZ: TffMaxPathZ;
Begin
  Result := SetCurrentDirectory(FFStrPCopy(DirZ, Path));
End;
{====================================================================}

{===Bitset routines==================================================}

Procedure FFClearAllBits(BitSet: PffByteArray; BitCount: Integer);
Begin
  FillChar(BitSet^, (BitCount + 7) Shr 3, 0);
End;
{--------}

Procedure FFClearBit(BitSet: PffByteArray; Bit: Integer);
Var
  BS: PAnsiChar Absolute BitSet;
  P: PAnsiChar;
  M: Byte;
Begin
  P := BS + (Bit Shr 3);
  M := 1 Shl (Byte(Bit) And 7);
  P^ := AnsiChar(Byte(P^) And Not M);
End;
{--------}

Function FFIsBitSet(BitSet: PffByteArray; Bit: Integer): boolean;
Var
  BS: PAnsiChar Absolute BitSet;
  P: PAnsiChar;
  M: Byte;
Begin
  P := BS + (Bit Shr 3);
  M := 1 Shl (Byte(Bit) And 7);
  Result := (Byte(P^) And M) <> 0;
End;

Function GetFlags(Value: Int64; Bit: Int64): Boolean;
Begin
  Result := (Value And Bit) <> 0;
End;

Procedure SetFlags(Var Value: Int64; Bit: Int64; Checked: boolean);
Var
  b: boolean;
Begin
  b := GetFlags(Value, Bit);
  If (b And Checked) Then Exit; // yes exists
  If (b And Not Checked) Then
    Value := Value - bit
  Else If (Not b And Checked) Then
    Value := Value + Bit;
End;
{--------}

Procedure FFSetAllBits(BitSet: PffByteArray; BitCount: Integer);
Begin
  FillChar(BitSet^, (BitCount + 7) Shr 3, $FF);
End;
{--------}

Procedure FFSetBit(BitSet: PffByteArray; Bit: Integer);
Var
  BS: PAnsiChar Absolute BitSet;
  P: PAnsiChar;
  M: Byte;
Begin
  P := BS + (Bit Shr 3);
  M := 1 Shl (Byte(Bit) And 7);
  P^ := AnsiChar(Byte(P^) Or M);
End;
{====================================================================}

{===Verification routines============================================}

Function FFVerifyBlockSize(BlockSize: Longint): boolean;
Begin
  Result := (BlockSize = 4 * 1024) Or
    (BlockSize = 8 * 1024) Or
    (BlockSize = 16 * 1024) Or
    (BlockSize = 32 * 1024) Or
    (BlockSize = 64 * 1024);
End;
{--------}

Function FFVerifyExtension(Const Ext: TffExtension): boolean;
Begin
  Result := ValidFileNameHelper(Ext, fscl_Extension);
End;
{--------}

Function FFVerifyFileName(Const FileName: TffFileName): boolean;
Begin
  Result := ValidFileNameHelper(FileName, fscl_FileName);
End;
{--------}

Function FFVerifyServerName(aName: TffNetAddress): Boolean;
Var
  I: Integer;
Begin
  aName := FFShStrTrim(aName);
  Result := Not ((aName = '') Or (Length(aName) > 15));
  If Result Then
    For I := 1 To Length(aName) Do
      If Not (aName[I] In ['A'..'Z', 'a'..'z', '0'..'9', ' ']) Then
        Begin
          Result := False;
          Break;
        End;
End;
{--------}

Function FFVerifyKeyLength(KeyLen: Word): boolean;
Begin
  Result := (0 < KeyLen) And (KeyLen <= fscl_MaxKeyLength);
End;
{====================================================================}

{===WWW Shell Routines===============================================}

Procedure ShellToWWW;
Resourcestring
  EX_Error = 'Unable to start web browser. Make sure you have it properly setup on your system.';
Begin
  If ShellExecute(0, 'open', 'http://sourceforge.net/projects/tpFSSQL&I', '',
    '', SW_SHOWNORMAL) <= 32 Then
    ShowMessage(EX_Error);
End;
{--------}

Procedure ShellToEMail;
Resourcestring
  EX_Error = 'Unable to start Internet mail client. Make sure you have it properly setup on your system.';
Begin
  ShowMessage('Email support disabled in open source version.');
  //  if ShellExecute(0, 'open',
  //       'mailto:support@turbopower.com',
  //       '', '', SW_SHOWNORMAL) <= 32 then
  //    ShowMessage(EX_Error);
End;
{====================================================================}

{===FSSQL&I TFSSpecObject class=======================================}

Class Function TFSSpecObject.NewInstance: TObject;
Begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
End;
{--------}

Procedure TFSSpecObject.FreeInstance;
Var
  Temp: pointer;
Begin
  {$IFDEF FF_DEBUG_THREADS} {!!.03}
  ThreadEnter; {!!.03}
  ThreadExit; {!!.03}
  ffoMethodLock := 2; {!!.03}
  {$ENDIF} {!!.03}
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
End;
{Begin !!.03}
{$IFDEF FF_DEBUG_THREADS}
{--------}

Procedure TFSSpecObject.ThreadEnter;
Begin
  Case LockedExchange(ffoMethodLock, 1) Of
    0: ; //ok
    2: Raise Exception.Create('Attempt to access a destroyed object!');
    Else
      ffoMethodLock := 3;
      Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
  End;
  Try
    If ffoThreadLockCount > 0 Then
      If ffoCurrentThreadID <> GetCurrentThreadID Then
        Raise Exception.Create('Multithreading violation [ObjID: ' +
          IntToStr(Integer(Self)) +
          ', Locking thread: ' +
          IntToStr(ffoCurrentThreadID) +
          ', Current thread: ' +
          IntToStr(GetCurrentThreadID) +
          ']')
      Else
        Inc(ffoThreadLockCount)
    Else
      Begin
        ffoCurrentThreadID := GetCurrentThreadID;
        Inc(ffoThreadLockCount);
      End;
  Finally
    Case LockedExchange(ffoMethodLock, 0) Of
      1: ; //ok
      2: Raise Exception.Create('Attemp to access a destroyed object!');
      Else
        ffoMethodLock := 3;
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
    End;
  End;
End;
{--------}

Procedure TFSSpecObject.ThreadExit;
Begin
  Case LockedExchange(ffoMethodLock, 1) Of
    0: ; //ok
    2: Raise Exception.Create('Attempt to access a destroyed object!');
    Else
      ffoMethodLock := 3;
      Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
  End;
  Try
    If ffoThreadLockCount > 0 Then
      If ffoCurrentThreadID <> GetCurrentThreadID Then
        Raise Exception.Create('Multithreading violation [ObjID: ' +
          IntToStr(Integer(Self)) +
          ', Locking thread: ' +
          IntToStr(ffoCurrentThreadID) +
          ', Current thread: ' +
          IntToStr(GetCurrentThreadID) +
          ']')
      Else
        Dec(ffoThreadLockCount)
    Else
      Raise Exception.Create('ThreadEnter <-> ThreadExit');
  Finally
    Case LockedExchange(ffoMethodLock, 0) Of
      1: ; //ok
      2: Raise Exception.Create('Attemp to access a destroyed object!');
      Else
        ffoMethodLock := 3;
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
    End;
  End;
End;
{$ENDIF}
{End !!.03}
{====================================================================}

{===FSSQL&I TfsVCLList class======================================}

Class Function TfsVCLList.NewInstance: TObject;
Begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
End;
{--------}

Procedure TfsVCLList.FreeInstance;
Var
  Temp: pointer;
Begin
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
End;
{====================================================================}

{===FSSQL&I TFSSpecComp class====================================}

Constructor TFSSpecComp.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  fcDestroying := False;
  fcLock := TfsPadlock.Create; {!!.11}
End;
{--------}

Destructor TFSSpecComp.Destroy;
Var
  Idx: Integer;
Begin
  FFNotifyDependents(ffn_Destroy);

  {Begin !!.11}
  If Assigned(fcDependentList) Then
    Begin
      fcLock.Lock;
      Try
        With fcDependentList Do
          For Idx := Pred(Count) Downto 0 Do
            DeleteAt(Idx);
      Finally
        fcLock.Unlock;
      End;
    End; { if }
  {End !!.11}
  fcDependentList.Free;
  fcLock.Free; {!!.11}
  Inherited Destroy;
End;
{--------}

Procedure TFSSpecComp.FFAddDependent(ADependent: TFSSpecComp);
{Rewritten!!.11}
Var
  Item: TfsIntListItem;
Begin
  If Not Assigned(ADependent) Then
    Exit;
  Assert(ADependent <> Self); {!!.02}

  If Not Assigned(fcDependentList) Then
    fcDependentList := TFSNormalList.Create;
  fcLock.Lock;
  Try
    With fcDependentList Do
      If Not Exists(Longint(ADependent)) Then
        Begin
          Item := TfsIntListItem.Create(Longint(ADependent));
          Item.MaintainLinks := False;
          Insert(Item);
        End;
  Finally
    fcLock.Unlock;
  End;
End;
{--------}

Procedure TFSSpecComp.FFNotification(Const AOp: Byte; AFrom: TFSSpecComp);
Begin
  FFNotificationEX(AOp, AFrom, 0);
End;
{--------}

Procedure TFSSpecComp.FFNotificationEx(Const AOp: Byte; AFrom: TFSSpecComp;
  Const aData: TffWord32);
Begin
  { do nothing at this level }
End;
{--------}

Procedure TFSSpecComp.FFNotifyDependents(Const AOp: Byte);
Var
  Idx: Integer;
Begin
  If (fcDestroying And (AOp = ffn_Destroy)) Then
    Exit;
  {Begin !!.11}
  If Assigned(fcDependentList) Then
    Begin
      fcLock.Lock;
      Try
        fcDestroying := AOp = ffn_Destroy;
        For Idx := Pred(fcDependentList.Count) Downto 0 Do
          TFSSpecComp(TfsIntListItem(fcDependentList[Idx]).KeyAsInt).FFNotification(AOp, Self);
      Finally
        fcLock.Unlock;
      End;
    End; { if }
  {End !!.11}
End;
{--------}

Procedure TFSSpecComp.FFNotifyDependentsEx(Const AOp: Byte; Const AData: TffWord32);
Var
  Idx: Integer;
Begin
  If (fcDestroying And (AOp = ffn_Destroy)) Then
    Exit;
  {Begin !!.11}
  If Assigned(fcDependentList) Then
    Begin
      fcLock.Lock;
      Try
        fcDestroying := AOp = ffn_Destroy;
        For Idx := Pred(fcDependentList.Count) Downto 0 Do
          TFSSpecComp(TfsIntListItem(fcDependentList[Idx]).KeyAsInt).FFNotificationEx(AOp, Self, AData);
      Finally
        fcLock.Unlock;
      End;
    End; { if }
End;
{--------}

Procedure TFSSpecComp.FFRemoveDependent(ADependent: TFSSpecComp);
Begin
  {Begin !!.11}
  If Assigned(ADependent) And Assigned(fcDependentList) Then
    Begin
      fcLock.Lock;
      Try
        fcDependentList.Delete(Longint(ADependent));
      Finally
        fcLock.Unlock;
      End;
    End; { if }
  {End !!.11}
End;
{--------}
{$IFDEF IsDelphi} {!!.03}

Class Function TFSSpecComp.NewInstance: TObject;
Begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
End;
{--------}

Procedure TFSSpecComp.FreeInstance;
Var
  Temp: pointer;
Begin
  {$IFDEF FF_DEBUG_THREADS} {!!.03}
  ThreadEnter; {!!.03}
  ThreadExit; {!!.03}
  ffcMethodLock := 2; {!!.03}
  {$ENDIF} {!!.03}
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
End;
{$ENDIF} {!!.03}
{Begin !!.03}
{$IFDEF FF_DEBUG_THREADS}
{--------}

Procedure TFSSpecComp.ThreadEnter;
Begin
  Case LockedExchange(ffcMethodLock, 1) Of
    0: ; //ok
    2: Raise Exception.Create('Attemp to access a destroyed object!');
    Else
      ffcMethodLock := 3;
      Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
  End;
  Try
    If ffcThreadLockCount > 0 Then
      If ffcCurrentThreadID <> GetCurrentThreadID Then
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']')
      Else
        Inc(ffcThreadLockCount)
    Else
      Begin
        ffcCurrentThreadID := GetCurrentThreadID;
        Inc(ffcThreadLockCount);
      End;
  Finally
    Case LockedExchange(ffcMethodLock, 0) Of
      1: ; //ok
      2: Raise Exception.Create('Attemp to access a destroyed object!');
      Else
        ffcMethodLock := 3;
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
    End;
  End;
End;
{--------}

Procedure TFSSpecComp.ThreadExit;
Begin
  Case LockedExchange(ffcMethodLock, 1) Of
    0: ; //ok
    2: Raise Exception.Create('Attemp to access a destroyed object!');
    Else
      ffcMethodLock := 3;
      Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
  End;
  Try
    If ffcThreadLockCount > 0 Then
      If ffcCurrentThreadID <> GetCurrentThreadID Then
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']')
      Else
        Dec(ffcThreadLockCount)
    Else
      Raise Exception.Create('ThreadEnter <-> ThreadExit');
  Finally
    Case LockedExchange(ffcMethodLock, 0) Of
      1: ; //ok
      2: Raise Exception.Create('Attemp to access a destroyed object!');
      Else
        ffcMethodLock := 3;
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
    End;
  End;
End;
{$ENDIF}
{End !!.03}
{--------}

Function TFSSpecComp.GetVersion: String;
Begin
  Result := Format('%5.3f', [fsVersionNumber / 1000.0]);
End;
{--------}

Procedure TFSSpecComp.SetVersion(Const Value: String);
Begin
  {do nothing}
End;
{====================================================================}

{===FSSQL&I TFSSpecPersis class===================================}

Class Function TFSSpecPersis.NewInstance: TObject;
Begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
End;
{--------}

Procedure TFSSpecPersis.FreeInstance;
Var
  Temp: pointer;
Begin
  {$IFDEF FF_DEBUG_THREADS} {!!.03}
  ThreadEnter; {!!.03}
  ThreadExit; {!!.03}
  ffpMethodLock := 2; {!!.03}
  {$ENDIF} {!!.03}
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
End;
{Begin !!.03}
{$IFDEF FF_DEBUG_THREADS}
{--------}

Procedure TFSSpecPersis.ThreadEnter;
Begin
  Case LockedExchange(ffpMethodLock, 1) Of
    0: ; //ok
    2: Raise Exception.Create('Attemp to access a destroyed object!');
    Else
      ffpMethodLock := 3;
      Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
  End;
  Try
    If (ffpThreadLockCount > 0) Then
      If ffpCurrentThreadID <> GetCurrentThreadID Then
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']')
      Else
        Inc(ffpThreadLockCount)
    Else
      Begin
        ffpCurrentThreadID := GetCurrentThreadID;
        Inc(ffpThreadLockCount);
      End;
  Finally
    Case LockedExchange(ffpMethodLock, 0) Of
      1: ; //ok
      2: Raise Exception.Create('Attemp to access a destroyed object!');
      Else
        ffpMethodLock := 3;
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
    End;
  End;
End;
{--------}

Procedure TFSSpecPersis.ThreadExit;
Begin
  Case LockedExchange(ffpMethodLock, 1) Of
    0: ; //ok
    2: Raise Exception.Create('Attemp to access a destroyed object!');
    Else
      ffpMethodLock := 3;
      Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
  End;
  Try
    If (ffpThreadLockCount > 0) Then
      If ffpCurrentThreadID <> GetCurrentThreadID Then
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']')
      Else
        Dec(ffpThreadLockCount)
    Else
      Raise Exception.Create('ThreadEnter <-> ThreadExit');
  Finally
    Case LockedExchange(ffpMethodLock, 0) Of
      1: ; //ok
      2: Raise Exception.Create('Attemp to access a destroyed object!');
      Else
        ffpMethodLock := 3;
        Raise Exception.Create('Multithreading violation [ObjID: ' + IntToStr(Integer(Self)) + ']');
    End;
  End;
End;
{$ENDIF}
{End !!.03}
{====================================================================}

{===FSSQL&I TfsThread class=======================================}

Procedure TfsThread.DoTerminate;
Begin
  If Assigned(OnTerminate) Then
    OnTerminate(Self);
End;
{--------}

Class Function TfsThread.NewInstance: TObject;
Begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
End;
{--------}

Procedure TfsThread.FreeInstance;
Var
  Temp: pointer;
Begin
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
End;
{Begin !!.02}
{--------}

Procedure TfsThread.WaitForEx(Const Timeout: Longint);
Var
  H: THandle;
  Msg: TMsg;
Begin
  H := Handle;

  If GetCurrentThreadID = MainThreadID Then
    While MsgWaitForMultipleObjects(1, H, False, Timeout, QS_SENDMESSAGE) =
      WAIT_OBJECT_0 + 1 Do
      PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE)
  Else
    WaitForSingleObject(H, Timeout);
End;
{End !!.02}
{====================================================================}

{===FSSQL&I List and List Item classes============================}

Constructor TFSSpecListItem.Create;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    ffliList := TFSNormalList.Create;
    ffliState := lsNormal;
    ffliMaintainLinks := True;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TFSSpecListItem.Destroy;
Var
  inx: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    ffliState := lsClearing;
    {Begin !!.11}
    If ffliList <> Nil Then
      Begin
        For inx := 0 To pred(ffliList.Count) Do
          TFSNormalList(TfsIntListItem(ffliList[inx]).KeyAsInt).InternalDelete(Key^); {!!.02}
        ffliList.Free;
      End;
    {End !!.11}
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecListItem.ffliAddListLink(L: TFSNormalList);
Var
  anItem: TfsIntListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {NOTE: this only gets called from a TFSNormalList object, so there's no
           need to insert Self into the calling list: it will do it
           itself}
    If (ffliList.Index(Longint(L)) = -1) Then
      Begin
        anItem := TfsIntListItem.Create(Longint(L));
        { Turn off link maintenance for the item otherwise we will
          get into an infinitely recursive death spiral. }
        anItem.MaintainLinks := False;
        ffliList.Insert(anItem);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecListItem.ffliBreakListLink(L: TFSNormalList);
Var
  inx: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {NOTE: this only gets called from a TFSNormalList object, so there's no
           need to remove Self from the calling list: it will do it
           itself}
    If (ffliState = lsNormal) Then
      Begin
        inx := ffliList.Index(Longint(L));
        If (inx <> -1) Then
          ffliList.DeleteAt(inx);
        If ffliFreeOnRemove Then
          Begin
            ffliState := lsClearing;
            For inx := pred(ffliList.Count) Downto 0 Do
              TFSNormalList(TfsIntListItem(ffliList[inx]).KeyAsInt).InternalDelete(Key^); {!!.02}
            ffliList.Empty;
            ffliState := lsNormal;
          End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.11}
{--------}

Procedure TFSSpecListItem.ffliSetMaintainLinks(Const Value: Boolean);
{ Rewritten !!.12}
Begin
  ffliMaintainLinks := Value;
  If Not Value Then
    Begin
      ffliList.Free;
      ffliList := Nil;
    End
  Else If ffliList = Nil Then
    ffliList := TFSNormalList.Create;
End;
{End !!.11}
{--------}

Function TFSSpecListItem.GetRefCount: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {Begin !!.11}
    If ffliList <> Nil Then
      Result := ffliList.Count
    Else
      Result := 0;
    {End !!.11}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Constructor TfsStrListItem.Create(Const aKey: TffShStr);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    sliKey := FFShStrAlloc(aKey);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TfsStrListItem.Destroy;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {NOTE: inherited Destroy must be called first, because it will in
           turn make a call to get the Key for the item, and so the
           Key pointer had still better exist.}
    Inherited Destroy;
    FFShStrFree(sliKey);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsStrListItem.Compare(aKey: pointer): Integer;
Begin
  Result := FFCmpShStr(PffShStr(aKey)^, sliKey^, 255);
End;
{--------}

Function TfsStrListItem.Key: pointer;
Begin
  Result := sliKey;
End;
{--------}

Function TfsStrListItem.KeyAsStr: TffShStr;
Begin
  Result := sliKey^;
End;
{--------}

Function TfsUCStrListItem.Compare(aKey: pointer): Integer;
Begin
  Result := FFCmpShStrUC(PffShStr(aKey)^, PffShStr(Key)^, 255);
End;
{--------}

Constructor TfsIntListItem.Create(Const aKey: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    iliKey := aKey;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsIntListItem.Compare(aKey: pointer): Integer;
Begin
  Result := FFCmpI32(PffLongint(aKey)^, iliKey);
End;
{--------}

Function TfsIntListItem.Key: pointer;
Begin
  Result := @iliKey;
End;
{--------}

Function TfsIntListItem.KeyAsInt: Longint;
Begin
  Result := iliKey;
End;
{--------}

Constructor TFSSpecW32ListItem.Create(Const aKey: TffWord32);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    wliKey := aKey;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecW32ListItem.Compare(aKey: pointer): Integer;
Begin
  Result := FFCmpDW(PffWord32(aKey)^, wliKey);
End;
{--------}

Function TFSSpecW32ListItem.Key: pointer;
Begin
  Result := @wliKey;
End;
{--------}

Function TFSSpecW32ListItem.KeyAsInt: TffWord32;
Begin
  Result := wliKey;
End;
{--------}

Function TFSSpecW32ListItem.KeyValue: TffWord32;
Begin
  Result := wliKey;
End;
{--------}

Constructor TfsI64ListItem.Create(Const aKey: TffInt64);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    iliKey := aKey;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsI64ListItem.Compare(aKey: pointer): Integer;
Begin
  Result := FFCmpI64(PffInt64(aKey)^, iliKey);
End;
{--------}

Function TfsI64ListItem.Key: pointer;
Begin
  Result := @iliKey;
End;
{--------}

Function TfsI64ListItem.KeyValue: TffInt64;
Begin
  Result := iliKey;
End;
{--------}

Constructor TfsSelfListItem.Create;
Begin
  Inherited Create(Longint(Self));
End;
{--------}

Constructor TFSNormalList.Create;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    fflState := lsNormal;
    { Allocate space for the initial number of items. }
    FFGetMem(fflList, fscl_InitialListSize * sizeOf(TFSSpecListItem));
    FillChar(fflList^, fscl_InitialListSize * sizeOf(TFSSpecListItem), 0);
    fflCapacity := fscl_InitialListSize;
    fflCount := 0;
    fflSorted := True;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TFSNormalList.Destroy;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Empty;
    FFFreeMem(fflList, fflCapacity * sizeOf(TFSSpecListItem));
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}
{Deleted !!.01}
{procedure TFSNormalList.Assign(Source : TPersistent);
var
  SrcList : TFSNormalList;
  i       : Longint;
begin
  if (Source is TFSNormalList) then begin
    Empty;
    SrcList := TFSNormalList(Source);
    for i := 0 to pred(SrcList.Count) do
      Insert(SrcList.Items[i]);
  end
  else
    inherited Assign(Source);
end;}
{--------}

Procedure TFSNormalList.Delete(Const aKey);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    fflDeleteAtPrim(fflIndexPrim(aKey));
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.02}
{--------}

Procedure TFSNormalList.InternalDelete(Const aKey);
Begin
  If Assigned(fflPortal) Then
    fflPortal.BeginWrite;
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      fflDeleteAtPrim(fflIndexPrim(aKey));
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    If Assigned(fflPortal) Then
      fflPortal.EndWrite;
  End;
End;
{End !!.02}
{--------}

Procedure TFSNormalList.DeleteAt(aInx: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    fflDeleteAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.fflDeleteAtPrim(aInx: Longint);
Var
  Item: TFSSpecListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (fflState = lsNormal) And
      (0 <= aInx) And
      (aInx < fflCount) Then
      Begin
        Item := fflList^[aInx];
        If assigned(Item) Then
          Begin
            If Item.MaintainLinks Then
              Item.ffliBreakListLink(Self);
            If (Item.ReferenceCount = 0) Then
              Item.Free;
            dec(fflCount);
            If aInx < fflCount Then
              Move(fflList^[aInx + 1], fflList^[aInx],
                (fflCount - aInx) * SizeOf(TFSSpecListItem));
          End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.Empty;
Var
  Inx: Longint;
  Item: TFSSpecListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    fflState := lsClearing;
    Try
      For Inx := pred(fflCount) Downto 0 Do
        Begin
          Item := fflList^[Inx];
          If assigned(Item) Then
            Begin
              If Item.MaintainLinks Then
                Item.ffliBreakListLink(Self);
              If (Item.ReferenceCount = 0) Then
                Item.Free;
              dec(fflCount);
            End;
        End;
      { Zero out the array. }
      fillChar(fflList^, fflCapacity * sizeOf(TFSSpecListItem), 0);
    Finally
      fflState := lsNormal;
    End; {try..finally}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSNormalList.Exists(Const aKey): boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := fflIndexPrim(aKey) <> -1;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.fflGrow;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    SetCapacity(fflCapacity + fscl_InitialListSize);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSNormalList.GetCapacity: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := fflCapacity;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSNormalList.GetCount: Longint;
Begin
  Result := fflCount;
End;
{--------}

Function TFSNormalList.GetInsertionPoint(aItem: TFSSpecListItem): Longint;
Var
  OurCount: Longint;
  L, R, M: Longint;
  CompareResult: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    OurCount := fflCount;
    {take care of the easy case}
    If (OurCount = 0) Then
      L := 0
    Else If Sorted Then
      Begin
        {standard binary search}
        L := 0;
        R := pred(OurCount);
        Repeat
          M := (L + R) Div 2;
          CompareResult := fflList^[M].Compare(aItem.Key);
          If (CompareResult = 0) Then
            Begin
              {do nothing, key already exists}
              Result := -1;
              Exit;
            End
          Else If (CompareResult < 0) Then
            R := M - 1
          Else
            L := M + 1
        Until (L > R);
        {as it happens, on exit from this repeat..until loop the
         algorithm will have set L to the correct insertion point}
      End
    Else {not Sorted}
      L := OurCount;

    Result := L;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSNormalList.GetItem(Const aInx: Longint): TFSSpecListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (aInx >= 0) And (aInx < fflCount) Then
      Result := fflList^[aInx]
    Else
      Result := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSNormalList.Insert(aItem: TFSSpecListItem): boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := InsertPrim(aItem) <> -1;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSNormalList.InsertPrim(aItem: TFSSpecListItem): Longint;
Var
  L: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Determine the insertion point. }
    L := GetInsertionPoint(aItem);
    If L >= 0 Then
      Begin
        { If we are at the limit then increase capacity. }
        If fflCount = fflCapacity Then
          fflGrow;

        { If we are before the last element in the list, shift everything up. }
        If L < fflCount Then
          Move(fflList^[L], fflList^[L + 1], (fflCount - L) * sizeOf(TFSSpecListItem));

        fflList^[L] := aItem;
        If aItem.MaintainLinks Then
          aItem.ffliAddListLink(Self);
        inc(fflCount);
      End;
    Result := L;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSNormalList.IsEmpty: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := Count = 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSNormalList.Index(Const aKey): Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := fflIndexPrim(aKey);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSNormalList.fflIndexPrim(Const aKey): Longint;
Var
  M, L, R: Longint;
  CompareResult: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (fflCount > 0) Then {!!.11}
      If Sorted Then
        Begin
          {standard binary search}
          L := 0;
          R := pred(fflCount);
          Repeat
            M := (L + R) Div 2;
            CompareResult := fflList^[M].Compare(@aKey);
            If (CompareResult = 0) Then
              Begin
                Result := M;
                Exit;
              End
            Else If (CompareResult < 0) Then
              R := M - 1
            Else
              L := M + 1
          Until (L > R);
        End
      Else {not Sorted}
        Begin
          {standard sequential search}
          For M := 0 To pred(fflCount) Do
            If (fflList^[M].Compare(@aKey) = 0) Then
              Begin
                Result := M;
                Exit;
              End
        End;
    Result := -1;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.Remove(Const aKey);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    fflRemoveAtPrim(fflIndexPrim(aKey));
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.RemoveAt(aInx: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    fflRemoveAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.fflRemoveAtPrim(aInx: Longint);
Var
  Item: TFSSpecListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (fflState = lsNormal) And
      (0 <= aInx) And
      (aInx < fflCount) Then
      Begin
        Item := fflList^[aInx];
        If assigned(Item) Then
          Begin
            If Item.MaintainLinks Then
              Item.ffliBreakListLink(Self);
            { Note: the item is not freed }
            dec(fflCount);
            If aInx < fflCount Then
              Move(fflList^[aInx + 1], fflList^[aInx],
                (fflCount - aInx) * SizeOf(TFSSpecListItem));
          End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.SetCapacity(Const C: Longint);
Var
  NewList: PfsListItemArray;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (C >= fflCount) And (C <> fflCapacity) Then
      Begin
        { Get a new block. }
        FFGetMem(NewList, C * sizeOf(TFSSpecListItem));
        FillChar(NewList^, C * sizeOf(TFSSpecListItem), 0);

        { Transfer the existing data. }
        Move(fflList^, NewList^, fflCount * SizeOf(TFSSpecListItem));

        { Free the existing data. }
        FFFreeMem(fflList, fflCapacity * SizeOf(TFSSpecListItem));
        fflList := NewList;
        fflCapacity := C;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.SetCount(Const C: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Do we need to grow the table? }
    If C <> fflCapacity Then
      SetCapacity(C);
    fflCount := C;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.SetItem(Const aInx: Longint; Item: TFSSpecListItem);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (0 <= aInx) And (aInx < fflCount) Then
      fflList^[aInx] := Item;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSNormalList.SetSorted(S: boolean);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (S <> fflSorted) Then
      fflSorted := (S And IsEmpty);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{===TfsPointerList===================================================}

Constructor TfsPointerList.Create;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    { Allocate space for the initial number of items. }
    FFGetMem(plList, fscl_InitialListSize * sizeOf(Pointer));
    FillChar(plList^, fscl_InitialListSize * sizeOf(Pointer), 0);
    plCapacity := fscl_InitialListSize;
    plCount := 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TfsPointerList.Destroy;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    FFFreeMem(plList, plCapacity * sizeOf(Pointer));
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsPointerList.Assign(Source: TPersistent);
Var
  SrcList: TfsPointerList;
  i: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (Source Is TfsPointerList) Then
      Begin
        Empty;
        SrcList := TfsPointerList(Source);
        For i := 0 To pred(SrcList.Count) Do
          Append(SrcList.Pointers[i]);
      End
    Else
      Inherited Assign(Source);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsPointerList.Empty;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Did the array contain anything? }
    If plCount > 0 Then
      { Yes. Zero it out. }
      FillChar(plList^, plCapacity * sizeOf(Pointer), 0);
    plCount := 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsPointerList.fflGrow;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    SetCapacity(plCapacity + fscl_InitialListSize);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsPointerList.GetCapacity: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := plCapacity;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsPointerList.GetCount: Longint;
Begin
  Result := plCount;
End;
{--------}

Function TfsPointerList.GetPointer(aInx: Longint): Pointer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (0 <= aInx) And (aInx < plCount) Then
      Result := plList^[aInx]
    Else
      Result := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsPointerList.GetInternalAddress: pointer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := pointer(plList);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsPointerList.Append(aPtr: Pointer): boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := AppendPrim(aPtr) <> -1;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsPointerList.AppendPrim(aPtr: Pointer): Longint;
Var
  L: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Determine the insertion point. }
    L := plCount;
    If L >= 0 Then
      Begin
        { If we are at the limit then increase capacity. }
        If plCount = plCapacity Then
          fflGrow;

        { If we are before the last element in the list, shift everything up. }
        If L < plCount Then
          Move(plList^[L], plList^[L + 1], (plCount - L) * sizeOf(Pointer));

        plList^[L] := aPtr;
        inc(plCount);
      End;
    Result := L;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsPointerList.IsEmpty: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := Count = 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsPointerList.RemoveAt(aInx: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    fflRemoveAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsPointerList.fflRemoveAtPrim(aInx: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (0 <= aInx) And
      (aInx < plCount) Then
      Begin
        dec(plCount);
        If aInx < plCount Then
          Move(plList^[aInx + 1], plList^[aInx],
            (plCount - aInx) * SizeOf(Pointer));
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsPointerList.SetCapacity(Const C: Longint);
Var
  NewList: PfsPointerArray;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (C >= plCount) And (C <> plCapacity) Then
      Begin
        { Get a new block. }
        FFGetMem(NewList, C * sizeOf(Pointer));
        FillChar(NewList^, C * sizeOf(Pointer), 0);

        { Transfer the existing data. }
        Move(plList^, NewList^, plCount * SizeOf(Pointer));

        { Free the existing data. }
        FFFreeMem(plList, plCapacity * SizeOf(Pointer));
        plList := NewList;
        plCapacity := C;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsPointerList.SetCount(Const C: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Do we need to grow the table? }
    If C > plCapacity Then
      SetCapacity(C);
    plCount := C;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsPointerList.SetPointer(aInx: Longint; aPtr: Pointer);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Is the index within range? }
    If (0 <= aInx) And (aInx < plCount) Then
      plList^[aInx] := aPtr;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{===TFSSpecList====================================================}

Constructor TFSSpecList.Create;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    { Allocate space for the initial number of items. }
    FFGetMem(FList, fscl_InitialListSize * sizeOf(THandle));
    FillChar(FList^, fscl_InitialListSize * sizeOf(THandle), 0);
    FCapacity := fscl_InitialListSize;
    FCount := 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TFSSpecList.Destroy;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Empty;
    FFFreeMem(FList, FCapacity * sizeOf(THandle));
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.Assign(Source: TPersistent);
Var
  SrcList: TFSSpecList;
  i: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (Source Is TFSSpecList) Then
      Begin
        Empty;
        SrcList := TFSSpecList(Source);
        For i := 0 To pred(SrcList.Count) Do
          Append(SrcList.Handles[i]);
      End
    Else
      Inherited Assign(Source);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.DeleteAt(aInx: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    fflDeleteAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.fflDeleteAtPrim(aInx: Longint);
Var
  aHandle: THandle;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (0 <= aInx) And
      (aInx < FCount) Then
      Begin
        aHandle := FList^[aInx];
        CloseHandle(aHandle);
        dec(FCount);
        If aInx < FCount Then
          Move(FList^[aInx + 1], FList^[aInx],
            (FCount - aInx) * SizeOf(THandle));
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.Empty;
Var
  Inx: Longint;
  aHandle: THandle;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    For Inx := pred(FCount) Downto 0 Do
      Begin
        aHandle := FList^[Inx];
        CloseHandle(aHandle);
        dec(FCount);
      End;
    { Zero out the array. }
    fillChar(FList^, FCapacity * sizeOf(THandle), 0);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.fflGrow;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    SetCapacity(FCapacity + fscl_InitialListSize);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecList.GetCapacity: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := FCapacity;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecList.GetCount: Longint;
Begin
  Result := FCount;
End;
{--------}

Function TFSSpecList.GetHandle(aInx: Longint): THandle;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (0 <= aInx) And (aInx < FCount) Then
      Result := FList^[aInx]
    Else
      Result := 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecList.GetInternalAddress: pointer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := pointer(FList);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecList.Append(aHandle: THandle): boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := AppendPrim(aHandle) <> -1;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecList.AppendPrim(aHandle: THandle): Longint;
Var
  L: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Determine the insertion point. }
    L := FCount;
    If L >= 0 Then
      Begin
        { If we are at the limit then increase capacity. }
        If FCount = FCapacity Then
          fflGrow;

        { If we are before the last element in the list, shift everything up. }
        If L < FCount Then
          Move(FList^[L], FList^[L + 1], (FCount - L) * sizeOf(THandle));

        FList^[L] := aHandle;
        inc(FCount);
      End;
    Result := L;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecList.IsEmpty: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := Count = 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.RemoveAll;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    FCount := 0;
    { Zero out the array. }
    fillChar(FList^, FCapacity * sizeOf(THandle), 0);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.RemoveAt(aInx: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    fflRemoveAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.fflRemoveAtPrim(aInx: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (0 <= aInx) And
      (aInx < FCount) Then
      Begin
        { Note: The handle is not closed. }
        dec(FCount);
        If aInx < FCount Then
          Move(FList^[aInx + 1], FList^[aInx],
            (FCount - aInx) * SizeOf(THandle));
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.SetCapacity(Const C: Longint);
Var
  NewList: PfsHandleArray;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (C >= FCount) And (C <> FCapacity) Then
      Begin
        { Get a new block. }
        FFGetMem(NewList, C * sizeOf(THandle));
        FillChar(NewList^, C * sizeOf(THandle), 0);

        { Transfer the existing data. }
        Move(FList^, NewList^, FCount * SizeOf(THandle));

        { Free the existing data. }
        FFFreeMem(FList, FCapacity * SizeOf(THandle));
        FList := NewList;
        FCapacity := C;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecList.SetCount(Const C: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Do we need to grow the table? }
    If C > FCapacity Then
      SetCapacity(C);
    FCount := C;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{===TFSSpecThreadList====================================================}

Constructor TFSSpecThreadList.Create;
Begin
  Inherited Create;
  fflPortal := TfsReadWritePortal.Create;
End;
{--------}

Destructor TFSSpecThreadList.Destroy;
Begin
  fflPortal.Free;
  Inherited Destroy;
End;
{--------}

Function TFSSpecThreadList.BeginRead: TFSSpecThreadList;
Begin
  If isMultiThread Then
    fflPortal.BeginRead;
  Result := Self;
End;
{--------}

Function TFSSpecThreadList.BeginWrite: TFSSpecThreadList;
Begin
  If isMultiThread Then
    fflPortal.BeginWrite;
  Result := Self;
End;
{--------}

Procedure TFSSpecThreadList.EndRead;
Begin
  If isMultiThread Then
    fflPortal.EndRead;
End;
{--------}

Procedure TFSSpecThreadList.EndWrite;
Begin
  If isMultiThread Then
    fflPortal.EndWrite;
End;
{====================================================================}

{===TFSSpecStringList====================================================}

Constructor TFSSpecStringList.Create;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    slCaseSensitive := True;
    slList := TFSNormalList.Create;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TFSSpecStringList.Destroy;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    slList.Free;
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.Assign(Source: TPersistent);
Var
  StrList: TFSSpecStringList;
  Strs: TStrings;
  I: Longint;
  Inx: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}

    StrList := TFSSpecStringList(Source);
    Strs := TStrings(Source);

    If Source Is TFSSpecStringList Then
      Begin
        Empty;

        CaseSensitive := StrList.CaseSensitive;
        Sorted := StrList.Sorted;

        For I := 0 To StrList.Count - 1 Do
          Begin
            Inx := InsertPrim(StrList.Strings[I]);
            Objects[Inx] := StrList.Objects[I];
          End;
      End
    Else If Source Is TStrings Then
      Begin
        Empty;
        Sorted := False;
        For I := 0 To Strs.Count - 1 Do
          Begin
            Insert(Strs.Strings[I]);
            Objects[I] := Strs.Objects[I];
          End;
      End
    Else
      Inherited Assign(Source);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.AssignTo(Dest: TPersistent);
Var
  StrList: TFSSpecStringList;
  Strs: TStrings;
  I: Longint;
  Inx: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}

    StrList := TFSSpecStringList(Dest);
    Strs := TStrings(Dest);

    If Dest Is TFSSpecStringList Then
      Begin

        StrList.Empty;
        StrList.CaseSensitive := CaseSensitive;
        StrList.Sorted := Sorted;

        For I := 0 To pred(Count) Do
          Begin
            Inx := StrList.InsertPrim(Strings[I]);
            StrList.Objects[Inx] := Objects[I];
          End;
      End
    Else If Dest Is TStrings Then
      Begin
        Strs.Clear;
        For I := 0 To pred(Count) Do
          Begin
            Strs.Add(Strings[I]);
            Strs.Objects[I] := Objects[I];
          End;
      End
    Else
      Inherited AssignTo(Dest);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.Delete(Const aStr: TffShStr);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    slList.Delete(aStr);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.DeleteAt(aInx: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    slList.DeleteAt(aInx);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.Empty;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    slList.Empty;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.Exists(Const aStr: TffShStr): boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := slList.Exists(aStr);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.GetCapacity: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := slList.Capacity;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.GetCount: Longint;
Begin
  Result := slList.Count;
End;
{--------}

Function TFSSpecStringList.GetObj(aInx: Longint): TObject;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := TObject(TfsStrListItem(slList.Items[aInx]).ExtraData);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.GetSorted: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := slList.Sorted;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.GetStr(aInx: Longint): TffShStr;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := TfsStrListItem(slList.Items[aInx]).KeyAsStr;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.GetValue(Const aName: TffShStr): TffShStr;
Var
  I: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    I := IndexOfName(aName);
    If I >= 0 Then
      Result := GetStr(I)
    Else
      Result := '';
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.IndexOfName(Const aName: TffShStr): Longint;
Var
  P: Longint;
  S: TffShStr;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    For Result := 0 To GetCount - 1 Do
      Begin
        S := GetStr(Result);
        P := Pos('=', S);
        If (P <> 0) And (FFCmpShStr(Copy(S, 1, P - 1), aName, 255) = 0) Then
          Exit;
      End;
    Result := -1;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.Insert(Const aStr: TffShStr): boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := InsertPrim(aStr) <> -1;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.InsertPrim(Const aStr: TffShStr): Longint;
Var
  Item: TfsStrListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If CaseSensitive Then
      Item := TfsStrListItem.Create(aStr)
    Else
      Item := TfsUCStrListItem.Create(aStr);
    Try
      Result := slList.InsertPrim(Item);
      If Result < 0 Then {!!.10}
        Item.Free; {!!.10}
    Except
      Item.Free;
      Raise;
    End; {try..except}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.IsEmpty: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := slList.Count = 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecStringList.Index(Const aStr: TffShStr): Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := slList.Index(aStr);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.SetCapacity(C: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    slList.Capacity := C;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.SetCaseSensitive(CS: boolean);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (slList.Count = 0) Then
      slCaseSensitive := CS;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.SetObj(aInx: Longint; Const aObj: TObject);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    TfsStrListItem(slList.Items[aInx]).ExtraData := pointer(aObj);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.SetSorted(S: boolean);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    slList.Sorted := S;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.SetStr(aInx: Longint; Const aStr: TffShStr);
Var
  Item: TfsStrListItem;
  Obj: TObject;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {get the current item}
    Item := TfsStrListItem(slList.Items[aInx]);
    If (Item = Nil) Then
      Exit;
    If slList.Sorted Then
      Begin
        {delete the old item, create a new one and insert it}
        Obj := TObject(Item.ExtraData);
        slList.DeleteAt(aInx);
        If CaseSensitive Then
          Item := TfsStrListItem.Create(aStr)
        Else
          Item := TfsUCStrListItem.Create(aStr);
        Item.ExtraData := pointer(Obj);
        Try
          slList.Insert(Item);
        Except
          Item.Free;
          Raise;
        End;
      End
    Else {the list is not sorted}
      Begin
        FFShStrFree(Item.sliKey);
        Item.sliKey := FFShStrAlloc(aStr);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecStringList.SetValue(Const aName, aStr: TffShStr);
Var
  Idx: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Idx := IndexOfName(aName);
    If aStr <> '' Then
      Begin
        If Idx < 0 Then
          Begin
            { Item doesn't already exist }
            Insert(aName);
            Idx := IndexOfName(aName);
          End;
        SetStr(Idx, aName + '=' + aStr);
      End
    Else
      Begin
        If Idx >= 0 Then
          DeleteAt(Idx);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{===TFSSpecThreadStringList==============================================}

Constructor TFSSpecThreadStringList.Create;
Begin
  Inherited Create;
  tslPortal := TfsReadWritePortal.Create;
  slList.fflPortal := tslPortal {!!.02}
End;
{--------}

Destructor TFSSpecThreadStringList.Destroy;
Begin
  tslPortal.Free;
  Inherited Destroy;
End;
{--------}

Function TFSSpecThreadStringList.BeginRead: TFSSpecThreadStringList;
Begin
  tslPortal.BeginRead;
  Result := Self;
End;
{--------}

Function TFSSpecThreadStringList.BeginWrite: TFSSpecThreadStringList;
Begin
  tslPortal.BeginWrite;
  Result := Self;
End;
{--------}

Procedure TFSSpecThreadStringList.EndRead;
Begin
  tslPortal.EndRead;
End;
{--------}

Procedure TFSSpecThreadStringList.EndWrite;
Begin
  tslPortal.EndWrite;
End;
{====================================================================}

{===TFSSpecQueue=========================================================}

Constructor TFSSpecQueue.Create;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    ffqList := TFSNormalList.Create;
    { Turn off sorting so that items are appended to list. }
    ffqList.Sorted := False;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TFSSpecQueue.Destroy;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    ffqList.Free;
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecQueue.Delete(Const aKey);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    ffqList.Delete(aKey);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecQueue.Dequeue: TFSSpecListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := Nil;
    If GetCount > 0 Then
      Begin
        Result := ffqList[0];
        ffqList.RemoveAt(0);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TFSSpecQueue.Enqueue(anItem: TFSSpecListItem);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    ffqList.Insert(anItem);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecQueue.GetCount: Longint;
Begin
  Result := ffqList.Count;
End;
{--------}

Function TFSSpecQueue.IsEmpty: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := (ffqList.Count = 0);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TFSSpecQueue.GetItem(aInx: Longint): TFSSpecListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := ffqList[aInx];
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}
{====================================================================}

{===TfsThreadQueue===================================================}

Constructor TfsThreadQueue.Create;
Begin
  Inherited Create;
  fftqPortal := TfsReadWritePortal.Create;
  ffqList.fflPortal := fftqPortal {!!.02}
End;
{--------}

Destructor TfsThreadQueue.Destroy;
Begin
  fftqPortal.Free;
  Inherited Destroy;
End;
{--------}

Function TfsThreadQueue.BeginRead: TfsThreadQueue;
Begin
  fftqPortal.BeginRead;
  Result := Self;
End;
{--------}

Function TfsThreadQueue.BeginWrite: TfsThreadQueue;
Begin
  fftqPortal.BeginWrite;
  Result := Self;
End;
{--------}

Procedure TfsThreadQueue.EndRead;
Begin
  fftqPortal.EndRead;
End;
{--------}

Procedure TfsThreadQueue.EndWrite;
Begin
  fftqPortal.EndWrite;
End;
{====================================================================}

{===TffLatch=========================================================}

Constructor TFSNormalEvent.Create;
Begin
  Inherited Create;
  {$IFDEF UseEventPool}
  If Assigned(FFEventPool) Then
    Begin
      ffeEvent := FFEventPool.Get;
      { Make sure the event is not signaled. }
      ResetEvent(ffeEvent);
    End
  Else
    ffeEvent := CreateEvent(Nil, False, False, Nil);
  {$ELSE}
  ffeEvent := CreateEvent(Nil, False, False, Nil);
  {$ENDIF}
End;
{--------}

Destructor TFSNormalEvent.Destroy;
Begin
  {$IFDEF UseEventPool}
  If Assigned(FFEventPool) Then
    FFEventPool.Put(ffeEvent)
  Else
    CloseHandle(ffeEvent);
  {$ELSE}
  CloseHandle(FEvent);
  {$ENDIF}
  Inherited Destroy;
End;
{--------}

Procedure TFSNormalEvent.WaitFor(Const timeOut: TffWord32);
Var
  aTimeOut: TffWord32;
  waitResult: DWord;
Begin
  If timeOut <= 0 Then
    aTimeOut := fscl_INFINITE {!!.06}
  Else
    aTimeout := timeOut;

  waitResult := WaitForSingleObject(ffeEvent, aTimeout);
  If waitResult = WAIT_TIMEOUT Then
    Raise EfsException.CreateEx(fsStrResGeneral, fserrReplyTimeout,
      [SysErrorMessage(GetLastError), GetLastError])
  Else If waitResult <> WAIT_OBJECT_0 Then
    Raise EfsException.CreateEx(fsStrResGeneral, fserrWaitFailed,
      [SysErrorMessage(GetLastError), GetLastError]);
End;
{--------}

Function TFSNormalEvent.WaitForQuietly(Const timeOut: TffWord32): DWORD;
Var
  aTimeOut: TffWord32;
Begin
  If timeOut <= 0 Then
    aTimeOut := fscl_INFINITE {!!.06}
  Else
    aTimeout := timeOut;

  Result := WaitForSingleObject(ffeEvent, aTimeout);

End;
{--------}

Procedure TFSNormalEvent.SignalEvent;
Begin
  SetEvent(ffeEvent);
End;
{====================================================================}

{===TfsReadWritePortal===============================================}

Constructor TfsReadWritePortal.Create;
Begin
  Inherited Create;
  //  rwpBlockedReaders := FFSemPool.Get;                                {Deleted !!.06}
  //  rwpBlockedWriters := FFSemPool.Get;                                {Deleted !!.06}
  FFSemPool.GetTwo(rwpBlockedReaders, rwpBlockedWriters); {!!.06}
  rwpGate := TfsPadlock.Create;
  rwpActiveReaders := 0;
  rwpActiveWriter := False;
  rwpActiveWriterID := 0;
  rwpWaitingReaders := 0;
  rwpWaitingWriters := 0;
  rwpWriterReadCount := 0;
  rwpWriterWriteCount := 0;
End;
{--------}

Destructor TfsReadWritePortal.Destroy;
Begin
  rwpGate.Free;
  FFSemPool.Put(rwpBlockedReaders);
  FFSemPool.Put(rwpBlockedWriters);
  Inherited Destroy; {!!.01}
End;
{--------}

Procedure TfsReadWritePortal.BeginRead;
Var
  MustWait: boolean;
Begin

  If Not IsMultiThread Then
    Exit;

  { Wait for access to internal data. }
  rwpGate.Lock;
  Try
    { If the active writer is trying to read then automatically grant access. }
    If rwpActiveWriter And (rwpActiveWriterID = GetCurrentThreadID) Then
      Begin
        inc(rwpWriterReadCount);
        Exit;
      End;

    { If a writer has been granted access or there is at least one writer
      waiting for access, add self as a waiting reader and make sure we
      wait for read access. }
    If rwpActiveWriter Or (rwpWaitingWriters <> 0) Then
      Begin
        inc(rwpWaitingReaders);
        MustWait := True;
      End
    Else
      Begin
        { Otherwise, add self as an active reader. }
        inc(rwpActiveReaders);
        MustWait := False;
      End;

  Finally
    rwpGate.Unlock;
  End;

  If MustWait Then
    WaitForSingleObject(rwpBlockedReaders, fscl_INFINITE); {!!.06}

End;
{--------}

Procedure TfsReadWritePortal.BeginWrite;
Var
  MustWait: boolean;
Begin

  If Not IsMultiThread Then
    Exit;

  { Wait for access to internal data. }
  rwpGate.Lock;
  Try

    { If the active writer is calling BeginWrite once more, increment our
      count of such calls, release the gate, and exit. }
    If rwpActiveWriter And (rwpActiveWriterID = GetCurrentThreadID) Then
      Begin
        Inc(rwpWriterWriteCount);
        Exit;
      End;

    { If there are active readers or an active writer, add self as a waiting
      writer. }
    If rwpActiveWriter Or (rwpActiveReaders <> 0) Then
      Begin
        Inc(rwpWaitingWriters);
        MustWait := True;
      End
    Else
      Begin
        { Otherwise, mark self as the active writer. }
        rwpActiveWriter := True;
        rwpActiveWriterID := GetCurrentThreadID; {!!.06}
        MustWait := False;
      End;
  Finally
    rwpGate.Unlock;
  End;

  If MustWait Then
    Begin {!!.06 - Start}
      WaitForSingleObject(rwpBlockedWriters, fscl_INFINITE); {!!.06}
      rwpActiveWriterID := GetCurrentThreadID;
    End;

  { If we reach this point then we have write access.  Store our threadID
    so that BeginRead knows who we are.  Set our reference counts. }
  {rwpActiveWriterID := GetCurrentThreadID;}{!!.06 - End}
  rwpWriterReadCount := 0; {!!.02}
  rwpWriterWriteCount := 1;
End;
{--------}

Procedure TfsReadWritePortal.EndRead;
Begin

  If Not IsMultiThread Then
    Exit;

  { Wait for access to internal data. }
  rwpGate.Lock;
  Try

    { If a writer is active and it is calling EndRead then decrement the read
      count. }
    If rwpActiveWriter And (rwpActiveWriterID = GetCurrentThreadID) Then
      Begin
        dec(rwpWriterReadCount);
        Exit;
      End;

    { Note: This method does not catch the following cases:
      1. Thread calls EndRead before a BeginRead was issued.
      2. Active writer threadcalls EndRead before a BeginRead was called or
         after EndWrite was called. }

    If rwpActiveReaders > 0 Then
      dec(rwpActiveReaders);

    { If we are the last reader and there is at least one waiting writer,
      activate the waiting writer. }
    If (rwpActiveReaders = 0) And (rwpWaitingWriters <> 0) Then
      Begin
        dec(rwpWaitingWriters);
        rwpActiveWriter := True;
        ReleaseSemaphore(rwpBlockedWriters, 1, Nil);
      End;
  Finally
    rwpGate.Unlock;
  End;

End;
{--------}

Procedure TfsReadWritePortal.EndWrite;
Var
  tmpWaiting: Integer;
Begin

  If Not IsMultiThread Then
    Exit;

  { Wait for access to internal data. }
  rwpGate.Lock;
  Try

    { If this is the writer thread, see if this is the final call to
      EndWrite.  If not then just exist the method. }
    If rwpActiveWriterID = GetCurrentThreadID Then
      Begin
        dec(rwpWriterWriteCount);
        If rwpWriterWriteCount > 0 Then
          Begin
            Exit;
          End;
      End
    Else
      Begin {!!.06 - Start}
        { This should NEVER happend. }
        Exit;
      End;

    { Note: This method doesn't catch the following cases:
      1. A thread other than the active thread calls EndWrite.
      2. A thread calls EndWrite before BeginWrite.
    }

    {rwpActiveWriter := False;}
    {rwpActiveWriterID := 0;}{!!.06 - End}

    { If there are any waiting readers then release them. }
    If (rwpWaitingReaders <> 0) Then
      Begin
        tmpWaiting := rwpWaitingReaders;
        Dec(rwpWaitingReaders, rwpWaitingReaders);
        Inc(rwpActiveReaders, tmpWaiting);
        rwpActiveWriterID := 0; {!!.06}
        rwpActiveWriter := False; {!!.06}
        ReleaseSemaphore(rwpBlockedReaders, tmpWaiting, Nil);
      End
    Else If (rwpWaitingWriters <> 0) Then
      Begin
        { Otherwise if there is at least one waiting writer then release one. }
        Dec(rwpWaitingWriters);
        {rwpActiveWriter := True;}{!!.06 - Start}
        rwpActiveWriterID := 0;
        ReleaseSemaphore(rwpBlockedWriters, 1, Nil);
      End
    Else
      Begin
        rwpActiveWriterID := 0;
        rwpActiveWriter := False;
      End; {!!.06 - End}
  Finally
    rwpGate.Unlock;
  End;
End;
{====================================================================}

{===TfsPadlock=======================================================}

Constructor TfsPadlock.Create;
Begin
  Inherited Create;
  InitializeCriticalSection(plCritSect);
  plCount := 0;
End;
{--------}

Destructor TfsPadlock.Destroy;
Begin
  DeleteCriticalSection(plCritSect);
  Inherited Destroy;
End;
{--------}

Function TfsPadlock.GetLocked: boolean;
Begin
  Result := plCount > 0;
End;
{--------}

Procedure TfsPadlock.Lock;
Begin
  If IsMultiThread Then
    Begin
      EnterCriticalSection(plCritSect);
      inc(plCount);
    End;
End;
{--------}

Procedure TfsPadlock.Unlock;
Begin
  If (plCount > 0) Then
    Begin
      dec(plCount);
      LeaveCriticalSection(plCritSect);
    End;
End;
{====================================================================}

{===Mutex pool=======================================================}

Constructor TfsMutexPool.Create(Const initialCount, retainCount: Integer);
Var
  aHandle: THandle;
  Index: Integer;
Begin
  Inherited Create;
  mpList := TFSSpecList.Create;
  mpRetainCount := retainCount;
  mpPadLock := TfsPadlock.Create;

  { Create the initial set of mutexes. }
  For Index := 1 To initialCount Do
    Begin
      aHandle := CreateMutex(Nil, False, Nil);
      mpList.Append(aHandle);
    End;
End;
{--------}

Destructor TfsMutexPool.Destroy;
Begin
  mpList.Free;
  mpPadLock.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsMutexPool.Flush;
Var
  Index: Integer;
Begin
  mpPadLock.Lock;
  Try
    If mpRetainCount < mpList.Count Then
      For Index := pred(mpList.Count) Downto mpRetainCount Do {!!.01}
        mpList.DeleteAt(Index);
  Finally
    mpPadLock.Unlock;
  End;
End;
{--------}

Function TfsMutexPool.Get: THandle;
Var
  aCount: Longint;
Begin
  mpPadLock.Lock;
  Try
    If mpList.IsEmpty Then
      Result := CreateMutex(Nil, False, Nil)
    Else
      Begin
        { Get the last item in the list.  This speeds up the RemoveAt
          operation incredibly since it won't have to shift any bytes in the
          list. }
        aCount := pred(mpList.Count);
        Result := mpList.Handles[aCount];
        mpList.RemoveAt(aCount);
      End;
  Finally
    mpPadLock.Unlock;
  End;
End;
{--------}

Procedure TfsMutexPool.Put(Const aHandle: THandle);
Begin
  mpPadLock.Lock;
  Try
    mpList.Append(aHandle);
  Finally
    mpPadLock.Unlock;
  End;
End;
{====================================================================}

{===Semaphore pool===================================================}

Constructor TfsSemaphorePool.Create(Const initialCount, retainCount: Integer);
Var
  aHandle: THandle;
  Index: Integer;
Begin
  Inherited Create;
  spList := TFSSpecList.Create;
  spRetainCount := retainCount;
  spPadLock := TfsPadlock.Create;

  { Create the initial set of semaphores. }
  For Index := 1 To initialCount Do
    Begin
      aHandle := CreateSemaphore(Nil, 0, fscl_MaxBlockedThreads, Nil);
      spList.Append(aHandle);
    End;
End;
{--------}

Destructor TfsSemaphorePool.Destroy;
Begin
  spList.Free;
  spPadLock.Free;
  Inherited Destroy;
End;
{--------}

Procedure TfsSemaphorePool.Flush;
Var
  Index: Integer;
Begin
  spPadLock.Lock;
  Try
    If spRetainCount < spList.Count Then
      For Index := pred(spList.Count) Downto spRetainCount Do {!!.01}
        spList.DeleteAt(Index);
  Finally
    spPadLock.Unlock;
  End;
End;
{--------}

Function TfsSemaphorePool.Get: THandle;
Var
  aCount: Longint;
Begin
  spPadLock.Lock;
  Try
    If spList.IsEmpty Then
      Result := CreateSemaphore(Nil, 0, fscl_MaxBlockedThreads, Nil)
    Else
      Begin
        { Get the last item in the list.  This speeds up the RemoveAt
          operation incredibly since it won't have to shift any bytes in the
          list. }
        aCount := pred(spList.Count);
        Result := spList.Handles[aCount];
        spList.RemoveAt(aCount);
      End;
  Finally
    spPadLock.Unlock;
  End;
End;
{Begin !!.06}
{--------}

Procedure TfsSemaphorePool.GetTwo(Var aHandle1,
  aHandle2: THandle);
Var
  aCount, i: Longint;
Begin
  spPadLock.Lock;
  Try
    aCount := spList.FCount;
    If (aCount < 2) Then
      Begin
        For i := 1 To fscl_InitialSemCount Do
          spList.Append(CreateSemaphore(Nil, 0, fscl_MaxBlockedThreads, Nil));
        aCount := aCount + fscl_InitialSemCount;
      End;
    { Get the last items in the list.  This speeds up the RemoveAt
      operation incredibly since it won't have to shift any bytes in the
      list. }
    aCount := aCount - 1;
    aHandle1 := spList.Handles[aCount];
    spList.RemoveAt(aCount);
    aCount := aCount - 1;
    aHandle2 := spList.Handles[aCount];
    spList.RemoveAt(aCount);
  Finally
    spPadLock.Unlock;
  End;
End;
{End !!.06}
{--------}

Procedure TfsSemaphorePool.Put(Const aHandle: THandle);
Begin
  spPadLock.Lock;
  Try
    spList.Append(aHandle);
  Finally
    spPadLock.Unlock;
  End;
End;
{====================================================================}

{$IFDEF UseEventPool}
{===Event pool=======================================================}

Constructor TFSEvent.Create(Const initialCount, retainCount: Integer);
Var
  aHandle: THandle;
  Index: Integer;
Begin
  Inherited Create;
  epList := TFSSpecList.Create;
  epRetainCount := RetainCount;
  epPadLock := TfsPadlock.Create;

  { Create the initial set of mutexes. }
  For Index := 1 To InitialCount Do
    Begin
      aHandle := CreateEvent(Nil, False, False, Nil); // manual reset, start signaled
      epList.Append(aHandle);
    End;
End;
{--------}

Destructor TFSEvent.Destroy;
Begin
  epList.Free;
  epPadLock.Free;
  Inherited Destroy;
End;
{--------}

Procedure TFSEvent.Flush;
Var
  Index: Integer;
Begin
  epPadLock.Lock;
  Try
    If epRetainCount < epList.Count Then
      For Index := Pred(epList.Count) Downto Pred(epRetainCount) Do
        epList.DeleteAt(Index);
  Finally
    epPadLock.Unlock;
  End;
End;
{--------}

Function TFSEvent.Get: THandle;
Var
  aCount: Longint;
Begin
  epPadLock.Lock;
  Try
    If epList.IsEmpty Then
      Result := CreateEvent(Nil, False, False, Nil) // manual reset, start signaled
    Else
      Begin
        { Get the last item in the list.  This speeds up the RemoveAt
          operation incredibly since it won't have to shift any bytes in the
          list. }
        aCount := Pred(epList.Count);
        Result := epList.Handles[aCount];
        epList.RemoveAt(aCount);
      End;
  Finally
    epPadLock.Unlock;
  End;
End;
{--------}

Procedure TFSEvent.Put(Const aHandle: THandle);
Begin
  epPadLock.Lock;
  Try
    epList.Append(aHandle);
  Finally
    epPadLock.Unlock;
  End;
End;
{=====================================================================}
{$ENDIF}

{== Memory pool ======================================================}
Type
  PffPoolItem = ^TffPoolItem;
  TffPoolItem = pointer {PffPoolItem};
  {--------}

Constructor TfsMemoryPool.Create(ItemSize: TffMemSize;
  ItemsInBlock: Integer);
Const
  BlockSizeAdjustment = SizeOf(TfsMemBlockInfo);

  MaxBlockSize = (64 * 1024) + (BlockSizeAdjustment * 2);
  {-We add a little bit of pad to account for a) the info stored at the
    beginning of each block and b) each item having a pointer back to the
    usage counter.  When we get up to the 32768 & 65536 item sizes, we
    need to make sure that at least 2 and 1 items are allocated,
    respectively.

    Note: Block size should not exceed 64k.  We use a Word to store an offset
    back to the block's usage counter.  The max value of a Word is 65535.
    Going over 64k block size leads to us storing a pointer to the usage
    counter instead of an offset. }
Var
  RealItemSize: Integer;
  TestSize: Longint;
Const
  MinItemSize = SizeOf(Word) + SizeOf(Pointer);
  {-An item must have room for an offset back to the block's usage counter
    & a pointer to the next free item. }
Begin

  { Calculate the minimum item size. }
  FItemSize := FFMaxL(ItemSize, MinItemSize);
  FItemsInBlock := ItemsInBlock;

  { Calculate # of bytes required for ItemsInBlock.  Real item size is the asked
    for ItemSize + 2 bytes.  The extra 2 bytes are for an offset that leads us
    back to the block's usage counter. }
  RealItemSize := FItemSize + sizeof(Word);
  TestSize := (RealItemSize * FItemsInBlock) + BlockSizeAdjustment;

  { If the number of items would require more bytes than the max block size
    then recalculate the number of items that we can hold in the max block
    size. }
  If (TestSize > MaxBlockSize) Then
    Begin
      FItemsInBlock := (MaxBlockSize - BlockSizeAdjustment) Div RealItemSize;
      TestSize := (RealItemSize * FItemsInBlock) + BlockSizeAdjustment;
    End;
  FBlockSize := TestSize;
  mpPadlock := TfsPadlock.Create;
End;
{--------}

Destructor TfsMemoryPool.Destroy;
Var
  Temp: PfsMemBlockInfo;
  Next: PfsMemBlockInfo;
Begin
  mpPadlock.Lock;
  Try
    Temp := FFirstBlock;
    While Assigned(Temp) Do
      Begin
        Next := Temp^.NextBlock;
        FreeMem(Temp, FBlockSize);
        Temp := Next;
      End;
  Finally
    mpPadlock.Unlock;
    mpPadlock.Free;
  End; {try..finally}
  Inherited Destroy; {!!.01}
End;
{--------}

Procedure TfsMemoryPool.mpAddBlock;
Var
  aBlock: PfsMemBlockInfo;
  Temp: PAnsiChar;
  Prev: Pointer;
  i: Integer;
Begin
  {$IFDEF MemPoolTrace}
  writeLn(Log, format('%d %d %d: Add block',
    [GetTickCount, FItemSize, GetCurrentThreadID]));
  flush(log);
  {$ENDIF}
  { Get pool, set links & usage counter. }
  GetMem(aBlock, FBlockSize);
  aBlock^.NextBlock := FFirstBlock;
  aBlock^.UsageCounter := 0;
  FFirstBlock := aBlock;
  Temp := PAnsiChar(aBlock);

  { Move to the first item in the block. }
  inc(Temp, sizeof(pointer) + sizeOf(Longint));

  { Set up the available item list. }
  Prev := Nil;
  For i := 0 To pred(FItemsInBlock) Do
    Begin

      { First 2 bytes are an offset back to usage counter. }
      PWord(Temp)^ := Temp - PAnsiChar(aBlock);

      { Next 4 bytes is the start of the item and it points to the previous
        available item. }
      inc(Temp, sizeOf(Word));
      PffPoolItem(Temp)^ := Prev;
      Prev := Temp;

      { Move to the next available item. }
      inc(Temp, FItemSize);
    End;
  FFreeList := Prev;
End;
{--------}

Function TfsMemoryPool.Alloc: Pointer;
Var
  aBlock: PfsMemBlockInfo;
  {$IFDEF MemPoolTrace}
  PtrString, PtrString2: Array[0..8] Of AnsiChar;
  {$ENDIF}
  Temp: PAnsiChar;
Begin
  {$IFDEF MemPoolTrace}
  WriteLn(Log, Format('%d, Block count %d', [FItemSize, BlockCount]));
  {$ENDIF}
  mpPadlock.Lock;
  Try
    If Not Assigned(FFreeList) Then
      mpAddBlock;
    Result := FFreeList;
    FFreeList := PffPoolItem(Result)^;

    {$IFDEF MemPoolTrace}
    FFPointerAsHex(PtrString, Result);
    FFPointerAsHex(PtrString2, FFreelist);
    writeLn(log, format('%d %d %d: Alloc, Result = %s, FFreeList = %s',
      [GetTickCount, FItemSize, GetCurrentThreadID,
      PtrString, PtrString2]));
    flush(log);
    {$ENDIF}

    { Get the offset to the start of the block.  It is in the 2 bytes just
      prior to the newly-allocated item. }
    Temp := Result;
    dec(Temp, sizeOf(Word));

    { Move back to the start of the block. }
    dec(Temp, PWord(Temp)^);
    aBlock := PfsMemBlockInfo(Temp);

    { Increment the usage counter. }
    inc(aBlock^.UsageCounter);

  Finally
    mpPadlock.UnLock;
  End; {try..finally}
End;
{--------}

Function TfsMemoryPool.BlockCount: Longint;
Var
  Temp: PfsMemBlockInfo;
Begin
  Result := 0;
  mpPadlock.Lock;
  Try
    Temp := FFirstBlock;
    While Assigned(Temp) Do
      Begin
        inc(Result);
        Temp := Temp^.NextBlock;
      End;
  Finally
    mpPadlock.Unlock;
  End; {try..finally}
End;
{--------}

Function TfsMemoryPool.BlockUsageCount(Const BlockIndex: Longint): Longint;
Var
  Index: Longint;
  Temp: PfsMemBlockInfo;
Begin
  Result := -1;
  Index := 0;
  mpPadlock.Lock;
  Try
    Temp := FFirstBlock;
    While Assigned(Temp) And (Index <= BlockIndex) Do
      Begin
        If Index = BlockIndex Then
          Begin
            { We have found the right block.  Return the usage counter. }
            Result := Temp^.UsageCounter;
            break;
          End
        Else
          Begin
            inc(Index);
            Temp := Temp^.NextBlock;
          End;
      End;
  Finally
    mpPadlock.Unlock;
  End; {try..finally}
End;
{--------}

Procedure TfsMemoryPool.Dispose(Var P);
Var
  aBlock: PfsMemBlockInfo;
  Pt: pointer Absolute P;
  {$IFDEF MemPoolTrace}
  PtrString: Array[0..8] Of AnsiChar;
  PtrString2: Array[0..8] Of AnsiChar;
  {$ENDIF}
  Temp: PAnsiChar;
Begin
  mpPadlock.Lock;
  Try
    {$IFDEF MemPoolTrace}
    FFPointerAsHex(PtrString, Pt);
    FFPointerAsHex(PtrString2, FFreeList);
    writeLn(log, format('%d %d %d: Dispose, Ptr = %s, FFreeList = %s',
      [GetTickCount, FItemSize, GetCurrentThreadID,
      PtrString, PtrString2]));
    flush(log);
    {$ENDIF}

    PffPoolItem(Pt)^ := FFreeList;
    FFreeList := Pt;

    { Get the offset to the start of the block. }
    Temp := FFreeList;
    dec(Temp, sizeOf(Word));

    { Move back to the start of the block. }
    dec(Temp, PWord(Temp)^);

    { Decrement the usage counter. }
    aBlock := PfsMemBlockInfo(Temp);
    dec(aBlock^.UsageCounter);

    Pt := Nil;
  Finally
    mpPadlock.UnLock;
  End; {try..finally}
End;
{--------}

Procedure TfsMemoryPool.mpCleanFreeList(Const BlockStart: Pointer);
Var
  BlockEnd: Pointer;
  ItemsFound: Longint;
  Prev: Pointer;
  Temp: Pointer;
Begin
  { Scan through the free list.  If we find an item that falls within the
    bounds of the block being freed then remove that item from the chain.
    Stop the scan when all of the block's items have been found. }
  BlockEnd := PAnsiChar(BlockStart) + FBlockSize;
  ItemsFound := 0;

  { Prev points to the last good item. }
  Prev := Nil;
  Temp := FFreeList;

  While assigned(Temp) And (ItemsFound < FItemsInBlock) Do
    Begin
      { Does this item fall within the bounds of the freed block? }
      If (PAnsiChar(Temp) > BlockStart) And (PAnsiChar(Temp) <= BlockEnd) Then
        Begin
          { Yes.  Increment item count.  }
          inc(ItemsFound);

          { Is this item the head of the free list? }
          If Temp = FFreeList Then
            { Yes.  Update the head of the free list. }
            FFreeList := PffPoolItem(Temp)^
          Else
            Begin
              { No.  Point the previous item to the next item. }
              PffPoolItem(Prev^) := PffPoolItem(Temp^);
            End;

          { Move to the next item. }
          Temp := PffPoolItem(Temp)^;

        End
      Else
        Begin
          { No.  Move to next item. }
          Prev := Temp;
          Temp := PffPoolItem(Temp)^;
        End;
    End;
End;
{--------}

Function TfsMemoryPool.RemoveUnusedBlocks: Integer;
Var
  Next: PfsMemBlockInfo;
  Prev: PfsMemBlockInfo;
  Temp: PfsMemBlockInfo;
Begin
  mpPadlock.Lock;
  Result := 0;
  Try
    { Loop through the chain of blocks, looking for those blocks with usage
      count = 0. }
    Prev := Nil;
    Temp := FFirstBlock;
    While assigned(Temp) Do
      Begin
        { Grab the pointer to the next block. }
        Next := Temp^.NextBlock;

        { Is this block's usage counter = 0? }
        If Temp^.UsageCounter = 0 Then
          Begin
            { Yes.  Is this the first block in the chain? }
            If Temp = FFirstBlock Then
              { Yes.  Set first block = next block in chain. }
              FFirstBlock := Next
            Else If assigned(Prev) Then
              { No.  Update the previous block's Next Block pointer. }
              Prev^.NextBlock := Next;
            { Remove the block's items from the free list. }
            mpCleanFreeList(Temp);
            { Free the block. }
            FreeMem(Temp, FBlockSize);
            inc(Result);
          End
        Else
          { No.  Update the pointer to the previous block. }
          Prev := Temp;

        { Position to the next block. }
        Temp := Next;
      End;
  Finally
    mpPadlock.Unlock;
  End
End;
{=====================================================================}

{== Initialization/Finalization ======================================}

Procedure FinalizeUnit;
Var
  Inx: Integer;
Begin
  FFSemPool.Free;
  {$IFDEF UseEventPool}
  FFEventPool.Free;
  {$ENDIF}
  For Inx := 0 To 91 Do
    FFMemPools[Inx].Free;

  {$IFDEF MemPoolTrace}
  {Close the log}
  System.Close(Log);
  {$ENDIF}
End;
{--------}

Procedure InitializeUnit;
Var
  Inx: Integer;
Begin
  FillChar(ValidPhysicalConversions, SizeOf(ValidPhysicalConversions), #00);
  ValidPhysicalConversions[fstUInt8, fstUInt8] := True;
  ValidPhysicalConversions[fstUInt8, fstUInt16] := True;
  ValidPhysicalConversions[fstUInt8, fstUInt32] := True;
  ValidPhysicalConversions[fstUInt8, fstInt8] := True;
  ValidPhysicalConversions[fstUInt8, fstInt16] := True;
  ValidPhysicalConversions[fstUInt8, fstInt32] := True;
  ValidPhysicalConversions[fstUInt8, fstSingle] := True;
  ValidPhysicalConversions[fstUInt8, fstDouble] := True;
  ValidPhysicalConversions[fstUInt8, fstExtended] := True;
  ValidPhysicalConversions[fstUInt8, fstInt64] := True;
  ValidPhysicalConversions[fstUInt8, fstRecVersion] := True;
  ValidPhysicalConversions[fstUInt8, fstCurrency] := True;
  ValidPhysicalConversions[fstUInt8, fstAutoInc32] := True;
  ValidPhysicalConversions[fstUInt8, fstAutoInc64] := True;

  // from fstUInt16
  ValidPhysicalConversions[fstUInt16, fstUInt16] := True;
  ValidPhysicalConversions[fstUInt16, fstUInt32] := True;
  ValidPhysicalConversions[fstUInt16, fstInt32] := True;
  ValidPhysicalConversions[fstUInt16, fstSingle] := True;
  ValidPhysicalConversions[fstUInt16, fstDouble] := True;
  ValidPhysicalConversions[fstUInt16, fstExtended] := True;
  ValidPhysicalConversions[fstUInt16, fstInt64] := True;
  ValidPhysicalConversions[fstUInt16, fstRecVersion] := True;
  ValidPhysicalConversions[fstUInt16, fstCurrency] := True;
  ValidPhysicalConversions[fstUInt16, fstAutoInc32] := True;
  ValidPhysicalConversions[fstUInt16, fstAutoInc64] := True;

  // from fstUInt32
  ValidPhysicalConversions[fstUInt32, fstUInt32] := True;
  ValidPhysicalConversions[fstUInt32, fstSingle] := True;
  ValidPhysicalConversions[fstUInt32, fstDouble] := True;
  ValidPhysicalConversions[fstUInt32, fstExtended] := True;
  ValidPhysicalConversions[fstUInt32, fstInt64] := True;
  ValidPhysicalConversions[fstUInt32, fstRecVersion] := True;
  ValidPhysicalConversions[fstUInt32, fstCurrency] := True;
  ValidPhysicalConversions[fstUInt32, fstAutoInc32] := True;
  ValidPhysicalConversions[fstUInt32, fstAutoInc64] := True;

  // --- Signed Integers -------------------------------------------------------
  // from fstInt8
  ValidPhysicalConversions[fstInt8, fstInt8] := True;
  ValidPhysicalConversions[fstInt8, fstInt16] := True;
  ValidPhysicalConversions[fstInt8, fstInt32] := True;
  ValidPhysicalConversions[fstInt8, fstSingle] := True;
  ValidPhysicalConversions[fstInt8, fstDouble] := True;
  ValidPhysicalConversions[fstInt8, fstExtended] := True;
  ValidPhysicalConversions[fstInt8, fstInt64] := True;
  ValidPhysicalConversions[fstInt8, fstRecVersion] := True;
  ValidPhysicalConversions[fstInt8, fstCurrency] := True;
  // from fstInt16
  ValidPhysicalConversions[fstInt16, fstInt16] := True;
  ValidPhysicalConversions[fstInt16, fstInt32] := True;
  ValidPhysicalConversions[fstInt16, fstSingle] := True;
  ValidPhysicalConversions[fstInt16, fstDouble] := True;
  ValidPhysicalConversions[fstInt16, fstExtended] := True;
  ValidPhysicalConversions[fstInt16, fstInt64] := True;
  ValidPhysicalConversions[fstInt16, fstRecVersion] := True;
  ValidPhysicalConversions[fstInt16, fstCurrency] := True;
  // from fstInt32
  ValidPhysicalConversions[fstInt32, fstInt32] := True;
  ValidPhysicalConversions[fstInt32, fstSingle] := True;
  ValidPhysicalConversions[fstInt32, fstDouble] := True;
  ValidPhysicalConversions[fstInt32, fstExtended] := True;
  ValidPhysicalConversions[fstInt32, fstInt64] := True;
  ValidPhysicalConversions[fstInt32, fstRecVersion] := True;
  ValidPhysicalConversions[fstInt32, fstCurrency] := True;
  // --- IEEE Floating Point Numbers -------------------------------------------
  // from fstSingle
  ValidPhysicalConversions[fstSingle, fstSingle] := True;
  ValidPhysicalConversions[fstSingle, fstDouble] := True;
  ValidPhysicalConversions[fstSingle, fstExtended] := True;
  ValidPhysicalConversions[fstSingle, fstCurrency] := True;
  // from fstDouble
  ValidPhysicalConversions[fstDouble, fstDouble] := True;
  ValidPhysicalConversions[fstDouble, fstExtended] := True;
  ValidPhysicalConversions[fstDouble, fstCurrency] := True;
  // from fstExtended
  ValidPhysicalConversions[fstExtended, fstExtended] := True;
  ValidPhysicalConversions[fstExtended, fstCurrency] := True;
  // from fstInt64
  ValidPhysicalConversions[fstInt64, fstInt64] := True;
  ValidPhysicalConversions[fstInt64, fstExtended] := True;
  ValidPhysicalConversions[fstInt64, fstRecVersion] := True;
  ValidPhysicalConversions[fstInt64, fstCurrency] := True;

  ValidPhysicalConversions[fstRecVersion, fstRecVersion] := True;
  ValidPhysicalConversions[fstRecVersion, fstExtended] := True;
  ValidPhysicalConversions[fstRecVersion, fstInt64] := True;
  ValidPhysicalConversions[fstRecVersion, fstCurrency] := True;

  // --- Currency Values -------------------------------------------------------
  // from fstCurrency
  ValidPhysicalConversions[fstCurrency, fstDouble] := True;
  ValidPhysicalConversions[fstCurrency, fstCurrency] := True;
  ValidPhysicalConversions[fstCurrency, fstExtended] := True;

  // --- Characters and Strings ------------------------------------------------
  // from fstSingleChar
  ValidPhysicalConversions[fstSingleChar, fstSingleChar] := True;
  ValidPhysicalConversions[fstSingleChar, fstShortString] := True;
  ValidPhysicalConversions[fstSingleChar, fstVarNullString] := True;
  ValidPhysicalConversions[fstSingleChar, fstNullString] := True;
  ValidPhysicalConversions[fstSingleChar, fstVarWideString] := True;
  ValidPhysicalConversions[fstSingleChar, fstSingleWideChar] := True;
  ValidPhysicalConversions[fstSingleChar, fstWideString] := True;
  // from fstShortString
  ValidPhysicalConversions[fstShortString, fstSingleChar] := True;
  ValidPhysicalConversions[fstShortString, fstShortString] := True;
  ValidPhysicalConversions[fstShortString, fstVarNullString] := True;
  ValidPhysicalConversions[fstShortString, fstNullString] := True;
  ValidPhysicalConversions[fstShortString, fstVarWideString] := True;
  ValidPhysicalConversions[fstShortString, fstSingleWideChar] := True;
  ValidPhysicalConversions[fstShortString, fstWideString] := True;
  // from fstVarNullString
  ValidPhysicalConversions[fstVarNullString, fstSingleChar] := True;
  ValidPhysicalConversions[fstVarNullString, fstShortString] := True;
  ValidPhysicalConversions[fstVarNullString, fstVarNullString] := True;
  ValidPhysicalConversions[fstVarNullString, fstNullString] := True;
  ValidPhysicalConversions[fstVarNullString, fstVarWideString] := True;
  ValidPhysicalConversions[fstVarNullString, fstSingleWideChar] := True;
  ValidPhysicalConversions[fstVarNullString, fstWideString] := True;
  // from fstNullString
  ValidPhysicalConversions[fstNullString, fstSingleChar] := True;
  ValidPhysicalConversions[fstNullString, fstShortString] := True;
  ValidPhysicalConversions[fstNullString, fstVarNullString] := True;
  ValidPhysicalConversions[fstNullString, fstNullString] := True;
  ValidPhysicalConversions[fstNullString, fstVarWideString] := True;
  ValidPhysicalConversions[fstNullString, fstSingleWideChar] := True;
  ValidPhysicalConversions[fstNullString, fstWideString] := True;
  // from fstVarWideString
  ValidPhysicalConversions[fstVarWideString, fstSingleChar] := True;
  ValidPhysicalConversions[fstVarWideString, fstShortString] := True;
  ValidPhysicalConversions[fstVarWideString, fstVarNullString] := True;
  ValidPhysicalConversions[fstVarWideString, fstNullString] := True;
  ValidPhysicalConversions[fstVarWideString, fstVarWideString] := True;
  ValidPhysicalConversions[fstVarWideString, fstSingleWideChar] := True;
  ValidPhysicalConversions[fstVarWideString, fstWideString] := True;
  // from fstSingleWideChar
  ValidPhysicalConversions[fstSingleWideChar, fstSingleChar] := True;
  ValidPhysicalConversions[fstSingleWideChar, fstShortString] := True;
  ValidPhysicalConversions[fstSingleWideChar, fstVarNullString] := True;
  ValidPhysicalConversions[fstSingleWideChar, fstNullString] := True;
  ValidPhysicalConversions[fstSingleWideChar, fstVarWideString] := True;
  ValidPhysicalConversions[fstSingleWideChar, fstSingleWideChar] := True;
  ValidPhysicalConversions[fstSingleWideChar, fstWideString] := True;
  // from fstWideString
  ValidPhysicalConversions[fstWideString, fstSingleChar] := True;
  ValidPhysicalConversions[fstWideString, fstShortString] := True;
  ValidPhysicalConversions[fstWideString, fstVarNullString] := True;
  ValidPhysicalConversions[fstWideString, fstNullString] := True;
  ValidPhysicalConversions[fstWideString, fstVarWideString] := True;
  ValidPhysicalConversions[fstWideString, fstSingleWideChar] := True;
  ValidPhysicalConversions[fstWideString, fstWideString] := True;
  // --- Date and Time Values --------------------------------------------------
  // from fstDateTime
  ValidPhysicalConversions[fstDateTime, fstDateTime] := True;
  ValidPhysicalConversions[fstDateTime, fstDate] := True;
  ValidPhysicalConversions[fstDateTime, fstTime] := True;
  // from fstDate
  ValidPhysicalConversions[fstDate, fstDateTime] := True;
  ValidPhysicalConversions[fstDate, fstDate] := True;
  // from fstTime
  ValidPhysicalConversions[fstTime, fstDateTime] := True;
  ValidPhysicalConversions[fstTime, fstTime] := True;
  // --- Autoincrement Values --------------------------------------------------
  // from fstAutoInc32
  ValidPhysicalConversions[fstAutoInc32, fstUInt32] := True;
  ValidPhysicalConversions[fstAutoInc32, fstDouble] := True;
  ValidPhysicalConversions[fstAutoInc32, fstExtended] := True;
  ValidPhysicalConversions[fstAutoInc32, fstInt64] := True;
  ValidPhysicalConversions[fstAutoInc32, fstRecVersion] := True;
  ValidPhysicalConversions[fstAutoInc32, fstCurrency] := True;
  ValidPhysicalConversions[fstAutoInc32, fstAutoInc32] := True;

  ValidPhysicalConversions[fstAutoInc64, fstExtended] := True;
  ValidPhysicalConversions[fstAutoInc64, fstInt64] := True;
  ValidPhysicalConversions[fstAutoInc64, fstRecVersion] := True;
  ValidPhysicalConversions[fstAutoInc64, fstCurrency] := True;
  ValidPhysicalConversions[fstAutoInc64, fstAutoInc64] := True;

  // --- Logical Values --------------------------------------------------------
  // from fstBoolean
  ValidPhysicalConversions[fstBoolean, fstBoolean] := True;
  // ... False ---> 0, True ---> 1
  ValidPhysicalConversions[fstBoolean, fstUInt8] := True;
  ValidPhysicalConversions[fstBoolean, fstUInt16] := True;
  ValidPhysicalConversions[fstBoolean, fstUInt32] := True;
  ValidPhysicalConversions[fstBoolean, fstInt8] := True;
  ValidPhysicalConversions[fstBoolean, fstInt16] := True;
  ValidPhysicalConversions[fstBoolean, fstInt32] := True;
  ValidPhysicalConversions[fstBoolean, fstInt64] := True;
  ValidPhysicalConversions[fstBoolean, fstRecVersion] := True;
  // ... False ---> 'N', True ---> 'Y'
  ValidPhysicalConversions[fstBoolean, fstSingleChar] := True;
  ValidPhysicalConversions[fstBoolean, fstShortString] := True;
  ValidPhysicalConversions[fstBoolean, fstVarNullString] := True;
  ValidPhysicalConversions[fstBoolean, fstNullString] := True;
  ValidPhysicalConversions[fstBoolean, fstVarWideString] := True;
  ValidPhysicalConversions[fstBoolean, fstSingleWideChar] := True;
  ValidPhysicalConversions[fstBoolean, fstWideString] := True;
  // --- Arrays ----------------------------------------------------------------
  // from fstUInt8Array
  ValidPhysicalConversions[fstArrayUInt8, fstUInt8] := True;
  ValidPhysicalConversions[fstArrayUInt8, fstBLOB] := True;
  ValidPhysicalConversions[fstArrayUInt16, fstArrayUInt16] := True;
  ValidPhysicalConversions[fstArrayUInt16, fstBLOB] := True;
  ValidPhysicalConversions[fstArrayInt32, fstArrayInt32] := True;
  ValidPhysicalConversions[fstArrayInt32, fstBLOB] := True;
  ValidPhysicalConversions[fstArrayDouble, fstArrayDouble] := True;
  ValidPhysicalConversions[fstArrayDouble, fstBLOB] := True;
  // --- Binary Large Objects --------------------------------------------------
  // from fstBLOB
  ValidPhysicalConversions[fstBLOB, fstBLOB] := True;
  ValidPhysicalConversions[fstBLOB, fstBLOBMemo] := True;
  ValidPhysicalConversions[fstBLOB, fstBLOBGraphic] := True;
  // from fstBLOBMemo
  ValidPhysicalConversions[fstBLOBMemo, fstBLOB] := True;
  ValidPhysicalConversions[fstBLOBMemo, fstBLOBMemo] := True;
  ValidPhysicalConversions[fstBLOBMemo, fstBLOBGraphic] := True;
  // from fstBLOBGraphic
  ValidPhysicalConversions[fstBLOBGraphic, fstBLOB] := True;
  ValidPhysicalConversions[fstBLOBGraphic, fstBLOBMemo] := True;
  ValidPhysicalConversions[fstBLOBGraphic, fstBLOBGraphic] := True;

  {$IFDEF MemPoolTrace}
  {open up the log file}
  System.Assign(Log, 'MplTrace.lg');
  System.Rewrite(Log);
  {$ENDIF}

  { Create the memory pools ahead of time.  We do it now instead of during
    normal execution so that we can avoid thread A and thread B both trying
    to create the memory pool at the same time. }
  For Inx := 0 To 31 Do
    FFMemPools[Inx] := TfsMemoryPool.Create(succ(Inx) * 32, 1024);

  For Inx := 32 To 91 Do
    FFMemPools[Inx] := TfsMemoryPool.Create(1024 + ((Inx - 31) * 256), 1024);

  FFSemPool := TfsSemaphorePool.Create(fscl_InitialSemCount, fscl_RetainSemCount);

  {$IFDEF UseEventPool}
  FFEventPool := TFSEvent.Create(fscl_InitialEventCount, fscl_RetainEventCount);
  {$ENDIF}
End;
{--------}

Function PreGetDiskFreeSpaceEx(Directory: PChar;
  Var FreeAvailable,
  TotalSpace: TLargeInteger;
  TotalFree: PLargeInteger)
  : Bool; Stdcall;
Var
  SectorsPerCluster,
    BytesPerSector,
    FreeClusters,
    TotalClusters: Longword;
  Root: String;
Begin
  Root := ExtractFileDrive(Directory) + '\';
  Result := GetDiskFreeSpaceA(PChar(Root),
    SectorsPerCluster,
    BytesPerSector,
    FreeClusters,
    TotalClusters);
  If Result Then
    Begin
      FreeAvailable := SectorsPerCluster * BytesPerSector * FreeClusters;
      TotalSpace := SectorsPerCluster * BytesPerSector * TotalClusters;
    End
  Else
    Raise Exception.Create('Error checking free disk space: ' +
      SysErrorMessage(GetLastError));
End;

Function FFGetDiskFreeSpace(Const aDirectory: String): Int64;
Var
  Kernel: THandle;
  Path: Array[0..255] Of char;
  {needed for GetDiskFreeSpaceEx}
  FreeAvailable: Int64;
  TotalSpace: Int64;
Begin
  FFLLGetDiskFreeSpaceEx := @PreGetDiskFreeSpaceEx;

  { Get API routine to use to check free disk space }
  Kernel := GetModuleHandle(Windows.Kernel32);
  If (Kernel <> 0) Then
    Begin
      @FFLLGetDiskFreeSpaceEx := GetProcAddress(Kernel,
        'GetDiskFreeSpaceExA');
      If Not assigned(FFLLGetDiskFreeSpaceEx) Then
        FFLLGetDiskFreeSpaceEx := @PreGetDiskFreeSpaceEx;
    End; { if }

  StrPCopy(Path, aDirectory);
  If FFLLGetDiskFreeSpaceEx(Path, FreeAvailable, TotalSpace, Nil) Then
    Result := FreeAvailable Div 1024
  Else
    Raise Exception.Create('Error getting free disk space: %s' +
      SysErrorMessage(GetLastError));
End;

Initialization
  InitializeUnit;

Finalization
  FinalizeUnit;

End.

