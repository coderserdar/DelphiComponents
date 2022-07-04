{*********************************************************}
{* FlashFiler: General low level routines, types, etc    *}
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
unit ffllbase;

interface

uses
  Dialogs,
  Windows,
  Messages,
  SysUtils,
  ShellApi,
  Classes,
  ffconst;

{$R ffllcnst.res}
{$R ffdbcnst.res}

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

{===FlashFiler Version Number===}
{ Version number is used to determine whether or not a client can properly
  work with a server.  The client supplies its version number to the
  server and the server decides whether or not the client is compatible.

  Reasons for incompatibility:

  1. The server's version number is less than the client's.
  2. The server's major version number is greater than the client's
     major version number (at least in the case of 1.x and 2.x).

 Following release of Flash Filer 1.0, there will be NO changes to any
 message structure without a major Version increment.  VersionNumber
 div 10000 gives the standard decimal version number.

 Minor version numbers increment in steps of 2 (to allow for DOS
 timestamps).

 If a message requires changes, an updated message will be added, and
 old messages will be retained.
}
const
  ffVersionNumber : Longint = 21300; {2.13.00}
{Begin !!.11}
  ffVersion2_10   : Longint = 20000 + 01000; {2_10_00 - The last release
                                              prior to our changing the
                                              BLOB nesting algorithm }
{End !!.11}

{===FlashFiler Version Number===}
const
  {$IFDEF Delphi3}
  ffSpecialString : string = 'Release (D3)';
  {$ENDIF}
  {$IFDEF Delphi4}
  ffSpecialString : string = 'Release (D4)';
  {$ENDIF}
  {$IFDEF Delphi5}
  ffSpecialString : string = 'Release (D5)';
  {$ENDIF}
  {$IFDEF Delphi6}
  ffSpecialString : string = 'Release (D6)';
  {$ENDIF}
  {$IFDEF Delphi7}
  ffSpecialString : string = 'Release (D7)';
  {$ENDIF}
  {$IFDEF CBuilder3}
  ffSpecialString : string = 'Release (C3)';
  {$ENDIF}
  {$IFDEF CBuilder4}
  ffSpecialString : string = 'Release (C4)';
  {$ENDIF}
  {$IFDEF CBuilder5}
  ffSpecialString : string = 'Release (C5)';
  {$ENDIF}
  {$IFDEF CBuilder6}
  ffSpecialString : string = 'Release (C6)';
  {$ENDIF}


{===FlashFiler Limits===}   {  ***DO NOT ALTER***  }
const
  ffcl_INFINITE = High(DWORD);                                        {!!.06}
  ffcl_MaxIndexes = 256;             {maximum number of indexes per table}
  ffcl_MaxIndexFlds = 16;            {maximum count of fields in a composite key}
  ffcl_MaxKeyLength = 1024;          {maximum length of a key}
  ffcl_FixedBookmarkSize = 24;       {size of fixed part of a bookmark (ie, without key value)}
  ffcl_MaxBookmarkSize = ffcl_FixedBookmarkSize + ffcl_MaxKeyLength;
                                     {maximum size of a bookmark}
  ffcl_MaxBLOBLength = 2147483647;   {maximum BLOB length(i.e., 2^31)}
  ffcl_GeneralNameSize = 31;         {count of chars in a (general) name}
  ffcl_NetNameSize = 31;             {count of chars in a network name}
  ffcl_NetAddressSize = 63;          {count of chars in a network address}
  ffcl_UserNameSize = 31;            {count of chars in a user/client name}
  ffcl_ServerNameSize = 15;          {count of chars in a server name}
  ffcl_DescriptionSize = 63;         {count of chars in a description}
  ffcl_TableNameSize = 31;           {count of chars in a table name}
  ffcl_FileName = 31;                {count of chars in a filename (no drive/path/ext)}
  ffcl_Extension = 3;                {count of chars in an extension}
  ffcl_Path = 219;                   {count of chars in a directory path (excl final \)}
  ffcl_MaxPictureLength = 175;       {count of chars in a picture}
  ffcl_MaxVCheckLength = 256;        {count of bytes in a validity check value}
  ffcl_MaxBlocks = 2147483647;       {maximum number of blocks (i.e., 2^31)}
  ffcl_MaxRecords = 2147483647;      {maximum number of records (i.e., 2^31)}
  ffcl_MinRecordLength = 8;          {Minimum logical record length for the data
                                      dictionary.  We have a minimum because
                                      we must have this many bytes to hold the
                                      offset to the next deleted record.  This
                                      value does not include the leading
                                      deleted flag byte in the physical
                                      record.  }
  ffcl_MaxBlockedThreads = 50;       {maximum number of threads that may be
                                      waiting on read or write access to a
                                      data structure protected by an instance
                                      of TffReadWritePortal}
  ffcl_InitialListSize = 64;         {Initial capacity of a TffList. }
  ffcl_1KB = 1024;                   {One kilobyte. }                  {!!.06}
  ffcl_1MB = 1024 * 1024;            {One megabyte. }
  ffcl_64MB = 64 * ffcl_1MB;         {64 megabytes. }
  ffcl_64k = 64 * 1024;              {64 kbytes. }
  ffcl_InitialSemCount = 250;        {Initial # of semaphores in sem pool. }
  ffcl_RetainSemCount = 2500;        {# of semaphores to retain when flush sem pool. } {!!.01}
  ffcl_PortalTimeout = 5000;         {# milliseconds for a BeginRead or BeginWrite
                                      timeout. }
  {$IFDEF UseEventPool}
  ffcl_InitialEventCount = 250;      {Initial # of events in event pool.}
  ffcl_RetainEventCount = 2500;      {# of events to retain when flush event pool. }  {!!.01}
  {$ENDIF}


  {file-size constants}
  ffcl_FourGigabytes = $FFFFFFFE;
  ffcl_TwoGigabytes = $7FFFFFFF;
  ffcl_MaxHDFloppy = $163E00;

  {Transaction constants}
  ffcl_TrImplicit = True;
  ffcl_TrExplicit = False;

  ffcl_CollectionFrequency = 300000;
  { Default garbage collection to every 5 minutes. }

  ffcl_TempStorageSize = 20;
  { Default temporary storage size to 20 MB.}


{===Extra 'primary' types===}
type
  PffLongint = ^Longint;                    {pointer to a Longint}
  {$IFNDEF DCC4OrLater}
  PShortInt = ^ShortInt;                    {pointer to a shortint}
  {$ENDIF}
  PffDateTime = ^TDateTime;                 {pointer to a TDateTime; required
                                             because we use PDateTime but it
                                             occurs only in D5+ or BCB4+ }
  TffWord16 = word;                         {16-bit unsigned integer}
  TffWord32 = type DWORD;                   {32-bit unsigned integer}
  PffWord32 = ^TffWord32;                   {pointer to a 32-bit unsigned integer}
  PffByteArray = ^TffByteArray;             {General array of bytes}
  TffByteArray = array[0..65531] of byte;
  PffCharArray = ^TffCharArray;             {For debugging purposes. }
  TffCharArray = array[0..65531] of AnsiChar;
  PffBLOBArray = ^TffBLOBArray;
  TffBLOBArray = array [0..pred(ffcl_MaxBLOBLength)] of byte;
  TffVarMsgField = array [0..1] of byte;    {Variably sized field (for messages)}
  PffLongintArray = ^TffLongintArray;       {General array of long integers}
  TffLongintArray = array [0..16382] of Longint;
  TffShStr = string[255];                   {a length-byte string}
  PffShStr = ^TffShStr;                     {pointer to a length-byte string}
  TffResult = Longint;                      {FlashFiler result error code}
  TffMemSize = integer;                     {type for size of memory to alloc/free}
  TffPicture = string[ffcl_MaxPictureLength];
                                            {picture mask}
  TffVCheckValue = array [0..pred(ffcl_MaxVCheckLength)] of byte;
                                            {a validity check}
  PffInt64 = ^TffInt64;                     {pointer to a TffInt64}
  TffInt64 = record                         {64-bit integer for Delphi 3}
    iLow : TffWord32;
    iHigh : TffWord32;
  end;

  PffBlock = ^TffBlock; { A FlashFiler file consists of a set of blocks. }
  TffBlock = array [0..65535] of byte; { A block may be 4k, 8k, 16k, 32k, or 64k
                                         in size. }

  TffBlockSize = (ffbs4k, ffbs8k, ffbs16k, ffbs32k, ffbs64k);
  TffBlockSizes = set of TffBlockSize;

  { The following types are used to improve parameter integrity. }
{Begin !!.10}
  TffBaseID = type TffWord32;
  TffClientID   = type TffBaseID;
  TffCursorID   = type TffBaseID;
  TffDatabaseID = type TffBaseID;
  TffSessionID  = type TffBaseID;
  TffSqlStmtID  = type TffBaseID;
  TffTransID    = type TffBaseID;
{End !!.10}

{===Important constants===}
const
  ffc_BlockHeaderSizeData = 32; {was defined in FFSRBASE}
  {file extensions (must NOT include period)}
  ffc_ExtForData   : string[ffcl_Extension] = 'FF2'; {extension for main table file}
  ffc_ExtForTrans  : string[ffcl_Extension] = 'FF$'; {extension for Transaction file}
  ffc_ExtForSQL    : string[ffcl_Extension] = 'SQL'; {extension for SQL text files}
  ffc_NoClientID   : TffClientID = 0;  { Represents no clientID specified }

{===component notification constants===}
const
  ffn_Insert         = $01;
  ffn_Remove         = $02;
  ffn_Activate       = $03;
  ffn_Deactivate     = $04;
  ffn_Destroy        = $05;
  ffn_OwnerChanged   = $06;
  ffn_ConnectionLost = $0A;

{===Misc constants===}
const
  ffcCRLF = #13#10;
  ffc_W32NoValue = $FFFFFFFF;

{===Enumeration types===}
type
  TffOpenMode = (                  {Open modes for opening databases, tables}
                 omReadOnly,       {..read only mode}
                 omReadWrite);     {..read/write mode}

  TffShareMode = (                 {Share modes for opening databases, tables}
                 smExclusive,      {..exclusive, no sharing}
                 smShared,         {..allows others to Read or Write}  {!!.06}
                 smShareRead);     {..allows others to Read only}      {!!.06}

  TffLockType = (                  {Types of lock...}
                 ffltNoLock,       {..no lock at all}
                 ffltReadLock,     {..read lock (not for record locks)}
                 ffltWriteLock);   {..write lock}

  TffSearchKeyAction = (           {Key search actions...}
                 skaEqual,         {..exactly equal to supplied key}
                 skaEqualCrack,    {..equal to supplied key or on crack before
                                      next key}
                 skaGreater,       {..greater than supplied key}
                 skaGreaterEqual); {..greater than or equal to supplied key}

type
  TffFieldType = (                 {Field types for the data dictionary}
                  fftBoolean,      {..8-bit boolean flag}
                  fftChar,         {..8-bit character}
                  fftWideChar,     {..16-bit character (UNICODE)}
                  fftByte,         {..byte (8-bit unsigned integer)}
                  fftWord16,       {..16-bit unsigned integer (aka word)}
                  fftWord32,       {..32-bit unsigned integer}
                  fftInt8,         {..8-bit signed integer}
                  fftInt16,        {..16-bit signed integer}
                  fftInt32,        {..32-bit signed integer}
                  fftAutoInc,      {..32-bit unsigned integer; auto incrementing}
                  fftSingle,       {..IEEE single (4 bytes)}
                  fftDouble,       {..IEEE double (8 bytes)}
                  fftExtended,     {..IEEE extended (10 bytes)}
                  fftComp,         {..IEEE comp type (8 bytes signed integer)}
                  fftCurrency,     {..Delphi currency type (8 bytes, scaled integer)}
                  fftStDate,       {..SysTools date type (4 bytes)}
                  fftStTime,       {..SysTools time type (4 bytes)}
                  fftDateTime,     {..Delphi date/time type (8 bytes)}
                  fftBLOB,         {..variable length BLOB field - general binary data}
                  fftBLOBMemo,     {..variable length BLOB field - text memo}
                  fftBLOBFmtMemo,  {..variable length BLOB field - formatted text memo}
                  fftBLOBOLEObj,   {..variable length BLOB field - OLE object (Paradox)}
                  fftBLOBGraphic,  {..variable length BLOB field - graphics object}
                  fftBLOBDBSOLEObj,{..variable length BLOB field - OLE object (dBase)}
                  fftBLOBTypedBin, {..variable length BLOB field - typed binary data}
                  fftBLOBFile,     {..variable lenght BLOB field - external file}

                                   {..reserved enumeration elements - DO NOT USE}
                  fftReserved2, fftReserved3, fftReserved4,
                  fftReserved5, fftReserved6, fftReserved7, fftReserved8,
                  fftReserved9, fftReserved10, fftReserved11, fftReserved12,
                  fftReserved13, fftReserved14, fftReserved15, fftReserved16,
                  fftReserved17, fftReserved18, fftReserved19,

                  { NOTE: The SQL engine uses fftReserved20 to represent an
                    Interval field type. We do not yet expose this field type
                    to the outside world. }
                  fftReserved20,

                  fftByteArray,    {..array of bytes}
                  {..EVERYTHING AFTER THIS POINT MUST BE A STRING TYPE}
                  fftShortString,  {..length byte string}
                  fftShortAnsiStr, {..length byte Ansi string}
                  fftNullString,   {..null-terminated string}
                  fftNullAnsiStr,  {..null-terminated Ansi string}
                  fftWideString    {..null-terminated string of wide chars}
                  );

  TffFieldTypes = set of TffFieldType;
  TffBLOBCopyMode = (ffbcmNoCopy, ffbcmCopyFull, ffbcmCreateLink);

const
  FieldDataTypes   : array[TffFieldType] of string[16] = ( //!!was string[20]
                     'Boolean',
                     'Char',
                     'Wide Char',
                     'Byte',
                     'Word16',
                     'Word32',
                     'Int8',
                     'Int16',
                     'Int32',
                     'AutoInc',
                     'Single',
                     'Double',
                     'Extended',
                     'Comp',
                     'Currency',
                     'SysTools Date',
                     'SysTools Time',
                     'DateTime',
                     'BLOB',
                     'BLOB Memo',
                     'BLOB Fmt Memo',
                     'BLOB OLE Obj',
                     'BLOB Graphic',
                     'BLOB DBS OLE Obj',
                     'BLOB Typed Bin',
                     'BLOB File',
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
                     'Reserved16',
                     'Reserved17',
                     'Reserved18',
                     'Reserved19',
                     'Reserved20',
                     'Byte Array',
                     'ShortString',
                     'ANSI ShortString',
                     'NullString',
                     'ANSI NullString',
                     'Wide String');

const
  ffcLastBLOBType = fftBLOBFile;     {the last BLOB type, all BLOB types fall
                                      between fftBLOB and this one}

type
  TffIndexType = (                {Index types for the data dictionary}
                  itComposite,    {..composite index}
                  itUserDefined); {..user defined index}

type
  TffFileType = (                {File types for the data dictionary}
                 ftBaseFile,     {..base file: at least data & dictionary}
                 ftIndexFile,    {..index file}
                 ftBLOBFile);    {..BLOB file}

type
  TffFileName = string[ffcl_FileName];         {File name type (no drive/path/extension)}
  TffExtension = string[ffcl_Extension];       {Extension identifier type}
  TffFileNameExt = string[succ(ffcl_FileName + ffcl_Extension)];
                                               {File name + extension type}
  TffFullFileName = string[255];               {Expanded file name (inc drive/path}
  TffPath = string[ffcl_Path];                 {Complete directory path (excl final \)}
  TffMaxPathZ = array [0..pred(MAX_PATH)] of AnsiChar;
                                               {Null-terminated path&file name type}

  TffName = string[ffcl_GeneralNameSize];      {A general name type}
{Begin !!.03}
{$IFDEF IsDelphi}
  TffNetName = string[ffcl_NetNameSize];       {a network name type}
  TffNetAddress = string[ffcl_NetAddressSize]; {a network address type}
{$ELSE}
  TffNetName = string;                            {a network name type}
  TffNetAddress = string;                         {a network address type}
  TffNetNameShr = string[ffcl_NetNameSize];       {a network name type - for requests}
  TffNetAddressShr = string[ffcl_NetAddressSize]; {a network address type - for requests}
{$ENDIF}
{End !!.03}
  TffTableName = string[ffcl_TableNameSize];   {Table name type}

  TffStringZ = array [0..255] of AnsiChar;     {For converting ShortStrings to StringZs}

{ !!.06 - Following type moved from FFNETMSG }
{===Network message enums===}
type
  TffNetMsgDataType = (      {Types of network message data...}
              nmdByteArray,  {..it's an array of bytes}
              nmdStream);    {..it's a stream (TStream descendant)}

type
  TffDirItemType = (                {types of items a directory can contain}
                    ditFile,        {..file}
                    ditDirectory,   {..directory}
                    ditVolumeID);   {..VolumeID}
  TffDirItemTypeSet = set of TffDirItemType;

  TffDirItemAttr = (                 {attributes of directory items}
                    diaNormal,       {..normal}
                    diaReadOnly,     {..readonly}
                    diaHidden,       {..hidden}
                    diaSystem,       {..system}
                    diaArchive);     {..not backed up}
  TffDirItemAttrSet = set of TffDirItemAttr;

  TffSearchRec = packed record     {FlashFiler directory search record}
    srTime     : TffWord32;        {..timestamp}
    srSize     : TffWord32;        {..size (low 32 bits)}
    srSizeHigh : TffWord32;        {..size (high 32 bits, generally 0)}
    srType     : TffDirItemType;   {..type}
    srAttr     : TffDirItemAttrSet;{..attributes}
    srName     : TffFileNameExt;   {..name, including extension}
    srHandle   : THandle;          {..internal use only}
    srData     : TWin32FindData;   {..internal use only}
    srFindType : TffDirItemTypeSet;{..internal use only}
    srFindAttr : TffDirItemAttrSet;{..internal use only}
  end;

const
  diaAnyAttr : TffDirItemAttrSet =
               [diaNormal, diaReadOnly, diaHidden, diaSystem, diaArchive];


{===FlashFiler data dictionary descriptors===}
type
  TffDictItemName = string[ffcl_GeneralNameSize];  {Field/Index name type}
  TffDictItemDesc = string[ffcl_DescriptionSize];  {Field/Index description type}

  PffVCheckDescriptor = ^TffVCheckDescriptor;
  TffVCheckDescriptor = packed record {Validity check descriptor}
    vdHasMinVal : boolean;            {..true if the field has a minimum value}
    vdHasMaxVal : boolean;            {..true if the field has a maximum value}
    vdHasDefVal : boolean;            {..true if the field has a default value}
    vdFiller    : byte;
    vdMinVal    : TffVCheckValue;     {..the field's minimum value}
    vdMaxVal    : TffVCheckValue;     {..the field's maximum value}
    vdDefVal    : TffVCheckValue;     {..the field's default value}
    vdPicture   : TffPicture;         {..the field's picture clause}
  end;

  PffFieldDescriptor = ^TffFieldDescriptor;
  TffFieldDescriptor = packed record  {Field descriptor}
    fdNumber   : Longint;             {..number of field in record (zero based)}
    fdName     : TffDictItemName;     {..name of field}
    fdDesc     : TffDictItemDesc;     {..description of field}
    fdUnits    : Longint;             {..number of characters/digits etc}
    fdDecPl    : Longint;             {..number of decimal places}
    fdOffset   : Longint;             {..offset of field in record}
    fdLength   : Longint;             {..length of field in bytes}
    fdVCheck   : PffVCheckDescriptor; {..validity check (if nil, there is none)}
    fdType     : TffFieldType;        {..type of field}
    fdRequired : boolean;             {..true, if field must have a value to be stored}
    fdFiller   : array [0..1] of byte;
  end;

  TffFieldList = array [0..pred(ffcl_MaxIndexFlds)] of Longint;
                                      {List of field numbers in an index}
  TffFieldIHList = array [0..pred(ffcl_MaxIndexFlds)] of TffDictItemName;
   {List of extension functions used to build/compare an index}

  PffIndexDescriptor = ^TffIndexDescriptor;
  TffIndexDescriptor = packed record  {Index descriptor}
    idNumber : Longint;               {..number of index (zero based)}
    idName   : TffDictItemName;       {..name of index}
    idDesc   : TffDictItemDesc;       {..description of index}
    idFile   : Longint;               {..number of file containing index}
    idKeyLen : Longint;               {..length of key in bytes}
    idCount  : Longint;               {..number of fields in composite index, or}
                                      {  -1 for user defined index}
    idFields : TffFieldList;          {..field numbers for composite index}
    idFieldIHlprs : TffFieldIHList;   {..index helpers used to build/compare
                                         a composite index}
    idDups   : boolean;               {..0=no duplicate keys, 1=dups allowed}
    idAscend : boolean;               {..0=descending keys; 1=ascending keys}
    idNoCase : boolean;               {..0=case sensitive indexing; 1=case insensitive}
  end;

  PffFileDescriptor = ^TffFileDescriptor;
  TffFileDescriptor = packed record   {File descriptor}
    fdNumber    : Longint;            {..number of file (zero based)}
    fdDesc      : TffDictItemDesc;    {..description of file}
    fdExtension : TffExtension;       {..extension for file}
    fdBlockSize : Longint;            {..block size for file}
    fdType      : TffFileType;        {..type of file}
  end;

  PffAliasDescriptor = ^TffAliasDescriptor;
  TffAliasDescriptor = packed record  {Database Alias descriptor}
    adAlias : TffName;                {..alias name}
    adPath  : TffPath;                {..directory path for database}
  end;

  PffTableDescriptor = ^TffTableDescriptor;
  TffTableDescriptor = packed record
    tdTableName : TffTableName;
    tdExt       : TffExtension;
    tdSizeLo    : TffWord32;
    tdSizeHi    : TffWord32;
    tdTimeStamp : TffWord32;
  end;

{===FlashFiler information types===}
type
  PffRebuildStatus = ^TffRebuildStatus;
  TffRebuildStatus = packed record  {Rebuild operation status info}
    rsStartTime    : DWord;         {..start time (tick count from server)}{!!.10}
    rsSnapshotTime : DWord;         {..snapshot time (tick count from server)}{!!.10}
    rsTotalRecs    : Longint;       {..total count of records to read}
    rsRecsRead     : Longint;       {..count of records read}
    rsRecsWritten  : Longint;       {..count of records written}
    rsPercentDone  : Longint;       {..RecsRead*100/TotalRecs}
    rsErrorCode    : TffResult;     {..error result for process}
    rsFinished     : boolean;       {..process has finished}
  end;

  PffRecordInfo = ^TffRecordInfo;
  TffRecordInfo = packed record     {Information block for data records}
    riRecLength    : Longint;       {..record length}
    riRecCount     : Longint;       {..number of active records}
    riDelRecCount  : Longint;       {..number of deleted records}
    riRecsPerBlock : Longint;       {..number of records in each block}
  end;

  PffIndexInfo = ^TffIndexInfo;
  TffIndexInfo = packed record    {Information block for an index}
    iiKeyCount       : Longint;   {..number of keys}
    iiPageCount      : Longint;   {..number of B-Tree pages}
    iiMaxKeysPerNode : Longint;   {..maximum number of keys per node page}
    iiMaxKeysPerLeaf : Longint;   {..maximum number of keys per leaf page}
    iiKeyLength      : word;      {..length of a key in bytes}
    iiAllowDups      : boolean;   {..duplicate keys allowed}
    iiKeysAreRefs    : boolean;   {..keys are reference numbers}
    iiBTreeHeight    : integer;   {..height of the b-tree}
  end;

  PffServerStatistics = ^TffServerStatistics;          {begin !!.10}
  TffServerStatistics = packed record {Server statistics info}
    ssName               : TffNetName;
    ssVersion            : Longint;
    ssState              : ShortString;
    ssClientCount        : TffWord32;
    ssSessionCount       : TffWord32;
    ssOpenDatabasesCount : TffWord32;
    ssOpenTablesCount    : TffWord32;
    ssOpenCursorsCount   : TffWord32;
    ssRamUsed            : TffWord32;
    ssMaxRam             : TffWord32;
    ssUpTimeSecs         : DWord;
    ssCmdHandlerCount    : Integer;
  end;

  PffCommandHandlerStatistics = ^TffCommandHandlerStatistics;
  TffCommandHandlerStatistics = packed record {stats for command handler}
    csTransportCount : Integer;
  end;

  PffTransportStatisticsInfo = ^TffTransportStatistics;
  TffTransportStatistics = packed record {stats related to a transport}
    tsName           : TffNetName;
    tsState          : ShortString;
    tsAddress        : TffNetAddress;
    tsClientCount    : TffWord32;
    tsMessageCount   : TffWord32;
    tsMessagesPerSec : Double;
  end;                                                           {end !!.10}


{===Notify event declarations===}
type
  TffNetIdle = procedure(Sender : TObject);


type

  { Delphi's memory management is not suitable for a 24x7 database server.  It
    will eat up memory and eventually crash.  To avoid this problem, we
    override certain VCL classes so that we can have the VCL classes use our
    own memory manager.  The new classes are listed below. }

  TffPadlock = class;  { forward declaration }

{===FlashFiler TffObject class===}
  { All FF classes that would normally inherit from TObject must inherit
    from this class instead. }
  TffObject = class(TObject)
{Begin !!.03}
  {$IFDEF FF_DEBUG_THREADS}
  protected {private}
    ffoMethodLock      : Integer;
    ffoCurrentThreadID : Cardinal;
    ffoThreadLockCount : Integer;
  protected
    procedure ThreadEnter;
    procedure ThreadExit;
  public
  {$ENDIF}
{End !!.03}
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

{===FlashFiler TffVCLList class===}
  { All FF classes using instances of TList should use this class instead. }
  TffVCLList = class(TList)
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

{===FlashFiler TFFPersistent class===}
  { All FF classes that would normally inherit from TPersistent must inherit
    from this class instead. }
  TffPersistent = class(TPersistent)
{Begin !!.03}
  {$IFDEF FF_DEBUG_THREADS}
  protected {private}
    ffpMethodLock      : Integer;
    ffpCurrentThreadID : Cardinal;
    ffpThreadLockCount : Integer;
  protected
    procedure ThreadEnter;
    procedure ThreadExit;
  public
  {$ENDIF}
{End !!.03}
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
  end;

{===FlashFiler TFFThread class===}
  { All FF classes that would normally inherit from TThread must inherit
    from this class instead.  Our reason for doing so is that Delphi's
    memory management is not suitable for a 24x7 database server.  It will
    eat up memory and eventually crash.  This class allocates its own memory.}
  TffThread = class(TThread)
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    protected
      procedure DoTerminate; override;
        { Note: We override DoTerminate because the standard TThread.DoTerminate
          will block when it calls Synchronize if the thread was not created
          in the main thread of the application. }
{Begin !!.02}
    public
      procedure WaitForEx(const Timeout : Longint);
{End !!.02}
  end;

{===Multithread support===}
  { Use TffEvent in those situations where Object A must wait for Object B to
    tell it something has happened.  For example, a TffRequest must wait for
    a reply to be received by the sending thread of a TffLegacyTransport. }
  TffEvent = class(TffObject)
    private
      ffeEvent : THandle;  { the actual event object }
    protected
    public
      constructor Create;

      destructor Destroy; override;

      procedure WaitFor(const timeOut : TffWord32);
        {-Call this method when an object must wait for this event to be
          signalled.  Timeout is the number of milliseconds the thread should
          wait for the event.  If timeOut is <= 0 then the thread will wait
          until the event is signalled otherwise it waits the specified
          number of milliseconds.  Raises an exception if the wait times out
          or a failure occurs. }

      function WaitForQuietly(const timeOut : TffWord32) : DWORD;
        {-This method is just like the WaitFor method except that it returns
          an error code instead of raising an exception if a failure occurs.
          Possible return values:
          WAIT_ABANDONED - See MS SDK help for WaitForSingleObject. It is much
                           more mind-twisting than should be documented here.
          WAIT_OBJECT_0 - The event was signalled.
          WAIT_TIMEOUT - The timeout interval elapsed without the event being
                         signaled. }

      procedure SignalEvent;
        {-Call this method when the event is to be set/raised/signalled.
          This releases a thread that called WaitFor. }

      property Handle : THandle read ffeEvent;
        {-Returns the events handle. }

    end;

  { Use TffReadWritePortal to protect a data structure accessible by multiple
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

  TffReadWritePortal = class(TffObject)
    private
      rwpBlockedReaders : THandle;  { semaphore used to release blocked readers }
      rwpBlockedWriters : THandle;  { semaphore used to release blocked writers }
      rwpGate           : TffPadlock; { critical section allowing single-threaded
                                      access to internal data structures }
      rwpActiveReaders  : integer;  { the number of threads given read access }
      rwpActiveWriter   : boolean;  { if True then a thread has been granted
                                    write access; all other readers and writers
                                    are blocked }
      rwpActiveWriterID : TffWord32;{ the threadID of the thread granted write
                                    access }
      rwpWaitingReaders : integer;  { the number of threads waiting for read
                                    access }
      rwpWaitingWriters : integer;  { the number of threads waiting for write
                                    access }
      rwpWriterReadCount : integer;  { the number of times the active writer has
                                    called BeginRead }
      rwpWriterWriteCount : integer;  { the number of times the active writer has
                                      called BeginWrite }
    protected
    public
      constructor Create;
        {-Use this method to create an instance of TffReadWritePortal.
          maxBlockedThreads is the maximum number of reader or writer threads
          that may wait for access to the protected data structure. }
      destructor Destroy; override;
      procedure BeginRead;
        {-Call this method when a thread wants to start reading the protected
          data structure.  BeginRead will not return until the thread has been
          granted read access. Each occurrence of BeginRead must have a
          corresponding call to EndRead. }
      procedure BeginWrite;
        {-Call this method when a thread wants to start writing the protected
          data structure.  BeginWrite will not return until the thread has
          been granted write access.  Each occurrence of BeginWrite must have a
          corresponding call to EndWrite. }
      procedure EndRead;
        {-Call this method when a thread has finished reading the protected
          data structure. }
      procedure EndWrite;
        {-Call this method when a thread has finished writing to the
          protected data structure. }
    end;

  { TffPadLock allows only one reader or writer at a time.
    This class is obsolete and should be phased out. }
  TffPadLock = class {*NOT* class (TffObject)}
    protected {public}
      plCount  : integer;
      plCritSect : TRTLCriticalSection;
    protected
      function GetLocked : boolean;
    public
      constructor Create;
        {-Create a multithread padlock}
      destructor Destroy; override;
        {-Free a multithread padlock}
      procedure Lock;
      procedure Unlock;
      property Locked : boolean read GetLocked;
  end;

{===FlashFiler List and List Item classes===}
type
  TffListState = (lsNormal, lsClearing);

  TffListFindType = (        {How to find an item in a list}
              ftFromID,      {..from the item's ID}
              ftFromIndex);  {..from the index of the item}

  TffList = class;

  TffListItem = class(TffObject)
    protected {private}
      ffliList          : TffList;
      ffliFreeOnRemove  : boolean;
      ffliState         : TffListState;
      ffliMaintainLinks : boolean;
        { If True then track what lists contain this item. }

    protected
      function GetRefCount : integer;
      procedure ffliAddListLink(L : TffList);
      procedure ffliBreakListLink(L : TffList);
      procedure ffliSetMaintainLinks(const Value : Boolean);           {!!.11}
    public
      constructor Create;
        {-create the list item}
      destructor Destroy; override;
        {-destroy the list item; if the item is attached to any lists,
          it removes itself from those lists as well}

      function Compare(aKey : pointer) : integer; virtual; abstract;
        {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
          equal, >0 otherwise}
      function Key : pointer; virtual; abstract;
        {-return a pointer to this item's key}
      property FreeOnRemove : boolean
        read ffliFreeOnRemove write ffliFreeOnRemove;
       {-if true, when item is removed from one list, it removes
         itself from all lists (and hence would be freed)}
      property MaintainLinks : boolean
        read ffliMaintainLinks write ffliSetMaintainLinks;
        {-If True then track which lists contain this list item.
          Note that if you set this property after adding the item
          to one or more lists then it will already have a list
          of links to those lists.  So set it as soon as the item
          is created or pay the consequences. }
      property ReferenceCount : integer
         read GetRefCount;
        {-the number of lists referencing this item}
  end;

  PffListItemArray = ^TffListItemArray;
  TffListItemArray =
     array [0..pred(MaxInt div sizeof(TffListItem))] of TffListItem;

  TffStrListItem = class(TffListItem)
    protected {private}
      sliKey       : PffShStr;
      sliExtraData : pointer;
    protected
    public
      constructor Create(const aKey : TffShStr);
        {-create the list item; aKey is its access/sort key}
      destructor Destroy; override;
        {-destroy the list item}

      function KeyAsStr : TffShStr;
        {-return this item's key as a string (for convenience)}

      function Compare(aKey : pointer) : integer; override;
        {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
          equal, >0 otherwise}
      function Key : pointer; override;
        {-return a pointer to this item's key: it'll be a pointer to a
          shortstring}

      property ExtraData : pointer
         read sliExtraData write sliExtraData;
  end;

  TffUCStrListItem = class(TffStrListItem)
    protected {private}
    protected
    public
      function Compare(aKey : pointer) : integer; override;
        {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
          equal, >0 otherwise; case insensitive compare}
  end;

  TffI64ListItem = class(TffListItem)
    protected {private}
      iliKey : TffInt64;
      iliExtraData : Pointer;
    public
      constructor Create(const aKey : TffInt64);
        {-create the list item; aKey is its access/sort key}
      function KeyValue : TffInt64;
        {-return this item's ket as a TffInt64 (for convenience)}
      function Compare(aKey : pointer) : integer; override;
        {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
          equal, >0 otherwise}
      function Key : pointer; override;
        {-return a pointer to this item's key: it'll be a pointer to a
          TffInt64}
      property ExtraData : Pointer
         read iliExtraData write iliExtraData;
         {-The additional data item attached to the list item.}
  end;

  TffIntListItem = class(TffListItem)
    protected {private}
      iliKey : Longint;
      iliExtraData : pointer;
    protected
    public
      constructor Create(const aKey : Longint);
        {-create the list item; aKey is its access/sort key}
      function KeyAsInt : Longint;
        {-return this item's key as a Longint (for convenience)}
      function Compare(aKey : pointer) : integer; override;
        {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
          equal, >0 otherwise}
      function Key : pointer; override;
        {-return a pointer to this item's key: it'll be a pointer to a
          Longint}
      property ExtraData : pointer
         read iliExtraData write iliExtraData;
        {-The additional data item attached to the list item.}
  end;

  TffWord32ListItem = class(TffListItem)
    protected {private}
      wliKey : TffWord32;
      wliExtraData : pointer;
      wliExtraData2 : Longint;
    protected
    public
      constructor Create(const aKey : TffWord32);
        {-create the list item; aKey is its access/sort key}
      function KeyValue : TffWord32;
        {-return this item's key as a TffWord32 (for convenience)}
      function Compare(aKey : pointer) : integer; override;
        {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
          equal, >0 otherwise}
      function Key : pointer; override;
        {-return a pointer to this item's key: it'll be a pointer to a
          Longint}
      function KeyAsInt : TffWord32;
        {-return this item's key as a TffWord32 (for convenience)}
      property ExtraData : pointer
         read wliExtraData write wliExtraData;
        {-An additional data item attached to the list item.}

      property ExtraData2 : Longint
         read wliExtraData2 write wliExtraData2;
        {-An additional data item attached to the list item.}
  end;

  TffSelfListItem = class(TffIntListItem)
    protected {private}
    protected
    public
      constructor Create;
        {-create the list item; Key is the Self pointer as integer}
  end;

  TffList = class(TffObject)                                           {!!.01}
    protected {private}
      fflCapacity : Longint;
      fflCount    : Longint;
      fflList     : PffListItemArray;
      fflSorted   : boolean;
      fflPortal   : TffReadWritePortal;                                {!!.02}
      fflState    : TffListState;
    protected
      procedure fflGrow;
      function GetCapacity : Longint;
      function GetCount : Longint;
      function GetItem(const aInx : Longint) : TffListItem;
      procedure SetCapacity(const C : Longint);
      procedure SetCount(const C : Longint);
      procedure SetItem(const aInx : Longint; Item : TffListItem);
      procedure SetSorted(S : boolean);

      procedure fflDeleteAtPrim(aInx : Longint);
        {-Removes an item from the list and frees the item if its reference
          count is zero. }
      function  fflIndexPrim(const aKey) : Longint;
      procedure fflRemoveAtPrim(aInx : Longint);
        {-Removes an item from the list but does not free the item. }

      procedure InternalDelete(const aKey);                            {!!.02}
    public
      constructor Create;
        {-create the list}
      destructor Destroy; override;
        {-destroy the list}
//      procedure Assign(Source : TPersistent); override;              {Deleted !!.01}
        {-assign another list's data to this one}
      procedure Delete(const aKey);
        {-Remove an item from the list, search for it.  Note this method
          will free the item if the item's reference count is zero.}
      procedure DeleteAt(aInx : Longint);
        {-Remove an item from the list using its index.  Note this method
          will free the item if the item's reference count is zero.}
      procedure Empty;
        {-empty the list of items}
      function  Exists(const aKey) : boolean;
        {-return true if the list has an item with the given key}
      function GetInsertionPoint(aItem : TffListItem) : Longint;
        {-Returns the index into which the item would be inserted. }
      function  Insert(aItem : TffListItem) : boolean;
        {-insert an item in key sequence; return true on success}
      function  InsertPrim(aItem : TffListItem) : Longint;
        {-insert an item in key sequence; return index or -1}
      function  IsEmpty : boolean;
        {-return true if the list is empty}
      function  Index(const aKey) : Longint;
        {-calculate the index of an item with the given key}

      procedure Remove(const aKey);
        {-Use this method to remove an item from the list without freeing
          the item. }
      procedure RemoveAt(aInx : Longint);
        {-Use this method to remove an item at the specified position.  The
          item is not freed after it is removed from the list. }

      property Capacity : Longint
        {-the total capacity of the list}
         read GetCapacity write SetCapacity;

      property Count : Longint
        {-the number of items in the list}
         read GetCount write SetCount;

      property Items [const aInx : Longint] : TffListItem
        {-the list of items}
         read GetItem write SetItem;
         default;

      property Sorted : boolean
        {-true (by default) if the list is sorted; cannot set true if
          list contains items}
         read fflSorted write SetSorted;
  end;

  { This class is a threadsafe version of TffList.  This class allows multiple
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
  TffThreadList = class(TffList)
    protected {private}
//      FPortal    : TffReadWritePortal;                               {Deleted !!.02}
    public

      constructor Create; virtual;

      destructor Destroy; override;

      function BeginRead : TffThreadList;
        {-A thread must call this method to gain read access to the list.
          Returns the instance of TffThreadList as a convenience. }

      function BeginWrite : TffThreadList;
        {-A thread must call this method to gain write access to the list.
          Returns the instance of TffThreadList as a convenience.}

      procedure EndRead;
        {-A thread must call this method when it no longer needs read access
          to the list.  If it does not call this method, all writers will
          be perpetually blocked. }

      procedure EndWrite;
        {-A thread must call this method when it no longer needs write access
          to the list.  If it does not call this method, all readers and writers
          will be perpetualy blocked. }
  end;


  TffStringList = class(TffPersistent)
    protected {private}
      slCaseSensitive : boolean;
      slList          : TffList;
    protected
      function GetCapacity : Longint;
      function GetCount : Longint;
      function GetObj(aInx : Longint) : TObject;
      function GetSorted : boolean;
      function GetStr(aInx : Longint) : TffShStr;
      function GetValue(const aName : TffShStr) : TffShStr;
      procedure SetCapacity(C : Longint);
      procedure SetCaseSensitive(CS : boolean);
      procedure SetObj(aInx : Longint; const aObj : TObject);
      procedure SetStr(aInx : Longint; const aStr : TffShStr);
      procedure SetSorted(S : boolean);
      procedure SetValue(const aName, aStr : TffShStr);

    public
      constructor Create;
        {-create the list}
      destructor Destroy; override;
        {-destroy the list}
      procedure Assign(Source : TPersistent); override;
        {-assign another list's string data to this one}
      procedure AssignTo(Dest : TPersistent); override;
        {-assign this string list's data to another one}
      procedure Delete(const aStr : TffShStr);
        {-remove a string from the list, search for it}
      procedure DeleteAt(aInx : Longint);
        {-remove a string from the list using its index}
      procedure Empty;
        {-empty the list of strings}
      function  Exists(const aStr : TffShStr) : boolean;
        {-return true if the list has an item with the given string}
      function  Index(const aStr : TffShStr) : Longint;
        {-calculate the index of an item with the given string}
      function IndexOfName(const aName: TffShStr) : Longint;
        {-return the index of the name part of a string which is of
          the form Name=Value}
      function  Insert(const aStr : TffShStr) : boolean;
        {-insert an item in string sequence; return true on success}
      function  InsertPrim(const aStr : TffShStr) : Longint;
        {-insert an item in string sequence; return index or -1}
      function  IsEmpty : boolean;
        {-return true if the list is empty}

      property Capacity : Longint
        {-the total capacity of the list}
         read GetCapacity write SetCapacity;

      property CaseSensitive : boolean
         read slCaseSensitive write SetCaseSensitive;
        {-whether string compares are case sensitive or not; cannot
          set true if the list contains items}

      property Count : Longint
        {-the number of strings in the list}
         read GetCount;

      property Strings [aInx : Longint] : TffShStr
        {-the list of strings}
         read GetStr write SetStr;
         default;

      property Objects [aInx : Longint] : TObject
        {-the list of objects associated with strings}
         read GetObj write SetObj;

      property Sorted : boolean
        {-true (by default) if the list is sorted; cannot set true if
          list contains items}
         read GetSorted write SetSorted;

      property Values [const aName: TffShStr] : TffShStr
        {-returns a string value given a string keyword.  Assumes the
          list of strings consists of "keyword=value" pairs. }
        read GetValue write SetValue;
  end;

  { The following types are used by TffPointerList to store a list of pointers. }
  PffPointerArray = ^TffPointerArray;
  TffPointerArray =
     array [0..pred(MaxInt div sizeof(Pointer))] of Pointer;

  { This is an unsorted list type dealing only with pointers.  Note that it is
    the responsibility of the application to free the memory referenced by the
    pointer. }
  TffPointerList = class(TffPersistent)
    protected {private}
      plCapacity  : Longint;
      plCount     : Longint;
      plList      : PffPointerArray;
    protected

      function AppendPrim(aPtr : Pointer) : Longint;
      procedure fflGrow;
      function GetCapacity : Longint;
      function GetCount : Longint;
      function GetPointer(aInx : Longint) : Pointer;
      function GetInternalAddress : Pointer;
      procedure SetCapacity(const C : Longint);
      procedure SetCount(const C : Longint);
      procedure SetPointer(aInx : Longint; aPtr : Pointer);

      procedure fflRemoveAtPrim(aInx : Longint);
        {-Removes an item from the list but does not free the item. }

    public
      constructor Create;
        {-create the list}
      destructor Destroy; override;
        {-destroy the list}
      procedure Assign(Source : TPersistent); override;
        {-assign another list's data to this one}
      function  Append(aPtr : Pointer) : boolean;
        {-append an item to the list; return true on success}
      procedure Empty;
        {-Empty the list of pointers.  Note that the application is
          responsible for freeing the memory referenced by the pointers. }
      function  IsEmpty : boolean;
        {-return true if the list is empty}

      procedure RemoveAt(aInx : Longint);
        {-Use this method to remove the pointer at the specified position. }

      property Capacity : Longint
        {-the total capacity of the list}
         read GetCapacity write SetCapacity;

      property Count : Longint
        {-the number of items in the list}
         read GetCount write SetCount;

      property InternalAddress : pointer read GetInternalAddress;
        {-Returns a pointer to the internal list of pointers.  Be careful with
          this.  It is to be used only when necessary. }

      property List : PffPointerArray read plList;
        {-Provides direct access to the internal list of pointers.  Use this
          only if you know what you are doing. }

      property Pointers[aInx : Longint] : Pointer
        {-the list of items}
         read GetPointer write SetPointer; default;
  end;


  { The following types are used by TffHandleList to store a list of handles. }
  PffHandleArray = ^TffHandleArray;
  TffHandleArray =
     array [0..pred(MaxInt div sizeof(THandle))] of THandle;

  { This is an unsorted list type dealing only with THandles.  It is used by
    TffSemaphorePool, TffMutexPool & TffEventPool. }
  TffHandleList = class(TffPersistent)
    protected {private}
      FCapacity  : Longint;
      FCount     : Longint;
      FList      : PffHandleArray;
    protected

      function AppendPrim(aHandle : THandle) : Longint;
      procedure fflGrow;
      function GetCapacity : Longint;
      function GetCount : Longint;
      function GetHandle(aInx : Longint) : THandle;
      function GetInternalAddress : pointer;
      procedure SetCapacity(const C : Longint);
      procedure SetCount(const C : Longint);

      procedure fflDeleteAtPrim(aInx : Longint);
        {-Removes an item from the list and frees the item if its reference
          count is zero. }
      procedure fflRemoveAtPrim(aInx : Longint);
        {-Removes an item from the list but does not free the item. }

    public
      constructor Create;
        {-create the list}
      destructor Destroy; override;
        {-destroy the list}
      procedure Assign(Source : TPersistent); override;
        {-assign another list's data to this one}
      procedure DeleteAt(aInx : Longint);
        {-Remove an item from the list using its index.  Note this method
          will close the handle. }
      procedure Empty;
        {-empty the list of items}
      function  Append(aHandle : THandle) : boolean;
        {-append an item to the list; return true on success}
      function  IsEmpty : boolean;
        {-return true if the list is empty}

      procedure RemoveAll;
        {-Removes all handles from the list without closing any of the
          handles. }

      procedure RemoveAt(aInx : Longint);
        {-Use this method to remove an item at the specified position.  The
          handle is not closed after it is removed from the list. }

      property Capacity : Longint
        {-the total capacity of the list}
         read GetCapacity write SetCapacity;

      property Count : Longint
        {-the number of items in the list}
         read GetCount write SetCount;

      property InternalAddress : pointer read GetInternalAddress;
        {-Returns a pointer to the internal list of handles.  Be careful with
          this.  It is to be used only when necessary. }

      property Handles[aInx : Longint] : THandle
        {-the list of items}
         read GetHandle; default;
  end;

  { This is a thread-safe string list class.  It handles read/write access issues
    identical to TffThreadList. }
  TffThreadStringList = class(TffStringList)
  protected
    tslPortal        : TffReadWritePortal;
  public

    constructor Create;

    destructor Destroy; override;

    function BeginRead : TffThreadStringList;
      {-A thread must call this method to gain read access to the list.
        Returns the instance of TffThreadList as a convenience. }

    function BeginWrite : TffThreadStringList;
      {-A thread must call this method to gain write access to the list.
        Returns the instance of TffThreadList as a convenience. }

    procedure EndRead;
      {-A thread must call this method when it no longer needs read access
        to the list.  If it does not call this method, all writers will
        be perpetually blocked. }

    procedure EndWrite;
      {-A thread must call this method when it no longer needs write access
        to the list.  If it does not call this method, all readers and writers
        will be perpetualy blocked. }

  end;

  TffQueue = class(TffObject)
  protected
    ffqList : TffList;

    function GetCount : Longint;

    function GetItem(aInx : Longint) : TffListItem;

  public

    constructor Create;

    destructor Destroy; override;

    procedure Delete(const aKey);
      { Remove an item from the queue based upon its key. }

    function Dequeue : TffListItem;
      {-Returns the first item inserted into the queue or nil if the queue
        is empty.  The item is automatically removed from the queue. }

    procedure Enqueue(anItem : TffListItem);
      {-Add an item to the queue. }

    function IsEmpty : boolean;
      {-Returns True if the queue is empty. }

    property Count : Longint read GetCount;
      {-Returns the number of items in the queue. }

    property Items [aInx : Longint] : TffListItem read GetItem; default;
      {-The list of queued items.  Items[0] is the first item in the
        queue. }

  end;

  TffThreadQueue = class(TffQueue)
  protected
    fftqPortal        : TffReadWritePortal;
  public
    constructor Create;

    destructor Destroy; override;

    function BeginRead : TffThreadQueue;
      {-A thread must call this method to gain read access to the queue.
        Returns the instance of TffThreadQueue as a convenience. }

    function BeginWrite : TffThreadQueue;
      {-A thread must call this method to gain write access to the queue.
        Returns the instance of TffThreadQueue as a convenience. }

    procedure EndRead;
      {-A thread must call this method when it no longer needs read access
        to the queue.  If it does not call this method, all writers will
        be perpetually blocked. }

    procedure EndWrite;
      {-A thread must call this method when it no longer needs write access
        to the queue.  If it does not call this method, all readers and writers
        will be perpetualy blocked. }

  end;

{===Semaphore Pool===}
type
  TffSemaphorePool = class
  protected
    spList : TffHandleList;
    spRetainCount : integer;
    spPadLock : TffPadlock;
  public
    constructor Create(const initialCount, retainCount : integer);
    destructor Destroy; override;
    procedure Flush;
    function Get : THandle;
    procedure GetTwo(var aHandle1, aHandle2 : THandle);                {!!.06}
    procedure Put(const aHandle : THandle);
  end;

{===Mutex Pool===}
type
  TffMutexPool = class
  protected
    mpList : TffHandleList;
    mpRetainCount : integer;
    mpPadLock : TffPadlock;
  public
    constructor Create(const initialCount, retainCount : integer);
    destructor Destroy; override;
    procedure Flush;
    function Get : THandle;
    procedure Put(const aHandle : THandle);
  end;

{$IFDEF UseEventPool}
{===Event Pool===}
type
  TffEventPool = class
  protected
    epList : TffHandleList;
    epRetainCount : Integer;
    epPadLock : TffPadLock;
  public
    constructor Create(const InitialCount, RetainCount : Integer);
    destructor Destroy; override;
    procedure Flush;
    function Get : THandle;
    procedure Put(const aHandle : THandle);
  end;
{$ENDIF}

{===Memory Pool===}
type
  { This type defines the format of the information at the head of each
    block allocated by a memory pool. }
  PffMemBlockInfo = ^TffMemBlockInfo;
  TffMemBlockInfo = packed record
    NextBlock : pointer;
    UsageCounter : Longint;
  end;
  TffMemoryPool = class
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
    protected {private}
      FItemSize    : TffMemSize;
      FItemsInBlock: integer;
      FBlockSize   : integer;
      FFirstBlock  : PffMemBlockInfo;
      FFreeList    : pointer;
        {-Points to the next available item in a chain of items that  The free
          list is updated as items are freed and removed. }

      mpPadlock    : TffPadlock;
    protected
      procedure mpAddBlock;
      procedure mpCleanFreeList(const BlockStart : pointer);
        {-When a block is removed from memory, this routine is used to remove
          the block's items from the free list. }
    public
      constructor Create(ItemSize : TffMemSize; ItemsInBlock : integer);
        {-Create a pool of items. Each item has size ItemSize;
          ItemsInBlock defines how many items are allocated at once
          from the Delphi heap manager.  If ItemSize * ItemsInBlock > 64k
          then ItemsInBlock will be reduced such that it fits within 64k. }
      destructor Destroy; override;
        {-Free all blocks in the memory pool; destroy the object; all
          non-freed allocations from the pool will be invalid after
          this point}
      function Alloc : pointer;
        {-Allocate a new item from the pool, return its address}
      function BlockCount : Longint;
        {-Return the number of blocks owned by the memory pool. }
      function BlockUsageCount(const BlockIndex : Longint) : Longint;
        {-Retrieves the usage count for a specific block.  BlockIndex identifies
          the block whose usage count is to be retrieved and is base 0.
          Returns -1 if the specified block could not be found. }
      procedure Dispose(var P);
        {-Return an item to the pool for reuse; set the pointer to nil}
      function RemoveUnusedBlocks : integer;
        {-Use this method to have the memory pool free its unused blocks.
          Returns the number of blocks freed. }

      property BlockSize : integer read FBlockSize;
        { The total size of a block in the memory pool. }

      property ItemsInBlock : integer read FItemsInBlock;
        { The number of items into which a block is subdivided. }

      property ItemSize : TffMemSize read FItemSize;
        { The size of each item within the block. }
  end;


{===FlashFiler TffComponent class===}
  { All FF classes that would normally inherit from TComponent must inherit
    from this class instead. }
  TffComponent = class(TComponent)
{$IFDEF IsDelphi}                                                      {!!.03}
    class function NewInstance : TObject; override;
    procedure FreeInstance; override;
{$ENDIF}                                                               {!!.03}
{Begin !!.03}
  {$IFDEF FF_DEBUG_THREADS}
  protected {private}
    ffcMethodLock      : Integer;
    ffcCurrentThreadID : Cardinal;
    ffcThreadLockCount : Integer;
  protected
    procedure ThreadEnter;
    procedure ThreadExit;
  public
  {$ENDIF}
{End !!.03}
  protected
    fcDependentList : TffList;                                         {!!.11}
    fcLock : TffPadlock;                                               {!!.11}
    fcDestroying : Boolean;
    function GetVersion : string;
    procedure SetVersion(const Value : string);
  public
    constructor Create(AOwner : TComponent); override;
    destructor Destroy; override;

    procedure FFAddDependent(ADependent : TffComponent); virtual;      {!!.11}
    procedure FFNotification(const AOp : Byte; AFrom : TffComponent);
    procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                               const AData : TffWord32); virtual;
    procedure FFRemoveDependent(ADependent : TffComponent); virtual;   {!!.11}
    procedure FFNotifyDependents(const AOp : Byte); virtual;           {!!.05}
    procedure FFNotifyDependentsEx(const AOp : Byte; const AData : TffWord32);
  published
    property Version : string
      read GetVersion
      write SetVersion
      stored False;
  end;

{===Timer declarations===}
type
  TffTimer = packed record
    trStart    : DWord;                                                {!!.10}
    trExpire   : DWord;                                                {!!.10}
    trWrapped  : boolean;
    trForEver  : boolean;
  end;

const
  ffc_TimerInfinite = 0;                                               {!!.06}
//  {$IFDEF FF_DEBUG}                                                  {Deleted !!.03}
    ffc_TimerMaxExpiry = 3600 * 1000;
//  {$ELSE}                                                            {Deleted !!.03}
//  ffc_TimerMaxExpiry = 30000;                                        {Deleted !!.03}
//  {$ENDIF FF_DEBUG}                                                  {Deleted !!.03}

procedure SetTimer(var T : TffTimer; Time : DWord);                    {!!.10}
  {-Set a timer to expire in Time milliseconds. 1 <= Time <= 30000.}
function HasTimerExpired(const T : TffTimer) : boolean;
  {-Return true if the timer has expired}


{===Comparison declarations===}
function FFCmpB(a, b : byte) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b unsigned 8-bit}
function FFCmpDW(const a, b : TffWord32) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b unsigned 32-bit}
function FFCmpI(a, b : integer) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed integers}
function FFCmpI16(a, b : smallint) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed 16-bit}
function FFCmpI32(a, b : Longint) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed 32-bit}
function FFCmpI8(a, b : shortint) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed 8-bit}
function FFCmpW(a, b : TffWord16) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b unsigned 16-bit}
function FFCmpBytes(const a, b : PffByteArray; MaxLen : integer) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b byte arrays}
  { At most MaxLen bytes are compared}
function FFCmpShStr(const a, b : TffShStr; MaxLen : byte) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b short strings}
  { At most MaxLen characters are compared}
function FFCmpShStrUC(const a, b : TffShStr; MaxLen : byte) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b short strings, case insensitive}
  { At most MaxLen characters are compared}
function FFCmpI64(const a, b : TffInt64) : integer;
  {-return -ve number if a<b, 0 if equal, +ve number if a>b; a,b signed TffWord32}

{===TffInt64 Operations===}
procedure ffCloneI64(var aDest : TffInt64; const aSrc : TffInt64);
  {-clone a variable of type TffInt64}
procedure ffInitI64(var I : TffInt64);
  {-initialize a variable of type TffInt64}
procedure ffShiftI64L(const I : TffInt64; const Bits : Byte; var Result : TffInt64);
  {-shift a TffInt64 to the left Bits spaces}
procedure ffShiftI64R(const I : TffInt64; const Bits : Byte; var Result : TffInt64);
  {-shift a TffInt64 to the right Bits spaces}
procedure ffI64MinusI64(const a, b : TffInt64; var Result : TffInt64);
  {-subtract a TffInt64 from a TffInt64}
procedure ffI64MinusInt(const aI64 : TffInt64; const aInt : TffWord32; var Result : TffInt64);
  {-subtract an integer from a TffInt64}
function  ffI64ModInt(const aI64 : TffInt64; const aInt : TffWord32) : integer;
  {-remainder of aI64 divided by aInt}
procedure ffI64DivInt(const aI64 : TffInt64; const aInt : TffWord32; var Result : TffInt64);
  {-divide a TffInt64 by an integer}
procedure ffI64MultInt(const aI64 : TffInt64; const aInt : TffWord32; var Result : TffInt64);
  {-Multiply a TffInt64 by an integer}
procedure ffI64AddInt(const aI64 : TffInt64; const aInt : TffWord32; var Result : TffInt64);
  {-add an integer to a TffInt64}
function  ffI64ToInt(const aI64 : TffInt64) : TffWord32;
  {-convert a TffInt64 to an integer}
function  ffI64ToStr(const aI64 : TffInt64) : string;
  {-convert a TffInt64 to a string}
procedure  ffIntToI64(const aInt : TffWord32; var Result : TffInt64);
  {-convert an integer to a TffInt64}
function ffI64IsZero(const aI64 : TffInt64) : boolean;
  {-If the specified Int64 is zero then return True. }


{===Minimum/maximum declarations===}
function FFMinDW(a, b : TffWord32) : TffWord32;
  {-calculate the (signed) minimum of two long integers}
function FFMaxDW(a, b : TffWord32) : TffWord32;
  {-calculate the (signed) maximum of two long integers}
function FFMinI(a, b : integer) : integer;
  {-calculate the (signed) minimum of two integers}
function FFMaxI(a, b : integer) : integer;
  {-calculate the (signed) maximum of two integers}
function FFMinL(a, b : Longint) : Longint;
  {-calculate the (signed) minimum of two long integers}
function FFMaxL(a, b : Longint) : Longint;
  {-calculate the (signed) maximum of two long integers}
function FFMinI64(a, b : TffInt64) : TffInt64;
  {-calculate the (signed) minimum of two TffInt64s}
function FFMaxI64(a, b : TffInt64) : TffInt64;
  {-calculate the (signed) maximum of two TffInt64s}

{===Calculate value declarations===}
function FFCheckDescend(aAscend : boolean; a : integer) : integer;
  {-if aAscend is false, -a is returned, if true a is returned}
function FFForceInRange(a, aLow, aHigh : Longint) : Longint;
  {-Force a to be in the range aLow..aHigh inclusive}
  { NOTE: no checks are made to see that aLow < aHigh}
function FFForceNonZero(a, b : integer) : integer;
  {-if first integer is non-zero return it, else return second}

{===Memory allocation, etc===}
procedure FFFreeMem(var P; Size : TffMemSize);
  {-deallocate memory allocated by FFGetMem}
procedure FFGetMem(var P; Size : TffMemSize);
  {-like GetMem, but uses memory pools}
procedure FFGetZeroMem(var P; Size : TffMemSize);
  {-like GetMem, but allocated memory is zeroed out}
procedure FFReallocMem(var P; OldSize, NewSize: Integer);
  {-deallocates OldSize bytes for P then allocates aNewSize bytes
    for P. }

{===String routines===}
function  FFCommaizeChL(L : Longint; Ch : AnsiChar) : AnsiString;
  {-Convert a long integer to a string with Ch in comma positions}
procedure FFShStrConcat(var Dest : TffShStr; const Src : TffShStr);
procedure FFShStrAddChar(var Dest : TffShStr; C : AnsiChar);
function  FFShStrAlloc(const S : TffShStr) : PffShStr;
procedure FFShStrFree(var P : PffShStr);
function  FFShStrRepChar(C : AnsiChar; N : integer) : TffShStr;
function  FFShStrUpper(const S : TffShStr) : TffShStr;
function  FFShStrUpperAnsi(const S : TffShStr) : TffShStr;
function  FFStrAlloc(aSize : integer) : PAnsiChar;
function  FFStrAllocCopy(S : PAnsiChar) : PAnsiChar;
procedure FFStrDispose(S : PAnsiChar);
function  FFStrNew(const S : TffShStr) : PAnsiChar;
function  FFStrPas(S : PAnsiChar) : TffShStr;
function  FFStrPasLimit(S : PAnsiChar; MaxCharCount : integer) : TffShStr;
function  FFStrPCopy(Dest : PAnsiChar; const S : TffShStr) : PAnsiChar;
function  FFStrPCopyLimit(Dest : PAnsiChar; const S : TffShStr;
                         MaxCharCount : integer) : PAnsiChar;
procedure FFShStrSplit(S: TffShStr; const SplitChars: TffShStr;
                       var Left, Right: TffShStr);
  {-Returns in Left and Right the substrings of S that exist to the left
    and right of any occurrence of any character given in SplitChars (see
    implementation) }
procedure FFStrTrim(P : PAnsiChar);
  {-Trim leading and trailing blanks from P}
function FFStrTrimR(S : PAnsiChar) : PAnsiChar;
  {-Return a string with trailing white space removed}
function FFShStrTrim(const S : TffShStr) : TffShStr;
function FFShStrTrimL(const S : TffShStr) : TffShStr;
function FFShStrTrimR(const S : TffShStr) : TffShStr;
function FFShStrTrimWhite(const S : TffShStr) : TffShStr;
function FFShStrTrimWhiteL(const S : TffShStr) : TffShStr;
function FFShStrTrimWhiteR(const S : TffShStr) : TffShStr;
function FFTrim(const S : string) : string;
function FFTrimL(const S : string) : string;
function FFTrimR(const S : string) : string;
function FFTrimWhite(const S : string) : string;
function FFTrimWhiteL(const S : string) : string;
function FFTrimWhiteR(const S : string) : string;
function FFOmitMisc(const S : string) : string;
  {-Omit whitespace and punctuation characters from a string. }
function FFAnsiCompareText(const S1, S2 : string) : Integer;           {!!.10}
  {-Includes an extra failsafe comparison option if SafeAnsiCompare
    is defined }
function FFAnsiStrIComp(S1, S2: PChar): Integer;                       {!!.10}
  {-Includes an extra failsafe comparison option if SafeAnsiCompare
    is defined }
function FFAnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;    {!!.10}
  {-Includes an extra failsafe comparison option if SafeAnsiCompare
    is defined }

{===Wide-String routines===}
function FFCharToWideChar(Ch: AnsiChar): WideChar;
{-Copies an ANSI character to a UNICODE wide character}

function FFWideCharToChar(WC: WideChar): AnsiChar;
{-Copies a UNICODE wide char to an ANSI character}

function FFShStrLToWideStr(S: TffShStr; WS: PWideChar; MaxLen: Longint): PWideChar;
{-Copies a short string to a null-terminated UNICODE wide string}

function FFWideStrLToShStr(WS: PWideChar; MaxLen: Longint): TffShStr;
{-Copies a null-terminated UNICODE wide string to a short string}

function FFNullStrLToWideStr(ZStr: PAnsiChar; WS: PWideChar; MaxLen: Longint): PWideChar;
{-Copies a null-terminated ANSI string to a null-terminated UNICODE wide string}

function FFWideStrLToNullStr(WS: PWideChar; ZStr: PAnsiChar; MaxLen: Longint): PAnsiChar;
{-Copies a null-terminated UNICODE wide string to a null-terminated ANSI string}

function FFWideStrLToWideStr(aSourceValue, aTargetValue: PWideChar; MaxLength: Longint): PWideChar;
{-Copies a null-terminated UNICODE wide string to another null-terminated UNICODE string}

{===File and Path name routines===}
function FFDirectoryExists(const Path : TffPath) : boolean;
  {-Returns true if the directory given by PN exists}
function FFExpandFileName(const FN : TffFullFileName) : TffFullFileName;
  {-Merges the filename with the current drive/directory to give a
    fully expanded file name; . and .. references are removed}
function FFExtractExtension(const PFN : TffFullFileName) : TffExtension;
  {-Extracts the file name extension from the path/file name PFN}
function FFExtractFileName(const PFN : TffFullFileName) : TffFileName;
  {-Strips the path and extension from the path/file name PFN}
function FFExtractPath(const PFN : TffFullFileName) : TffPath;
  {-Extracts the path from the path/file name PFN (excluding final \)}
function FFExtractTableName(const PFN : TffFullFileName) : TffTableName;
  {-Strips the path and extension from the path/file name PFN to give a table name}
function FFFileExists(const PFN : TffFullFileName) : boolean;
  {-Return true if the file exists; wildcards are not allowed: if any
    are found, returns false}
procedure FFFindClose(var SR : TffSearchRec);
function FFFindFirst(const PFN      : TffFullFileName;
                           ItemType : TffDirItemTypeSet;
                           Attr     : TffDirItemAttrSet;
                       var SR       : TffSearchRec) : integer;
function FFFindNext(var SR : TffSearchRec) : integer;
  {-Directory 'find file' routines, in 32-bit they use shortstrings
    instead}
function FFForceExtension(const PFN : TffFullFileName;
                          const Ext : TffExtension) : TffFullFileName;
  {-Forces the path/file name PFN to have a given extension Ext}
function FFGetCurDir : TffPath;
  {-Returns the current directory (in 16-bit, on the current drive)}
function FFGetDirList(const Path : TffPath; FileSpec : TffFileNameExt) : TffStringList;
  {-Reads a directory with a given file spec, creates a string list to
    hold each file+ext encountered (the caller must free the list)}
function FFGetEXEName : TffFullFileName;
  {-Retrieves the full expanded file name of the calling program}
function FFHasExtension(const PFN : TffFullFileName; var DotPos : integer) : boolean;
  {-Returns true and the period position if the given path/file name
    has an extension}
function FFMakeFileNameExt(const FileName : TffFileName;
                           const Ext      : TffExtension) : TffFileNameExt;
  {-Concatenate a file name with extension}
function FFMakeFullFileName(const Path     : TffPath;
                            const FileName : TffFileNameExt) : TffFullFileName;
  {-Prepend a path to a file name with extension}
function FFSetCurDir(Path : TffPath) : boolean;
  {-Set the current directory}


{===BitSet routines===}
procedure FFClearAllBits(BitSet : PffByteArray; BitCount : integer);
  {-Clear all bits in a bit set}
procedure FFClearBit(BitSet : PffByteArray; Bit : integer);
  {-Clear a bit in a bit set}
function FFIsBitSet(BitSet : PffByteArray; Bit : integer) : boolean;
  {-Return whether a bit is set}
procedure FFSetAllBits(BitSet : PffByteArray; BitCount : integer);
  {-Clear a bit set, ie set all bits off}
procedure FFSetBit(BitSet : PffByteArray; Bit : integer);
  {-Set all bits in a bit set}


{===Verification routines===}
function FFVerifyBlockSize(BlockSize : Longint) : boolean;
  {-Verify BlockSize to be 4K, 8K, 16K or 32K}
function FFVerifyKeyLength(KeyLen : word) : boolean;
  {-Verify length of key to be between 1 and 1024}
function FFVerifyExtension(const Ext : TffExtension) : boolean;
  {-Validates a string to contain a valid extension; allowed: a-z, 0-9
    and _}
function FFVerifyFileName(const FileName : TffFileName) : boolean;
  {-Validates a string to contain a valid filename (no drive, path or
    extension allowed); in 16-bit the length must be 8 or less; in
    32-bit it must be 31 characters or less; allowed: a-z, 0-9 and _}
function FFVerifyServerName(aName: TffNetAddress): Boolean;
  {-Validates a string to contain a valid server name; must be 15
    chars or less; valid chars are A-Z, a-z, 0-9, or space }

{===WWW Interfaces===}
procedure ShellToWWW;
  {-Shell out to TurboPower WWW site}
procedure ShellToEMail;
  {-Shell to e-mail to TurboPower tech support}

{===Mutex & Semaphore pools===}
var
  FFSemPool : TffSemaphorePool;
    { FF uses a lot of semaphores for managing threadsafe lists & queues.
      It takes a lot of time to create semaphores so we store unused
      semaphores in a pool until they are needed. }

  {$IFDEF UseEventPool}
  FFEventPool : TffEventPool;
  {$ENDIF}
    { FF uses a lot of semaphores for managing access to threadsafe lists
      & queues.  It takes a lot  of time to create events so we store unused
      events in a pool until they are needed. }

{===Utility routines===}
function FFByteAsHex(Dest : PAnsiChar; B : byte) : PAnsiChar;
function FFMapBlockSize(const aBlockSize : Longint) : TffBlockSize;
function FFPointerAsHex(Dest : PAnsiChar; P : pointer) : PAnsiChar;
procedure FFFlushMemPools;                                             {!!.01}
procedure FFValCurr(const S : string; var V : Currency; var Code : Integer); {!!.06}

{== File-related utility routines ====================================}{!!.11 - Start}
{$IFDEF DCC4OrLater}
function PreGetDiskFreeSpaceEx(Directory     : PChar;
                           var FreeAvailable,
                               TotalSpace    : TLargeInteger;
                               TotalFree     : PLargeInteger)
                                             : Bool; stdcall;

function FFGetDiskFreeSpace(const aDirectory : string) : Integer;
  { Returns the amount of free space on the specified drive & directory,
    in kilobytes. }

var
  FFLLGetDiskFreeSpaceEx : function (Directory     : PChar;
                                 var FreeAvailable,
                                     TotalSpace    : TLargeInteger;
                                     TotalFree     : PLargeInteger)
                                                   : Bool stdcall;
{$ELSE}
function PreGetDiskFreeSpaceEx(Directory     : PChar;
                           var FreeAvailable,
                               TotalSpace    : Integer;
                               TotalFree     : PInteger)
                                             : Bool; stdcall;

function FFGetDiskFreeSpace(const aDirectory : string) : Integer;
  { Returns the amount of free space on the specified drive & directory,
    in kilobytes. }

var
  FFLLGetDiskFreeSpaceEx : function (Directory     : PChar;
                                 var FreeAvailable,
                                     TotalSpace    : Integer;
                                     TotalFree     : PInteger)
                                                   : Bool stdcall;
{$ENDIF}


{$IFDEF MemPoolTrace}
var
  Log : System.Text;
{$ENDIF}

implementation

uses
  {$IFDEF FF_DEBUG_THREADS}
  JclSynch,
  {$ENDIF}
  ffllexcp;

{===Timer routines===================================================}
procedure SetTimer(var T : TffTimer; Time : DWord);                    {!!.10}
begin
  with T do begin
    if (Time = ffc_TimerInfinite) then begin
      trForEver := true;
      trStart := 0;
      trExpire := 0;
      trWrapped := false;
    end
    else begin
      trForEver := false;
      Time := FFForceInRange(Time, 1, ffc_TimerMaxExpiry);
      trStart := GetTickCount;
      trExpire := trStart + Time;
      trWrapped := FFCmpDW(trStart, trExpire) < 0;
    end;
  end;
end;
{--------}
function HasTimerExpired(const T : TffTimer) : boolean;
asm
  push ebx
  xor ebx, ebx
  cmp [eax].TffTimer.trForEver, 0
  jne @@Exit
  push eax
  call GetTickCount
  pop edx
  mov ecx, [edx].TffTimer.trExpire
  mov edx, [edx].TffTimer.trStart
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
end;
{====================================================================}

{===Utility routines=================================================}
function FFByteAsHex(Dest : PAnsiChar; B : byte) : PAnsiChar;
const
  HexChars : array [0..15] of AnsiChar = '0123456789abcdef';
begin
  if (Dest <> nil) then begin
    Dest[0] := HexChars[B shr 4];
    Dest[1] := HexChars[B and $F];
    Dest[2] := #0;
  end;
  Result := Dest;
end;
{--------}
function FFMapBlockSize(const aBlockSize : Longint) : TffBlockSize;
begin
  case aBlockSize of
    4 * 1024 : Result := ffbs4k;
    8 * 1024 : Result := ffbs8k;
   16 * 1024 : Result := ffbs16k;
   32 * 1024 : Result := ffbs32k;
   64 * 1024 : Result := ffbs64k;
  else
    Result := ffbs4k
  end;  { case }
end;
{--------}
function FFPointerAsHex(Dest : PAnsiChar; P : pointer) : PAnsiChar;
var
  L : Longint;
begin
  Result := Dest;
  if (Dest <> nil) then begin
    L := Longint(P);
    FFByteAsHex(Dest, L shr 24);
    inc(Dest, 2);
    FFByteAsHex(Dest, (L shr 16) and $FF);
    inc(Dest, 2);
    FFByteAsHex(Dest, (L shr 8) and $FF);
    inc(Dest, 2);
    FFByteAsHex(Dest, L and $FF);
  end;
end;
{====================================================================}

{===Integer comparison declarations==================================}
function FFCmpB(a, b : byte) : integer;
asm
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
end;
{--------}
function FFCmpDW(const a, b : TffWord32) : integer;
asm
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
end;
{--------}
function FFCmpI(a, b : integer) : integer;
asm
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
end;
{--------}
function FFCmpI16(a, b : smallint) : integer;
asm
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
end;
{--------}
function FFCmpI8(a, b : shortint) : integer;
asm
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
end;
{--------}
function FFCmpI32(a, b : Longint) : integer;
asm
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
end;
{--------}
function FFCmpW(a, b : TffWord16) : integer;
asm
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
end;
{--------}
function FFCmpBytes(const a, b : PffByteArray; MaxLen : integer) : integer;
asm
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
end;
{--------}
function FFCmpShStr(const a, b : TffShStr; MaxLen : byte) : integer;
asm
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
end;
{--------}
function FFCmpShStrUC(const a, b : TffShStr; MaxLen : byte) : integer;
asm
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
end;
{--------}
procedure  ffCloneI64(var aDest : TffInt64; const aSrc : TffInt64);
begin
  aDest.iLow := aSrc.iLow;
  aDest.iHigh := aSrc.iHigh;
end;
{--------}
procedure ffInitI64(var I : TffInt64);
begin
  I.iLow := 0;
  I.iHigh := 0;
end;
{--------}
function FFCmpI64(const a, b : TffInt64) : Integer;                    {!!.06 - Rewritten}
begin
  if (a.iHigh = b.iHigh) then
    Result := FFCmpDW(a.iLow, b.iLow)
  else
    Result := FFCmpDW(a.iHigh, b.iHigh);
end;                                                                   {!!.06 - End rewritten}
{--------}
procedure ffShiftI64L(const I      : TffInt64;
                      const Bits   : Byte;
                        var Result : TffInt64);
asm
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
end;
{--------}
procedure ffShiftI64R(const I : TffInt64; const Bits : Byte; var Result : TffInt64);
asm
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
end;
{--------}
procedure ffI64MinusI64(const a, b : TffInt64; var Result : TffInt64);
asm
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
end;
{--------}
procedure ffI64MinusInt(const aI64 : TffInt64; const aInt : TffWord32; var Result : TffInt64);
asm
  push edi
  mov edi, edx
  mov edx, [eax+4]
  mov eax, [eax]
  sub eax, edi
  sbb edx,  0
  mov [ecx], eax
  mov [ecx+4], edx
  pop edi
end;
{--------}
function  ffI64ModInt(const aI64 : TffInt64; const aInt : TffWord32) : integer;
var
  Quotient : TffInt64;
  QSum     : TffInt64;
begin
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
end;
{--------}
procedure ffI64DivInt(const aI64 : TffInt64; const aInt : TffWord32; var Result : TffInt64);
  {This procedure was originally intended to divide a 64-bit word by a
   64-bit word.  Since we are now dividing a 64-bit word by a 32-bit word,
   we are forcing the divisor's high word to a 0.  This is an area for
   improvement}
asm
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
end;
{--------}
procedure ffI64MultInt(const aI64 : TffInt64; const aInt : TffWord32; var Result : TffInt64);
asm
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
end;
{--------}
procedure ffI64AddInt(const aI64 : TffInt64; const aInt : TffWord32; var Result : TffInt64);
asm
 push ebx
  mov ebx, [eax].TffInt64.iLow
  add ebx, edx
  mov [ecx].TffInt64.iLow, ebx
  mov ebx, [eax].TffInt64.iHigh
  adc ebx, 0
  mov [ecx].TffInt64.iHigh, ebx
  pop ebx
end;
{--------}
function  ffI64toInt(const aI64 : TffInt64) : TffWord32;
begin
  {What should we do if aI64 larger than DWord?
   - D5 doesn't do anything}
  Result := aI64.iLow;
end;
{--------}
function  ffI64ToStr(const aI64 : TffInt64) : string;
begin
  Result := IntToStr(aI64.iHigh) + IntToStr(aI64.iLow);
end;
{--------}
procedure ffIntToI64(const aInt : TffWord32; var Result : TffInt64);
begin
  Result.iLow := aInt;
  Result.iHigh := 0;
end;
{--------}
function ffI64IsZero(const aI64 : TffInt64) : boolean;
begin
  Result := ((aI64.iHigh = 0) and (aI64.iLow = 0));
end;
{====================================================================}


{===Minimum/maximum routines=========================================}
function FFMinDW(a, b : TffWord32) : TffWord32;
asm
  cmp eax, edx
  jbe @@Exit
  mov eax, edx
@@Exit:
end;
{--------}
function FFMaxDW(a, b : TffWord32) : TffWord32;
asm
  cmp eax, edx
  jae @@Exit
  mov eax, edx
@@Exit:
end;
{--------}
function FFMinI(a, b : integer) : integer;
asm
  cmp eax, edx
  jle @@Exit
  mov eax, edx
@@Exit:
end;
{--------}
function FFMaxI(a, b : integer) : integer;
asm
  cmp eax, edx
  jge @@Exit
  mov eax, edx
@@Exit:
end;
{--------}
function FFMinL(a, b : Longint) : Longint;
asm
  cmp eax, edx
  jle @@Exit
  mov eax, edx
@@Exit:
end;
{--------}
function FFMaxL(a, b : Longint) : Longint;
asm
  cmp eax, edx
  jge @@Exit
  mov eax, edx
@@Exit:
end;
{--------}
function FFMinI64(a, b : TffInt64) : TffInt64;
begin
  if FFCmpI64(a,b) <= 0 then
    Result := a
  else
    Result := b;
end;
{--------}
function FFMaxI64(a, b : TffInt64) : TffInt64;
begin
  if FFCmpI64(a,b) >= 0 then
    Result := a
  else
    Result := b;
end;
{====================================================================}


{====================================================================}
function FFCheckDescend(aAscend : boolean; a : integer) : integer;
register;
asm
  or al, al
  jnz @@Exit
  neg edx
@@Exit:
  mov eax, edx
end;
{--------}
function FFForceInRange(a, aLow, aHigh : Longint) : Longint;
register;
asm
  cmp eax, edx
  jg @@CheckHigh
  mov eax, edx
  jmp @@Exit
@@CheckHigh:
  cmp eax, ecx
  jl @@Exit
  mov eax, ecx
@@Exit:
end;
{--------}
function FFForceNonZero(a, b : integer) : integer;
register;
asm
  or eax, eax
  jnz @@Exit
  mov eax, edx
@@Exit:
end;
{====================================================================}


{===Memory allocation, etc===========================================}
var

  FFMemPools : array [0..91] of TffMemoryPool;
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
function CalcPoolIndex(Size : TffMemSize) : integer;
begin
  if (Size <= 1024) then
    Result := (Size-1) div 32              {ie, 0..31}
  else
    Result := ((Size-1) div 256) - 4 + 32; {ie, 32..91}
end;
{--------}
procedure FFFreeMem(var P; Size : TffMemSize);
{$IFNDEF MemCheck}
var
  Pt  : pointer;
  Inx : integer;
{$ENDIF}
begin
  {$IFDEF MemCheck}
  FreeMem(pointer(P), Size);
  {$ELSE}
  Pt := pointer(P);
  if (Pt <> nil) then begin
    if (Size <= 16*1024) then begin
      Inx := CalcPoolIndex(Size);
      FFMemPools[Inx].Dispose(Pt);
    end
    else
      FreeMem(Pt, Size);
  end;
  {$ENDIF}
end;
{--------}
procedure FFGetMem(var P; Size : TffMemSize);
{$IFNDEF MemCheck}
var
  Pt  : pointer absolute P;
  Inx : integer;
{$ENDIF}
begin
  {$IFDEF MemCheck}
  GetMem(pointer(P), Size);
  {$ELSE}
  if (Size <= 16*1024) then begin
    Inx := CalcPoolIndex(Size);
    Pt := FFMemPools[Inx].Alloc;
  end
  else
    GetMem(Pt, Size);
  {$ENDIF}
end;
{--------}
procedure FFGetZeroMem(var P; Size : TffMemSize);
var
  Pt : pointer absolute P;
begin
  FFGetMem(Pt, Size);
  FillChar(Pt^, Size, 0);
end;
{--------}
procedure FFReallocMem(var P; OldSize, NewSize: Integer);
{$IFNDEF MemCheck}
var
  Pt : Pointer absolute P;
  P2 : Pointer;
  OldInx, NewInx: Integer;
{$ENDIF}
begin
  {$IFDEF MemCheck}
  ReallocMem(pointer(P), NewSize);
  {$ELSE}
  if Pt = nil then
    FFGetMem(P, NewSize)
  else
  if NewSize = 0 then begin
    FFFreeMem(P, OldSize);
    Pt := nil;
  end
  else
  if (OldSize > 16*1024) and (NewSize > 16*1024) then
    ReAllocMem(Pt, NewSize)
  else begin
    OldInx := CalcPoolIndex(OldSize);
    NewInx := CalcPoolIndex(NewSize);
    if OldInx <> NewInx then begin
      if NewInx <= 91 then
        P2 := FFMemPools[NewInx].Alloc
      else
        GetMem(P2, NewSize);
      if NewSize < OldSize then                                        {!!.02}
        Move(Pt^, P2^, NewSize)                                        {!!.02}
      else                                                             {!!.02}
        Move(Pt^, P2^, OldSize);                                       {!!.02}
      if OldInx <= 91 then
        FFMemPools[OldInx].Dispose(Pt)
      else
        FreeMem(Pt);
      Pointer(P) := P2;
    end;
  end;
  {$ENDIF}
end;
{Begin !!.01}
{--------}
procedure FFFlushMemPools;
var
  anInx : integer;
begin
  for anInx := 0 to 91 do
    FFMemPools[anInx].RemoveUnusedBlocks;
end;
{End !!.01}
{--------}
{begin !!.06}
procedure FFValCurr(const S : string; var V : Currency; var Code : Integer); {!!.06}
{
Evaluate string as a floating point number, emulates Borlandish Pascal's
Val() intrinsic

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

type
  { recognizer machine states }
  TNumConvertState = (ncStart, ncSign, ncWhole, ncDecimal, ncStartDecimal,
    ncFraction, ncE, ncExpSign, ncExponent, ncEndSpaces, ncBadChar);
const
  { valid stop states for machine }
  StopStates: set of TNumConvertState = [ncWhole, ncDecimal, ncFraction,
    ncExponent, ncEndSpaces];

var
  i        : Integer;        { general purpose counter }
  P        : PChar;          { current position in evaluated string }
  NegVal   : Boolean;        { is entire value negative? }
  NegExp   : Boolean;        { is exponent negative? }
  Exponent : LongInt;        { accumulator for exponent }
  Mantissa : Currency;       { mantissa }
  FracMul  : Currency;       { decimal place holder }
  State : TNumConvertState;  { current state of recognizer machine }


begin
{initializations}
  V := 0.0;
  Code := 0;

  State := ncStart;

  NegVal := False;
  NegExp := False;

  Mantissa := 0.0;
  FracMul  := 0.1;
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

  P := PChar(S);
  for i := 1 to Length(S) do begin
(*****)
  case State of
    ncStart : begin
      if P^ = '.' then begin
        State := ncStartDecimal;   { decimal point detected in mantissa }
      end else

      case P^ of
        ' ': begin
          {ignore}
        end;

        '+': begin
          State := ncSign;
        end;

        '-': begin
          NegVal := True;
          State := ncSign;
        end;

        'e', 'E': begin
          Mantissa := 0;
          State := ncE;     { exponent detected }
        end;

        '0'..'9': begin
          State := ncWhole;    { start of whole portion of mantissa }
          Mantissa := (Mantissa * 10) + (Ord(P^) - Ord('0'));
        end;

        else
          State := ncBadChar;
      end;

    end;

    ncSign : begin
      if P^ = '.' then begin
        State := ncDecimal;   { decimal point detected in mantissa }
      end else

      case P^ of
        '0'..'9': begin
          State := ncWhole;    { start of whole portion of mantissa }
          Mantissa := (Mantissa * 10) + (Ord(P^) - Ord('0'));
        end;

        'e', 'E': begin
          Mantissa := 0;
          State := ncE;     { exponent detected }
        end;

        else
          State := ncBadChar;
      end;
    end;

    ncWhole : begin
      if P^ = '.' then begin
        State := ncDecimal;   { decimal point detected in mantissa }
      end else

      case P^ of
        '0'..'9': begin
          Mantissa := (Mantissa * 10) + (Ord(P^) - Ord('0'));
        end;

        '.': begin
        end;

        'e', 'E': begin
          State := ncE;     { exponent detected }
        end;

        ' ': begin
          State := ncEndSpaces;
        end;

        else
          State := ncBadChar;
      end;
    end;

    ncDecimal : begin
      case P^ of
        '0'..'9': begin
          State := ncFraction; { start of fractional portion of mantissa }
          Mantissa := Mantissa + (FracMul * (Ord(P^) - Ord('0')));
          FracMul := FracMul * 0.1;
        end;

        'e', 'E': begin
          State := ncE;     { exponent detected }
        end;

        ' ': begin
          State := ncEndSpaces;
        end;

        else
          State := ncBadChar;
      end;

    end;

    ncStartDecimal : begin
      case P^ of
        '0'..'9': begin
          State := ncFraction; { start of fractional portion of mantissa }
          Mantissa := Mantissa + (FracMul * (Ord(P^) - Ord('0')));
          FracMul := FracMul * 0.1;
        end;

        ' ': begin
          State := ncEndSpaces;
        end;

        else
          State := ncBadChar;
      end;
    end;

    ncFraction : begin
      case P^ of
        '0'..'9': begin
          Mantissa := Mantissa + (FracMul * (Ord(P^) - Ord('0')));
          FracMul := FracMul * 0.1;
        end;

        'e', 'E': begin
          State := ncE;     { exponent detected }
        end;

        ' ': begin
          State := ncEndSpaces;
        end;

        else
          State := ncBadChar;
      end;
    end;

    ncE : begin
      case P^ of
        '0'..'9': begin
          State := ncExponent;  { start of exponent }
          Exponent := Exponent * 10 + (Ord(P^) - Ord('0'));
        end;

        '+': begin
          State := ncExpSign;
        end;

        '-': begin
          NegExp := True;   { exponent is negative }
          State := ncExpSign;
        end;

        else
          State := ncBadChar;
      end;
    end;

    ncExpSign : begin
      case P^ of
        '0'..'9': begin
          State := ncExponent;  { start of exponent }
          Exponent := Exponent * 10 + (Ord(P^) - Ord('0'));
        end;

        else
          State := ncBadChar;
      end;
    end;

    ncExponent : begin
      case P^ of
        '0'..'9': begin
          Exponent := Exponent * 10 + (Ord(P^) - Ord('0'));
        end;

        ' ': begin
          State := ncEndSpaces;
        end;

        else
          State := ncBadChar;
      end;
    end;

    ncEndSpaces : begin
      case P^ of
        ' ': begin
          {ignore}
        end;
        else
          State := ncBadChar;
      end;
    end;
  end;

(*****)
    Inc(P);
    if State = ncBadChar then begin
      Code := i;
      Break;
    end;
  end;
{
Final calculations
}
  if not (State in StopStates) then begin
      Code := i;  { point to error }
  end else begin
    { negate if needed }
    if NegVal then
      Mantissa := -Mantissa;


    { apply exponent if any }
    if Exponent <> 0 then begin
      if NegExp then
        for i := 1 to Exponent do
          Mantissa := Mantissa * 0.1
      else
        for i := 1 to Exponent do
          Mantissa := Mantissa * 10.0;
    end;

    V := Mantissa;
  end;
end;
{end !!.06}
{====================================================================}


{===String routines==================================================}
const
  EmptyShStr : array [0..1] of AnsiChar = #0#0;

{--------}
function FFCommaizeChL(L : Longint; Ch : AnsiChar) : AnsiString;
  {-Convert a long integer to a string with Ch in comma positions}
var
  Temp : string;
  NumCommas, I, Len : Cardinal;
  Neg : Boolean;
begin
  SetLength(Temp, 1);
  Temp[1] := Ch;
  if L < 0 then begin
    Neg := True;
    L := Abs(L);
  end else
    Neg := False;
  Result := IntToStr(L);
  Len := Length(Result);
  NumCommas := (Pred(Len)) div 3;
  for I := 1 to NumCommas do
    System.Insert(Temp, Result, Succ(Len-(I * 3)));
  if Neg then
    System.Insert('-', Result, 1);
end;
{--------}
procedure FFShStrConcat(var Dest : TffShStr; const Src : TffShStr);
begin
  Move(Src[1], Dest[succ(length(Dest))], length(Src));
  inc(Dest[0], length(Src));
end;
{--------}
procedure FFShStrAddChar(var Dest : TffShStr; C : AnsiChar);
begin
  inc(Dest[0]);
  Dest[length(Dest)] := C;
end;
{--------}
function  FFShStrAlloc(const S : TffShStr) : PffShStr;
begin
  if (S = '') then
    Result := PffShStr(@EmptyShStr)
  else begin
    {save room for length byte and terminating #0}
    FFGetMem(Result, length(S)+2);
    Result^ := S;
    Result^[succ(length(S))] := #0;
  end;
end;
{--------}
procedure FFShStrFree(var P : PffShStr);
begin
  if (P <> nil) and (P <> PffShStr(@EmptyShStr)) then
    FFFreeMem(P, length(P^)+2);
  P := nil;
end;
{--------}
procedure FFShStrSplit(S: TffShStr; const SplitChars: TffShStr;
                       var Left, Right: TffShStr);
  {-This procedure locates the first occurrence in S of any of the
    characters listed in SplitChars and returns the substring to the
    left of the split char (exclusive) in Left and the substring to the
    right of the split char (exclusive) in Right.  If none of the chars
    given in SplitChar exist in S, then Left = S and Right = ''. }
var
  I: Integer;
begin
  Left := S;
  Right := '';
  for I := 1 to Length(S) do begin
    if Pos(SplitChars, Copy(S, I, 1)) <> 0 then begin
      Left := Copy(S, 1, I - 1);
      Right := Copy(S, I + 1, 255);
      Break;
    end;
  end;
end;
{--------}
function StrStDeletePrim(P : PAnsiChar; Pos, Count : Cardinal) : PAnsiChar; register;
asm
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
end;
{--------}
procedure FFStrTrim(P : PAnsiChar);
  {-Trim leading and trailing blanks from P}
var
  I : Integer;
  PT : PAnsiChar;
begin
  I := StrLen(P);
  if I = 0 then
    Exit;

  {delete trailing spaces}
  Dec(I);
  while (I >= 0) and (P[I] = ' ') do begin
    P[I] := #0;
    Dec(I);
  end;

  {delete leading spaces}
  I := 0;
  PT := P;
  while PT^ = ' ' do begin
    Inc(I);
    Inc(PT);
  end;
  if I > 0 then
    StrStDeletePrim(P, 0, I);
end;

function FFStrTrimR(S : PAnsiChar) : PAnsiChar; register;
asm
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
end;
{--------}
function FFShStrTrim(const S : TffShStr) : TffShStr;
var
  StartCh : integer;
  EndCh   : integer;
  LenS    : integer;
begin
  LenS := length(S);
  StartCh := 1;
  while (StartCh <= LenS) and (S[StartCh] = ' ') do
    inc(StartCh);
  if (StartCh > LenS) then
    Result := ''
  else begin
    EndCh := LenS;
    while (EndCh > 0) and (S[EndCh] = ' ') do
      dec(EndCh);
    Result := Copy(S, StartCh, succ(EndCh - StartCh));
  end;
end;
{--------}
function FFShStrTrimL(const S : TffShStr) : TffShStr;
var
  StartCh : integer;
  LenS    : integer;
begin
  LenS := length(S);
  StartCh := 1;
  while (StartCh <= LenS) and (S[StartCh] = ' ') do
    inc(StartCh);
  if (StartCh > LenS) then
    Result := ''
  else
    Result := Copy(S, StartCh, succ(LenS - StartCh));
end;
{--------}
function FFShStrTrimR(const S : TffShStr) : TffShStr;
begin
  Result := S;
  while (length(Result) > 0) and (Result[length(Result)] = ' ') do
    dec(Result[0]);
end;
{--------}
function FFShStrTrimWhite(const S : TffShStr) : TffShStr;
var
  StartCh : integer;
  EndCh   : integer;
  LenS    : integer;
begin
  LenS := length(S);
  StartCh := 1;
  while (StartCh <= LenS) and (S[StartCh] <= ' ') do
    inc(StartCh);
  if (StartCh > LenS) then
    Result := ''
  else begin
    EndCh := LenS;
    while (EndCh > 0) and (S[EndCh] <= ' ') do
      dec(EndCh);
    Result := Copy(S, StartCh, succ(EndCh - StartCh));
  end;
end;
{--------}
function FFShStrTrimWhiteL(const S : TffShStr) : TffShStr;
var
  StartCh : integer;
  LenS    : integer;
begin
  LenS := length(S);
  StartCh := 1;
  while (StartCh <= LenS) and (S[StartCh] <= ' ') do
    inc(StartCh);
  if (StartCh > LenS) then
    Result := ''
  else
    Result := Copy(S, StartCh, succ(LenS - StartCh));
end;
{--------}
function FFShStrTrimWhiteR(const S : TffShStr) : TffShStr;
begin
  Result := S;
  while (length(Result) > 0) and (Result[length(Result)] <= ' ') do
    dec(Result[0]);
end;
{--------}
function FFShStrRepChar(C : AnsiChar; N : integer) : TffShStr;
var
  i : integer;
begin
  if (N < 0) then
    N := 0
  else if (N > 255) then
    N := 255;
  Result[0] := AnsiChar(N);
  for i := 1 to N do
    Result[i] := C;
end;
{--------}
function FFShStrUpper(const S : TffShStr) : TffShStr;
var
  i : integer;
begin
  Result[0] := S[0];
  for i := 1 to length(S) do
    Result[i] := upcase(S[i]);
end;
{--------}
function FFShStrUpperAnsi(const S : TffShStr) : TffShStr;
begin
  Result := S;
  CharUpperBuff(@Result[1], length(Result));
end;
{--------}
function  FFStrAlloc(aSize : integer) : PAnsiChar;
begin
  inc(aSize, sizeof(longint));
  FFGetMem(Result, aSize);
  PLongInt(Result)^ := aSize;
  inc(Result, sizeof(longint));
  Result[0] := #0;
end;
{--------}
function  FFStrAllocCopy(S : PAnsiChar) : PAnsiChar;
var
  Len  : integer;
  Size : longint;
begin
  Len := StrLen(S);
  if (Len = 0) then
    Result := nil
  else begin
    Size := succ(Len) + sizeof(longint);
    FFGetMem(Result, Size);
    PLongInt(Result)^ := Size;
    inc(Result, sizeof(longint));
    StrCopy(Result, S);
  end;
end;
{--------}
procedure FFStrDispose(S : PAnsiChar);
begin
  if (S <> nil) then begin
    dec(S, sizeof(longint));
    FFFreeMem(S, PLongint(S)^);
  end;
end;
{--------}
function  FFStrNew(const S : TffShStr) : PAnsiChar;
var
  Len  : integer;
  Size : longint;
begin
  Len := length(S);
  if (Len = 0) then
    Result := nil
  else begin
    Size := succ(Len) + sizeof(longint);
    FFGetMem(Result, Size);
    PLongInt(Result)^ := Size;
    inc(Result, sizeof(longint));
    Move(S[1], Result^, Len);
    Result[Len] := #0;
  end;
end;
{--------}
function FFStrPas(S : PAnsiChar) : TffShStr;
var
  Len : integer;
begin
  if (S = nil) then
    Result := ''
  else begin
    Len := FFMinI(StrLen(S), 255);
    Move(S[0], Result[1], Len);
    Result[0] := AnsiChar(Len);
  end;
end;
{--------}
function FFStrPasLimit(S : PAnsiChar; MaxCharCount : integer) : TffShStr;
var
  Len : integer;
begin
  Len := FFMinI(StrLen(S), MaxCharCount);
  Move(S[0], Result[1], Len);
  Result[0] := AnsiChar(Len);
end;
{--------}
function FFStrPCopy(Dest : PAnsiChar; const S : TffShStr) : PAnsiChar;
begin
  Result := Dest;
  if (Dest <> nil) then begin
    Move(S[1], Dest[0], length(S));
    Dest[length(S)] := #0;
  end;
end;
{--------}
function FFStrPCopyLimit(Dest : PAnsiChar; const S : TffShStr;
                         MaxCharCount : integer) : PAnsiChar;
var
  Len : integer;
begin
  Result := Dest;
  if (Dest <> nil) then begin
    Len := FFMinI(MaxCharCount, length(S));
    Move(S[1], Dest[0], Len);
    Dest[Len] := #0;
  end;
end;
{--------}
function FFTrim(const S : string) : string;
var
  StartCh : integer;
  EndCh   : integer;
  LenS    : integer;
begin
  LenS := length(S);
  StartCh := 1;
  while (StartCh <= LenS) and (S[StartCh] = ' ') do
    inc(StartCh);
  if (StartCh > LenS) then
    Result := ''
  else begin
    EndCh := LenS;
    while (EndCh > 0) and (S[EndCh] = ' ') do
      dec(EndCh);
    Result := Copy(S, StartCh, succ(EndCh - StartCh));
  end;
end;
{--------}
function FFTrimL(const S : string) : string;
var
  StartCh : integer;
  LenS    : integer;
begin
  LenS := length(S);
  StartCh := 1;
  while (StartCh <= LenS) and (S[StartCh] = ' ') do
    inc(StartCh);
  if (StartCh > LenS) then
    Result := ''
  else
    Result := Copy(S, StartCh, succ(LenS - StartCh));
end;
{--------}
function FFTrimR(const S : string) : string;
var
  EndCh   : integer;
begin
  EndCh := length(S);
  while (EndCh > 0) and (S[EndCh] = ' ') do
    dec(EndCh);
  if (EndCh > 0) then
    Result := Copy(S, 1, EndCh)
  else
    Result := '';
end;
{--------}
function FFTrimWhite(const S : string) : string;
var
  StartCh : integer;
  EndCh   : integer;
  LenS    : integer;
begin
  LenS := length(S);
  StartCh := 1;
  while (StartCh <= LenS) and (S[StartCh] <= ' ') do
    inc(StartCh);
  if (StartCh > LenS) then
    Result := ''
  else begin
    EndCh := LenS;
    while (EndCh > 0) and (S[EndCh] <= ' ') do
      dec(EndCh);
    Result := Copy(S, StartCh, succ(EndCh - StartCh));
  end;
end;
{--------}
function FFTrimWhiteL(const S : string) : string;
var
  StartCh : integer;
  LenS    : integer;
begin
  LenS := length(S);
  StartCh := 1;
  while (StartCh <= LenS) and (S[StartCh] <= ' ') do
    inc(StartCh);
  if (StartCh > LenS) then
    Result := ''
  else
    Result := Copy(S, StartCh, succ(LenS - StartCh));
end;
{--------}
function FFTrimWhiteR(const S : string) : string;
var
  EndCh   : integer;
begin
  EndCh := length(S);
  while (EndCh > 0) and (S[EndCh] <= ' ') do
    dec(EndCh);
  if (EndCh > 0) then
    Result := Copy(S, 1, EndCh)
  else
    Result := '';
end;
{--------}
function FFOmitMisc(const S : string) : string;
var
  CurCh : integer;
  LenS : integer;
begin
  Result := '';
  LenS := length(S);
  CurCh := 1;
  while (CurCh <= LenS) do begin
    if S[CurCh] in ['0'..'9', 'A'..'Z', 'a'..'z'] then
      Result := Result + S[CurCh];
    inc(CurCh);
  end;
end;
{--------}
function FFAnsiCompareText(const S1, S2 : string) : Integer;           {!!.10}
begin
  {$IFDEF SafeAnsiCompare}
  Result := AnsiCompareText(AnsiLowerCase(S1), AnsiLowerCase(S2));
  {$ELSE}
  Result := AnsiCompareText(S1, S2);
  {$ENDIF}
end;
{--------}
function FFAnsiStrIComp(S1, S2: PChar): Integer;                       {!!.10}
begin
  {$IFDEF SafeAnsiCompare}
  Result := AnsiStrIComp(AnsiStrLower(S1), AnsiStrLower(S2));
  {$ELSE}
  Result := AnsiStrIComp(S1, S2);
  {$ENDIF}
end;
{--------}
function FFAnsiStrLIComp(S1, S2: PChar; MaxLen: Cardinal): Integer;    {!!.10}
begin
  {$IFDEF SafeAnsiCompare}
  Result := AnsiStrLIComp(AnsiStrLower(S1), AnsiStrLower(S2), MaxLen);
  {$ELSE}
  Result := AnsiStrLIComp(S1, S2, MaxLen);
  {$ENDIF}
end;
{====================================================================}


{===Wide-String Routines=============================================}
function FFCharToWideChar(Ch: AnsiChar): WideChar;
begin
  Result := WideChar(Ord(Ch));
end;

function FFWideCharToChar(WC: WideChar): AnsiChar;
begin
  if WC >= #256 then WC := #0;
  Result := AnsiChar(Ord(WC));
end;

function FFShStrLToWideStr(S: TffShStr; WS: PWideChar; MaxLen: Longint): PWideChar;
begin
  WS[MultiByteToWideChar(0, 0, @S[1], MaxLen, WS, MaxLen + 1)] := #0;
  Result := WS;
end;

function FFWideStrLToShStr(WS: PWideChar; MaxLen: Longint): TffShStr;
begin
  Result := WideCharLenToString(WS, MaxLen);
end;

function FFNullStrLToWideStr(ZStr: PAnsiChar; WS: PWideChar; MaxLen: Longint): PWideChar;
begin
  WS[MultiByteToWideChar(0, 0, ZStr, MaxLen, WS, MaxLen)] := #0;
  Result := WS;
end;

function FFWideStrLToNullStr(WS: PWideChar; ZStr: PAnsiChar; MaxLen: Longint): PAnsiChar;
begin
  ZStr[WideCharToMultiByte(0, 0, WS, MaxLen, ZStr, MaxLen, nil, nil)] := #0;
  Result := ZStr;
end;

function FFWideStrLToWideStr(aSourceValue, aTargetValue: PWideChar; MaxLength: Longint): PWideChar;
begin
  { Assumption: MaxLength is really # units multiplied by 2, which is how
    a Wide String's length is stored in the table's data dictionary. }
  Move(aSourceValue^, aTargetValue^, MaxLength);
  aTargetValue[MaxLength div 2] := #0;
  Result := aTargetValue;
end;
{=============
=======================================================}

{===File and Path name routines======================================}
{===Helpers===}
const
{$IFDEF DCC6OrLater}
  {$WARN SYMBOL_PLATFORM OFF}
{$ENDIF}
  faNotNormal = faReadOnly or faHidden or faSysFile or faArchive;
{$IFDEF DCC6OrLater}
  {$WARN SYMBOL_PLATFORM ON}
{$ENDIF}
{--------}
procedure SearchRecConvertPrim(var SR : TffSearchRec);
type
  LH = packed record L, H : word; end;
var
  LocalFileTime : TFileTime;
begin
  with SR do begin
    srName := FFStrPasLimit(srData.cFileName, pred(sizeof(srName)));
    FileTimeToLocalFileTime(srData.ftLastWriteTime, LocalFileTime);
    FileTimeToDosDateTime(LocalFileTime, LH(srTime).H, LH(srTime).L);
    srSize := srData.nFileSizeLow;
    srSizeHigh := srData.nFileSizeHigh;
    if ((srData.dwFileAttributes and faDirectory) <> 0) then
      srType := ditDirectory
    {$IFDEF DCC6OrLater}
      {$WARN SYMBOL_PLATFORM OFF}
    {$ENDIF}
    else if ((srData.dwFileAttributes and faVolumeID) <> 0) then
    {$IFDEF DCC6OrLater}
      {$WARN SYMBOL_PLATFORM ON}
    {$ENDIF}
      srType := ditVolumeID
    else
      srType := ditFile;
    srAttr := [];
    {$IFDEF DCC6OrLater}
      {$WARN SYMBOL_PLATFORM OFF}
    {$ENDIF}
    if ((srData.dwFileAttributes and faHidden) <> 0) then
      include(srAttr, diaHidden);
    if ((srData.dwFileAttributes and faReadOnly) <> 0) then
      include(srAttr, diaReadOnly);
    if ((srData.dwFileAttributes and faSysFile) <> 0) then
      include(srAttr, diaSystem);
    if ((srData.dwFileAttributes and faArchive) <> 0) then
      include(srAttr, diaArchive);
    if ((srData.dwFileAttributes and faNotNormal) = 0) then
      include(srAttr, diaNormal);
    {$IFDEF DCC6OrLater}
      {$WARN SYMBOL_PLATFORM ON}
    {$ENDIF}
  end;
end;
{--------}
function TypeAndAttrMatch(OSAttr : TffWord32;
                          aType  : TffDirItemTypeSet;
                          aAttr  : TffDirItemAttrSet) : boolean;
begin
  {$IFDEF DCC6OrLater}
    {$WARN SYMBOL_PLATFORM OFF}
  {$ENDIF}
  Result := ((ditFile in aType) and ((OSAttr and (faDirectory or faVolumeID)) = 0)) or
            ((ditDirectory in aType) and ((OSAttr and faDirectory) <> 0)) or
            ((ditVolumeID in aType) and ((OSAttr and faVolumeID) <> 0));
  {$IFDEF DCC6OrLater}
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}

  if not Result then
    Exit;
  {$IFDEF DCC6OrLater}
    {$WARN SYMBOL_PLATFORM OFF}
  {$ENDIF}
  Result := ((diaReadOnly in aAttr) and ((OSAttr and faReadOnly) <> 0)) or
            ((diaHidden in aAttr) and ((OSAttr and faHidden) <> 0)) or
            ((diaSystem in aAttr) and ((OSAttr and faSysFile) <> 0)) or
            ((diaArchive in aAttr) and ((OSAttr and faArchive) <> 0)) or
            ((diaNormal in aAttr) and ((OSAttr and faNotNormal) = 0));
  {$IFDEF DCC6OrLater}
    {$WARN SYMBOL_PLATFORM ON}
  {$ENDIF}

end;
{--------}
procedure ExtractHelper(const PFN      : TffFullFileName;
                          var DotPos   : integer;
                          var SlashPos : integer);
var
  i : integer;
begin
  {Note: if there is no period, DotPos is returned as one greater than
         the length of the full file name. If there is no slash
         SlashPos is returned as zero}
  DotPos := 0;
  SlashPos := 0;
  i := length(PFN);
  while (i > 0) and ((DotPos = 0) or (SlashPos = 0)) do begin
    if (PFN[i] = '.') then begin
      if (DotPos = 0) then
        DotPos := i;
    end
    else if (PFN[i] = '\') then begin
      SlashPos := i;
      if (DotPos = 0) then
        DotPos := succ(length(PFN));
    end;
    dec(i);
  end;
  if (DotPos = 0) then
    DotPos := succ(length(PFN));
end;
{--------}
function ValidFileNameHelper(const S : TffShStr; MaxLen : integer) : boolean;
const
  UnacceptableChars : set of AnsiChar =
    ['"', '*', '.', '/', ':', '<', '>', '?', '\', '|'];
var
  i    : integer;
  LenS : integer;
begin
  Result := false;
  LenS := length(S);
  if (0 < LenS) and (LenS <= MaxLen) then begin
    for i := 1 to LenS do
      if (S[i] in UnacceptableChars) then
        Exit;
    Result := true;
  end;
end;
{===end Helpers===}
function FFDirectoryExists(const Path : TffPath) : boolean;
var
  Attr : TffWord32;
  PathZ: TffStringZ;
begin
  Result := false;
  {we don't support wildcards}
  if (Pos('*', Path) <> 0) or (Pos('?', Path) <> 0) then
    Exit;
  Attr := GetFileAttributes(FFStrPCopy(PathZ, Path));
  if (Attr <> TffWord32(-1)) and ((Attr and FILE_ATTRIBUTE_DIRECTORY) <> 0) then
    Result := true;
end;
{--------}
function FFExpandFileName(const FN : TffFullFileName) : TffFullFileName;
var
  FNZ         : TffMaxPathZ;
  EFNZ        : TffMaxPathZ;
  FileNamePos : PAnsiChar;
begin
  GetFullPathName(FFStrPCopy(FNZ, FN), sizeof(EFNZ), EFNZ, FileNamePos);
  Result := FFStrPasLimit(EFNZ, pred(sizeof(TffFullFileName)));
end;
{--------}
function FFExtractExtension(const PFN : TffFullFileName) : TffExtension;
var
  DotPos   : integer;
  SlashPos : integer;
begin
  ExtractHelper(PFN, DotPos, SlashPos);
  if (DotPos >= length(PFN)) then
    Result := ''
  else
    Result := Copy(PFN, succ(DotPos), (length(PFN) - DotPos));
end;
{--------}
function FFExtractFileName(const PFN : TffFullFileName) : TffFileName;
var
  DotPos   : integer;
  SlashPos : integer;
begin
  ExtractHelper(PFN, DotPos, SlashPos);
  Result := Copy(PFN, succ(SlashPos), FFMinI(pred(DotPos - SlashPos), ffcl_FileName));
end;
{--------}
function FFExtractPath(const PFN : TffFullFileName) : TffPath;
var
  DotPos   : integer;
  SlashPos : integer;
begin
  ExtractHelper(PFN, DotPos, SlashPos);
  if (SlashPos = 0) then
    Result := ''
  else
    Result := Copy(PFN, 1, FFMinI(pred(SlashPos), ffcl_Path));
end;
{--------}
function FFExtractTableName(const PFN : TffFullFileName) : TffTableName;

var
  DotPos   : integer;
  SlashPos : integer;
begin
  ExtractHelper(PFN, DotPos, SlashPos);
  Result := Copy(PFN, succ(SlashPos), FFMinI(pred(DotPos - SlashPos), ffcl_TableNameSize));
end;
{--------}
function FFFileExists(const PFN : TffFullFileName) : boolean;
var
  SR : TffSearchRec;
begin
  if (Pos('*', PFN) <> 0) or (Pos('?', PFN) <> 0) then
    Result := false
  else if (FFFindFirst(PFN, [ditFile], diaAnyAttr, SR) = 0) then begin
    Result := true;
    FFFindClose(SR);
  end
  else
    Result := false;
end;
{--------}
procedure FFFindClose(var SR : TffSearchRec);
begin
  if (SR.srHandle <> INVALID_HANDLE_VALUE) then begin
    Windows.FindClose(SR.srHandle);
    SR.srHandle := INVALID_HANDLE_VALUE;
  end;
end;
{--------}
function FFFindFirst(const PFN      : TffFullFileName;
                           ItemType : TffDirItemTypeSet;
                           Attr     : TffDirItemAttrSet;
                       var SR       : TffSearchRec) : integer;
var
  PathZ      : TffStringZ;
  GotAnError : boolean;
begin
  FillChar(SR, sizeof(SR), 0);
  SR.srFindType := ItemType;
  SR.srFindAttr := Attr;
  SR.srHandle := Windows.FindFirstFile(FFStrPCopy(PathZ, PFN), SR.srData);
  if (SR.srHandle = INVALID_HANDLE_VALUE) then
    Result := GetLastError
  else begin
    GotAnError := false;
    while (not GotAnError) and
          (not TypeAndAttrMatch(SR.srData.dwFileAttributes, SR.srFindType, SR.srFindAttr)) do
      if not Windows.FindNextFile(SR.srHandle, SR.srData) then
        GotAnError := true;
    if GotAnError then begin
      Windows.FindClose(SR.srHandle);
      Result := GetLastError;
    end
    else begin
      Result := 0;
      SearchRecConvertPrim(SR);
    end;
  end;
end;
{--------}
function FFFindNext(var SR : TffSearchRec) : integer;
var
  GotAnError : boolean;
begin
  if Windows.FindNextFile(SR.srHandle, SR.srData) then begin
    GotAnError := false;
    while (not GotAnError) and
          (not TypeAndAttrMatch(SR.srData.dwFileAttributes, SR.srFindType, SR.srFindAttr)) do
      if not Windows.FindNextFile(SR.srHandle, SR.srData) then
        GotAnError := true;
    if GotAnError then begin
      Result := GetLastError;
    end
    else begin
      Result := 0;
      SearchRecConvertPrim(SR);
    end;
  end
  else
    Result := GetLastError;
end;
{--------}
function FFForceExtension(const PFN : TffFullFileName;
                          const Ext : TffExtension) : TffFullFileName;
var
  DotPos : integer;
begin
  Result := PFN;
  if FFHasExtension(PFN, DotPos) then
    if (Ext = '') then
      SetLength(Result, pred(DotPos))
    else begin
      SetLength(Result, DotPos + length(Ext));
      Move(Ext[1], Result[succ(DotPos)], length(Ext));
    end
  else if (PFN <> '') and (Ext <> '') then begin
    FFShStrAddChar(Result, '.');
    FFShStrConcat(Result, Ext);
  end;
end;
{--------}
function FFGetCurDir : TffPath;
var
  CurDirZ : TffMaxPathZ;
  Len     : integer;
begin
  Len := GetCurrentDirectory(sizeof(CurDirZ), CurDirZ);
  if (Len = 0) then
    Result := ''
  else
    Result := FFStrPasLimit(CurDirZ, 255);
end;
{--------}
function FFGetDirList(const Path : TffPath; FileSpec : TffFileNameExt) : TffStringList;
var
  FullSearchPath : TffFullFileName;
  ErrorCode      : integer;
  SR             : TffSearchRec;
begin
  Result := TffStringList.Create;
  Try
  Result.Capacity := 32; {to avoid too many reallocs}
  Result.CaseSensitive := false;
  FullSearchPath := FFMakeFullFileName(Path, FileSpec);
  ErrorCode := FFFindFirst(FullSearchPath, [ditFile], diaAnyAttr, SR);
  while (ErrorCode = 0) do begin
    Result.Insert(SR.srName);
    ErrorCode := FFFindNext(SR);
  end;
  FFFindClose(SR);
  except
     Result.Free;
     Raise;
  end;
end;
{--------}
function FFGetEXEName : TffFullFileName;
begin
  Result := FFExpandFileName(ParamStr(0));
end;
{--------}
function FFHasExtension(const PFN : TffFullFileName; var DotPos : integer) : boolean;
var
  i : integer;
begin
  Result := false;
  DotPos := 0;
  for i := length(PFN) downto 1 do
    if (PFN[i] = '.') then begin
      DotPos := i;
      Result := true;
      Exit;
    end
    else if (PFN[i] = '\') then
      Exit;
end;
{--------}
function FFMakeFileNameExt(const FileName : TffFileName;
                           const Ext      : TffExtension) : TffFileNameExt;
begin
  Result := FileName;
  FFShStrAddChar(Result, '.');
  FFShStrConcat(Result, Ext);
end;
{--------}
function FFMakeFullFileName(const Path     : TffPath;
                            const FileName : TffFileNameExt) : TffFullFileName;
begin
  Result := Path;
  if (Result[length(Result)] <> '\') then
    FFShStrAddChar(Result, '\');
  FFShStrConcat(Result, FileName);
end;
{--------}
function FFSetCurDir(Path : TffPath) : boolean;
var
  DirZ : TffMaxPathZ;
begin
  Result := SetCurrentDirectory(FFStrPCopy(DirZ, Path));
end;
{====================================================================}


{===Bitset routines==================================================}
procedure FFClearAllBits(BitSet : PffByteArray; BitCount : integer);
begin
  FillChar(BitSet^, (BitCount+7) shr 3, 0);
end;
{--------}
procedure FFClearBit(BitSet : PffByteArray; Bit : integer);
var
  BS : PAnsiChar absolute BitSet;
  P  : PAnsiChar;
  M  : byte;
begin
  P := BS + (Bit shr 3);
  M := 1 shl (byte(Bit) and 7);
  P^ := AnsiChar(byte(P^) and not M);
end;
{--------}
function FFIsBitSet(BitSet : PffByteArray; Bit : integer) : boolean;
var
  BS : PAnsiChar absolute BitSet;
  P  : PAnsiChar;
  M  : byte;
begin
  P := BS + (Bit shr 3);
  M := 1 shl (byte(Bit) and 7);
  Result := (byte(P^) and M) <> 0;
end;
{--------}
procedure FFSetAllBits(BitSet : PffByteArray; BitCount : integer);
begin
  FillChar(BitSet^, (BitCount+7) shr 3, $FF);
end;
{--------}
procedure FFSetBit(BitSet : PffByteArray; Bit : integer);
var
  BS : PAnsiChar absolute BitSet;
  P  : PAnsiChar;
  M  : byte;
begin
  P := BS + (Bit shr 3);
  M := 1 shl (byte(Bit) and 7);
  P^ := AnsiChar(byte(P^) or M);
end;
{====================================================================}


{===Verification routines============================================}
function FFVerifyBlockSize(BlockSize : Longint) : boolean;
begin
  Result := (BlockSize =  4*1024) or
            (BlockSize =  8*1024) or
            (BlockSize = 16*1024) or
            (BlockSize = 32*1024) or
            (BlockSize = 64*1024);
end;
{--------}
function FFVerifyExtension(const Ext : TffExtension) : boolean;
begin
  Result := ValidFileNameHelper(Ext, ffcl_Extension);
end;
{--------}
function FFVerifyFileName(const FileName : TffFileName) : boolean;
begin
  Result := ValidFileNameHelper(FileName, ffcl_FileName);
end;
{--------}
function FFVerifyServerName(aName: TffNetAddress): Boolean;
var
  I: Integer;
begin
  aName := FFShStrTrim(aName);
  Result := not ((aName = '') or (Length(aName) > 15));
  if Result then
    for I := 1 to Length(aName) do
      if not (aName[I] in ['A'..'Z', 'a'..'z', '0'..'9', ' ']) then begin
        Result := False;
        Break;
      end;
end;
{--------}
function FFVerifyKeyLength(KeyLen : word) : boolean;
begin
  Result := (0 < KeyLen) and (KeyLen <= ffcl_MaxKeyLength);
end;
{====================================================================}

{===WWW Shell Routines===============================================}
procedure ShellToWWW;
resourcestring
  EX_Error = 'Unable to start web browser. Make sure you have it properly setup on your system.';
begin
  if ShellExecute(0, 'open', 'http://sourceforge.net/projects/tpflashfiler', '',
                  '', SW_SHOWNORMAL) <= 32 then
    ShowMessage(EX_Error);
end;
{--------}
procedure ShellToEMail;
resourcestring
  EX_Error = 'Unable to start Internet mail client. Make sure you have it properly setup on your system.';
begin
  ShowMessage('Email support disabled in open source version.');
//  if ShellExecute(0, 'open',
//       'mailto:support@turbopower.com',
//       '', '', SW_SHOWNORMAL) <= 32 then
//    ShowMessage(EX_Error);
end;
{====================================================================}


{===FlashFiler TffObject class=======================================}
class function TffObject.NewInstance: TObject;
begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
end;
{--------}
procedure TffObject.FreeInstance;
var
  Temp : pointer;
begin
  {$IFDEF FF_DEBUG_THREADS}                                            {!!.03}
  ThreadEnter;                                                         {!!.03}
  ThreadExit;                                                          {!!.03}
  ffoMethodLock := 2;                                                  {!!.03}
  {$ENDIF}                                                             {!!.03}
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
end;
{Begin !!.03}
{$IFDEF FF_DEBUG_THREADS}
{--------}
procedure TffObject.ThreadEnter;
begin
  case LockedExchange(ffoMethodLock, 1) of
    0: ; //ok
    2: raise Exception.Create('Attempt to access a destroyed object!');
  else
    ffoMethodLock := 3;
    raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
  end;
  try
    if ffoThreadLockCount > 0 then
      if ffoCurrentThreadID <> GetCurrentThreadID then
        raise Exception.Create('Multithreading violation [ObjID: ' +
                               IntToStr(Integer(Self)) +
                               ', Locking thread: ' +
                               IntToStr(ffoCurrentThreadID) +
                               ', Current thread: ' +
                               IntToStr(GetCurrentThreadID) +
                               ']')
      else
        Inc(ffoThreadLockCount)
    else begin
      ffoCurrentThreadID := GetCurrentThreadID;
      Inc(ffoThreadLockCount);
    end;
  finally
    case LockedExchange(ffoMethodLock, 0) of
      1: ; //ok
      2: raise Exception.Create('Attemp to access a destroyed object!');
    else
      ffoMethodLock := 3;
      raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
    end;
  end;
end;
{--------}
procedure TffObject.ThreadExit;
begin
  case LockedExchange(ffoMethodLock, 1) of
    0: ; //ok
    2: raise Exception.Create('Attempt to access a destroyed object!');
  else
    ffoMethodLock := 3;
    raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
  end;
  try
    if ffoThreadLockCount > 0 then
      if ffoCurrentThreadID <> GetCurrentThreadID then
        raise Exception.Create('Multithreading violation [ObjID: ' +
                               IntToStr(Integer(Self)) +
                               ', Locking thread: ' +
                               IntToStr(ffoCurrentThreadID) +
                               ', Current thread: ' +
                               IntToStr(GetCurrentThreadID) +
                               ']')
      else
        Dec(ffoThreadLockCount)
    else
      raise Exception.Create('ThreadEnter <-> ThreadExit');
  finally
    case LockedExchange(ffoMethodLock, 0) of
      1: ; //ok
      2: raise Exception.Create('Attemp to access a destroyed object!');
    else
      ffoMethodLock := 3;
      raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
    end;
  end;
end;
{$ENDIF}
{End !!.03}
{====================================================================}

{===FlashFiler TffVCLList class======================================}
class function TffVCLList.NewInstance: TObject;
begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
end;
{--------}
procedure TffVCLList.FreeInstance;
var
  Temp : pointer;
begin
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
end;
{====================================================================}

{===FlashFiler TffComponent class====================================}
constructor TffComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fcDestroying := False;
  fcLock := TffPadlock.Create;                                         {!!.11}
end;
{--------}
destructor TffComponent.Destroy;
var
  Idx : Integer;
begin
  FFNotifyDependents(ffn_Destroy);

{Begin !!.11}
  if Assigned(fcDependentList) then begin
    fcLock.Lock;
    try
      with fcDependentList do
        for Idx := Pred(Count) downto 0 do
          DeleteAt(Idx);
    finally
      fcLock.Unlock;
    end;
  end;  { if }
{End !!.11}
  fcDependentList.Free;
  fcLock.Free;                                                         {!!.11}
  inherited Destroy;
end;
{--------}
procedure TffComponent.FFAddDependent(ADependent : TffComponent);
{Rewritten!!.11}
var
  Item : TffIntListItem;                                             
begin
  if not Assigned(ADependent) then Exit;
  Assert(ADependent <> Self);                                          {!!.02}

  if not Assigned(fcDependentList) then
    fcDependentList := TffList.Create;
  fcLock.Lock;
  try
    with fcDependentList do
      if not Exists(Longint(ADependent)) then begin
        Item := TffIntListItem.Create(Longint(ADependent));
        Item.MaintainLinks := False;
        Insert(Item);
      end;
  finally
    fcLock.Unlock;
  end;
end;
{--------}
procedure TffComponent.FFNotification(const AOp : Byte; AFrom : TffComponent);
begin
  FFNotificationEX(AOp, AFrom, 0);
end;
{--------}
procedure TffComponent.FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                        const aData : TffWord32);
begin
  { do nothing at this level }
end;
{--------}
procedure TffComponent.FFNotifyDependents(const AOp : Byte);
var
  Idx : Integer;
begin
  if (fcDestroying and (AOp = ffn_Destroy)) then
    Exit;
{Begin !!.11}
  if Assigned(fcDependentList) then begin
    fcLock.Lock;
    try
      fcDestroying := AOp = ffn_Destroy;
      for Idx := Pred(fcDependentList.Count) downto 0 do
        TffComponent(TffIntListItem(fcDependentList[Idx]).KeyAsInt).FFNotification(AOp, Self);
    finally
      fcLock.Unlock;
    end;
  end;  { if }
{End !!.11}
end;
{--------}
procedure TffComponent.FFNotifyDependentsEx(const AOp : Byte; const AData : TffWord32);
var
  Idx : Integer;
begin
  if (fcDestroying and (AOp = ffn_Destroy)) then
    Exit;
{Begin !!.11}
  if Assigned(fcDependentList) then begin
    fcLock.Lock;
    try
      fcDestroying := AOp = ffn_Destroy;
      for Idx := Pred(fcDependentList.Count) downto 0 do
        TffComponent(TffIntListItem(fcDependentList[Idx]).KeyAsInt).FFNotificationEx(AOp, Self, AData);
    finally
      fcLock.Unlock;
    end;
  end;  { if }
end;
{--------}
procedure TffComponent.FFRemoveDependent(ADependent: TffComponent);
begin
{Begin !!.11}
  if Assigned(ADependent) and Assigned(fcDependentList) then begin
    fcLock.Lock;
    try
      fcDependentList.Delete(Longint(ADependent));
    finally
      fcLock.Unlock;
    end;
  end;  { if }
{End !!.11}
end;
{--------}
{$IFDEF IsDelphi}                                                      {!!.03}
class function TffComponent.NewInstance: TObject;
begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
end;
{--------}
procedure TffComponent.FreeInstance;
var
  Temp : pointer;
begin
  {$IFDEF FF_DEBUG_THREADS}                                            {!!.03}
  ThreadEnter;                                                         {!!.03}
  ThreadExit;                                                          {!!.03}
  ffcMethodLock := 2;                                                  {!!.03}
  {$ENDIF}                                                             {!!.03}
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
end;
{$ENDIF}                                                               {!!.03}
{Begin !!.03}
{$IFDEF FF_DEBUG_THREADS}
{--------}
procedure TffComponent.ThreadEnter;
begin
  case LockedExchange(ffcMethodLock, 1) of
    0: ; //ok
    2: raise Exception.Create('Attemp to access a destroyed object!');
  else
    ffcMethodLock := 3;
    raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
  end;
  try
    if ffcThreadLockCount>0 then
      if ffcCurrentThreadID <> GetCurrentThreadID then
        raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']')
      else
        Inc(ffcThreadLockCount)
    else begin
      ffcCurrentThreadID := GetCurrentThreadID;
      Inc(ffcThreadLockCount);
    end;
  finally
    case LockedExchange(ffcMethodLock, 0) of
      1: ; //ok
      2: raise Exception.Create('Attemp to access a destroyed object!');
    else
      ffcMethodLock := 3;
      raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
    end;
  end;
end;
{--------}
procedure TffComponent.ThreadExit;
begin
  case LockedExchange(ffcMethodLock, 1) of
    0: ; //ok
    2: raise Exception.Create('Attemp to access a destroyed object!');
  else
    ffcMethodLock := 3;
    raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
  end;
  try
    if ffcThreadLockCount>0 then
      if ffcCurrentThreadID <> GetCurrentThreadID then
        raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']')
      else
        Dec(ffcThreadLockCount)
    else
      raise Exception.Create('ThreadEnter <-> ThreadExit');
  finally
    case LockedExchange(ffcMethodLock, 0) of
      1: ; //ok
      2: raise Exception.Create('Attemp to access a destroyed object!');
    else
      ffcMethodLock := 3;
      raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
    end;
  end;
end;
{$ENDIF}
{End !!.03}
{--------}
function TffComponent.GetVersion : string;
begin
  Result := Format('%5.4f', [ffVersionNumber / 10000.0]);
end;
{--------}
procedure TffComponent.SetVersion(const Value : string);
begin
  {do nothing}
end;
{====================================================================}

{===FlashFiler TffPersistent class===================================}
class function TffPersistent.NewInstance: TObject;
begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
end;
{--------}
procedure TffPersistent.FreeInstance;
var
  Temp : pointer;
begin
  {$IFDEF FF_DEBUG_THREADS}                                            {!!.03}
  ThreadEnter;                                                         {!!.03}
  ThreadExit;                                                          {!!.03}
  ffpMethodLock := 2;                                                  {!!.03}
  {$ENDIF}                                                             {!!.03}
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
end;
{Begin !!.03}
{$IFDEF FF_DEBUG_THREADS}
{--------}
procedure TffPersistent.ThreadEnter;
begin
  case LockedExchange(ffpMethodLock, 1) of
    0: ; //ok
    2: raise Exception.Create('Attemp to access a destroyed object!');
  else
    ffpMethodLock := 3;
    raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
  end;
  try
    if (ffpThreadLockCount>0) then
      if ffpCurrentThreadID <> GetCurrentThreadID then
        raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']')
      else
        Inc(ffpThreadLockCount)
    else begin
      ffpCurrentThreadID := GetCurrentThreadID;
      Inc(ffpThreadLockCount);
    end;
  finally
    case LockedExchange(ffpMethodLock, 0) of
      1: ; //ok
      2: raise Exception.Create('Attemp to access a destroyed object!');
    else
      ffpMethodLock := 3;
      raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
    end;
  end;
end;
{--------}
procedure TffPersistent.ThreadExit;
begin
  case LockedExchange(ffpMethodLock, 1) of
    0: ; //ok
    2: raise Exception.Create('Attemp to access a destroyed object!');
  else
    ffpMethodLock := 3;
    raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
  end;
  try
    if (ffpThreadLockCount>0) then
      if ffpCurrentThreadID <> GetCurrentThreadID then
        raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']')
      else
        Dec(ffpThreadLockCount)
    else
      raise Exception.Create('ThreadEnter <-> ThreadExit');
  finally
    case LockedExchange(ffpMethodLock, 0) of
      1: ; //ok
      2: raise Exception.Create('Attemp to access a destroyed object!');
    else
      ffpMethodLock := 3;
      raise Exception.Create('Multithreading violation [ObjID: '+IntToStr(Integer(Self))+']');
    end;
  end;
end;
{$ENDIF}
{End !!.03}
{====================================================================}

{===FlashFiler TffThread class=======================================}
procedure TffThread.DoTerminate;
begin
  if Assigned(OnTerminate) then OnTerminate(Self);
end;
{--------}
class function TffThread.NewInstance: TObject;
begin
  FFGetMem(Result, InstanceSize);
  InitInstance(Result);
end;
{--------}
procedure TffThread.FreeInstance;
var
  Temp : pointer;
begin
  Temp := Self;
  CleanupInstance;
  FFFreeMem(Temp, InstanceSize);
end;
{Begin !!.02}
{--------}
procedure TffThread.WaitForEx(const Timeout : Longint);
var
  H: THandle;
  Msg: TMsg;
begin
  H := Handle;

  if GetCurrentThreadID = MainThreadID then
    while MsgWaitForMultipleObjects(1, H, False, Timeout, QS_SENDMESSAGE) =
          WAIT_OBJECT_0 + 1 do
      PeekMessage(Msg, 0, 0, 0, PM_NOREMOVE)
  else
    WaitForSingleObject(H, Timeout);
end;
{End !!.02}
{====================================================================}

{===FlashFiler List and List Item classes============================}
constructor TffListItem.Create;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  ffliList := TffList.Create;
  ffliState := lsNormal;
  ffliMaintainLinks := True;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffListItem.Destroy;
var
  inx : integer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  ffliState := lsClearing;
{Begin !!.11}
  if ffliList <> nil then begin
    for inx := 0 to pred(ffliList.Count) do
      TffList(TffIntListItem(ffliList[inx]).KeyAsInt).InternalDelete(Key^); {!!.02}
    ffliList.Free;
  end;
{End !!.11}
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffListItem.ffliAddListLink(L : TffList);
var
  anItem : TffIntListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  {NOTE: this only gets called from a TffList object, so there's no
         need to insert Self into the calling list: it will do it
         itself}
  if (ffliList.Index(Longint(L)) = -1) then begin
    anItem := TffIntListItem.Create(Longint(L));
    { Turn off link maintenance for the item otherwise we will
      get into an infinitely recursive death spiral. }
    anItem.MaintainLinks := False;
    ffliList.Insert(anItem);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffListItem.ffliBreakListLink(L : TffList);
var
  inx : integer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  {NOTE: this only gets called from a TffList object, so there's no
         need to remove Self from the calling list: it will do it
         itself}
  if (ffliState = lsNormal) then begin
    inx := ffliList.Index(Longint(L));
    if (inx <> -1) then
      ffliList.DeleteAt(inx);
    if ffliFreeOnRemove then begin
      ffliState := lsClearing;
      for inx := pred(ffliList.Count) downto 0 do
        TffList(TffIntListItem(ffliList[inx]).KeyAsInt).InternalDelete(Key^); {!!.02}
      ffliList.Empty;
      ffliState := lsNormal;
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.11}
{--------}
procedure TffListItem.ffliSetMaintainLinks(const Value : Boolean);
{ Rewritten !!.12}
begin
  ffliMaintainLinks := Value;
  if not Value then begin
    ffliList.Free;
    ffliList := nil;
  end
  else if ffliList = nil then
    ffliList := TffList.Create;
end;
{End !!.11}
{--------}
function TffListItem.GetRefCount : integer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
{Begin !!.11}
  if ffliList <> nil then
    Result := ffliList.Count
  else
    Result := 0;
{End !!.11}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
constructor TffStrListItem.Create(const aKey : TffShStr);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  sliKey := FFShStrAlloc(aKey);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffStrListItem.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  {NOTE: inherited Destroy must be called first, because it will in
         turn make a call to get the Key for the item, and so the
         Key pointer had still better exist.}
  inherited Destroy;
  FFShStrFree(sliKey);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStrListItem.Compare(aKey : pointer) : integer;
begin
  Result := FFCmpShStr(PffShStr(aKey)^, sliKey^, 255);
end;
{--------}
function TffStrListItem.Key : pointer;
begin
  Result := sliKey;
end;
{--------}
function TffStrListItem.KeyAsStr : TffShStr;
begin
  Result := sliKey^;
end;
{--------}
function TffUCStrListItem.Compare(aKey : pointer) : integer;
begin
  Result := FFCmpShStrUC(PffShStr(aKey)^, PffShStr(Key)^, 255);
end;
{--------}
constructor TffIntListItem.Create(const aKey : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  iliKey := aKey;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffIntListItem.Compare(aKey : pointer) : integer;
begin
  Result := FFCmpI32(PffLongint(aKey)^, iliKey);
end;
{--------}
function TffIntListItem.Key : pointer;
begin
  Result := @iliKey;
end;
{--------}
function TffIntListItem.KeyAsInt : Longint;
begin
  Result := iliKey;
end;
{--------}
constructor TffWord32ListItem.Create(const aKey : TffWord32);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  wliKey := aKey;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffWord32ListItem.Compare(aKey : pointer) : integer;
begin
  Result := FFCmpDW(PffWord32(aKey)^, wliKey);
end;
{--------}
function TffWord32ListItem.Key : pointer;
begin
  Result := @wliKey;
end;
{--------}
function TffWord32ListItem.KeyAsInt : TffWord32;
begin
  Result := wliKey;
end;
{--------}
function TffWord32ListItem.KeyValue : TffWord32;
begin
  Result := wliKey;
end;
{--------}
constructor TffI64ListItem.Create(const aKey : TffInt64);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  iliKey := aKey;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffI64ListItem.Compare(aKey : pointer) : integer;
begin
  Result := FFCmpI64(PffInt64(aKey)^, iliKey);
end;
{--------}
function TffI64ListItem.Key : pointer;
begin
  Result := @iliKey;
end;
{--------}
function TffI64ListItem.KeyValue : TffInt64;
begin
  Result := iliKey;
end;
{--------}
constructor TffSelfListItem.Create;
begin
  inherited Create(Longint(Self));
end;
{--------}
constructor TffList.Create;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  fflState := lsNormal;
  { Allocate space for the initial number of items. }
  FFGetMem(fflList, ffcl_InitialListSize * sizeOf(TffListItem));
  FillChar(fflList^, ffcl_InitialListSize * sizeOf(TffListItem), 0);
  fflCapacity := ffcl_InitialListSize;
  fflCount := 0;
  fflSorted := true;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffList.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Empty;
  FFFreeMem(fflList, fflCapacity * sizeOf(TffListItem));
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
{Deleted !!.01}
{procedure TffList.Assign(Source : TPersistent);
var
  SrcList : TffList;
  i       : Longint;
begin
  if (Source is TffList) then begin
    Empty;
    SrcList := TffList(Source);
    for i := 0 to pred(SrcList.Count) do
      Insert(SrcList.Items[i]);
  end
  else
    inherited Assign(Source);
end;}
{--------}
procedure TffList.Delete(const aKey);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  fflDeleteAtPrim(fflIndexPrim(aKey));
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.02}
{--------}
procedure TffList.InternalDelete(const aKey);
begin
  if Assigned(fflPortal) then
    fflPortal.BeginWrite;
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    fflDeleteAtPrim(fflIndexPrim(aKey));
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    if Assigned(fflPortal) then
      fflPortal.EndWrite;
  end;
end;
{End !!.02}
{--------}
procedure TffList.DeleteAt(aInx : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  fflDeleteAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.fflDeleteAtPrim(aInx : Longint);
var
  Item : TffListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (fflState = lsNormal) and
     (0 <= aInx) and
     (aInx < fflCount) then begin
    Item := fflList^[aInx];
    if assigned(Item) then begin
      if Item.MaintainLinks then
        Item.ffliBreakListLink(Self);
      if (Item.ReferenceCount = 0) then
        Item.Free;
      dec(fflCount);
      if aInx < fflCount then
       Move(fflList^[aInx + 1], fflList^[aInx],
           (fflCount - aInx) * SizeOf(TffListItem));
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.Empty;
var
  Inx  : Longint;
  Item : TffListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  fflState := lsClearing;
  try
    for Inx := pred(fflCount) downto 0 do begin
      Item := fflList^[Inx];
      if assigned(Item) then begin
        if Item.MaintainLinks then
          Item.ffliBreakListLink(Self);
        if (Item.ReferenceCount = 0) then
          Item.Free;
        dec(fflCount);
      end;
    end;
  { Zero out the array. }
  fillChar(fflList^, fflCapacity * sizeOf(TffListItem), 0);
  finally
    fflState := lsNormal;
  end;{try..finally}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function  TffList.Exists(const aKey) : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := fflIndexPrim(aKey) <> -1;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.fflGrow;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  SetCapacity(fflCapacity + ffcl_InitialListSize);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffList.GetCapacity : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := fflCapacity;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffList.GetCount : Longint;
begin
  Result := fflCount;
end;
{--------}
function TffList.GetInsertionPoint(aItem : TffListItem) : Longint;
var
  OurCount: Longint;
  L, R, M : Longint;
  CompareResult : integer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  OurCount := fflCount;
  {take care of the easy case}
  if (OurCount = 0) then
    L := 0
  else if Sorted then begin
    {standard binary search}
    L := 0;
    R := pred(OurCount);
    repeat
      M := (L + R) div 2;
      CompareResult := fflList^[M].Compare(aItem.Key);
      if (CompareResult = 0) then begin
        {do nothing, key already exists}
        Result := -1;
        Exit;
      end
      else if (CompareResult < 0) then
        R := M - 1
      else
        L := M + 1
    until (L > R);
    {as it happens, on exit from this repeat..until loop the
     algorithm will have set L to the correct insertion point}
  end
  else {not Sorted}
    L := OurCount;

  Result := L;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffList.GetItem(const aInx : Longint) : TffListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (aInx >= 0) and (aInx < fflCount) then
    Result := fflList^[aInx]
  else
    Result := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffList.Insert(aItem : TffListItem) : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := InsertPrim(aItem) <> -1;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffList.InsertPrim(aItem : TffListItem) : Longint;
var
  L : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Determine the insertion point. }
  L := GetInsertionPoint(aItem);
  if L >= 0 then begin
    { If we are at the limit then increase capacity. }
    if fflCount = fflCapacity then
      fflGrow;

    { If we are before the last element in the list, shift everything up. }
    if L < fflCount then
      Move(fflList^[L], fflList^[L + 1], (fflCount - L) * sizeOf(TffListItem));

    fflList^[L] := aItem;
    if aItem.MaintainLinks then
      aItem.ffliAddListLink(Self);
    inc(fflCount);
  end;
  Result := L;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function  TffList.IsEmpty : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := Count = 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function  TffList.Index(const aKey) : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := fflIndexPrim(aKey);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function  TffList.fflIndexPrim(const aKey) : Longint;
var
  M, L, R : Longint;
  CompareResult : integer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (fflCount > 0) then                                               {!!.11}
    if Sorted then begin
      {standard binary search}
      L := 0;
      R := pred(fflCount);
      repeat
        M := (L + R) div 2;
        CompareResult := fflList^[M].Compare(@aKey);
        if (CompareResult = 0) then begin
          Result := M;
          Exit;
        end
        else if (CompareResult < 0) then
          R := M - 1
        else
          L := M + 1
      until (L > R);
    end
    else {not Sorted} begin
      {standard sequential search}
      for M := 0 to pred(fflCount) do
        if (fflList^[M].Compare(@aKey) = 0) then begin
          Result := M;
          Exit;
        end
    end;
  Result := -1;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.Remove(const aKey);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  fflRemoveAtPrim(fflIndexPrim(aKey));
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.RemoveAt(aInx : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  fflRemoveAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.fflRemoveAtPrim(aInx : Longint);
var
  Item : TffListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (fflState = lsNormal) and
     (0 <= aInx) and
     (aInx < fflCount) then begin
    Item := fflList^[aInx];
    if assigned(Item) then begin
      if Item.MaintainLinks then
        Item.ffliBreakListLink(Self);
      { Note: the item is not freed }
      dec(fflCount);
      if aInx < fflCount then
       Move(fflList^[aInx + 1], fflList^[aInx],
           (fflCount - aInx) * SizeOf(TffListItem));
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.SetCapacity(const C : Longint);
var
  NewList : PffListItemArray;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (C >= fflCount) and (C <> fflCapacity) then begin
    { Get a new block. }
    FFGetMem(NewList, C * sizeOf(TffListItem));
    FillChar(NewList^, C * sizeOf(TffListItem), 0);

    { Transfer the existing data. }
    Move(fflList^, NewList^, fflCount * SizeOf(TffListItem));

    { Free the existing data. }
    FFFreeMem(fflList, fflCapacity * SizeOf(TffListItem));
    fflList := NewList;
    fflCapacity := C;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.SetCount(const C : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Do we need to grow the table? }
  if C <> fflCapacity then
    SetCapacity(C);
  fflCount := C;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.SetItem(const aInx : Longint; Item : TffListItem);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (0 <= aInx) and (aInx < fflCount) then
    fflList^[aInx] := Item;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffList.SetSorted(S : boolean);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (S <> fflSorted) then
    fflSorted := (S and IsEmpty);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}

{===TffPointerList===================================================}
constructor TffPointerList.Create;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  { Allocate space for the initial number of items. }
  FFGetMem(plList, ffcl_InitialListSize * sizeOf(Pointer));
  FillChar(plList^, ffcl_InitialListSize * sizeOf(Pointer), 0);
  plCapacity := ffcl_InitialListSize;
  plCount := 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffPointerList.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  FFFreeMem(plList, plCapacity * sizeOf(Pointer));
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffPointerList.Assign(Source : TPersistent);
var
  SrcList : TffPointerList;
  i       : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (Source is TffPointerList) then begin
    Empty;
    SrcList := TffPointerList(Source);
    for i := 0 to pred(SrcList.Count) do
      Append(SrcList.Pointers[i]);
  end
  else
    inherited Assign(Source);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffPointerList.Empty;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Did the array contain anything? }
  if plCount > 0 then
    { Yes. Zero it out. }
    FillChar(plList^, plCapacity * sizeOf(Pointer), 0);
  plCount := 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffPointerList.fflGrow;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  SetCapacity(plCapacity + ffcl_InitialListSize);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffPointerList.GetCapacity : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := plCapacity;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffPointerList.GetCount : Longint;
begin
  Result := plCount;
end;
{--------}
function TffPointerList.GetPointer(aInx : Longint) : Pointer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (0 <= aInx) and (aInx < plCount) then
    Result := plList^[aInx]
  else
    Result := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffPointerList.GetInternalAddress : pointer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := pointer(plList);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffPointerList.Append(aPtr : Pointer) : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := AppendPrim(aPtr) <> -1;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffPointerList.AppendPrim(aPtr : Pointer) : Longint;
var
  L : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Determine the insertion point. }
  L := plCount;
  if L >= 0 then begin
    { If we are at the limit then increase capacity. }
    if plCount = plCapacity then
      fflGrow;

    { If we are before the last element in the list, shift everything up. }
    if L < plCount then
      Move(plList^[L], plList^[L + 1], (plCount - L) * sizeOf(Pointer));

    plList^[L] := aPtr;
    inc(plCount);
  end;
  Result := L;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function  TffPointerList.IsEmpty : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := Count = 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffPointerList.RemoveAt(aInx : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  fflRemoveAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffPointerList.fflRemoveAtPrim(aInx : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (0 <= aInx) and
     (aInx < plCount) then begin
    dec(plCount);
    if aInx < plCount then
     Move(plList^[aInx + 1], plList^[aInx],
         (plCount - aInx) * SizeOf(Pointer));
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffPointerList.SetCapacity(const C : Longint);
var
  NewList : PffPointerArray;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (C >= plCount) and (C <> plCapacity) then begin
    { Get a new block. }
    FFGetMem(NewList, C * sizeOf(Pointer));
    FillChar(NewList^, C * sizeOf(Pointer), 0);

    { Transfer the existing data. }
    Move(plList^, NewList^, plCount * SizeOf(Pointer));

    { Free the existing data. }
    FFFreeMem(plList, plCapacity * SizeOf(Pointer));
    plList := NewList;
    plCapacity := C;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffPointerList.SetCount(const C : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Do we need to grow the table? }
  if C > plCapacity then
    SetCapacity(C);
  plCount := C;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffPointerList.SetPointer(aInx : Longint; aPtr : Pointer);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Is the index within range? }
  if (0 <= aInx) and (aInx < plCount) then
    plList^[aInx] := aPtr;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}

{===TffHandleList====================================================}
constructor TffHandleList.Create;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  { Allocate space for the initial number of items. }
  FFGetMem(FList, ffcl_InitialListSize * sizeOf(THandle));
  FillChar(FList^, ffcl_InitialListSize * sizeOf(THandle), 0);
  FCapacity := ffcl_InitialListSize;
  FCount := 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffHandleList.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Empty;
  FFFreeMem(FList, FCapacity * sizeOf(THandle));
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.Assign(Source : TPersistent);
var
  SrcList : TffHandleList;
  i       : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (Source is TffHandleList) then begin
    Empty;
    SrcList := TffHandleList(Source);
    for i := 0 to pred(SrcList.Count) do
      Append(SrcList.Handles[i]);
  end
  else
    inherited Assign(Source);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.DeleteAt(aInx : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  fflDeleteAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.fflDeleteAtPrim(aInx : Longint);
var
  aHandle : THandle;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (0 <= aInx) and
     (aInx < FCount) then begin
    aHandle := FList^[aInx];
    CloseHandle(aHandle);
    dec(FCount);
    if aInx < FCount then
     Move(FList^[aInx + 1], FList^[aInx],
         (FCount - aInx) * SizeOf(THandle));
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.Empty;
var
  Inx  : Longint;
  aHandle : THandle;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  for Inx := pred(FCount) downto 0 do begin
    aHandle := FList^[Inx];
    CloseHandle(aHandle);
    dec(FCount);
  end;
  { Zero out the array. }
  fillChar(FList^, FCapacity * sizeOf(THandle), 0);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.fflGrow;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  SetCapacity(FCapacity + ffcl_InitialListSize);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffHandleList.GetCapacity : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := FCapacity;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffHandleList.GetCount : Longint;
begin
  Result := FCount;
end;
{--------}
function TffHandleList.GetHandle(aInx : Longint) : THandle;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (0 <= aInx) and (aInx < FCount) then
    Result := FList^[aInx]
  else
    Result := 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffHandleList.GetInternalAddress : pointer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := pointer(FList);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffHandleList.Append(aHandle : THandle) : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := AppendPrim(aHandle) <> -1;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffHandleList.AppendPrim(aHandle : THandle) : Longint;
var
  L : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Determine the insertion point. }
  L := FCount;
  if L >= 0 then begin
    { If we are at the limit then increase capacity. }
    if FCount = FCapacity then
      fflGrow;

    { If we are before the last element in the list, shift everything up. }
    if L < FCount then
      Move(FList^[L], FList^[L + 1], (FCount - L) * sizeOf(THandle));

    FList^[L] := aHandle;
    inc(FCount);
  end;
  Result := L;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function  TffHandleList.IsEmpty : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := Count = 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.RemoveAll;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  FCount := 0;
  { Zero out the array. }
  fillChar(FList^, FCapacity * sizeOf(THandle), 0);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.RemoveAt(aInx : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  fflRemoveAtPrim(aInx);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.fflRemoveAtPrim(aInx : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (0 <= aInx) and
     (aInx < FCount) then begin
    { Note: The handle is not closed. }
    dec(FCount);
    if aInx < FCount then
     Move(FList^[aInx + 1], FList^[aInx],
         (FCount - aInx) * SizeOf(THandle));
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.SetCapacity(const C : Longint);
var
  NewList : PffHandleArray;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (C >= FCount) and (C <> FCapacity) then begin
    { Get a new block. }
    FFGetMem(NewList, C * sizeOf(THandle));
    FillChar(NewList^, C * sizeOf(THandle), 0);

    { Transfer the existing data. }
    Move(FList^, NewList^, FCount * SizeOf(THandle));

    { Free the existing data. }
    FFFreeMem(FList, FCapacity * SizeOf(THandle));
    FList := NewList;
    FCapacity := C;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffHandleList.SetCount(const C : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Do we need to grow the table? }
  if C > FCapacity then
    SetCapacity(C);
  FCount := C;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}

{===TffThreadList====================================================}
constructor TffThreadList.Create;
begin
  inherited Create;
  fflPortal := TffReadWritePortal.Create;
end;
{--------}
destructor TffThreadList.Destroy;
begin
  fflPortal.Free;
  inherited Destroy;
end;
{--------}
function TffThreadList.BeginRead : TffThreadList;
begin
  if isMultiThread then
    fflPortal.BeginRead;
  Result := Self;
end;
{--------}
function TffThreadList.BeginWrite : TffThreadList;
begin
  if isMultiThread then
    fflPortal.BeginWrite;
  Result := Self;
end;
{--------}
procedure TffThreadList.EndRead;
begin
  if isMultiThread then
    fflPortal.EndRead;
end;
{--------}
procedure TffThreadList.EndWrite;
begin
  if isMultiThread then
    fflPortal.EndWrite;
end;
{====================================================================}


{===TffStringList====================================================}
constructor TffStringList.Create;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  slCaseSensitive := true;
  slList := TffList.Create;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffStringList.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  slList.Free;
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.Assign(Source : TPersistent);
var
  StrList : TffStringList;
  Strs    : TStrings;
  I       : Longint;
  Inx     : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}

  StrList := TffStringList(Source);
  Strs := TStrings(Source);

  if Source is TffStringList then begin
    Empty;

    CaseSensitive := StrList.CaseSensitive;
    Sorted := StrList.Sorted;

    for I := 0 to StrList.Count - 1 do begin
      Inx := InsertPrim(StrList.Strings[I]);
      Objects[Inx] := StrList.Objects[I];
    end;
  end
  else if Source is TStrings then begin
    Empty;
    Sorted := false;
    for I := 0 to Strs.Count - 1 do begin
      Insert(Strs.Strings[I]);
      Objects[I] := Strs.Objects[I];
    end;
  end
  else
    inherited Assign(Source);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.AssignTo(Dest : TPersistent);
var
  StrList : TffStringList;
  Strs    : TStrings;
  I       : Longint;
  Inx     : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}

  StrList := TffStringList(Dest);
  Strs := TStrings(Dest);

  if Dest is TffStringList then begin

    StrList.Empty;
    StrList.CaseSensitive := CaseSensitive;
    StrList.Sorted := Sorted;

    for I := 0 to pred(Count) do begin
      Inx := StrList.InsertPrim(Strings[I]);
      StrList.Objects[Inx] := Objects[I];
    end;
  end
  else if Dest is TStrings then begin
    Strs.Clear;
    for I := 0 to pred(Count) do begin
      Strs.Add(Strings[I]);
      Strs.Objects[I] := Objects[I];
    end;
  end
  else
    inherited AssignTo(Dest);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.Delete(const aStr : TffShStr);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  slList.Delete(aStr);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.DeleteAt(aInx : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  slList.DeleteAt(aInx);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.Empty;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  slList.Empty;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.Exists(const aStr : TffShStr) : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := slList.Exists(aStr);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.GetCapacity : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := slList.Capacity;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.GetCount : Longint;
begin
  Result := slList.Count;
end;
{--------}
function TffStringList.GetObj(aInx : Longint) : TObject;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := TObject(TffStrListItem(slList.Items[aInx]).ExtraData);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.GetSorted : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := slList.Sorted;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.GetStr(aInx : Longint) : TffShStr;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := TffStrListItem(slList.Items[aInx]).KeyAsStr;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.GetValue(const aName: TffShStr) : TffShStr;
var
  I: Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  I := IndexOfName(aName);
  if I >= 0 then Result := GetStr(I)
  else Result := '';
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.IndexOfName(const aName: TffShStr): Longint;
var
  P: Longint;
  S: TffShStr;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  for Result := 0 to GetCount - 1 do
  begin
    S := GetStr(Result);
    P := Pos('=', S);
    if (P <> 0) and (FFCmpShStr(Copy(S, 1, P - 1), aName, 255) = 0) then Exit;
  end;
  Result := -1;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.Insert(const aStr : TffShStr) : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := InsertPrim(aStr) <> -1;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.InsertPrim(const aStr : TffShStr) : Longint;
var
  Item    : TffStrListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if CaseSensitive then
    Item := TffStrListItem.Create(aStr)
  else
    Item := TffUCStrListItem.Create(aStr);
  try
    Result := slList.InsertPrim(Item);
    if Result < 0 then                                                 {!!.10}
      Item.Free;                                                       {!!.10}
  except
    Item.Free;
    raise;
  end;{try..except}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.IsEmpty : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := slList.Count = 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffStringList.Index(const aStr : TffShStr) : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := slList.Index(aStr);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.SetCapacity(C : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  slList.Capacity := C;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.SetCaseSensitive(CS : boolean);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (slList.Count = 0) then
    slCaseSensitive := CS;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.SetObj(aInx : Longint; const aObj : TObject);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  TffStrListItem(slList.Items[aInx]).ExtraData := pointer(aObj);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.SetSorted(S : boolean);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  slList.Sorted := S;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.SetStr(aInx : Longint; const aStr : TffShStr);
var
  Item : TffStrListItem;
  Obj  : TObject;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  {get the current item}
  Item := TffStrListItem(slList.Items[aInx]);
  if (Item = nil) then
    Exit;
  if slList.Sorted then begin
    {delete the old item, create a new one and insert it}
    Obj := TObject(Item.ExtraData);
    slList.DeleteAt(aInx);
    if CaseSensitive then
      Item := TffStrListItem.Create(aStr)
    else
      Item := TffUCStrListItem.Create(aStr);
    Item.ExtraData := pointer(Obj);
    try
      slList.Insert(Item);
    except
      Item.Free;
      raise;
    end;
  end
  else {the list is not sorted} begin
    FFShStrFree(Item.sliKey);
    Item.sliKey := FFShStrAlloc(aStr);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffStringList.SetValue(const aName, aStr : TffShStr);
var
  Idx: Integer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Idx := IndexOfName(aName);
  if aStr <> '' then begin
    if Idx < 0 then begin
      { Item doesn't already exist }
      Insert(aName);
      Idx := IndexOfName(aName);
    end;
    SetStr(Idx, aName + '=' + aStr);
  end
  else begin
    if Idx >= 0 then DeleteAt(Idx);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}

{===TffThreadStringList==============================================}
constructor TffThreadStringList.Create;
begin
  inherited Create;
  tslPortal := TffReadWritePortal.Create;
  slList.fflPortal := tslPortal                                             {!!.02}
end;
{--------}
destructor TffThreadStringList.Destroy;
begin
  tslPortal.Free;
  inherited Destroy;
end;
{--------}
function TffThreadStringList.BeginRead : TffThreadStringList;
begin
  tslPortal.BeginRead;
  Result := Self;
end;
{--------}
function TffThreadStringList.BeginWrite : TffThreadStringList;
begin
  tslPortal.BeginWrite;
  Result := Self;
end;
{--------}
procedure TffThreadStringList.EndRead;
begin
  tslPortal.EndRead;
end;
{--------}
procedure TffThreadStringList.EndWrite;
begin
  tslPortal.EndWrite;
end;
{====================================================================}

{===TffQueue=========================================================}
constructor TffQueue.Create;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  ffqList := TffList.Create;
  { Turn off sorting so that items are appended to list. }
  ffqList.Sorted := False;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffQueue.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  ffqList.Free;
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffQueue.Delete(const aKey);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  ffqList.Delete(aKey);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffQueue.Dequeue : TffListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := nil;
  if GetCount > 0 then begin
    Result := ffqList[0];
    ffqList.RemoveAt(0);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffQueue.Enqueue(anItem : TffListItem);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  ffqList.Insert(anItem);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffQueue.GetCount : Longint;
begin
  Result := ffqList.Count;
end;
{--------}
function TffQueue.IsEmpty : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := (ffqList.Count = 0);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffQueue.GetItem(aInx : Longint) : TffListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := ffqList[aInx];
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
{====================================================================}

{===TffThreadQueue===================================================}
constructor TffThreadQueue.Create;
begin
  inherited Create;
  fftqPortal := TffReadWritePortal.Create;
  ffqList.fflPortal := fftqPortal                                       {!!.02}
end;
{--------}
destructor TffThreadQueue.Destroy;
begin
  fftqPortal.Free;
  inherited Destroy;
end;
{--------}
function TffThreadQueue.BeginRead : TffThreadQueue;
begin
  fftqPortal.BeginRead;
  Result := Self;
end;
{--------}
function TffThreadQueue.BeginWrite : TffThreadQueue;
begin
  fftqPortal.BeginWrite;
  Result := Self;
end;
{--------}
procedure TffThreadQueue.EndRead;
begin
  fftqPortal.EndRead;
end;
{--------}
procedure TffThreadQueue.EndWrite;
begin
  fftqPortal.EndWrite;
end;
{====================================================================}

{===TffLatch=========================================================}
constructor TffEvent.Create;
begin
  inherited Create;
  {$IFDEF UseEventPool}
  if Assigned(FFEventPool) then begin
    ffeEvent := FFEventPool.Get;
    { Make sure the event is not signaled. }
    ResetEvent(ffeEvent);
  end
  else
    ffeEvent := CreateEvent(nil, False, False, nil);
  {$ELSE}
  ffeEvent := CreateEvent(nil, False, False, nil);
  {$ENDIF}
end;
{--------}
destructor TffEvent.Destroy;
begin
  {$IFDEF UseEventPool}
  if Assigned(FFEventPool) then
    FFEventPool.Put(ffeEvent)
  else
    CloseHandle(ffeEvent);
  {$ELSE}
  CloseHandle(FEvent);
  {$ENDIF}
  inherited Destroy;
end;
{--------}
procedure TffEvent.WaitFor(const timeOut : TffWord32);
var
  aTimeOut : TffWord32;
  waitResult : DWord;
begin
  if timeOut <= 0 then
    aTimeOut := ffcl_INFINITE                                         {!!.06}
  else
    aTimeout := timeOut;

  waitResult := WaitForSingleObject(ffeEvent, aTimeout);
  if waitResult = WAIT_TIMEOUT then
    raise EffException.CreateEx(ffStrResGeneral, fferrReplyTimeout,
                                [SysErrorMessage(GetLastError), GetLastError])
  else if waitResult <> WAIT_OBJECT_0 then
    raise EffException.CreateEx(ffStrResGeneral, fferrWaitFailed,
                                [SysErrorMessage(GetLastError), GetLastError]);
end;
{--------}
function TffEvent.WaitForQuietly(const timeOut : TffWord32) : DWORD;
var
  aTimeOut : TffWord32;
begin
  if timeOut <= 0 then
    aTimeOut := ffcl_INFINITE                                         {!!.06}
  else
    aTimeout := timeOut;

  Result := WaitForSingleObject(ffeEvent, aTimeout);

end;
{--------}
procedure TffEvent.SignalEvent;
begin
  SetEvent(ffeEvent);
end;
{====================================================================}

{===TffReadWritePortal===============================================}
constructor TffReadWritePortal.Create;
begin
  inherited Create;
//  rwpBlockedReaders := FFSemPool.Get;                                {Deleted !!.06}
//  rwpBlockedWriters := FFSemPool.Get;                                {Deleted !!.06}
  FFSemPool.GetTwo(rwpBlockedReaders, rwpBlockedWriters);              {!!.06}
  rwpGate := TffPadlock.Create;
  rwpActiveReaders := 0;
  rwpActiveWriter := false;
  rwpActiveWriterID := 0;
  rwpWaitingReaders := 0;
  rwpWaitingWriters := 0;
  rwpWriterReadCount := 0;
  rwpWriterWriteCount := 0;
end;
{--------}
destructor TffReadWritePortal.Destroy;
begin
  rwpGate.Free;
  FFSemPool.Put(rwpBlockedReaders);
  FFSemPool.Put(rwpBlockedWriters);
  inherited Destroy;                                                   {!!.01}
end;
{--------}
procedure TffReadWritePortal.BeginRead;
var
  MustWait : boolean;
begin

  if not IsMultiThread then
    Exit;

  { Wait for access to internal data. }
  rwpGate.Lock;
  try
    { If the active writer is trying to read then automatically grant access. }
    if rwpActiveWriter and (rwpActiveWriterID = GetCurrentThreadID) then begin
      inc(rwpWriterReadCount);
      exit;
    end;

    { If a writer has been granted access or there is at least one writer
      waiting for access, add self as a waiting reader and make sure we
      wait for read access. }
    if rwpActiveWriter or (rwpWaitingWriters <> 0) then begin
      inc(rwpWaitingReaders);
      MustWait := true;
    end else begin
      { Otherwise, add self as an active reader. }
      inc(rwpActiveReaders);
      MustWait := false;
    end;

  finally
    rwpGate.Unlock;
  end;

  if MustWait then
    WaitForSingleObject(rwpBlockedReaders, ffcl_INFINITE);            {!!.06}

end;
{--------}
procedure TffReadWritePortal.BeginWrite;
var
  MustWait : boolean;
begin

  if not IsMultiThread then
    Exit;

  { Wait for access to internal data. }
  rwpGate.Lock;
  try

    { If the active writer is calling BeginWrite once more, increment our
      count of such calls, release the gate, and exit. }
    if rwpActiveWriter and (rwpActiveWriterID = GetCurrentThreadID) then begin
      Inc(rwpWriterWriteCount);
      Exit;
    end;

    { If there are active readers or an active writer, add self as a waiting
      writer. }
    if rwpActiveWriter or (rwpActiveReaders <> 0) then begin
      Inc(rwpWaitingWriters);
      MustWait := True;
    end else begin
      { Otherwise, mark self as the active writer. }
      rwpActiveWriter := True;
      rwpActiveWriterID := GetCurrentThreadID;                         {!!.06}
      MustWait := False;
    end;
  finally
    rwpGate.Unlock;
  end;

  if MustWait then begin                                               {!!.06 - Start}
    WaitForSingleObject(rwpBlockedWriters, ffcl_INFINITE);            {!!.06}
    rwpActiveWriterID := GetCurrentThreadID;
  end;

  { If we reach this point then we have write access.  Store our threadID
    so that BeginRead knows who we are.  Set our reference counts. }
  {rwpActiveWriterID := GetCurrentThreadID;}                           {!!.06 - End}
  rwpWriterReadCount := 0;                                             {!!.02}
  rwpWriterWriteCount := 1;
end;
{--------}
procedure TffReadWritePortal.EndRead;
begin

  if not IsMultiThread then
    Exit;

  { Wait for access to internal data. }
  rwpGate.Lock;
  try

    { If a writer is active and it is calling EndRead then decrement the read
      count. }
    if rwpActiveWriter and (rwpActiveWriterID = GetCurrentThreadID) then begin
        dec(rwpWriterReadCount);
        exit;
    end;

    { Note: This method does not catch the following cases:
      1. Thread calls EndRead before a BeginRead was issued.
      2. Active writer threadcalls EndRead before a BeginRead was called or
         after EndWrite was called. }

    if rwpActiveReaders > 0 then
      dec(rwpActiveReaders);

    { If we are the last reader and there is at least one waiting writer,
      activate the waiting writer. }
    if (rwpActiveReaders = 0) and (rwpWaitingWriters <> 0) then begin
      dec(rwpWaitingWriters);
      rwpActiveWriter := true;
      ReleaseSemaphore(rwpBlockedWriters, 1, nil);
    end;
  finally
    rwpGate.Unlock;
  end;

end;
{--------}
procedure TffReadWritePortal.EndWrite;
var
  tmpWaiting : integer;
begin

  if not IsMultiThread then
    Exit;

  { Wait for access to internal data. }
  rwpGate.Lock;
  try

    { If this is the writer thread, see if this is the final call to
      EndWrite.  If not then just exist the method. }
    if rwpActiveWriterID = GetCurrentThreadID then begin
      dec(rwpWriterWriteCount);
      if rwpWriterWriteCount > 0 then begin
        exit;
      end;
    end else begin                                                     {!!.06 - Start}
      { This should NEVER happend. }
      Exit;
    end;

    { Note: This method doesn't catch the following cases:
      1. A thread other than the active thread calls EndWrite.
      2. A thread calls EndWrite before BeginWrite.
    }

    {rwpActiveWriter := False;}
    {rwpActiveWriterID := 0;}                                          {!!.06 - End}

    { If there are any waiting readers then release them. }
    if (rwpWaitingReaders <> 0) then begin
      tmpWaiting := rwpWaitingReaders;
      Dec(rwpWaitingReaders, rwpWaitingReaders);
      Inc(rwpActiveReaders, tmpWaiting);
      rwpActiveWriterID := 0;                                          {!!.06}
      rwpActiveWriter := False;                                        {!!.06}
      ReleaseSemaphore(rwpBlockedReaders, tmpWaiting, nil);
    end else if (rwpWaitingWriters <> 0) then begin
      { Otherwise if there is at least one waiting writer then release one. }
      Dec(rwpWaitingWriters);
      {rwpActiveWriter := True;}                                       {!!.06 - Start}
      rwpActiveWriterID := 0;
      ReleaseSemaphore(rwpBlockedWriters, 1, nil);
    end else begin
      rwpActiveWriterID := 0;
      rwpActiveWriter := False;
    end;                                                               {!!.06 - End}
  finally
    rwpGate.Unlock;
  end;
end;
{====================================================================}

{===TffPadlock=======================================================}
constructor TffPadLock.Create;
begin
  inherited Create;
  InitializeCriticalSection(plCritSect);
  plCount := 0;
end;
{--------}
destructor TffPadLock.Destroy;
begin
  DeleteCriticalSection(plCritSect);
  inherited Destroy;
end;
{--------}
function TffPadLock.GetLocked : boolean;
begin
  Result := plCount > 0;
end;
{--------}
procedure TffPadLock.Lock;
begin
  if IsMultiThread then begin
    EnterCriticalSection(plCritSect);
    inc(plCount);
  end;
end;
{--------}
procedure TffPadLock.Unlock;
begin
  if (plCount > 0) then begin
    dec(plCount);
    LeaveCriticalSection(plCritSect);
  end;
end;
{====================================================================}

{===Mutex pool=======================================================}
constructor TffMutexPool.Create(const initialCount, retainCount : integer);
var
  aHandle : THandle;
  Index : integer;
begin
  inherited Create;
  mpList := TffHandleList.Create;
  mpRetainCount := retainCount;
  mpPadLock := TffPadlock.Create;

  { Create the initial set of mutexes. }
  for Index := 1 to initialCount do begin
    aHandle := CreateMutex(nil, false, nil);
    mpList.Append(aHandle);
  end;
end;
{--------}
destructor TffMutexPool.Destroy;
begin
  mpList.Free;
  mpPadLock.Free;
  inherited Destroy;
end;
{--------}
procedure TffMutexPool.Flush;
var
  Index : integer;
begin
  mpPadLock.Lock;
  try
    if mpRetainCount < mpList.Count then
      for Index := pred(mpList.Count) downto mpRetainCount do          {!!.01}
        mpList.DeleteAt(Index);
  finally
    mpPadLock.Unlock;
  end;
end;
{--------}
function TffMutexPool.Get : THandle;
var
  aCount : Longint;
begin
  mpPadLock.Lock;
  try
    if mpList.IsEmpty then
      Result := CreateMutex(nil, false, nil)
    else begin
      { Get the last item in the list.  This speeds up the RemoveAt
        operation incredibly since it won't have to shift any bytes in the
        list. }
      aCount := pred(mpList.Count);
      Result := mpList.Handles[aCount];
      mpList.RemoveAt(aCount);
    end;
  finally
    mpPadLock.Unlock;
  end;
end;
{--------}
procedure TffMutexPool.Put(const aHandle : THandle);
begin
  mpPadLock.Lock;
  try
    mpList.Append(aHandle);
  finally
    mpPadLock.Unlock;
  end;
end;
{====================================================================}

{===Semaphore pool===================================================}
constructor TffSemaphorePool.Create(const initialCount, retainCount : integer);
var
  aHandle : THandle;
  Index : integer;
begin
  inherited Create;
  spList := TffHandleList.Create;
  spRetainCount := retainCount;
  spPadLock := TffPadlock.Create;

  { Create the initial set of semaphores. }
  for Index := 1 to initialCount do begin
    aHandle := CreateSemaphore(nil, 0, ffcl_MaxBlockedThreads, nil);
    spList.Append(aHandle);
  end;
end;
{--------}
destructor TffSemaphorePool.Destroy;
begin
  spList.Free;
  spPadLock.Free;
  inherited Destroy;
end;
{--------}
procedure TffSemaphorePool.Flush;
var
  Index : integer;
begin
  spPadLock.Lock;
  try
    if spRetainCount < spList.Count then
      for Index := pred(spList.Count) downto spRetainCount do            {!!.01}
        spList.DeleteAt(Index);
  finally
    spPadLock.Unlock;
  end;
end;
{--------}
function TffSemaphorePool.Get : THandle;
var
  aCount : Longint;
begin
  spPadLock.Lock;
  try
    if spList.IsEmpty then
      Result := CreateSemaphore(nil, 0, ffcl_MaxBlockedThreads, nil)
    else begin
      { Get the last item in the list.  This speeds up the RemoveAt
        operation incredibly since it won't have to shift any bytes in the
        list. }
      aCount := pred(spList.Count);
      Result := spList.Handles[aCount];
      spList.RemoveAt(aCount);
    end;
  finally
    spPadLock.Unlock;
  end;
end;
{Begin !!.06}
{--------}
procedure TffSemaphorePool.GetTwo(var aHandle1,
                                      aHandle2 : THandle);
var
  aCount, i  : Longint;
begin
  spPadLock.Lock;
  try
    aCount := spList.FCount;
    if (aCount < 2) then begin
      for i := 1 to ffcl_InitialSemCount do
        spList.Append(CreateSemaphore(nil, 0, ffcl_MaxBlockedThreads, nil));
      aCount := aCount + ffcl_InitialSemCount;
    end;
    { Get the last items in the list.  This speeds up the RemoveAt
      operation incredibly since it won't have to shift any bytes in the
      list. }
    aCount := aCount - 1;
    aHandle1 := spList.Handles[aCount];
    spList.RemoveAt(aCount);
    aCount := aCount - 1;
    aHandle2 := spList.Handles[aCount];
    spList.RemoveAt(aCount);
  finally
    spPadLock.Unlock;
  end;
end;
{End !!.06}
{--------}
procedure TffSemaphorePool.Put(const aHandle : THandle);
begin
  spPadLock.Lock;
  try
    spList.Append(aHandle);
  finally
    spPadLock.Unlock;
  end;
end;
{====================================================================}

{$IFDEF UseEventPool}
{===Event pool=======================================================}
constructor TffEventPool.Create(const initialCount, retainCount : integer);
var
  aHandle : THandle;
  Index : integer;
begin
  inherited Create;
  epList := TffHandleList.Create;
  epRetainCount := RetainCount;
  epPadLock := TffPadlock.Create;

  { Create the initial set of mutexes. }
  for Index := 1 to InitialCount do begin
    aHandle := CreateEvent(nil, False, False, nil);  // manual reset, start signaled
    epList.Append(aHandle);
  end;
end;
{--------}
destructor TffEventPool.Destroy;
begin
  epList.Free;
  epPadLock.Free;
  inherited Destroy;
end;
{--------}
procedure TffEventPool.Flush;
var
  Index : integer;
begin
  epPadLock.Lock;
  try
    if epRetainCount < epList.Count then
      for Index := Pred(epList.Count) downto Pred(epRetainCount) do
        epList.DeleteAt(Index);
  finally
    epPadLock.Unlock;
  end;
end;
{--------}
function TffEventPool.Get : THandle;
var
  aCount : Longint;
begin
  epPadLock.Lock;
  try
    if epList.IsEmpty then
      Result := CreateEvent(nil, False, False, nil)  // manual reset, start signaled
    else begin
      { Get the last item in the list.  This speeds up the RemoveAt
        operation incredibly since it won't have to shift any bytes in the
        list. }
      aCount := Pred(epList.Count);
      Result := epList.Handles[aCount];
      epList.RemoveAt(aCount);
    end;
  finally
    epPadLock.Unlock;
  end;
end;
{--------}
procedure TffEventPool.Put(const aHandle : THandle);
begin
  epPadLock.Lock;
  try
    epList.Append(aHandle);
  finally
    epPadLock.Unlock;
  end;
end;
{=====================================================================}
{$ENDIF}

{== Memory pool ======================================================}
type
  PffPoolItem = ^TffPoolItem;
  TffPoolItem = pointer {PffPoolItem};
{--------}
constructor TffMemoryPool.Create(ItemSize     : TffMemSize;
                                 ItemsInBlock : Integer);
const
  BlockSizeAdjustment = SizeOf(TffMemBlockInfo);

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
var
  RealItemSize : Integer;
  TestSize     : Longint;
const
  MinItemSize = SizeOf(Word) + SizeOf(Pointer);
    {-An item must have room for an offset back to the block's usage counter
      & a pointer to the next free item. }
begin

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
  if (TestSize > MaxBlockSize) then begin
    FItemsInBlock := (MaxBlockSize - BlockSizeAdjustment) div RealItemSize;
    TestSize := (RealItemSize * FItemsInBlock) + BlockSizeAdjustment;
  end;
  FBlockSize := TestSize;
  mpPadlock := TffPadlock.Create;
end;
{--------}
destructor TffMemoryPool.Destroy;
var
  Temp : PffMemBlockInfo;
  Next : PffMemBlockInfo;
begin
  mpPadlock.Lock;
  try
    Temp := FFirstBlock;
    while Assigned(Temp) do begin
      Next := Temp^.NextBlock;
      FreeMem(Temp, FBlockSize);
      Temp := Next;
    end;
  finally
    mpPadlock.Unlock;
    mpPadlock.Free;
  end;{try..finally}
  inherited Destroy;                                                   {!!.01}
end;
{--------}
procedure TffMemoryPool.mpAddBlock;
var
  aBlock : PffMemBlockInfo;
  Temp   : PAnsiChar;
  Prev   : Pointer;
  i      : Integer;
begin
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
  Prev := nil;
  for i := 0 to pred(FItemsInBlock) do begin

    { First 2 bytes are an offset back to usage counter. }
    PWord(Temp)^ := Temp - PAnsiChar(aBlock);

    { Next 4 bytes is the start of the item and it points to the previous
      available item. }
    inc(Temp, sizeOf(Word));
    PffPoolItem(Temp)^ := Prev;
    Prev := Temp;

    { Move to the next available item. }
    inc(Temp, FItemSize);
  end;
  FFreeList := Prev;
end;
{--------}
function TffMemoryPool.Alloc : Pointer;
var
  aBlock : PffMemBlockInfo;
  {$IFDEF MemPoolTrace}
  PtrString, PtrString2 : array[0..8] of AnsiChar;
  {$ENDIF}
  Temp : PAnsiChar;
begin
  {$IFDEF MemPoolTrace}
  WriteLn(Log, Format('%d, Block count %d', [FItemSize, BlockCount]));
  {$ENDIF}
  mpPadlock.Lock;
  try
    if not Assigned(FFreeList) then
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
    aBlock := PffMemBlockInfo(Temp);

    { Increment the usage counter. }
    inc(aBlock^.UsageCounter);

  finally
    mpPadlock.UnLock;
  end;{try..finally}
end;
{--------}
function TffMemoryPool.BlockCount : Longint;
var
  Temp : PffMemBlockInfo;
begin
  Result := 0;
  mpPadlock.Lock;
  try
    Temp := FFirstBlock;
    while Assigned(Temp) do begin
      inc(Result);
      Temp := Temp^.NextBlock;
    end;
  finally
    mpPadlock.Unlock;
  end;{try..finally}
end;
{--------}
function TffMemoryPool.BlockUsageCount(const BlockIndex : Longint) : Longint;
var
  Index : Longint;
  Temp : PffMemBlockInfo;
begin
  Result := -1;
  Index := 0;
  mpPadlock.Lock;
  try
    Temp := FFirstBlock;
    while Assigned(Temp) and (Index <= BlockIndex) do begin
      if Index = BlockIndex then begin
        { We have found the right block.  Return the usage counter. }
        Result := Temp^.UsageCounter;
        break;
      end
      else begin
        inc(Index);
        Temp := Temp^.NextBlock;
      end;
    end;
  finally
    mpPadlock.Unlock;
  end;{try..finally}
end;
{--------}
procedure TffMemoryPool.Dispose(var P);
var
  aBlock : PffMemBlockInfo;
  Pt : pointer absolute P;
  {$IFDEF MemPoolTrace}
  PtrString : array[0..8] of AnsiChar;
  PtrString2 : array[0..8] of AnsiChar;
  {$ENDIF}
  Temp : PAnsiChar;
begin
  mpPadlock.Lock;
  try
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
    aBlock := PffMemBlockInfo(Temp);
    dec(aBlock^.UsageCounter);

    Pt := nil;
  finally
    mpPadlock.UnLock;
  end;{try..finally}
end;
{--------}
procedure TffMemoryPool.mpCleanFreeList(const BlockStart : Pointer);
var
  BlockEnd   : Pointer;
  ItemsFound : Longint;
  Prev       : Pointer;
  Temp       : Pointer;
begin
  { Scan through the free list.  If we find an item that falls within the
    bounds of the block being freed then remove that item from the chain.
    Stop the scan when all of the block's items have been found. }
  BlockEnd := PAnsiChar(BlockStart) + FBlockSize;
  ItemsFound := 0;

  { Prev points to the last good item. }
  Prev := nil;
  Temp := FFreeList;

  while assigned(Temp) and (ItemsFound < FItemsInBlock) do begin
    { Does this item fall within the bounds of the freed block? }
    if (PAnsiChar(Temp) > BlockStart) and (PAnsiChar(Temp) <= BlockEnd) then begin
      { Yes.  Increment item count.  }
      inc(ItemsFound);

      { Is this item the head of the free list? }
      if Temp = FFreeList then
        { Yes.  Update the head of the free list. }
        FFreeList := PffPoolItem(Temp)^
      else begin
        { No.  Point the previous item to the next item. }
        PffPoolItem(Prev^) := PffPoolItem(Temp^);
      end;

      { Move to the next item. }
      Temp := PffPoolItem(Temp)^;

    end else begin
      { No.  Move to next item. }
      Prev := Temp;
      Temp := PffPoolItem(Temp)^;
    end;
  end;
end;
{--------}
function TffMemoryPool.RemoveUnusedBlocks : Integer;
var
  Next : PffMemBlockInfo;
  Prev : PffMemBlockInfo;
  Temp : PffMemBlockInfo;
begin
  mpPadlock.Lock;
  Result := 0;
  try
    { Loop through the chain of blocks, looking for those blocks with usage
      count = 0. }
    Prev := nil;
    Temp := FFirstBlock;
    while assigned(Temp) do begin
      { Grab the pointer to the next block. }
      Next := Temp^.NextBlock;

      { Is this block's usage counter = 0? }
      if Temp^.UsageCounter = 0 then begin
        { Yes.  Is this the first block in the chain? }
        if Temp = FFirstBlock then
          { Yes.  Set first block = next block in chain. }
          FFirstBlock := Next
        else if assigned(Prev) then
          { No.  Update the previous block's Next Block pointer. }
          Prev^.NextBlock := Next;
        { Remove the block's items from the free list. }
        mpCleanFreeList(Temp);
        { Free the block. }
        Freemem(Temp, FBlockSize);
        inc(Result);
      end
      else
        { No.  Update the pointer to the previous block. }
        Prev := Temp;

      { Position to the next block. }
      Temp := Next;
    end;
  finally
    mpPadlock.Unlock;
  end
end;
{=====================================================================}

{== Initialization/Finalization ======================================}
procedure FinalizeUnit;
var
  Inx : Integer;
begin
  FFSemPool.Free;
  {$IFDEF UseEventPool}
  FFEventPool.Free;
  {$ENDIF}
  for Inx := 0 to 91 do
    FFMemPools[Inx].Free;

  {$IFDEF MemPoolTrace}
  {Close the log}
  System.Close(Log);
  {$ENDIF}
end;
{--------}
procedure InitializeUnit;
var
  Inx : Integer;
begin
  {$IFDEF MemPoolTrace}
  {open up the log file}
  System.Assign(Log, 'MplTrace.log');
  System.Rewrite(Log);
  {$ENDIF}

  { Create the memory pools ahead of time.  We do it now instead of during
    normal execution so that we can avoid thread A and thread B both trying
    to create the memory pool at the same time. }
  for Inx := 0 to 31 do
    FFMemPools[Inx] := TffMemoryPool.Create(succ(Inx) * 32, 1024);

  for Inx := 32 to 91 do
    FFMemPools[Inx] := TffMemoryPool.Create(1024 + ((Inx - 31) * 256), 1024);

  FFSemPool := TffSemaphorePool.Create(ffcl_InitialSemCount, ffcl_RetainSemCount);

  {$IFDEF UseEventPool}
  FFEventPool := TffEventPool.Create(ffcl_InitialEventCount, ffcl_RetainEventCount);
  {$ENDIF}
 end;
{--------}
{Begin !!.11}
{$IFDEF DCC4OrLater}
function PreGetDiskFreeSpaceEx(Directory     : PChar;
                           var FreeAvailable,
                               TotalSpace    : TLargeInteger;
                               TotalFree     : PLargeInteger)
                                             : Bool; stdcall;
var
  SectorsPerCluster,
  BytesPerSector,
  FreeClusters,
  TotalClusters     : LongWord;
{$ELSE}
function PreGetDiskFreeSpaceEx(Directory     : PChar;
                           var FreeAvailable,
                               TotalSpace    : Integer;
                               TotalFree     : PInteger)
                                             : Bool; stdcall;
var
  SectorsPerCluster,
  BytesPerSector,
  FreeClusters,
  TotalClusters     : DWord;
{$ENDIF}
  Root : string;                                                       {!!.12}
begin
  Root := ExtractFileDrive(Directory) + '\';                           {!!.12}
  Result := GetDiskFreeSpaceA(PChar(Root),                             {!!.12}
                              SectorsPerCluster,
                              BytesPerSector,
                              FreeClusters,
                              TotalClusters);
  if Result then begin
    FreeAvailable := SectorsPerCluster * BytesPerSector * FreeClusters;
    TotalSpace := SectorsPerCluster * BytesPerSector * TotalClusters;
  end
  else
    raise Exception.Create('Error checking free disk space: ' +
                           SysErrorMessage(GetLastError));
end;

function FFGetDiskFreeSpace(const aDirectory : string) : Integer;
var
  Kernel : THandle;
  Path   : array[0..255] of char;

  {needed for GetDiskFreeSpaceEx}
  {$IFDEF DCC4OrLater}
  FreeAvailable : Int64;
  TotalSpace    : Int64;
  {$ELSE}
  FreeAvailable : Integer;
  TotalSpace    : Integer;
  {$ENDIF}
begin
  FFLLGetDiskFreeSpaceEx := @PreGetDiskFreeSpaceEx;

  { Get API routine to use to check free disk space }
  Kernel := GetModuleHandle(Windows.Kernel32);
{Begin !!.12}
  if (Kernel <> 0) then begin
    @FFLLGetDiskFreeSpaceEx := GetProcAddress(Kernel,
                                              'GetDiskFreeSpaceExA');
    if not assigned(FFLLGetDiskFreeSpaceEx) then
      FFLLGetDiskFreeSpaceEx := @PreGetDiskFreeSpaceEx;
  end;  { if }
{End !!.12}

  StrPCopy(Path, aDirectory);
  if FFLLGetDiskFreeSpaceEx(Path, FreeAvailable, TotalSpace, nil) then
    Result := FreeAvailable div 1024
  else
    raise Exception.Create('Error getting free disk space: %s' +
                           SysErrorMessage(GetLastError));
end;
{End !!.11}                                                                   

initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.

