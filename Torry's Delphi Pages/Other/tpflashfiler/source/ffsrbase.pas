{*********************************************************}
{* FlashFiler: Base unit for FlashFiler Server           *}
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

{ Uncomment the following to enable logging of RAM page actions. }
{.$DEFINE RAMPageCheck}

{Note: to avoid later confusion, here are what various 'numbers' mean,
       including their types:
         "block number"
            zero-based number of a block in the file, each block being
            4K, 8K, 16K, 32K, or 64k in size; 32-bit signed integer (with
            the smallest block size it has a range of 0..1024*1024-1)
         "record reference number"
            an offset into file, 64-bit unsigned word (TffInt64)
         "BLOB number"
            an offset into file, 64-bit unsigned word (TffInt64)
         "BLOB segment number"
            an offset into file, 64-bit unsigned word (TffInt64)
         "stream number"
            a block number

       it is only the table files that are encrypted, journal files
       are not. Hence journal files are always read and written with
       the non-encrypt/decrypt versions of the file access routines.
       In table files, the header record is never encrypted either:
       this means that the buffer manager can work out if a table file
       is encrypted or not. All files for a given table have the same
       encryption level (ie, they all are, or they all are not).
       }


unit ffsrbase;

interface

uses
  Windows,
  SysUtils,
  Classes,
  ffconst,
  ffhash,
  ffllbase,
  {$IFDEF RAMPageCheck}
  fflllog,
  {$ENDIF}
  fflltemp,
  ffsrmgr,
  ffllexcp,
  {$IFDEF SecureServer}
  fftbcryp,
  {$ENDIF}
  ffsrintf;

{$R ffsrcnst.res}

var
  ffStrResServer : TffStringResource;
  {$IFDEF RAMPageCheck}
  aLog : TffEventLog;
  {$ENDIF}

{---Handy constants for readability---}
const
  ffc_MarkDirty = true;
  ffc_ReadOnly  = false;
  ffcl_PageLife = 5 * 60 * 1000;
    { A RAM page whose FRefCount > 0 may not be re-used unless the last access
      was 5 or more minutes ago. }

{---Enumerated types---}
type
  TffLockPresent = (                  {Whether a lock is present...}
                    lpNotAtAll,       {..no, not at all}
                    lpYesByUs,        {..yes, and by current session}
                    lpYesByAnother);  {..yes, and by another session}

  {:The types of BLOB segments.
   @enum bstHeader Segment containing BLOB info and first set of lookup entries.
   @enum bstLookup Segment containing additional BLOB lookup entries that
     couldn't fit in the header segment.
   @enum bstContent Segment containing BLOB content. }
  TffBLOBSegment = (bstHeader, bstLookup, bstContent);

  TffTransactionMode = (         {Transaction modes for the buffer manager}
                    tmOff,       {..no transaction active}
                    tmNormal,    {..non-fail safe transaction in progress}
                    tmFailSafe); {..fail safe transaction in progress}

  TffFindKeyAction = (       {Find key actions if orig key not found}
             fkaNone,        {..do nothing, return error}
             fkaNextKey,     {..return next key in index, or error if none}
             fkaPrevKey,     {..return previous key in index, or error if none}
             fkaPartialKey,  {..key provided is partial, find full key that matches}
             fkaNearestKey); {..return next key, or if none, previous key}

  TffAccessRight = (            {user access rights}
                    arAdmin,    {..administration right}
                    arRead,     {..read right}
                    arInsert,   {..insert right}
                    arUpdate,   {..update right}
                    arDelete);  {..delete right}
  TffUserRights = set of TffAccessRight;

  {---The FlashFiler primitive file type and buffer manager class---}
  TffBaseBLOBResourceMgr = class;     {..forward declaration}
  TffBufferManager = class;

  TffbmRAMPage = class;

  PffPageArray = ^TffPageArray;
  TffPageArray = array[Byte] of Pointer;
    {-This type is used in the TffFileInfo.fiPages structure.
      An element of a leaf array will point to a TffbmRAMpage.
      An element of a node array will point to a TffPageContainer. }

  PffPageContainer = ^TffPageContainer;
  TffPageContainer = record
    pcNext: PffPageContainer;
    pcPrev: PffPageContainer;
    pcPages: TffPageArray;
    pcCount: Word;
  end;
    {-This type is used in the TffFileInfo.fiPages structure. }

  TffBlockNum = packed array [0..3] of Byte;
    {-This type is used to transform a block number into an array.  The
      various parts of the array are then used to index into the
      TffFileInfo.fiPages structure. }

  TffFileAttribute = (fffaSeqScan, fffaTemporary, fffaBLOBChainSafe);
    { Each file may have zero or more special attributes. Attributes are as
      follows:

      fffaSeqScan - The file was created for a sequential scan of its data.
        The buffer manager may elect to keep a limited number of the
        file's blocks in memory.

      fffaTemporary - This is a temporary file that exists only as long as it
        is needed. It is not to be saved to the hard drive. Files of this type
        are typically created by a SQL cursor.

      fffaBLOBChainSafe  - The in-memory BLOB deleted chain does not need to
        respect transactions. Normally, the in-memory BLOB deleted chain is not
        updated until the current transaction is committed or rolled back. In
        certain situations, such as packing a table or building a SQL result
        set, the in-memory BLOB deleted chain can be updated real-time without
        becoming out of sync with the BLOB deleted chain on disk.
    }

  TffFileAttributes = set of TffFileAttribute;

  PffFileInfo = ^TffFileInfo;
  TffFileInfo = packed record       {A FlashFiler file..}
    fiVerify    : TffWord32;        {..verification value}
    fiHandle    : THandle;          {..file handle}
    fiBlockSize : Longint;          {..block size--4K, 8K, 16K, 32K, or 64K}
    fiBlockSizeK : Longint;         {..block size in kilobytes--4, 8, 16, 32, or 64 } {!!.11}
    fiLog2BlockSize: TffWord32;     {..log base 2 of fiBlockSize (12, 13, 14, 15 or 16)}
    fiUsedBlocks   : TffWord32;     {..number of blocks in file.  We store this
                                       value here in order to reduce number of
                                       locks on block 0.  This field is updated
                                       when a new block is added to the file. }
    fiRecordLength : Longint;       {..record length}
    fiRecLenPlusTrailer : Longint;  {..record length plus deletion link}
    fiBufMgr    : TffBufferManager; {..the buffer manager being used}
    fiName      : PffShStr;         {..fully expanded file name}
    fiOpenMode  : TffOpenMode;      {..open mode}
    fiShareMode : TffShareMode;     {..share mode.  Indicates how the file
                                       has been opened by the server.  The
                                       server usually opens files in
                                       smExclusive mode.}
    fiWriteThru : Boolean;          {..file has been opened in writethru mode}
    fiForServer : Boolean;          {..file is for the server, not the client}
    fiEncrypted : Boolean;          {..file is encrypted}
    fiBLOBrscMgr: TffBaseBLOBResourceMgr;{.the resource manager being used} {!!.11}
    fiMaxBlocks : TffWord32;        {..max # of blocks for 4 GB file size}
    fiMaxSegSize : TffWord32;       {..max size of BLOB segment}
    fiPageListHead: TffbmRAMPage;   {..first RAM page in this file's list of
                                       loaded blocks. }
    fiPageListTail: TffbmRAMPage;   {..last RAM page in this file's list of
                                       loaded blocks. }
    fiPageZero : TffbmRAMPage;      {..The TffbmRAMPage for block 0.
                                       We cache it here since it is frequently-
                                       requested. }
    fiPageContainerList: PffPageContainer;
                                    {..the list of page containers used to build
                                       the fiPages structure.  We maintain
                                       a separate list of these objects so that
                                       we can quickly free them when this file
                                       structure is destroyed. }
    fiPages: TffPageArray;          {..The blocks stored in memory as RAM pages.}
      { Note: fiPages is a tree structure having multiple roots.  We use the
              structure to quickly determine whether or not a block is
              loaded in memory. }
    fiRecordLocks : TffThreadHash64;{..The record locks for this file.  Used by
                                       the lock manager. }
    fiFFVersion : Longint;          {..Version of FF used to create file}
    fiExclOwner : TffCursorID;      {..if <> ffc_W32NoValue then this is the
                                       ID of a cursor that has exclusively
                                       opened this file. }
    fiAttributes : TffFileAttributes;
                                    {..special attributes of the file. }
    fiTempStore : TffObject;        {..temporary storage used by this file.
                                       For regular files, this will start off
                                       as nil and then the buffer manager will
                                       fill it with the buffer manager's
                                       temporary storage object. For merge sort
                                       files, the sorting algorithm will fill
                                       this field with the file's own
                                       temporary storage instance. }
  end;

  TffSrTransactionLevel = class;  { forward declaration }              {!!.10}
  TffSrTransaction = class;  { forward declaration }

  PFFBlockCommonHeader = ^TFFBlockCommonHeader;
  TFFBlockCommonHeader = packed record
    bchSignature : Longint;
    bchNextBlock : Longint;
    bchThisBlock : TFFWord32;
    bchLSN       : TFFWord32;
  end;

  { The following record structure is used to pass transaction-specific
    information to low-level routines in FFTBDATA, FFTBINDX, and FFSRBASE.
    Note that we have to pass around the transaction because its LSN may
    change due to an LSN rollover.  We always want the latest LSN.

    Note: It is included in this unit because it is needed both by FFSRBASE
    and FFSRLOCK. }
  PffTransInfo = ^TffTransInfo;
  TffTransInfo = packed record
    tirLockMgr : TffObject;  { Really an instance of TffLockManager. }
    tirTrans : TffSrTransaction;
  end;

  { Stored in TffbmRAMPage.rpBlockList.  Helps us track the nesting level
    of each ram page. }
  TffbmModifiedBlock = class(TffObject)
  protected {private}
    mbBlock : PffBlock;
    mbBlockNumTmp : TffWord32;
      {-The block in temporary storage to which this block was written.
        Set to ffc_W32NoValue if not in temp storage. }

    mbTransLevelPrev: TffbmModifiedBlock;                              {!!.10}
    mbTransLevelNext: TffbmModifiedBlock;                              {!!.10}

    function mbGetBlock : PffBlock;
  protected
    procedure AddToTransLevel;                                         {!!.10}
    procedure RemoveFromTransLevel;                                    {!!.10}
  public
    Prev : TffbmModifiedBlock;
    TransLevel : TffSrTransactionLevel;
    RAMPage : TffbmRAMPage;

    constructor Create(aRAMPage : TffbmRAMPage;
                       aPrevBlock : TffbmModifiedBlock;
                       aTransLevel : TffSrTransactionLevel);           {!!.10}
    destructor Destroy; override;

    procedure Copy(aBlock : PffBlock);
    procedure CopyTo(aBlock : PffBlock);

    procedure DecreaseTransLevel;                                      {!!.10}

    procedure FreeBlock;
      { Frees the object's block. }
    procedure SendToTempStore;
      { Sends the block to temporary storage. }

    property Block : PffBlock read mbGetBlock write mbBlock;

  end;

  PffReleaseMethod = ^TffReleaseMethod;
  TffReleaseMethod = procedure(var aBlock : PffBlock) of object;
    { The type of method to be called once a thread has finished accessing
      a RAM page. }

  PffReleaseInfo = ^TffReleaseInfo;
  TffReleaseInfo = packed record
    BlockPtr : PffBlock;
    MethodVar : TffInt64;
  end;
    { TffReleaseInfo is used in complicated routines to track which RAM pages
      should be released. MethodVar is declared as a TffInt64 because
      it is an easy way to store a method variable, where the first 4 bytes
      are a pointer to the method code and the second 4 bytes are a pointer
      to the object instance (i.e., RAM page) to which the method belongs. }

  TffbmPageReuseMode = (ffrmTempStore, ffrmUseAsIs);
    { Identifies how a RAM page may be re-used.  Values:
      ffrmTempStore - The RAM page is to be placed in temporary storage &
        another RAM page created to take its place (temporarily).
      ffrmUseAsIs - The RAM page may be re-used. }

  {This class represents a file block that has been read from the hard drive
   into memory.  Since disk I/O is the most time-consuming operation for the
   database, the buffer manager uses RAM pages to cache file blocks in memory.

   Any given RAM page may be a member of one or more lists.  For example, each
   instance of TffSrTransaction maintains a list of TffbmRAMPages that have been
   modified by the transaction. A file maintains a list of the RAM pages that
   have been read from the file.

   A RAM page supports clean reads and nested transactions.  In regards to
   clean reads, reading clients always access a read-only copy of the file
   block (variable rpBlock).  The read-only copy is updated when a transaction
   commits its changes.

   When a transaction starts and dirties a RAM page, the RAM page copies the
   read-only block and adds it to an internal list. Variable rpBlockListTail
   points to the last modified block in the list.

   When a transaction is nested, the RAM page makes a copy of the most recently
   modified block in rpBlockList.  Commits cause the 2nd to last block to be
   removed from the list.  When only one block is left, the block is copied to
   the read-only block.

   Rollbacks cause the highest block to be removed from the list.

   When a page's block is retrieved by a thread, the page's internal reference
   count is incremented. This prevents the buffer manager from re-using the
   page while the page is in use by one or more threads. Any thread that
   retrieves a page's block must call the TffbmRAMPage.Release method when it
   has finished using the block. Doing so decrements the page's ref count. }
  TffbmRAMPage = class (TffObject)
    protected {private}
      FLastAccess : DWORD;       {..the time (obtained via GetTickCount) when
                                    this page was last accessed. }
      FNew : Boolean;            {..if True then this is a new file block. } {!!.07}
      FRefCount  : integer;      {..the number of times this page has been
                                    requested. If zero then no threads are
                                    accessing the page. If greater than zero
                                    then one or more threads are accessing
                                    the page.
                                    The count increments when the page's
                                    block is retrieved and decrements when the
                                    Release method is called. }
      rpBlock     : PffBlock;    {..block data (variably sized)}
      rpBlockBits : TffWord32;   {..bits identifying which modified blocks
                                    are in temporary storage. }
      rpBlockListTail : TffbmModifiedBlock;
                                 {..the last modified block in this page's
                                    list of modified blocks. We only need the
                                    tail because a commit or rollback of the
                                    page affects the tail. }
      rpBlockNum  : TffWord32;   {..zero-based block number in file}
      rpBlockSize : Longint;     {..sizeof rpBlock}
      rpBlockSizeEnum : TffBlockSize;
      rpBlockNumTmp : TffWord32; {..if not equal to ffc_W32NoValue then this
                                    block is currently located in temporary
                                    storage & this is the block in which it
                                    resides in temporary storage. }
      rpBufMgr : TffBufferManager;
                                 {..the buffer mgr with which this page is
                                    associated }
      rpFI        : PffFileInfo; {..the file with which this page is associated}
      rpFileNext  : TffbmRAMPage;{..next     ram page in file page list}
      rpFilePrev  : TffbmRAMPage;{..previous ram page in file page list}
      rpHeader    : PffBlockCommonHeader;
      rpInUseNext : TffbmRAMPage;{..next     ram page in InUse or Recycle list}
      rpInUsePrev : TffbmRAMPage;{..previous ram page in InUse list}
      rpReuseMode : TffbmPageReuseMode; {..indicates how the page may be re-used }
      rpTrans     : TffSrTransaction;  {..server transaction for which the block is dirty}
      rpTransNext : TffbmRAMPage;{..next     ram page in Transaction list}
      rpTransPrev : TffbmRAMPage;{..previous ram page in Transaction list}
    protected
      procedure AddToFilePageList;
        {-Adds the page to its file's list of RAM pages. }
      procedure AddToRecycleList;
        {-Adds the page to the recycle list. }
      procedure AddToTransList(aTrans: TffSrTransaction);
        {-Adds the page to a transaction item's page list. }
      procedure AddToUseList;
        {-Add the RAM page to the buffer manager's InUse list. }
      procedure MoveToEndOfTransList;
        {-Moves the RAM page to the end of its transaction's list of RAM
          pages. }
      procedure MoveToEndOfUseList;
        {-Moves the RAM page to the end of the InUse list.  This is done
          so that the Least Recently Used (LRU) pages appear at the beginning
          of the list. }
      procedure MoveToRecycleList;
        {-Moves a page from the buffer manager's InUse list to the Recycle
          list. }
      procedure RemoveFromFilePageList;
        {-Removes the page from its file's list of RAM pages. }
      procedure RemoveFromRecycleList;
        {-Removes the page from the recycle list. }
      procedure RemoveFromTransList(aTrans: TffSrTransaction);
        {-Removes the page from a transaction item's page list. }
      procedure RemoveFromUseList;
        {-Remove the RAM page from the buffer manager's InUse list. }
      procedure rpAllocBlock(aBlockSize : Longint);
        {-Allocates a new read-only block. }
      function rpAllocBlockPrim(aBlockSize : Longint) : PffBlock;
        {-Carries out the actual allocation of a block. }
      function rpDirty : boolean;
        {-If returns True then this block is dirty. }
      procedure rpFreeBlock(aBlock : PffBlock; aBlockSize : Longint);
        {-Frees a specific block. }
      function rpGetInTempStore : boolean;
        {-If the block is in temporary storage then returns True otherwise
          returns False. }
      function  rpGetLSN : TffWord32;
        {-If no transaction has dirtied the block then returns the LSN of the
          read-only block.  Otherwise returns the LSN of the most recent
          version. }
      function rpGetTransLevel : TffSrTransactionLevel;                {!!.10}
        {-Returns nest level of last transaction to modify this page. }
      procedure rpRelease(aBlock : PffBlock);
        {-Alternative to Release method that does not nil the input parameter. }
      procedure rpReplaceBlock(aNewBlock : PffBlock);
        {-Replaces the read-only block with another block. }
      procedure rpRetrieveFromTemp;
        {-Retrieves the read-only block from temp storage. }
      procedure rpSetBlockSize(aBlockSize : Longint);
      procedure rpSetFI(FI : PffFileInfo);
      procedure rpSetLSN(const aLSN : TffWord32);
        {-Sets the LSN of a RAM page that has not been modified by a
          transaction. }
    public
      constructor Create(aBufMgr : TffBufferManager; aFI : PffFileInfo;
                         const aBlockNumber : TffWord32);
      destructor Destroy; override;

      function Block(aTrans : TffSrTransaction;
                 var aReleaseMethod : TffReleaseMethod) : PffBlock;
        { Returns a copy of the file block.  If the transaction requesting the
          block previously modified the block then this routine returns the
          last modified version of the block.  If the block has not been
          modified by the transaction or the aTrans parameter is nil, the
          read-only copy of the block is returned.

          Once the requesting thread has finished with the block, it must call
          the procedure specified by aReleaseMethod. }

      function Commit(forceWrite : boolean) : boolean;
        { Commits a changed RAM page.  If forceWrite is True then changes are
          committed to disk regardless of nesting level and this function
          returns True.

          If forceWrite is False then the following logic is used:
            If the nesting level is greater than zero then this merely
            decrements the TransLevel of the RAM page and returns False.
            Otherwise, it writes the RAM page to disk and returns True. }

      function  DirtiedForTrans(aTrans : TffSrTransaction) : boolean;
        { Returns True if this block has been modified by the transaction.
          This function returns True only if the following is true:
            1. aTrans is a transaction.
            2. The block is marked as dirty.
            3. The block's LSN matches the transaction's LSN.
            4. The block's nesting level matches the transaction's nesting
               level. }

      procedure MakeClean;

      procedure MakeDirty(aTrans : TffSrTransaction);

      function ReadOnlyBlock : PffBlock;
        { Returns the page's read-only block. }

      procedure Release(var aBlock: PffBlock);
        { Use this method to tell the buffer manager that a thread is
          no longer using a ram page. Every retrieval of a page must be
          accompanied by a call to this method, otherwise the buffer manager
          will not re-use the ram page as soon as it normally would. }

      function Removable(var RemoveMode : TffbmPageReuseMode) : boolean;
        { Use this method to determine if a RAM page may be removed from
          the buffer manager.

          If returns False then this page may not be removed.

          If returns True then this page may be removed.  Look at the
          RemoveMode parameter to determine how it may be removed. If it
          returns ffrmUseAsIs then you may free the page. If it returns
          ffrmTempStore then the page may be moved to temporary storage,
          which removes its data block from memory. Do not free a page that
          returns a mode of ffrmTempStore. }

      function Reusable(var ReuseMode : TffbmPageReuseMode) : boolean;
        { Use this method to determine if the RAM page may be re-used.

          If returns False then this page may not be re-used.

          If returns True then this page may be re-used. Look at the
          ReuseMode parameter to determine how it may be reused. If it returns
          ffrmUseAsIs then you may use this RAM page instance as is. If it
          returns ffrmTempStore then you may send the RAM page to temporary
          storage and create a new RAM page to take its place.  Do not free
          or re-use the instance of a RAM page that returns a mode of
          ffrmTempStore. }

      procedure Rollback;
        { Rolls back the most recent changes to the RAM page.  Assumes that
          a transaction has modified the page. }

      procedure SendToTempStore;
        { Use this method to send a RAM page to temp storage. }

      property BlockNumber : TffWord32 read rpBlockNum write rpBlockNum;

      property BlockSize : Longint read rpBlockSize write rpSetBlockSize;

      property Dirty : boolean read rpDirty;
        { If returns True then the page has been modified by a transaction.
          The Block method returns the modified block to the transaction
          that dirtied the page. The Block method returns the read-only block
          to all other threads. }

      property FileInfo : PffFileInfo read rpFI write rpSetFI;

      property InTempStore : boolean read rpGetInTempStore;
        { If returns True then this block is currently in temporary storage. }

      property LastAccess : DWORD read FLastAccess;
        { The time, obtained via GetTickCount, when this page was last
          accessed by a thread. }

      property LSN : TffWord32 read rpGetLSN write rpSetLSN;
        { Log Sequence Number (LSN) of the last transaction to modify the
          RAM page.  A RAM page already loaded into memory can be re-used
          if its LSN is less than the buffer manager's CommitLSN. }

      property TransLevel : TffSrTransactionLevel read rpGetTransLevel;{!!.10}
        { The nesting level of the page.  If -1 then this block has not been
          modified by a transaction.  If zero then only one transaction
          has started and modified this block.  If >= 1 then there are one or
          more nested transactions. }

{Begin !!.07}
      property New : Boolean read FNew write FNew;
        { Indicates whether this page represents a new file block (i.e., just
          added to the file). }
{End !!.07}

      property RefCount : integer read FRefCount;
        { The number of times a thread has requested this page. If this
          property returns zero then no threads are currently accessing the
          page. If this property returns a value greater than zero then
          one or more threads are reading the contents of the page. }

      property ReuseMode : TffbmPageReuseMode read rpReuseMode;
        { Use this property to determine the page's reuse mode. }

  end;

{---Transaction types---}
  TffSrTransactionLevel = class(TffObject)
  protected {private}
    tlPrev: TffSrTransactionLevel;
    tlLevel: Integer;
    tlTransaction: TffSrTransaction;

    tlModifiedBlocksHead: TffbmModifiedBlock;
    tlModifiedBlocksTail: TffbmModifiedBlock;
  public
    constructor Create(aTrans: TffSrTransaction);
    destructor Destroy; override;

    property Level: Integer read tlLevel;
  end;

  { This class represents an active transaction within a folder (i.e.,
    directory).
    A transaction maintains a list of the RAM pages that have been dirtied
    by the transaction. }
  TffSrTransaction = class(TffSelfListItem)
  protected {private}
    FCorrupt    : boolean;
    FDatabaseID : TffDatabaseID;
    FImplicit   : boolean;
    FJnlFile    : PffFileInfo;
    FLSN        : TffWord32;
    FNewSpace   : Integer;                                             {!!.11}
    FTransLevel  : integer;
    FReadOnly   : boolean;
    FSignature  : Longint;
    FTransMode : TffTransactionMode;
    FLockContainer : TffListItem;

    trTransLevelListTail: TffSrTransactionLevel;

    trTransPageListHead : TffbmRAMPage;
      {-The first RAM page associated with this transaction. }
    trTransPageListTail : TffbmRAMPage;
      {-The last RAM page associated with this transaction. }

 protected
    function trGetNested : boolean;
    function trGetTransactionID : TffTransID;
    function trGetTransLevel: TffSrTransactionLevel;                   {!!.10}
 public
    constructor Create(const aDatabaseID : TffDatabaseID;
                       const aImplicit, readOnly   : boolean);
    destructor Destroy; override;

    function AdjustLSN(const Adjustment : TffWord32) : TffWord32;
      { Adjusts the transaction's LSN.  The adjusted LSN is then applied to
        each RAM page dirtied by the transaction.  Returns the new LSN of
        the transaction. }

    procedure StartNested;                                             {!!.10}
      { Increases the nesting level of the transaction }               {!!.10}
    procedure EndNested;                                               {!!.10}
      { Decreases the nesting level of the transaction }               {!!.10}

    property DatabaseID : TffDatabaseID read FDatabaseID;
    property IsCorrupt : boolean read FCorrupt write FCorrupt;
    property IsImplicit : boolean read FImplicit;
    property IsReadOnly : boolean read FReadOnly write FReadOnly;      {!!.06}
    property JournalFile : PffFileInfo
       read FJnlFile write FJnlFile;
      { If TransactionMode = tmFailSafe then this property identifies
        the journal file. }

    property LSN : TffWord32 read FLSN write FLSN;
      { The Log Sequence Number of this transaction.  In the future,
        this number will reflect the position within the log file of
        the transaction's next log record.
        For now, this is a static number assigned when the transaction
        is created. }

    property Nested : boolean read trGetNested;
      { Returns True if the transaction is nested. }

{Begin !!.11}
    property NewSpace : Integer read FNewspace write FNewSpace;
      { # of kb in free space required for blocks added by this transaction. }
{End !!.11}

    property TransLevel : TffSrTransactionLevel read trGetTransLevel;  {!!.10}
      { The nesting level of the transaction.  For a non-nested transaction,
        this property returns zero.  For a transaction that has been nested
        1 level, this property returns one, and so on. }

    property TransactionID : TffTransID read trGetTransactionID;
      { The unique ID of the transaction.  This will be unique across all
        transactions on an FF server. }

    property TransactionMode : TffTransactionMode
       read FTransMode write FTransMode;
      { Indicates whether this is a normal or failsafe transaction. }

    property TransLockContainer : TffListItem
       read FLockContainer write FLockContainer;

  end;

  { The most time-consuming operation performed by the database is disk I/O.
    To drastically improve performance, the buffer manager caches file blocks
    in memory in the form of RAM pages.  It brings in blocks as needed and
    writes them back to disk as needed.

    When the database needs a file block, the buffer manager will first see
    if the block is already in memory as a RAM page.  If the file block is
    not in memory then the buffer manager chooses to allocate a new RAM page
    or re-use an existing RAM page to hold the file block.

    The buffer manager, the files it accesses (i.e., the tables from
    which it reads file blocks), and transactions maintain lists of RAM pages.
    There may be only one instance of a file block instantiated as a RAM page.
    However, that RAM page will appear in multiple lists.

    The lists maintained by the buffer manager are as follows:

     - The InUse list contains all RAM pages created by the buffer manager.
       As a RAM page is accessed, it is moved to the end of this list.  The
       result is that the least recently used (LRU) RAM pages appear at or
       near the head of this list, speeding up the buffer manager's search
       for a re-usable RAM page.

     - The RecycleList contains all RAM pages no longer associated with a
       file.  The RAM pages are re-used for subsequent operations.

    Because multiple transactions may occur concurrently within a given
    database, it is important that RAM pages be locked prior to their being
    accessed.  Lock requests must be managed by the TffLockManager associated
    with the database containing the file.  Specific locking requirements are
    listed in the comments for public functions providing access to RAM pages.
  }
  TffBufferManager = class(TffObject)
    protected {private}
{Deleted !!.10}
//      bmCommitLSN       : TffWord32;  { The starting LSN of the oldest
//                                        uncommitted transaction.  Used to
//                                        indicate the LSN after which blocks may
//                                        not be re-used. }

      bmConfigDir       : TffPath;
      bmInUseListHead   : TffbmRAMPage;
      bmInUseListTail   : TffbmRAMPage;
      bmRecycleListHead : TffbmRAMPage;
{Begin !!.02}
      bmPortal          : TffPadlock;  { Provides thread-safe access
                                         to data structures. }
{End !!.02}
      bmMaxRAM          : Longint;  { Max number of megabytes for cache. }
      bmMaxRAMDetail    : TffInt64; { Max number of bytes for cache.  For comparisons. }
      bmRAMDetail       : TffInt64; { Number of bytes used.  For comparisons. }
      bmRAMUsed         : Longint;  { Number of megabytes used.  For status. }
      bmLRUValue        : TffWord32; { The latest LRU value.  Indicator for
                                       when the block was last used. }
      bmLRULockValue    : TffWord32; { The LRU value of the last started
                                       transaction.  Used to indicate the point
                                       after which blocks may not be re-used. }
      bmTempStore       : TffBaseTempStorage;

    protected
      function GetRAM : integer;
      procedure SetMaxRAM(aNumber : Longint);

      procedure bmClearRecycleList; virtual;                           {!!.07}
      procedure bmCommitPrim(aTrans : TffSrTransaction);
      function bmRAMPageCount : Longint;
      function bmFileRAMPageCount(aFI : PffFileInfo) : Longint;
      procedure bmFailSafeCommit(aTrans : TffSrTransaction);
      function bmGetBlock(aFI : PffFileInfo; aBlockNumber : TffWord32) : TffbmRAMPage;
        { Find a block in the internal data structure.  If the block is not
          already in memory then retrieve it. }

      function bmGetNewRAMPage(aFI : PffFileInfo; aBlockNumber : TffWord32) : TffbmRAMPage;
        { Obtains a new RAM page. It tries to reuse a recycled page. If none is
          available then it checks to see if adding a new page would push it
          over the RAM limit. If it would push the buffer manager over the RAM
          limit then it looks for a page that may be re-used. If one is found
          then the page is re-used. If none is found a new page is created
          from scratch. When the next transaction ends, the buffer manager tries
          to remove the excess page(s). }

      function bmGetRAMPage(const anIndex : Longint) : TffbmRAMPage;
        {-Returns a specific RAM page managed by the buffer manager. }

      function bmGetRecycledCount : Longint;
        {-Returns the total # of RAM pages in the recycled list. }

      function bmGetTempStoreSize : integer;
        {-Returns the size of temporary storage in megabytes. }

      procedure bmJournalRAMPage(aTrans : TffSrTransaction;
                                 aRAMPage : TffbmRAMPage; aBeforeImage : boolean);
      function bmOverRAMLimit(sizeOfNewBlock : Longint) : boolean;
        {-Used to determine if adding a new block of the specified size would
          push the buffer manager over its RAM limit. }

      procedure bmReadBlock(aFI : PffFileInfo; aBlockNumber : TffWord32;
                            aRAMPage : TffbmRAMPage);
        {-Reads the specified block from the file, placing it into aRAMPage.
          If aBlockNumber is set to ffc_W32NoValue then this method reads
          block zero of the file, transferring information from the header
          block into the aFI structure. }

      procedure bmRemoveCommittedPages(const aTran : TffSrTransaction);
        { Called after committing a transaction subset, this procedure removes
          the RAM pages associated with the specified transaction. }

      procedure bmRemoveExcessPages;
        { Called after a commit or rollback, this method removes RAM pages
          from the cache if the amount of memory occupied by the RAM pages
          exceeds the MaxRAM property. }

      function bmSearch(aFI : PffFileInfo; aBlockNumber : TffWord32) : TffbmRAMPage;
        { Determines if the page specified by aBlockNumber is already in
          memory. }

      procedure bmSetTempStoreSize(aSizeInMB : integer);
        { Changes the size of temporary storage. Note that this method may
          be used only when the temporary storage has not been written to.
          That's because this method does not handle transfer of data from
          existing temporary storage to the newly-size temporary storage. }

      procedure bmDecreaseRAMDetail(const numberBytes : Longint);
      procedure bmIncreaseRAMDetail(const numberBytes : Longint);
      procedure bmWriteCompleteJnlHeader(aJnlFile : PffFileInfo);
      procedure bmWriteIncompleteJnlHeader(aJnlFile : PffFileInfo);
      procedure bmRemovePageFromTransaction(aPage: TffbmRAMPage);
    public
      constructor Create(const ConfigDir : TffPath;
                         const TempStoreSizeInMB : integer);
      destructor Destroy; override;

      function  AddBlock(aFI : PffFileInfo;
                         aTI : PffTransInfo;
                   const aBlockNumber : TffWord32;
                     var aReleaseMethod : TffReleaseMethod) : PffBlock;
        { Adds a new block to the specified file (i.e., increases the size
          of the file). }

      function  AddFile(aFI : PffFileInfo;
                        aTI : PffTransInfo;
                  const aMarkHeaderDirty : boolean;
                    var aReleaseMethod : TffReleaseMethod) : PffBlock;
        { Adds a file to the buffer manager's list of managed files.  }

      procedure BeginWrite;
        { Must be called prior to accessing the buffer manager's internal
          data structures.  This method is public due to its being used by
          TffSrTransaction. }

      procedure DirtyBlock(aFI : PffFileInfo;
                     const aBlockNumber : TffWord32;
                           aTI : PffTransInfo;
                       var aModifiableBlock : PffBlock);
        { Marks a block as modified by the specified transaction.  The
          transaction's LSN (as specified in parameter aTI) is written to
          the block. Returns the modifiable copy of the block. Any method
          calling this function *MUST* use the returned block instead of
          the current block. }

      procedure EndWrite;
        { Must be called after finished accessing the buffer manager's internal
          data structures.  Must be preceded by a call to BeginWrite.
          This method is public due to its use by TffSrTransaction. }

      procedure FlushPools(const blockSizes : TffBlockSizes);
        { Use this method to have the buffer manager flush any unused blocks
          from the memory pools. aBlockSize contains enumerated values
          representing the memory pools that are to be flushed. Only those
          memory pools having an enumerated value in blockSizes are flushed. }

      function  GetBlock(aFI : PffFileInfo;
                   const aBlockNumber : TffWord32;
                         aTI : PffTransInfo;
                   const aMarkDirty : boolean;
                     var aReleaseMethod : TffReleaseMethod) : PffBlock;
        { Retrieves a block from a file.  If the block is already in the
          RAM cache then it is retrieved from the cache otherwise it is
          retrieved from the physical file and stored in the RAM cache. }

      function GetRAMPage(aFI : PffFileInfo;
                    const aBlockNumber : TffWord32) : TffbmRAMPage;
        { Retrieves the RAM page for a specific block in a file. }

{Begin !!.10}
      function GetRAMPageLSN(aRAMPage : TffbmRAMPage) : TffWord32;
        { Retrieve the LSN of the specified RAM page. }

      function GetRAMPageLSN2(aFI : PffFileInfo;
                        const aBlockNumber : TffWord32) : TffWord32;
        { Retrieves the RAM page for a specific block in a file. }
{End !!.10}

      procedure HandleLSNRollover;
        { Called when the transaction manager rolls over its LSN.  For each
          RAM page that is not associated with a transaction, the buffer
          manager resets the LSN of that RAM page to 1. }

      procedure Lock;                                                  {!!.05}
      procedure Unlock;                                                {!!.05}

      procedure RemoveFile(aFI : PffFileInfo);
        { Moves a file's RAM pages to the buffer manager's Recycle list and
          frees the structure used to index the file's RAM pages. }

      procedure UnlockBlock(aFI : PffFileInfo; aBlockNumber : TffWord32);
        { This function recycles a page, removing it from the header list
          (i.e., page of file header blocks) or file list and from a
          transaction list if the block is associated with a transaction.

          Currently, this function is not called from the engine. }

      procedure CommitFileChanges(aFI : PffFileInfo; aTrans : TffSrTransaction);
        { Use this method to commit changes to a file that is being closed
          in the midst of a transaction. }

      procedure CommitTransaction(aTrans : TffSrTransaction);
      procedure CommitTransactionSubset(aTrans : TffSrTransaction);
      procedure RollbackTransaction(aTrans : TffSrTransaction);
      procedure RollbackTransactionSubset(aTrans : TffSrTransaction);
      procedure StartTransaction(aTrans : TffSrTransaction;
                           const aFailSafe  : boolean;
                           const aFileName  : TffFullFileName);

{Deleted !!.10}
//      property CommitLSN : TffWord32 read bmCommitLSN write bmCommitLSN;
//        { The starting LSN of the oldest uncommitted transaction.  Used to
//          indicate the LSN after which blocks may not be re-used. }

      property ConfigDir : TffPath read bmConfigDir write bmConfigDir;
        { The server engine's configuration directory.  Passed on to temporary
          storage. }

      property MaxRAM : integer read bmMaxRAM write SetMaxRAM;
        { The maximum amount of RAM the buffer manager may allocate to hold
          RAM pages. }

      property RAMPageCount : Longint read bmRAMPageCount;
        { Returns the number of RAM pages being managed by the buffer
          manager. }

      property RAMPages[const aIndex : Longint] : TffbmRAMPage
        read bmGetRAMPage;
        { Use this property to access the RAM pages managed by the buffer
          manager. This property is base zero. The upper bound is
          pred(RAMPageCount).

          Note: This property is for unit testing purposes only. The buffer
          manager uses a sequential search to find the specified RAM page
          so accessing this property could lead to poor performance. }

      property RAMUsed : integer read GetRAM;
        { The total amount of RAM allocated to RAM pages by the buffer
          manager.  Note that this property is not thread-safe. It returns
          whatever value is available at the time and does not worry about
          the value being modified while it is being read. }

      property RecycledCount : Longint read bmGetRecycledCount;
        { Returns the total number of RAM pages in the recycled list. }

      property TempStoreSize : integer read bmGetTempStoreSize
                                       write bmSetTempStoreSize;
        { Gets and sets the size of temporary storage, in MegaBytes (MB).
          Note that you should never change the size of temporary storage
          after temporary storage has already been written to.  This is
          because the change routine does not transfer blocks already
          written to temp storage from the existing temp storage to the
          new temp storage. }
  end;

{---Primitive file access: procedural types, vars---}
  TffCloseFilePrim = procedure (aFI : PffFileInfo);
    {-to close a file}
  TffFlushFilePrim = procedure (aFI : PffFileInfo);
    {-to flush a file}
  TffGetPositionFilePrim = function (aFI : PffFileInfo) : TffInt64;
    {-to return the position of the file cursor}
  TffOpenFilePrim = function (aName       : PAnsiChar;
                              aOpenMode   : TffOpenMode;
                              aShareMode  : TffShareMode;
                              aWriteThru  : boolean;
                              aCreateFile : boolean) : THandle;
    {-to open/create file}
  TffPositionFilePrim = procedure (aFI : PffFileInfo; const aOffset : TffInt64);
    {-to position file cursor}
  TffPositionFileEOFPrim = function (aFI : PffFileInfo) : TffInt64;
    {-to position file cursor at EOF, returning file size}
  TffReadFilePrim = function (aFI : PffFileInfo; aToRead : TffWord32; var aBuffer) : TffWord32;
    {-to read from file, returning bytes read}
  TffSetEOFPrim = procedure (aFI : PffFileInfo; const aOffset : TffInt64);
    {-to truncate/extend file}
  TffSleepPrim = procedure (MilliSecs : Longint);
    {-to sleep/delay a period of time}
  TffWriteFilePrim = function (aFI : PffFileInfo; aToWrite : TffWord32; const aBuffer) : TffWord32;
    {-to write to file, returning bytes written}


{---Type definitions of the different block headers---}
{   Note: all block headers start with a signature, a next block field,
          a this block field, and a log sequence number field}
  PffBlockHeaderFile = ^TffBlockHeaderFile;
  TffBlockHeaderFile = packed record  {Block header for file}
    bhfSignature    : Longint;        {'FFFH'}
    bhfNextBlock    : TffWord32;      {should always be -1}
    bhfThisBlock    : TffWord32;      {should be equal to this block number}
    bhfLSN          : TffWord32;      {highest LSN of any block in the table;
                                       updated each time a non-readonly
                                       transaction is committed}
    bhfBlockSize    : Longint;        {size of blocks in bytes (4K, 8K, 16K, 32K, 64K)}
    bhfEncrypted    : Longint;        {0-not encrypted, 1-encrypted}
    bhfLog2BlockSize: TffWord32;      {log base 2 of bhfBlockSize (12, 13, 14, 15 or 16)}
    bhfUsedBlocks   : TffWord32;      {number of blocks in file}
    bhfAvailBlocks  : Longint;        {number of free blocks}
    bhf1stFreeBlock : TffWord32;      {number of first free block, or -1}
    bhfRecordCount  : Longint;        {number of records in file}
    bhfDelRecCount  : Longint;        {number of deleted records in file}
    bhf1stDelRec    : TffInt64;       {offset of 1st deleted record, or -1}
    bhfRecordLength : Longint;        {record length}
    bhfRecLenPlusTrailer : Longint;   {record length plus deletion link}
    bhfRecsPerBlock : Longint;        {number of records per block}
    bhf1stDataBlock : TffWord32;      {first data block, or -1}
    bhfLastDataBlock: TffWord32;      {last data block, or -1}
    bhfBLOBCount    : TffWord32;      {number of BLOBs in file}
    bhfDelBLOBHead  : TffInt64;       {file-relative offset of deleted BLOB chain head}
    bhfDelBLOBTail  : TffInt64;       {file-relative offset of deleted BLOB chain tail}
    bhfAutoIncValue : TffWord32;      {Last used autoinc value}
    bhfIndexCount   : Longint;        {number of indexes}
    bhfHasSeqIndex  : Longint;        {0-no seq access index; 1-has seq access index}
    bhfIndexHeader  : TffWord32;      {block number of index header}
    bhfFieldCount   : Longint;        {number of fields}
    bhfDataDict     : TffWord32;      {data dictionary stream, or 0}
    bhfFFVersion    : Longint;        {FF Version this file was created with}
    bhfReserved     : array [1..5] of Longint;
                                      {reserved for expansion of Longint values}
    bhfReserved2    : array [1..892] of byte;
                                      {reserved for expansion up to 1036 bytes}
  end;

  PffBlockHeaderData = ^TffBlockHeaderData;
  TffBlockHeaderData = packed record {Block header for data}
    bhdSignature    : Longint;       {'FFDH'}
    bhdNextBlock    : TffWord32;     {number of next block in chain, or -1}
    bhdThisBlock    : TffWord32;     {should be equal to this block number}
    bhdLSN          : TffWord32;     {log sequence number}
    bhdRecCount     : Longint;       {number of records in block, =bhfRecsPerBlock}
    bhdRecLength    : Longint;       {record length, =bhfRecordLength}
    bhdNextDataBlock: TffWord32;     {number of next data block}
    bhdPrevDataBlock: TffWord32;       {number of previous data block}
  end;

  PffBlockHeaderIndex = ^TffBlockHeaderIndex;
  TffBlockHeaderIndex = packed record {Block header for index}
    bhiSignature    : Longint;        {'FFIH'}
    bhiNextBlock    : TffWord32;      {number of next block in chain, or -1}
    bhiThisBlock    : TffWord32;      {should be equal to this block number}
    bhiLSN          : TffWord32;      {log sequence number}
    bhiBlockType    : byte;           {0=header, 1=btree page}
    bhiIsLeafPage   : boolean;        {0=internal btree page, 1=leaf btree page}
    bhiNodeLevel    : byte;           {node level (leaves are 1, increments)}
    bhiKeysAreRefs  : boolean;        {true if keys are reference numbers}
    bhiIndexNum     : word;           {index number to which page belongs}
    bhiKeyLength    : word;           {length of each key}
    bhiKeyCount     : Longint;        {current number of keys in page}
    bhiMaxKeyCount  : Longint;        {maximum number of keys in page}
    bhiPrevPageRef  : TffWord32;      {previous page reference !!MUST BE HERE!!}
  end;

  PffBlockHeaderBLOB = ^TffBlockHeaderBLOB;
  TffBlockHeaderBLOB = packed record {Block header for BLOB}
    bhbSignature    : Longint;       {'FFBH'}
    bhbNextBlock    : TffWord32;     {number of next block in chain, or -1}
    bhbThisBlock    : TffWord32;     {should be equal to this block number}
    bhbLSN          : TffWord32;     {log sequence number}
    bhbAssignedSegCount : TffWord32; {number of segments in a BLOB block; this
                                      field is not maintained as of v2.13 }
    bhbReserved     : array [0..1] of Longint;
  end;

  PffBlockHeaderStream = ^TffBlockHeaderStream;
  TffBlockHeaderStream = packed record {Block header for stream}
    bhsSignature    : Longint;         {'FFSH'}
    bhsNextBlock    : TffWord32;       {number of next block in chain, or -1}
    bhsThisBlock    : TffWord32;       {should be equal to this block number}
    bhsLSN          : TffWord32;       {log sequence number}
    bhsNextStrmBlock: TffWord32;       {next stream block in chain, or -1}
    bhsStreamType   : Longint;         {user-defined type of stream}
    bhsStreamLength : Longint;         {length of stream (only in first block)}
    bhsOwningStream : Longint;         {number of stream that owns block}
  end;

  PffBLOBHeader = ^TffBLOBHeader;
  TffBLOBHeader = packed record  {Header for BLOBs}
    bbhSignature    : Byte;      {..'H' for header segment, 'D' for deleted   !!.01
                                    BLOB}                                    {!!.01}
    bhbFiller       : Byte;      {..used to align bytes in memory}
    bbhSegmentLen   : Word;      {..length of this segment}
    bbhBLOBLength   : TffWord32; {..length of BLOB in bytes}           {!!.06}
    bbhSegCount     : Longint;   {..number of segments,
                                    -1 for file BLOBs, -2 for BLOB links }
    bbh1stLookupSeg : TffInt64;  {..file-relative offset of 1st lookup segment,
                                    -1 for file BLOBs}
  end;

  PffIndexHeader = ^TffIndexHeader;
  TffIndexHeader = packed record  {Header for index data}
    bihIndexKeyLen   : array [0..pred(ffcl_MaxIndexes)] of word;
                                  {..key lengths for each index}
    bihIndexFlags    : array [0..pred(ffcl_MaxIndexes)] of byte;
                                  {..flags for each index}
    bihIndexKeyCount : array [0..pred(ffcl_MaxIndexes)] of Longint;
                                  {..number of keys for each index}
    bihIndexRoot     : array [0..pred(ffcl_MaxIndexes)] of TffWord32;
                                  {..root page for each index}
    bihIndexPageCount: array [0..pred(ffcl_MaxIndexes)] of Longint;
                                  {..number of pages for each index}
  end;

  PffBLOBLookupEntry = ^TffBLOBLookupEntry;
  TffBLOBLookupEntry = packed record     {Lookup entry for BLOB}
    bleSegmentOffset : TffInt64;         {File-relative offset of segment}
    bleContentLength : TffWord32;        {Length of the content, may be < length}  {!!.11}
                                         {of segment}
  end;

  PffBLOBSegmentHeader = ^TffBLOBSegmentHeader;
  TffBLOBSegmentHeader = packed record   {Segment header for active BLOB}
    bshSignature     : byte;             {'C' for content, 'D' for deleted,
                                          'L' for lookup segments}
    bshFiller        : byte;             {aligns bytes in memory}
    bshSegmentLen    : word;             {Length of this segment}
    bshParentBLOB    : TffInt64;         {File-relative offset of header
                                          segment, or -1}
    bshNextSegment   : TffInt64;         {File-relative offset of next segment,
                                          or -1}
  end;

  PffBLOBSegmentHeaderDel = ^TffBLOBSegmentHeaderDel;
  TffBLOBSegmentHeaderDel = packed record{Segment header for deleted BLOB}
    bshSignature     : byte;             {'D' for deleted}
    bshFiller        : byte;             {aligns bytes in memory}
    bshSegmentLen    : word;             {Length of this segment}
    bshNextSegment   : TffInt64;         {File-relative offset of next segment,
                                          or -1}
    bshPrevSegment   : TffInt64;         {File-relative offset of prev segment,
                                          or -1}
  end;

{Begin !!.03}
  TffBLOBSegAction = (bsaNone, bsaAddToList, bsaDeleteFromList);
{End !!.03}

  TffBLOBSegListItem = class(TffListItem)
  protected
    FSize   : Longint;
    FOffset : TffInt64;
{Begin !!.03}
    FPendingAction : TffBLOBSegAction;
      { Identifies the action to be taken upon the list item pending the
        commit or rollback of the current transaction. }
    FTranNextItem : TffBLOBSegListItem;
      { The next BLOB segment list item modified by the current transaction.
        Allows for quick iteration through modified segments. }
{End !!.03}
  public
    constructor Create;
    function Compare(aKey : pointer) : integer; override;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise}
    function Key : pointer; override;
    {-return a pointer to this item's key}

    property Size : Longint read fSize write fSize;
      { The total size of the segment including header information. }

    property Offset : TffInt64 read fOffset write fOffset;
      { The offset of the segment within the file. }
  end;

{Begin !!.11}
  TffBaseBLOBSegmentMgr = class(TffObject)
    { Base class representing a BLOB segment manager. The segment manager
      carries out the dirty work of managing an internal free segment list for
      instances of TffBaseBLOBResourceMgr. }
  protected
    bsmUseTranList    : Boolean;
    bsmDelChain     : TffList;
    bsmDelChainSize : integer; { defaults to ciDelChainSize }
    bsmTranListHead : TffBLOBSegListItem;
    procedure bsmAddToDeletedSegChain(aFI         : PffFileInfo;
                                      aTI         : PffTransInfo;
                                      aFileHeader : PffBlockHeaderFile;
                                      aDelSeg     : TffBLOBSegListItem;
                                      aSegment    : PffBLOBSegmentHeaderDel);
      {-Inserts the deleted segment into the deleted chain within the
        physical file. }

    procedure bsmAddToTranList(aSegItem : TffBLOBSegListItem;
                               anAction : TffBLOBSegAction);
      { Adds a segment list item to the list of items modified by the current
        transaction. }

    procedure bsmRemoveFromTranList(aSegItem : TffBlobSegListItem);
    procedure bsmSliceSegment(aFI         : PffFileInfo;
                              aTI         : PffTransInfo;
                              aSegOfs     : TffInt64;
                              aSegSize    : TffWord32;
                        const aNewSize    : TffWord32;
                              aInDelChain : Boolean);
    {makes two smaller deleted segments from a larger one}
    procedure bsmRemoveFromDeletedChain(aFI     : PffFileInfo;
                                        aTI     : PffTransInfo;
                                        aSegOfs : TffInt64);
    {removes segment from deleted chain and updates file header}
  public
    constructor Create(aFI : PffFileInfo; aTI : PffTransInfo);
    destructor Destroy; override;

    procedure Commit; virtual;
    procedure DeleteSegment(aFI        : PffFileInfo;
                            aTI        : PffTransInfo;
                      const aSegOffset : TffInt64); virtual;
    function  GetNewSeg(aFI   : PffFileInfo;
                        aTI   : PffTransInfo;
                  const aSize : TffWord32) : TffInt64; virtual;
    function  GetRecycledSeg(aFI             : PffFileInfo;
                             aTI             : PffTransInfo;
                         var aSizeNeeded     : Longint;
                       const aMinSizeAllowed : Longint)
                                             : TffInt64; virtual; abstract;
    procedure ListFreeSpace(aFI : PffFileInfo; aTI : PffTransInfo;
                      const aInMemory : Boolean;
                            aStream : TStream); virtual;
    procedure Rollback; virtual;
  end;

  TffBLOBSegmentMgr = class(TffBaseBLOBSegmentMgr)
    { This version of the BLOB segment manager supports the improved nesting
      algorithm that makes use of available segments even if they are smaller
      than the requested size. }
  public
    function  GetRecycledSeg(aFI             : PffFileInfo;
                             aTI             : PffTransInfo;
                         var aSizeNeeded     : Longint;
                       const aMinSizeAllowed : Longint)
                                             : TffInt64; override;
  end;

  Tff210BLOBSegmentMgr = class(TffBaseBLOBSegmentMgr)
    { This version of the BLOB segment manager supports tables created prior
      to version 2.1.0.1. }
  public
    function  GetRecycledSeg(aFI             : PffFileInfo;
                             aTI             : PffTransInfo;
                         var aSizeNeeded     : Longint;
                       const aMinSizeAllowed : Longint)
                                             : TffInt64; override;
  end;

  TffBLOBSegmentMgrClass = class of TffBaseBLOBSegmentMgr;
  TffBLOBResourceMgrClass = class of TffBaseBLOBResourceMgr;
  
  TffBaseBLOBResourceMgr = class(TffObject)
    { Base class is used by a table to manage the creation and
      deletion of BLOB segments.  One instance of a concrete subclass
      should be created per table. }
  private
    brmPadlock : TffPadlock;
      { Used to ensure only one thread actually tries to create a BLOB
        segment manager. }
  protected
    brmDelChainSize : integer; { defaults to ciDelChainSize }
    brmSegmentMgr   : TffBaseBLOBSegmentMgr;
    brmSegMgrLoaded : boolean;

    function brmGetSegMgrClass : TffBLOBSegmentMgrClass; virtual; abstract;
    procedure brmLoadSegMgr(aFI : PffFileInfo; aTI : PffTransInfo); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    class function GetMgr(aFI : PffFileInfo) : TffBaseBLOBResourceMgr;
      { Determines which BLOB resource manager implementation should be used
        for the specified file. }

    procedure Commit; virtual;
    procedure DeleteSegment(aFI        : PffFileInfo;
                            aTI        : PffTransInfo;
                      const aSegOffset : TffInt64); virtual;
     { Use this method to delete an existing segment once it is no longer needed.
       This class will zero out the segment and place it in the recycle list.
       @param aFI The file containing the segment.
       @param segOffset The offset of the existing segment within the file. }

    function  NewSegment(aFI             : PffFileInfo;
                         aTI             : PffTransInfo;
                     var aSizeNeeded     : TffWord32;
                   const aMinSizeAllowed : TffWord32) : TffInt64; virtual; abstract;
      { Use this method to obtain a new segment of the specified size.
        You may ask for any size segment.  However, this class will not allocate
        a segment larger than the specified file's block size. Parameters:
          aFI - The file that is to contain the segment.
          aTI - The transaction in which the action is being taken.
          aSizeNeeded - The number of bytes to store in the segment.
          aMinSizeAllowed - For those segment mgr implementations that support
            it, the minimum size of the segment.
        This function returns the file-relative offset of the segment or -1 if
          a new segment could not be obtained. }

    procedure ListFreeSpace(aFI : PffFileInfo; aTI : PffTransInfo;
                      const aInMemory : Boolean;
                            aStream : TStream); virtual;
    procedure Rollback; virtual;
  end;

  TffBLOBResourceMgr = class(TffBaseBLOBResourceMgr)
    { This version of the BLOB resource manager supports the improved nesting
      algorithm that makes use of available segments even if they are smaller
      than the requested size. }
  protected
    function brmGetSegMgrClass : TffBLOBSegmentMgrClass; override;
  public
    function  NewSegment(aFI             : PffFileInfo;
                         aTI             : PffTransInfo;
                     var aSizeNeeded     : TffWord32;
                   const aMinSizeAllowed : TffWord32)
                                         : TffInt64; override;
  end;

  Tff210BLOBResourceMgr = class(TffBaseBLOBResourceMgr)
    { This version of the BLOB resource manager supports tables created prior
      to version 2.1.0.1. }
  protected
    function brmGetSegMgrClass : TffBLOBSegmentMgrClass; override;
  public
    function  NewSegment(aFI             : PffFileInfo;
                         aTI             : PffTransInfo;
                     var aSizeNeeded     : TffWord32;
                   const aMinSizeAllowed : TffWord32)
                                         : TffInt64; override;
  end;
{End !!.11}

var
  FFCloseFilePrim : TffCloseFilePrim;
    {-Primitive routine to close a file}
  FFFlushFilePrim : TffFlushFilePrim;
    {-Primitive routine to flush a file}
  FFGetPositionFilePrim : TffGetPositionFilePrim;
    {-Primitive routine to get position of file cursor}
  FFOpenFilePrim : TffOpenFilePrim;
    {-Primitive routine to open/create a file}
  FFPositionFilePrim : TffPositionFilePrim;
    {-Primitive routine to position file cursor}
  FFPositionFileEOFPrim : TffPositionFileEOFPrim;
    {-Primitive routine to position file cursor at EOF, returning file size}
  FFReadFilePrim : TffReadFilePrim;
    {-Primitive routine to read from file, returning bytes read}
  FFSetEOFPrim : TffSetEOFPrim;
    {-Primitive routine to truncate/extend file}
  FFSleepPrim : TffSleepPrim;
    {-Primitive routine to sleep/delay a period of time}
  FFWriteFilePrim : TffWriteFilePrim;
    {-Primitive routine to write to file, returning bytes written}

const
  ffc_AdminRights : TffUserRights =
     [arAdmin, arRead, arInsert, arUpdate, arDelete];
  ffc_AllUserRights : TffUserRights =
     [arRead, arInsert, arUpdate, arDelete];

{---constants for the file data---}
const
  {signatures}
  ffc_SigHeaderBlock    = $48024646; {'FF2H'}
  ffc_SigHeaderBlockv1  = $48464646; {'FFFH'}
  ffc_SigDataBlock      = $48444646; {'FFDH'}
  ffc_SigIndexBlock     = $48494646; {'FFIH'}
  ffc_SigBLOBBlock      = $48424646; {'FFBH'}
  ffc_SigStreamBlock    = $48534646; {'FFSH'}
  ffc_SigFreeBlock      = $44414544; {'DEAD'}
  ffc_SigJnlHeader      = $4846464A; {'JFFH'}
  ffc_SigJnlRecHeader   = $4852464A; {'JFRH'}
  ffc_SigDictStream     = $54434944; {'DICT'}

  {block header sizes}
  ffc_BlockHeaderSizeHeader = sizeof(TffBlockHeaderFile);
(*ffc_BlockHeaderSizeData   = sizeof(TffBlockHeaderData); moved to FFLLBASE *)
  ffc_BlockHeaderSizeIndex  = sizeof(TffBlockHeaderIndex);
  ffc_BlockHeaderSizeBLOB   = sizeof(TffBlockHeaderBLOB);
  ffc_BlockHeaderSizeStream = sizeof(TffBlockHeaderStream);

  {BLOB-specific constants}
  ffc_BLOBHeaderSize        = sizeof(TffBLOBHeader);
  ffc_BLOBBlockTypeHeader   = 0;
  ffc_BLOBBlockTypeSeg      = 1;
  ffc_BLOBSegmentHeaderSize = sizeof(TffBLOBSegmentHeader);
  ffc_BLOBLookupEntrySize   = sizeof(TffBLOBLookupEntry);
  ffc_BLOBSegmentIncrement  = 64;

  {Index-specific constants}
  ffc_InxBlockTypeHeader    = 0;
  ffc_InxBlockTypeBtreePage = 1;
  ffc_InxFlagAllowDups      = 1;
  ffc_InxFlagKeysAreRefs    = 2;

  {BLOB segment signatures}
  ffc_SigBLOBSegHeader      = $48; {'H'}
  ffc_SigBLOBSegContent     = $43; {'C'}
  ffc_SigBLOBSegDeleted     = $44; {'D'}
  ffc_SigBLOBSegLookup      = $4C; {'L'}

  ciDelChainSize = 20;     { Default # of entries in deleted chain linked list. }
  ciSegmentMultiple = 64;  { Size increment for segments. }

{---Journal file header types---}
type
  TffJournalFileHeader = packed record {journal file header}
    jfhSignature    : Longint;         {..signature: 'TFFH'}
    jfhState        : Longint;         {..0=incomplete transaction, 1=complete}
  end;

  TffJournalFileRecordHeader = packed record {journal file record header}
    jfrhSignature   : Longint;               {..signature: 'TFRH'}
    jfrhBlockNumber : TffWord32;             {..block number in file}
    jfrhBlockSize   : Longint;               {..size of block}
    jfrhBeforeImg   : Longint;               {..0=after image, 1=before image}
    jfrhFileName    : TffMaxPathZ;           {..file name}
  end;



{---Verification routines---}
function FFVerifyBLOBNr(const aBLOBNr : TffInt64;
                        aLog2BlockSize: Longint) : boolean;
  {-Verify a BLOB number to be valid}
function FFVerifyIndexCount(IndexCount : Longint) : boolean;
  {-Verify number of indexes to be between 0 and 255}
function FFVerifyRefNr(const aRefNr             : TffInt64;
                             aLog2BlockSize     : Longint;
                             aRecLenPlusTrailer : TffWord32) : boolean;
  {-Verify a record's RefNr to be valid}


{---Internal File Info routines---}
function FFAllocFileInfo(const aName   : TffFullFileName;
                         const aExt    : TffExtension;
                               aBufMgr : TffBufferManager) : PffFileInfo;
  {-Allocate a file information record for file with name aName}
procedure FFChangeFileInfo(aFI : PffFileInfo;
                     const aNewName : TffFullFileName;
                     const aExt     : TffExtension);
  {-Change a file information record for a new name aName
    Note: file must be closed}
procedure FFFreeFileInfo(var aFI : PffFileInfo);
  {-Free a file information record}
procedure FFVerifyFileHeaderSignature(aFI : PffFileInfo; const signature : Longint);
  {-Verify a file has a valid file header}
procedure FFVerifyFileInfo(aFI : PffFileInfo; IsOpen : boolean);
  {-Verify a file information record to be valid and open/closed}


{---File Access Routines---}
procedure FFCloseFile(aFI : PffFileInfo);
  {-Close file aFI}
  { Exception raised if close call fails}
function FFFileIsOpen(aFI : PffFileInfo) : boolean;
  {-Return true if the file aFI is open}
  { All exceptions are trapped and generate a result of False}
procedure FFFlushFile(aFI : PffFileInfo);
  {-Flushes file aFI}
  { Exception raised if flush call fails}
procedure FFForceFlushFile(aFI : PffFileInfo);
  {-Flushes file aFI by closing and reopening it}
  { Exception raised if anything fails}
function FFGetPositionFile(aFI : PffFileInfo) : TffInt64;
  {-Get position (offset from start) of file pointer of file aFI}
  { Exception raised if seek call fails}
function FFGetFileSize(aFI : PffFileInfo) : TffInt64;
  {-Get size of file aFI}
  { Exception raised if seek call fails}
procedure FFOpenFile(aFI         : PffFileInfo;
                     aOpenMode   : TffOpenMode;
                     aShareMode  : TffShareMode;
                     aWriteThru  : boolean;
                     aCreateFile : boolean);
  {-Allocate new file aFI, open it}
  { Exception raised if open call fails, if out of memory}
procedure FFPositionFile(    aFI : PffFileInfo;
                   const aOffset : TffInt64);
  {-Position file pointer of file aFI at aOffset}
  { Exception raised if seek call fails}
function FFPositionFileEOF(aFI : PffFileInfo) : TffInt64;
  {-Position file pointer of file aFI at EOF, return file length}
  { Exception raised if seek call fails}
function FFReadFile(aFI : PffFileInfo;
                    aToRead : TffWord32;
                var aBuffer) : TffWord32;
  {-Read aToRead bytes from file aFI into aBuffer, return bytes read}
  { Exception raised if read call fails}
procedure FFReadFileExact(aFI : PffFileInfo;
                          const aToRead : TffWord32;
                      var aBuffer);
  {-Read exactly aToRead bytes from file aFI into aBuffer}
  { Exception raised if not exactly aToRead bytes read}
procedure FFReadFileExactAt(aFI : PffFileInfo;
                            const aOffset : TffInt64;
                            aToRead : TffWord32;
                        var aBuffer);
  {-Read exactly aToRead bytes from file aFI at position aOffset into aBuffer}
procedure FFSetEOF(aFI : PffFileInfo;
                   const aOffset : TffInt64);
  {-Truncates/extends file aFI to position aOffset}
function FFWriteFile(aFI : PffFileInfo;
                     aToWrite : TffWord32;
               const aBuffer) : TffWord32;
  {-Write aToWrite bytes to file aFI from aBuffer, return bytes written}
  { Exception raised if write call fails}
procedure FFWriteFileExact(aFI : PffFileInfo;
                           aToWrite : TffWord32;
                     const aBuffer);
  {-Write exactly aToWrite bytes to file aFI from aBuffer}
  { Exception raised if not exactly aToWrite bytes written}
procedure FFWriteFileExactAt(aFI : PffFileInfo;
                             const aOffset  : TffInt64;
                             aToWrite : TffWord32;
                       const aBuffer);
  {-Write exactly aToWrite bytes to file aFI at position aOffset from aBuffer}
function  FFCalcMaxFileSize(aFI : PffFileInfo) : TffInt64;
  {-Calculate maximum file size for a table}
function  FFCalcMaxBLOBSegSize(aFI : PffFileInfo) : TffWord32;
  {-Calculate maximum BLOB segment size}

{---Encrypted File Access Routines---}
procedure FFReadDecryptFileExact(aFI     : PffFileInfo;
                                 aToRead : TffWord32;
                             var aBuffer);
  {-Read/decrypt exactly aToRead bytes from file aFI into aBuffer}
  { Exception raised if not exactly aToRead bytes read}
procedure FFReadDecryptFileExactAt(aFI     : PffFileInfo;
                             const aOffset : TffInt64;
                                   aToRead : TffWord32;
                               var aBuffer);
  {-Read/decrypt exactly aToRead bytes from file aFI at position
    aOffset into aBuffer}
procedure FFWriteEncryptFileExact(aFI      : PffFileInfo;
                                  aToWrite : TffWord32;
                              var aBuffer);
  {-Write/encrypt exactly aToWrite bytes to file aFI from aBuffer}
  { Exception raised if not exactly aToWrite bytes written}
procedure FFWriteEncryptFileExactAt(aFI      : PffFileInfo;
                              const aOffset  : TffInt64;
                                    aToWrite : TffWord32;
                                var aBuffer);
  {-Write/encrypt exactly aToWrite bytes to file aFI at position
    aOffset from aBuffer}


{---File Management Routines---}
procedure FFDeleteFile(const FileName : TffFullFileName);
  {-Delete file FileName}
procedure FFCopyFile(const FromFileName, ToFileName : TffFullFileName);
  {-Copy file FromFileName to file ToFileName, overwrite if exists}
procedure FFRenameFile(const OldFileName, NewFileName : TffFullFileName);
  {-Rename file OldFileName to NewFileName}

{---Retry Management---}
procedure FFCheckRemainingTime;                                        {!!.02}
  { Determines if the operation has timed out. }                       {!!.02}
function FFGetRetry : DWORD;
  { Returns the end time of the operation. }
function FFGetRemainingTime : Longint;                                 {!!.01}
  { Returns the # of milliseconds until the operation times out. }
procedure FFSetRetry(const aTimeout : DWORD);
  { Sets the end time of the operation. aTimeout is the number of milliseconds
    the current operation has to complete. }

{---Utility Routines---}
function FFCalcLog2BlockSize(const BlockSize : Longint) : TffWord32;
function FFCalcMaxLookupEntries(LookupSegPtr : PffBLOBSegmentHeader) : TffWord32; {!!.11}
function FFGetBlockNum(aFI      : PffFileInfo;
                 const anOffset : TffInt64) : TffWord32;
function FFAllocReleaseInfo(aBlock : PffBlock;
                            aMethod : TffInt64) : PffReleaseInfo;
procedure FFDeallocReleaseInfo(aReleaseInfo : PffReleaseInfo);

implementation

uses
  ffsrblob,
  ffsrlock,
  fftbbase;

const
  VerificationValue = $FF15FABB;
  ciReopenSleep : DWORD = 25;                                          {!!.06}
    { # of milliseconds to sleep before attempting to reopen a file.
      Used in FFForceFlushFile. On W2K machines, it is possible for the OS
      to consider the file open even though it was just previously closed.
      Not sure why this happens. This behavior has been seen by at least one
      other person outside TurboPower and waiting for the OS to flush the
      closed file seems to be the only answer. }

  { Signatures }
  ffc_SigTransaction = $51544646; {'FFTR'}


var
  Pool4K : TffMemoryPool;       {Block pool - 4K}
  Pool8K : TffMemoryPool;       {Block pool - 8K}
  Pool16K: TffMemoryPool;       {Block pool - 16K}
  Pool32K: TffMemoryPool;       {Block pool - 32K}
  Pool64K: TffMemoryPool;       {Block pool - 64K}
  EncryptBuffer : PffByteArray; {for encryption}

type
  PFIBlockKey = ^TFIBlockKey;
  TFIBlockKey = record
    FI : PffFileInfo;
    BN : TffWord32;
  end;

{$IFDEF RAMPageCheck}
procedure Log(aMsg : string; args : array of const);
begin
  if aLog <> nil then
    aLog.WriteStringFmt(aMsg, args);
end;
{$ENDIF}

{===File Management Routines=========================================}
{$I FFSRBASE.INC}
{====================================================================}

{===Retry Management=================================================}

threadvar
  fftv_RetryUntil : DWORD;
    { This variable is set on a per thread basis in the TffServerEngine
      for each database operation.  It indicates the tickcount at which an
      operation is considered to be timed out.  This variable is used to
      determine the timeout for lock requests in the lower parts of the
      engine.

      NOTE: SPW - 9/13/2000 - Moved this to the implementation section because
        D3.02 was failing with an L1086 error when the variable was in the
        interface section.
    }
{Begin !!.02}
{--------}
procedure FFCheckRemainingTime;
var
  RetryUntil : DWORD;
  TickCount  : DWORD;
begin
  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left? }
  if (RetryUntil < TickCount) or
     ((RetryUntil - TickCount) < 10) then
    { No. }
    FFRaiseExceptionNoData(EffServerException, ffStrResServer,
                           fferrGeneralTimeout);
end;
{End !!.02}
{--------}
function FFGetRemainingTime : Longint;                                 {!!.01}
begin
  if (fftv_RetryUntil = ffcl_INFINITE) or                              {!!.01}{!!.06}
     (fftv_RetryUntil = 0) then                                        {!!.01}
    Result := 0                                                        {!!.01}
  else if fftv_RetryUntil < GetTickCount then                          {!!.02}
    Result := 1                                                        {!!.02}
  else                                                                 {!!.01}
    Result := fftv_RetryUntil - GetTickCount;
end;
{--------}
function FFGetRetry : DWORD;
begin
  Result := fftv_RetryUntil;
end;
{--------}
procedure FFSetRetry(const aTimeout : DWORD);
  {-Sets the retry limit for the current thread.  Assumes that
    aTimeout is specified in milliseconds.  The retry limit is
    stored in variable fftv_RetryUntil (unit FFSRBASE).  The retry
    limit is used when acquiring table & record locks.

    This routine should be called in the public methods of
    TffServerEngine.  If a public method is sending a notification
    to extenders, the calling of this routine should occur before
    the extender notification as the extender may be doing something
    that involves table & record locking. }
begin
  if aTimeout <= 0 then
    fftv_RetryUntil := ffcl_INFINITE                                  {!!.06}
  else
    fftv_RetryUntil := GetTickCount + aTimeout;
end;
{====================================================================}

{===Utility routines=================================================}
function FFCalcLog2BlockSize(const BlockSize : Longint) : TffWord32;
begin
  case BlockSize of
     4*1024 : Result := 12;
     8*1024 : Result := 13;
    16*1024 : Result := 14;
    32*1024 : Result := 15;
  else
    Result := 16;
  end;{case}
end;
{--------}
function FFCalcMaxLookupEntries(LookupSegPtr : PffBLOBSegmentHeader) : TffWord32; {!!.11}
begin
  Result := ((LookupSegPtr^.bshSegmentLen - sizeof(TffBLOBSegmentHeader))
             div sizeof(TffBLOBLookupEntry));
end;
{--------}
function FFGetBlockNum(aFI      : PffFileInfo;
                 const anOffset : TffInt64) : TffWord32;
  { Returns the block number for the specified file offset. }
var
  TempI64 : TffInt64;
begin
  ffShiftI64R(anOffset, aFI^.fiLog2BlockSize, TempI64);
  Result := TempI64.iLow;
end;
{--------}
function FFAllocReleaseInfo(aBlock : PffBlock;
                            aMethod : TffInt64) : PffReleaseInfo;
begin
  FFGetMem(Result, SizeOf(TffReleaseInfo));
  Result^.BlockPtr := aBlock;
  Result^.MethodVar := aMethod;
end;
{--------}
procedure FFDeallocReleaseInfo(aReleaseInfo : PffReleaseInfo);
begin
  TffReleaseMethod(aReleaseInfo^.MethodVar)(aReleaseInfo^.BlockPtr);
  FFFreeMem(aReleaseInfo, SizeOf(TffReleaseInfo));
end;
{====================================================================}

{===Verification routines for BLOB segments==========================}
function FFVerifyBLOBNr(const aBLOBNr : TffInt64;
                        aLog2BlockSize: Longint) : boolean;
{Note: a BLOB number is a file-offset to a BLOB header}
var
  Offset : TffInt64;
  TempI64 : TffInt64;
begin
  Result := false;
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  {BLOB Number can't be = 0}
  if (ffCmpI64(aBLOBNr, TempI64) <> 0) then begin
    ffShiftI64R(aBLOBNr, aLog2BlockSize, Offset);
    ffShiftI64L(Offset, aLog2BlockSize, Offset);
    ffI64AddInt(Offset, ffc_BlockHeaderSizeBLOB, Offset);
    ffI64MinusI64(aBLOBNr, Offset, Offset);
    if (ffCmpI64(Offset, TempI64) = 0) then
      Result := true
    else if (ffCmpI64(Offset, TempI64) > 0) then begin
      ffI64DivInt(Offset, ffc_BLOBSegmentIncrement, TempI64);
      ffI64MultInt(TempI64, ffc_BLOBSegmentIncrement, TempI64);
      if ffCmpI64(Offset, TempI64) = 0 then
        Result := true;
    end; {if..else}
  end;
end;
{--------}
function FFVerifyIndexCount(IndexCount : Longint) : boolean;
begin
  Result := (IndexCount and $FFFFFF00) = 0;
end;
{--------}
function FFVerifyRefNr(const aRefNr             : TffInt64;
                             aLog2BlockSize     : Longint;
                             aRecLenPlusTrailer : TffWord32) : boolean;
var
  Offset  : TffInt64;
  TempI64 : TffInt64;
begin
  Result := false;
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  if (ffCmpI64(aRefNr, TempI64) <> 0) then begin
    ffShiftI64R(aRefNr, aLog2BlockSize, TempI64);
    ffShiftI64L(TempI64, aLog2BlockSize, Offset);
    ffI64MinusInt(aRefNr, Offset.iLow, TempI64);
    ffI64MinusInt(TempI64, ffc_BlockHeaderSizeData, Offset);
    if (Offset.iLow = 0) then
      Result := true
    else if (Offset.iLow > 0) then
      if (((Offset.iLow div aRecLenPlusTrailer) * aRecLenPlusTrailer) = Offset.iLow) then
        Result := true;
  end;
end;
{====================================================================}

{===Fileblock info routines==========================================}
procedure FFFreeFileInfo(var aFI : PffFileInfo);
begin
  if Assigned(aFI) then begin
    with aFI^ do begin
      FFShStrFree(fiName);
    end;
    FFFreeMem(aFI, sizeof(TffFileInfo));
  end;
end;
{--------}
procedure FFChangeFileInfo(aFI : PffFileInfo;
                     const aNewName : TffFullFileName;
                     const aExt     : TffExtension);
var
  S : TffFullFileName;
begin
  FFVerifyFileInfo(aFI, false);
  with aFI^ do begin
    FFShStrFree(fiName);
    S := FFForceExtension(FFExpandFileName(aNewName), aExt);
    fiName := FFShStrAlloc(S);
  end;
end;
{--------}
function FFAllocFileInfo(const aName   : TffFullFileName;
                         const aExt    : TffExtension;
                               aBufMgr : TffBufferManager) : PffFileInfo;
var
  S : string;
begin
  FFGetMem(Result, sizeof(TffFileInfo));
  try
    FillChar(Result^, sizeof(TffFileInfo), 0);
    with Result^ do begin
      fiVerify := VerificationValue;
      fiHandle := INVALID_HANDLE_VALUE;
      S := FFForceExtension(FFExpandFileName(aName), aExt);
      fiName := FFShStrAlloc(S);
      fiBufMgr := aBufMgr;
      fiMaxBlocks := 0;
      fiRecordLocks := nil;
      fiExclOwner := ffc_W32NoValue;
      fiAttributes := [];
      fiTempStore := nil;
    end;
  except
    FFFreeFileInfo(Result);
    raise;
  end;{try..except}
end;
{--------}
procedure FFVerifyFileHeaderSignature(aFI : PffFileInfo; const signature : Longint);
begin
  if signature <> ffc_SigHeaderBlock then
    if signature = ffc_SigHeaderBlockv1 then
      {FF v1.x tables must be converted before FF2 can read them}
      FFRaiseException(EffServerException, ffStrResServer, fferrFFV1File,
                       [aFI^.fiName^, signature])
    else
      {Not a FF File header}
      FFRaiseExceptionNoData(EffServerException, ffStrResServer, fferrNotAnFFFile);
end;
{--------}
procedure FFVerifyFileInfo(aFI : PffFileInfo; IsOpen : boolean);
begin
  if IsOpen then {should be open} begin
    if Assigned(aFI) and
       (aFI^.fiVerify = VerificationValue) and
       Assigned(aFI^.fiName) and
       (aFI^.fiHandle <> INVALID_HANDLE_VALUE) then Exit;
    FFRaiseExceptionNoData(EffServerException, ffStrResServer, fferrBadStruct);
  end
  else {should be closed} begin
    if Assigned(aFI) and
       (aFI^.fiVerify = VerificationValue) and
       Assigned(aFI^.fiName) and
       (aFI^.fiHandle = INVALID_HANDLE_VALUE) then Exit;
    FFRaiseExceptionNoData(EffServerException, ffStrResServer, fferrBadStruct);
  end
end;
{====================================================================}


{===File access routines=============================================}
procedure FFCloseFile(aFI : PffFileInfo);
begin
  FFVerifyFileInfo(aFI, true);
  if not (fffaTemporary in aFI^.fiAttributes) then
    FFCloseFilePrim(aFI);
  with aFI^ do begin
    fiHandle := INVALID_HANDLE_VALUE;
    fiBLOBrscMgr.Free;
    fiBLOBrscMgr := nil;
    fiRecordLocks.Free;
    fiRecordLocks := nil;
  end;
end;
{--------}
function FFFileIsOpen(aFI : PffFileInfo) : boolean;
begin
  try
    FFVerifyFileInfo(aFI, true);
    Result := aFI^.fiHandle <> INVALID_HANDLE_VALUE;
  except
    Result := false;
  end;{try..except}
end;
{--------}
procedure FFFlushFile(aFI : PffFileInfo);
begin
  FFVerifyFileInfo(aFI, true);
  if not (fffaTemporary in aFI^.fiAttributes) then
    FFFlushFilePrim(aFI);
end;
{--------}
procedure FFForceFlushFile(aFI : PffFileInfo);
begin
  FFVerifyFileInfo(aFI, true);
  if not (fffaTemporary in aFI^.fiAttributes) then begin
    FFCloseFilePrim(aFI);
    with aFI^ do
{Begin !!.05}
      try
        fiHandle := FFOpenFilePrim(@fiName^[1], fiOpenMode, fiShareMode,
                                   false, false);
      except
        { Re-attempt in event of failure. The failure could have occurred
          due to a timing issue (i.e., OS still thinks file is open). }
        Sleep(ciReopenSleep);                                          {!!.06}
        fiHandle := FFOpenFilePrim(@fiName^[1], fiOpenMode, fiShareMode,
                                   false, false);
      end;
{End !!.05}
  end;
end;
{--------}
function FFGetPositionFile(aFI : PffFileInfo) : TffInt64;
begin
  FFVerifyFileInfo(aFI, true);
  Result := FFGetPositionFilePrim(aFI);
end;
{--------}
function FFGetFileSize(aFI : PffFileInfo) : TffInt64;
var
  CurPos : TffInt64;
begin
  FFVerifyFileInfo(aFI, true);
  CurPos := FFGetPositionFilePrim(aFI);
  Result := FFPositionFileEOFPrim(aFI);
  FFPositionFilePrim(aFI, CurPos);
end;
{--------}
procedure FFOpenFile(aFI         : PffFileInfo;
                     aOpenMode   : TffOpenMode;
                     aShareMode  : TffShareMode;
                     aWriteThru  : boolean;
                     aCreateFile : boolean);
var
  Attr : integer;
begin
  FFVerifyFileInfo(aFI, false);
  with aFI^ do begin
    { Is this a temporary file? }
    if fffaTemporary in fiAttributes then
      { Yes. Obtain a fake file handle. }
      fiHandle := THandle(aFI)
    else begin
      { No. Are we creating the file? }
      if not aCreateFile then begin
        { No. Is the existing file marked read-only? }
    {$IFDEF DCC6OrLater}
      {$WARN SYMBOL_PLATFORM OFF}
    {$ENDIF}
        Attr := FileGetAttr(fiName^);
        if ((Attr and faReadOnly) <> 0) then begin
          { Yes. Force the file to be opened in read-only shared mode. } 
          aOpenMode := omReadOnly;
          aShareMode := smShared;                                      {!!.10}
        end;
      end;
      {$IFDEF DCC6OrLater}
        {$WARN SYMBOL_PLATFORM ON}
      {$ENDIF}
      fiHandle := FFOpenFilePrim(@fiName^[1], aOpenMode, aShareMode, aWriteThru, aCreateFile);
    end;
    fiOpenMode := aOpenMode;
    fiShareMode := aShareMode;
    fiWriteThru := aWriteThru;
  end;
end;
{--------}
procedure FFPositionFile(aFI : PffFileInfo;
                   const aOffset : TffInt64);
  begin
    FFVerifyFileInfo(aFI, true);
    FFPositionFilePrim(aFI, aOffset);
  end;
{--------}
function FFPositionFileEOF(aFI : PffFileInfo) : TffInt64;
  begin
    FFVerifyFileInfo(aFI, true);
    Result := FFPositionFileEOFPrim(aFI);
  end;
{--------}
function FFReadFile(aFI : PffFileInfo;
                    aToRead : TffWord32;
                var aBuffer) : TffWord32;
begin
  FFVerifyFileInfo(aFI, true);
  Result := FFReadFilePrim(aFI, aToRead, aBuffer);
end;
{--------}
procedure FFReadFileExact(aFI : PffFileInfo;
                          const aToRead : TffWord32;
                      var aBuffer);
begin
  FFVerifyFileInfo(aFI, true);
  if FFReadFilePrim(aFI, aToRead, aBuffer) <> aToRead then begin
    FFRaiseException(EffServerException, ffStrResServer, fferrReadExact, [aFI^.fiName^, aToRead]);
  end;
end;
{--------}
procedure FFReadFileExactAt(aFI : PffFileInfo;
                            const aOffset : TffInt64;
                            aToRead : TffWord32;
                        var aBuffer);
begin
  {note: this routine is not thread safe: the file handle is
   available to many threads, and the file pointer is handle-
   relative not thread-relative}
  FFVerifyFileInfo(aFI, true);
  FFPositionFilePrim(aFI, aOffset);
  if FFReadFilePrim(aFI, aToRead, aBuffer) <> aToRead then begin
    FFRaiseException(EffServerException, ffStrResServer, fferrReadExact, [aFI^.fiName^, aToRead]);
  end;
end;
{--------}
procedure FFSetEOF(aFI : PffFileInfo;
                   const aOffset : TffInt64);
begin
  FFVerifyFileInfo(aFI, true);
  FFSetEOFPrim(aFI, aOffset);
end;
{--------}
function FFWriteFile(aFI : PffFileInfo;
                     aToWrite : TffWord32;
               const aBuffer) : TffWord32;
begin
  FFVerifyFileInfo(aFI, true);
  Result := FFWriteFilePrim(aFI, aToWrite, aBuffer);
end;
{--------}
procedure FFWriteFileExact(aFI : PffFileInfo;
                           aToWrite : TffWord32;
                     const aBuffer);
begin
  FFVerifyFileInfo(aFI, true);
  if (FFWriteFilePrim(aFI, aToWrite, aBuffer) <> aToWrite) then begin
    FFRaiseException(EffServerException, ffStrResServer, fferrWriteExact, [aFI^.fiName^, aToWrite]);
  end;
end;
{--------}
procedure FFWriteFileExactAt(aFI      : PffFileInfo;
                       const aOffset  : TffInt64;
                             aToWrite : TffWord32;
                       const aBuffer);
begin
  {note: this routine is not thread safe: the file handle is
   available to many threads, and the file pointer is handle-
   relative not thread-relative}
  FFVerifyFileInfo(aFI, true);
  FFPositionFilePrim(aFI, aOffset);
  if (FFWriteFilePrim(aFI, aToWrite, aBuffer) <> aToWrite) then begin
    FFRaiseException(EffServerException, ffStrResServer, fferrWriteExact, [aFI^.fiName^, aToWrite]);
  end;
end;
{--------}
function  FFCalcMaxFileSize(aFI : PffFileInfo) : TffInt64;
var
  MaxFileNameLen   : DWord;
  FileSysFlags     : Dword;
  FileSysName      : array[0..MAX_PATH - 1] of AnsiChar;
  VolumeName       : array[0..MAX_PATH - 1] of AnsiChar;
  OSVersion        : TOSVersionInfo;
  OSNumber         : Byte;
  FileDrive        : string;
begin
  OSVersion.dwOSVersionInfoSize := SizeOf(OSVersion);
  GetVersionEx(OSVersion);
  if OSVersion.dwPlatformId = 1 then begin
    if OSVersion.dwMinorVersion = 0 then
      OSNumber := 1  {Win95}
    else
      OSNumber := 2; {Win98}
  end else {OSVersion.dwPlatformID = 2} begin
    if OSVersion.dwMajorVersion = 3 then
      OSNumber := 3  {WinNT 3.51}
    else if OSVersion.dwMajorVersion = 4 then
      OSNumber := 4  {WinNT 4}
    else
      OSNumber := 5; {Win2K}
  end;
  FileDrive := PChar(ExtractFileDrive(aFI^.fiName^));
  FileDrive := FileDrive + '\';
  if GetVolumeInformation(PChar(FileDrive), VolumeName, Length(VolumeName), NIL, Maxfilenamelen, FileSysFlags, FileSysName, SizeOf(FileSysName)) then begin
    {!! check on other possibilites for types of filesystems}
    if FileSysName = 'FAT32' then begin
      if OSNumber = 5 then begin
        {Win2K max FAT32 partition = 8TB, but only 4GB files}
        Result.iLow  := ffcl_FourGigabytes;
        Result.iHigh := 0;
      end else begin
        {Win95/98 max FAT32 partition size = (4GB - 2 bytes)}
        Result.iLow  := ffcl_FourGigabytes;
        Result.iHigh := 0;
      end;
    end else if FileSysName = 'NTFS' then begin
      {NTFS max file size is 2^64}
      Result.iLow := ffc_W32NoValue;
      Result.iHigh := ffc_W32NoValue;
    end else if FileSysName = 'FAT16' then begin
      if OSNumber >= 4 then begin
        {NT max FAT16 partition = 4GB; Max File Size = 2GB }
        Result.iLow  := ffcl_TwoGigabytes;
        Result.iHigh := 0;
      end else begin
        {Win95/98 max FAT16 partition = 2GB}
        Result.iLow  := ffcl_TwoGigabytes;
        Result.iHigh := 0;
      end;
    end else if FileSysName = 'CDFS' then begin
      {Can't write to a CD-ROM drive}
      Result.iLow  := 0;
      Result.iHigh := 0;
    end else if FileSysName = 'FAT' then begin
      if FileDrive = 'A:\' then begin
        {1.44 floppy}
        Result.iLow  := ffcl_MaxHDFloppy;
        Result.iHigh := 0;
      end else begin
        {Any other FAT drive}
        Result.iLow  := ffcl_TwoGigabytes;
        Result.iHigh := 0;
      end;
    end;
  end else begin
    Result.iLow  := 0;
    Result.iHigh := 0;
  end;
end;
{--------}
function  FFCalcMaxBLOBSegSize(aFI : PffFileInfo) : TffWord32;
begin
  {calc max segment size: excluding the segment header}
  Result := (((aFI^.fiBlockSize - ffc_BlockHeaderSizeBLOB - ffc_BLOBSegmentHeaderSize)
              div ffc_BLOBSegmentIncrement) * ffc_BLOBSegmentIncrement);
end;
{====================================================================}


{===Encrypted file routines==========================================}
procedure FFReadDecryptFileExact(aFI : PffFileInfo;
                                 aToRead : TffWord32;
                             var aBuffer);
begin
  FFReadFileExact(aFI, aToRead, aBuffer);
  {$IFDEF SecureServer}
  if aFI^.fiEncrypted then
    if aFI^.fiForServer then
      FFDecodeBlockServer(@aBuffer, aToRead, 0)
    else
      FFDecodeBlock(@aBuffer, aToRead, 0);
  {$ENDIF}
end;
{--------}
procedure FFReadDecryptFileExactAt(aFI : PffFileInfo;
                                   const aOffset : TffInt64;
                                   aToRead : TffWord32;
                               var aBuffer);
{$IFDEF SecureServer}                                                 {!!.01}
var
  tmpOffset : TffWord32;
{$ENDIF}                                                              {!!.01}
begin
  FFReadFileExactAt(aFI, aOffset, aToRead, aBuffer);
  {$IFDEF SecureServer}
  tmpOffset := aOffset.iLow;
  if ((aOffset.iHigh <> 0) or (tmpOffset <> 0)) and aFI^.fiEncrypted then
    if aFI^.fiForServer then
      FFDecodeBlockServer(@aBuffer, aToRead, tmpOffset)
    else
      FFDecodeBlock(@aBuffer, aToRead, tmpOffset);
  {$ENDIF}
end;
{--------}
procedure FFWriteEncryptFileExact(aFI : PffFileInfo;
                                  aToWrite : TffWord32;
                              var aBuffer);
begin
  FFVerifyFileInfo(aFI, true);
  {$IFDEF SecureServer}
  if (EncryptBuffer = nil) then
    GetMem(EncryptBuffer, 64*1024);
  Move(aBuffer, EncryptBuffer^, aToWrite);
  if aFI^.fiEncrypted then
    if aFI^.fiForServer then
      FFCodeBlockServer(EncryptBuffer, aToWrite, 0)
    else
      FFCodeBlock(EncryptBuffer, aToWrite, 0);
  if (FFWriteFilePrim(aFI, aToWrite, EncryptBuffer^) <> aToWrite) then begin
    FFRaiseException(EffServerException, ffStrResServer, fferrWriteExact, [aFI^.fiName^, aToWrite]);
  end;
  {$ELSE}
  if (FFWriteFilePrim(aFI, aToWrite, aBuffer) <> aToWrite) then begin
    FFRaiseException(EffServerException, ffStrResServer, fferrWriteExact, [aFI^.fiName^, aToWrite]);
  end;
  {$ENDIF}
end;
{--------}
procedure FFWriteEncryptFileExactAt(aFI : PffFileInfo;
                              const aOffset  : TffInt64;
                                    aToWrite : TffWord32;
                                var aBuffer);
{$IFDEF SecureServer}
var
  tmpOffset : TffWord32;
{$ENDIF}
begin
  FFVerifyFileInfo(aFI, true);
  {$IFDEF SecureServer}
  tmpOffset := aOffset.iLow;
  if (EncryptBuffer = nil) then
    GetMem(EncryptBuffer, 64*1024);
  Move(aBuffer, EncryptBuffer^, aToWrite);
  if ((aOffset.iHigh <> 0) or (tmpOffset <> 0))and aFI^.fiEncrypted then
    if aFI^.fiForServer then
      FFCodeBlockServer(EncryptBuffer, aToWrite, tmpOffset)
    else
      FFCodeBlock(EncryptBuffer, aToWrite, tmpOffset);
  FFPositionFilePrim(aFI, aOffset);
  if (FFWriteFilePrim(aFI, aToWrite, EncryptBuffer^) <> aToWrite) then begin
    FFRaiseException(EffServerException, ffStrResServer, fferrWriteExact, [aFI^.fiName^, aToWrite]);
  end;
  {$ELSE}
  FFPositionFilePrim(aFI, aOffset);
  if (FFWriteFilePrim(aFI, aToWrite, aBuffer) <> aToWrite) then begin
    FFRaiseException(EffServerException, ffStrResServer, fferrWriteExact, [aFI^.fiName^, aToWrite]);
  end;
  {$ENDIF}
end;
{====================================================================}


{===Manager for list of files to flush===============================}
type
  TffFlushList = class(TffObject)
    protected
      FList : TffVCLList;
      function GetCount : integer;
    public
      constructor Create;
      destructor Destroy; override;
      function Add(FI : PffFileInfo) : boolean;
      procedure Flush(aTrans : TffSrTransaction);
      property Count : integer read GetCount;
  end;
{--------}
constructor TffFlushList.Create;
begin
  inherited Create;
  FList := TffVCLList.Create;
end;
{--------}
destructor TffFlushList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;
{--------}
function TffFlushList.Add(FI : PffFileInfo) : boolean;
var
  i : integer;
begin
  { SPW - 11/7/2000 - Note that this routine is as optimized as possible.
    Turns out that accessing List[i] is about 4 times faster than accessing
    Items[i].  I tried replacing the use of TList with other list classes
    declared in FFLLBASE but it turns out they run slower than TList, even
    though the same kind of code is being executed.
    Interestingly, using TList.Items is much faster than TffVCLList.Items even
    though the TList.Get method is being called in either case.  We haven't
    been able to figure out why.  Regardless, using TList.List or
    TffVCLList.List gives us the fastest performance in this situation. }
  Result := false;
  for i := 0 to pred(Count) do
    if FList.List[i] = pointer(FI) then
      Exit;
  FList.Add(pointer(FI));
  Result := true;
end;
{--------}
procedure TffFlushList.Flush(aTrans : TffSrTransaction);
var
  CurrFile : PffFileInfo;
  Inx      : Integer;
begin
  for Inx := 0 to Pred(FList.Count) do begin
    CurrFile := PffFileInfo(FList[Inx]);
    {if block 0's LSN is less than the LSN of the current transaction,
     we need to change block 0's LSN to the current transaction's LSN}
    with CurrFile^ do begin
      if fiPageZero.LSN < aTrans.LSN then begin
        fiPageZero.MakeDirty(aTrans);
        fiPageZero.LSN := aTrans.LSN;
        fiPageZero.Commit(False);
      end;
    end;
    if aTrans.TransactionMode = tmFailSafe then                        {!!.12}
      FFFlushFile(CurrFile);                                           {!!.12}
    FFForceFlushFile(CurrFile);
  end;
end;
{--------}
function TffFlushList.GetCount : integer;
begin
  Result := FList.Count;
end;
{====================================================================}

{===TffbmModifiedBlock=================================================}
constructor TffbmModifiedBlock.Create(aRAMPage : TffbmRAMPage;
                                      aPrevBlock : TffbmModifiedBlock;
                                      aTransLevel : TffSrTransactionLevel);{!!.10}
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Assert(assigned(aRAMPage));
  inherited Create;
  RAMPage := aRAMPage;
  TransLevel := aTransLevel;
  mbBlock := RAMPage.rpAllocBlockPrim(RAMPage.BlockSize);
  mbBlockNumTmp := ffc_W32NoValue;
  Prev := aPrevBlock;
  AddToTransLevel;                                                     {!!.10}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffbmModifiedBlock.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { The modified block may have been used to replace another block.  If the
    block is still available to us, free it. }
  if assigned(Block) then
    RAMPage.rpFreeBlock(Block, RAMPage.BlockSize);
  inherited Destroy;
  RemoveFromTransLevel;                                                {!!.10}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmModifiedBlock.Copy(aBlock : PffBlock);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Move(aBlock^, Block^, RAMPage.BlockSize);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmModifiedBlock.CopyTo(aBlock : PffBlock);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Move(Block^, aBlock^, RAMPage.BlockSize);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.10}
{--------}
procedure TffbmModifiedBlock.DecreaseTransLevel;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  RemoveFromTransLevel;
  TransLevel := TransLevel.tlPrev;
  Assert(Assigned(TransLevel));
  AddToTransLevel;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{End !!.10}
{--------}
procedure TffbmModifiedBlock.FreeBlock;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  RAMPage.rpFreeBlock(Block, RAMPage.BlockSize);
  Block := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmModifiedBlock.mbGetBlock : PffBlock;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if mbBlockNumTmp <> ffc_W32NoValue then begin
    Assert(mbBlock = nil, 'Modified block still in memory');
    Assert(assigned(RAMPage.FileInfo^.fiTempStore), 'Temp storage not assigned');
    mbBlock := RAMPage.rpAllocBlockPrim(RAMPage.BlockSize);
    TffBaseTempStorage(RAMPage.FileInfo^.fiTempStore).ReadBlock(mbBlockNumTmp, mbBlock);
    if TransLevel.Level < SizeOf(TffWord32) * 8 then                   {!!.10}
      FFClearBit(@RAMPage.rpBlockBits, TransLevel.Level);              {!!.10}
    mbBlockNumTmp := ffc_W32NoValue;
  end;
  Result := mbBlock;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.10}
{--------}
procedure TffbmModifiedBlock.AddToTransLevel;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  mbTransLevelPrev := TransLevel.tlModifiedBlocksTail;
  TransLevel.tlModifiedBlocksTail := Self;

  { If there was a tail, make sure the old tail points to this page. }
  if Assigned(mbTransLevelPrev) then
    mbTransLevelPrev.mbTransLevelNext:=Self;

  { If this is the first page in the list, put self in the
    head position. }
  if not Assigned(TransLevel.tlModifiedBlocksHead) then
    TransLevel.tlModifiedBlocksHead := Self;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffbmModifiedBlock.RemoveFromTransLevel;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  { If this page is not at the tail then make sure the following page
    points back to the page before this page. }
  if Assigned(mbTransLevelNext) then begin
    mbTransLevelNext.mbTransLevelPrev := mbTransLevelPrev;
  end else begin
    { This page is at the tail.  The tail should now be the page before
      this page. }
    if TransLevel.tlModifiedBlocksTail = Self then
      TransLevel.tlModifiedBlocksTail := mbTransLevelPrev;
  end;

  { The page before this page should point to the page following this page. }
  if Assigned(mbTransLevelPrev) then begin
    mbTransLevelPrev.mbTransLevelNext := mbTransLevelNext;
  end else begin
    { Otherwise we are at the head of the list so make sure the head points
      to the page following this page. }
    if TransLevel.tlModifiedBlocksHead = Self then
      TransLevel.tlModifiedBlocksHead := mbTransLevelNext;
  end;
  mbTransLevelNext := nil;
  mbTransLevelPrev := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{End !!.10}
{--------}
procedure TffbmModifiedBlock.SendToTempStore;
var
  aTmpStore : TffBaseTempStorage;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Assert(mbBlockNumTmp = ffc_W32NoValue, 'Modified block already in temp store');
  aTmpStore := TffBaseTempStorage(RAMPage.FileInfo^.fiTempStore);
  if not aTmpStore.Full then begin
    mbBlockNumTmp := aTmpStore.WriteBlock(mbBlock);
    RAMPage.rpFreeBlock(mbBlock, RAMPage.BlockSize);
    mbBlock := nil;
    if TransLevel.Level < SizeOf(TffWord32) * 8 then                   {!!.10}
      FFSetBit(@RAMPage.rpBlockBits, TransLevel.Level);                {!!.10}
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}

{===TffbmRAMPage=====================================================}
constructor TffbmRAMPage.Create(aBufMgr : TffBufferManager; aFI : PffFileInfo;
                          const aBlockNumber : TffWord32);
begin
  {$IFDEF RAMPageCheck}
  Log('Create RAMPage %d',[aBlockNumber]);
  {$ENDIF}
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  FNew := True;                                                        {!!.11}
  rpBlockBits := 0;
  rpBlockListTail := nil;
  rpBufMgr := aBufMgr;
  rpFI := aFI;
  rpBlockNum := aBlockNumber;
  rpBlockNumTmp := ffc_W32NoValue;
  BlockSize := aFI^.fiBlockSize;
  rpBlockSizeEnum := FFMapBlockSize(aFI^.fiBlockSize);
  FLastAccess := ffcl_INFINITE;                                       {!!.06}
  FRefCount := 0;
  if fffaTemporary in aFI^.fiAttributes then
    rpReuseMode := ffrmTempStore
  else
    rpReuseMode := ffrmUseAsIs;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffbmRAMPage.Destroy;
var
  aBlock : TffbmModifiedBlock;
begin
  {$IFDEF RAMPageCheck}
  Log('Free RAMPage %d',[rpBlockNum]);
  {$ENDIF}
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Is the read-only block in temporary storage? }
  if rpBlockNumTmp <> ffc_W32NoValue then begin
    { Yes. Retrieve. }
    rpAllocBlock(rpBlockSize);
    TffBaseTempStorage(rpFI^.fiTempStore).ReadBlock(rpBlockNumTmp, rpBlock);
  end;

  { Free the block. }
  BlockSize := 0;
  while assigned(rpBlockListTail) do begin
    aBlock := rpBlockListTail;
    rpBlockListTail := rpBlockListTail.Prev;
    aBlock.Free;
  end;
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.AddToFilePageList;
var
  pc1: PffPageContainer;
  pc2: PffPageContainer;
  pc3: PffPageContainer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Insert self into the list of RAM pages maintained by the
    RAM pages themselves.  Add the page to the tail of the list of RAM
    pages maintained by the file structure. }
  rpFilePrev := rpFI^.fiPageListTail;
  rpFI^.fiPageListTail := Self;
  if Assigned(rpFilePrev) then
    rpFilePrev.rpFileNext:=Self;
  { If this page is the first in the list then update
    the file's head pointer. }
  if not Assigned(rpFI^.fiPageListHead) then
    rpFI^.fiPageListHead := Self;

  { If this is the header page store it in a special field for quick access }
  if BlockNumber = 0 then begin
    Assert(not Assigned(rpFI^.fiPageZero));
    rpFI^.fiPageZero := Self;
    Exit;
  end;

  { Walk through the tree to the spot where this page should be located. }
  pc1 := rpFI^.fiPages[TffBlockNum(rpBlockNum)[3]];
  if not Assigned(pc1) then begin
    FFGetMem(pc1, sizeOf(TffPageContainer));
    FillChar(pc1^,SizeOf(pc1^),0);
    pc1.pcNext := rpFI^.fiPageContainerList;
    if Assigned(pc1.pcNext) then begin
      Assert(not Assigned(pc1.pcNext.pcPrev));
      pc1.pcNext.pcPrev := pc1;
    end;
    rpFI^.fiPageContainerList := pc1;
    rpFI^.fiPages[TffBlockNum(rpBlockNum)[3]] := pc1;
  end;

  pc2 := pc1.pcPages[TffBlockNum(rpBlockNum)[2]];
  if not Assigned(pc2) then begin
    FFGetMem(pc2, sizeOf(TffPageContainer));
    FillChar(pc2^,SizeOf(pc2^),0);
    pc2.pcNext := rpFI^.fiPageContainerList;
    if Assigned(pc2.pcNext) then begin
      Assert(not Assigned(pc2.pcNext.pcPrev));
      pc2.pcNext.pcPrev := pc2;
    end;
    rpFI^.fiPageContainerList := pc2;
    pc1.pcPages[TffBlockNum(rpBlockNum)[2]] := pc2;
    Inc(pc1.pcCount);
  end;

  pc3 := pc2.pcPages[TffBlockNum(rpBlockNum)[1]];
  if not Assigned(pc3) then begin
    FFGetMem(pc3, sizeOf(TffPageContainer));
    FillChar(pc3^,SizeOf(pc3^),0);
    pc3.pcNext := rpFI^.fiPageContainerList;
    if Assigned(pc3.pcNext) then begin
      Assert(not Assigned(pc3.pcNext.pcPrev));
      pc3.pcNext.pcPrev := pc3;
    end;
    rpFI^.fiPageContainerList := pc3;
    pc2.pcPages[TffBlockNum(rpBlockNum)[1]] := pc3;
    Inc(pc2.pcCount);
  end;

  { Add self to the leaf node. }
  Assert(not Assigned(pc3.pcPages[TffBlockNum(rpBlockNum)[0]]));
  pc3.pcPages[TffBlockNum(rpBlockNum)[0]] := Self;
  Inc(pc3.pcCount);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.AddToRecycleList;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Assumption: rpInUsePrev already set to nil. }
  rpInUseNext := rpBufMgr.bmRecycleListHead;
  rpBufMgr.bmRecycleListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.AddToTransList(aTrans : TffSrTransaction);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Assert(assigned(aTrans));
  rpTransPrev := aTrans.trTransPageListTail;
  aTrans.trTransPageListTail := Self;
  if Assigned(rpTransPrev) then
    rpTransPrev.rpTransNext := Self;
  if not Assigned(aTrans.trTransPageListHead) then
    aTrans.trTransPageListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.AddToUseList;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  rpInUsePrev := rpBufMgr.bmInUseListTail;
  rpBufMgr.bmInUseListTail := Self;

  { If there was a tail, make sure the old tail points to this page. }
  if Assigned(rpInUsePrev) then
    rpInUsePrev.rpInUseNext:=Self;

  { If this is the first page in the list, put self in the
    head position. }
  if not Assigned(rpBufMgr.bmInUseListHead) then
    rpBufMgr.bmInUseListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.Block(aTrans : TffSrTransaction;
                        var aReleaseMethod : TffReleaseMethod) : PffBlock;
{$IFDEF RAMPageCheck}
var
  PStr : array[0..8] of char;
{$ENDIF}
begin
{$IFDEF RAMPageCheck}
  Log('Page %d: TffbmRAMPage.Block', [rpBlockNum]);
{$ENDIF}
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { No transaction specified or this is a transaction other than the one
    that has modified this block? }
  if (not assigned(aTrans)) or (rpTrans <> aTrans) then begin
    { Yes.  Is the read-only block currently in temporary storage? }
    if rpBlockNumTmp <> ffc_W32NoValue then
      { Yes. Retrieve from temp storage. }
      rpRetrieveFromTemp;
    { Return the read-only block. }
    Result := rpBlock;
    {$IFDEF RAMPageCheck}
    FFPointerAsHex(PStr, Result);
    Log('Page %d: Acq read-only block, ref Count %d, address %s',
        [rpBlockNum, FRefCount + 1, PStr]);
    {$ENDIF}
  end
  else begin
    { No.  Return the most-recent modification. }
    Result := rpBlockListTail.Block;
    {$IFDEF RAMPageCheck}
    FFPointerAsHex(PStr, Result);
    Log('Page %d: Acq modified block, ref count %d, address %s',
        [rpBlockNum, FRefCount + 1, PStr]);
    {$ENDIF}
  end;

  { Ensure the ram page is looking at the header of the retrieved block.}
  rpHeader := PffBlockCommonHeader(Result);

  { Increment the reference count. }
  InterlockedIncrement(FRefCount);
  aReleaseMethod := Self.Release;
  FLastAccess := GetTickCount;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.Commit(forceWrite : boolean) : boolean;
var
  anItem    : TffbmModifiedBlock;
  aPrevItem : TffbmModifiedBlock;
  TempI64   : TffInt64;
  {$IFDEF RAMPageCheck}
  PStr, PStr2 : array[0..8] of char;
  {$ENDIF}
begin
  {$IFDEF RAMPageCheck}
  Log('Commit page %d', [rpBlockNum]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Assumption: If transaction is being committed to disk then the transaction
    has obtained write access to the table content. }

  { Requirement: Must have been modified. }
  Assert(rpDirty);

  { Was the read-only block written to temporary storage? }
  if rpBlockNumTmp <> ffc_W32NoValue then
    { Yes. Restore the read-only block so that things work out properly. }
    rpRetrieveFromTemp;

  { Assume we are not committing to disk. }
  Result := False;

  { Are we forcing commit to disk? }
  if forceWrite then begin
    { Yes.  Copy the most recently modified block to the read-only block. }
    rpReplaceBlock(rpBlockListTail.Block);
    rpHeader^.bchLSN := rpTrans.LSN;
    rpBlockListTail.Block := nil;

    { If this is not a temporary file then write to disk. }
    if not (fffaTemporary in rpFI^.fiAttributes) then begin
      TempI64.iLow := BlockNumber;
      TempI64.iHigh := 0;
      FFI64MultInt(TempI64, BlockSize, TempI64);
      FFWriteEncryptFileExactAt(FileInfo, TempI64, rpBlockSize, rpBlock^);
    end;

    { Get rid of all modified block versions. }
    while assigned(rpBlockListTail) do begin
      anItem := rpBlockListTail;
      rpBlockListTail := rpBlockListTail.Prev;
      anItem.Free;
    end;
    RemoveFromTransList(rpTrans);
    MakeClean;
    FNew := False;                                                     {!!.07}
    Result := True;
  end
  else
    { No.  Does this block's nest level match that of the transaction's? }
    if rpGetTransLevel = rpTrans.TransLevel then         
      { Yes. Do we have more than one modified block? }
      if assigned(rpBlockListTail.Prev) then begin
        { Yes. Is the previous block one nest level behind the most recent
          block? }
        aPrevItem := rpBlockListTail.Prev;
        if aPrevItem.TransLevel = (rpBlockListTail.TransLevel.tlPrev ) then begin
          { Yes.  Replace the previous block with the most recent block. }
          aPrevItem.FreeBlock;
          aPrevItem.Block := rpBlockListTail.Block;
          rpBlockListTail.Block := nil;
          { Delete the most recent block. }
          rpBlockListTail.Free;
          rpBlockListTail := aPrevItem;
        end
        else
          { No.  The previous block is two or more levels below us.  Decrement the
           nest level of the most recent block. }
          rpBlockListTail.DecreaseTransLevel;                          {!!.10}
      end
      else begin
        { No.  We have only 1 modified block. Is this block ready to be written
          to disk?  }
        if rpBlockListTail.TransLevel.Level = 0 then begin             {!!.10}
          {$IFDEF RAMPageCheck}
          FFPointerAsHex(PStr, rpBlock);
          FFPointerAsHex(PStr2, rpBlockListTail.Block);
          Log('Page %d: Commit, read-only block %s, new block %s',
              [rpBlockNum, PStr, PStr2]);
          {$ENDIF}

          { Yes.  Replace the read-only copy with the modified copy.  Note that
            decrease of RAM detail occurs when rpBlock is freed in
            rpReplaceBlock. }
          rpReplaceBlock(rpBlockListTail.Block);
          rpBlockListTail.Block := nil;
          rpHeader^.bchLSN := rpTrans.LSN;

          { If this is not a temporary file then write to disk. }
          if not (fffaTemporary in rpFI^.fiAttributes) then begin
            TempI64.iLow := BlockNumber;
            TempI64.iHigh := 0;
            FFI64MultInt(TempI64, BlockSize, TempI64);
            FFWriteEncryptFileExactAt(FileInfo, TempI64, rpBlockSize, rpBlock^);
          end;

          { Get rid of the modified block since it is no longer needed. }
          rpBlockListTail.Free;
          rpBlockListTail := nil;
          RemoveFromTransList(rpTrans);
          MakeClean;
          FNew := False;                                               {!!.07}
          Result := True;
        end
        else
          rpBlockListTail.DecreaseTransLevel;                          {!!.10}
      end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.DirtiedForTrans(aTrans : TffSrTransaction) : Boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := assigned(aTrans) and
            assigned(rpBlockListTail) and
            (rpTrans = aTrans) and
            (rpGetTransLevel = aTrans.TransLevel);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.MakeClean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  rpBlockListTail := nil;
  rpTrans := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.MakeDirty(aTrans : TffSrTransaction);
var
  anItem : TffbmModifiedBlock;
begin
  {$IFDEF RAMPageCheck}
  Log('Page %d: MakeDirty',[rpBlockNum]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Assumption: If already marked dirty then never marked dirty by a different
    transaction. }

  Assert((rpTrans = nil) or (aTrans = rpTrans));

  { Is this block already dirty? }
  if assigned(rpBlockListTail) then begin
    { Yes.  Does the transaction have a higher nesting level? }
    if rpGetTransLevel.Level < aTrans.TransLevel.Level then begin      {!!.10}
      { Yes. Make a copy of the last modified block and add it to the list
        of modified blocks.  Assumption: There is at least one modified block
        in the modified block list. }
      anItem := TffbmModifiedBlock.Create(Self, rpBlockListTail, aTrans.TransLevel);

      { Copy the last modified block. }
      anItem.Copy(rpBlockListTail.Block);

      { Add the block to the list. }
      rpBlockListTail := anItem;
    end;
  end
  else begin
    { No.  Record the transaction. }
    rpTrans := aTrans;

    { Make a copy of the read-only block and add it to the modified block
      list. }
    rpBlockListTail := TffbmModifiedBlock.Create(Self, nil, aTrans.TransLevel);
    { Is the read-only block currently in temporary storage? }
    if rpBlockNumTmp <> ffc_W32NoValue then
      { Yes. Retrieve from temp storage. }
      rpRetrieveFromTemp;
    rpBlockListTail.Copy(rpBlock);
    AddToTransList(aTrans);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.MoveToEndOfTransList;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { If this page is followed by another page, the following page
    should point back to the page before this page. }
  if Assigned(rpTransNext) then begin
    rpTransNext.rpTransPrev := rpTransPrev;
  end else
    { Otherwise this page is already at the end of the list so do nothing. }
    Exit;

  { If a page precedes this page then it should point to the page following
    this page. }
  if Assigned(rpTransPrev) then begin
    rpTransPrev.rpTransNext := rpTransNext;
  end else begin
    { Otherwise we are at the head of the list so the head should point to
      the page following this page. }
    if rpTrans.trTransPageListHead = Self then
      rpTrans.trTransPageListHead := rpTransNext;
  end;

  { The page at the end of the list should now point to this page. }
  rpTransPrev := rpTrans.trTransPageListTail;
  rpTrans.trTransPageListTail := Self;
  rpTransNext := nil;
  if Assigned(rpTransPrev) then
    rpTransPrev.rpTransNext := Self;
  if not Assigned(rpTrans.trTransPageListHead) then
    rpTrans.trTransPageListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.MoveToEndOfUseList;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
{Begin !!.01}
  { Already at end of list? }
  if rpInUseNext = nil then
    { Yes. Exit. }
    Exit;

  { Point the following page to the page before this page. }
  rpInUseNext.rpInUsePrev := rpInUsePrev;

  { If a page precedes this page then it should point to the page following
    this page. }
  if Assigned(rpInUsePrev) then begin
    rpInUsePrev.rpInUseNext := rpInUseNext;
  end else begin
    { Otherwise we are at the head of the list so the head should point to
      the page following this page. }
    if rpBufMgr.bmInUseListHead = Self then
      rpBufMgr.bmInUseListHead := rpInUseNext;
  end;

  { The page at the end of the list should now point to this page. }
  rpInUsePrev := rpBufMgr.bmInUseListTail;
  rpBufMgr.bmInUseListTail := Self;
  rpInUseNext := nil;
  if Assigned(rpInUsePrev) then
    rpInUsePrev.rpInUseNext := Self;
  if rpBufMgr.bmInUseListHead = nil then
    rpBufMgr.bmInUseListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.MoveToRecycleList;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  RemoveFromUseList;
  AddToRecycleList;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.Release(var aBlock: PffBlock);
{$IFDEF RAMPageCheck}
var
  Pstr : array[0..8] of char;
{$ENDIF}
begin
{ Assumption: This method may be accessed by multiple threads at the same time.
  This is allowed hence no checks for ThreadEnter and ThreadExist when
  the FF_DEBUG_THREADS define is enabled. The routine is threadsafe since
  it uses the InterlockedDecrement function. }

  {$IFDEF RAMPageCheck}
  FFPointerAsHex(PStr, aBlock);
  Log('Page %d: Release, refCount %d, address %s',
      [rpBlockNum, FRefCount - 1, PStr]);
  {$ENDIF}
//  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}

  { The first check in this assertion should be fairly obvious --
    something is outta whack if we're releasing a block that isn't
    referenced by anything. The remaining checks ensure that we have
    not lost the proper association between a block and its assigned
    release method. The first of these is a check for read-only blocks
    and the second is for modified blocks.

    NOTE: In some situations, a block may be marked dirty after it was
    previously marked dirty. The check against rpBlockListTail.Prev is to
    catch the case where a block that is being released is no longer the
    tail block of the modified list. }
  Assert((FRefCount > 0) and
         ((aBlock = rpBlock) or
          (assigned(rpBlockListTail) and                                 {!!.10}
          ((aBlock = rpBlockListTail.Block) or                           {!!.10}
          (assigned(rpBlockListTail.Prev) and                            {!!.10}
          (aBlock = rpBlockListTail.Prev.Block))))));                    {!!.10}
  aBlock := nil;                                      
  InterlockedDecrement(FRefCount);
//  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.Removable(var RemoveMode : TffbmPageReuseMode) : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  RemoveMode := rpReuseMode;
  Result := False;

  { Can't be removed if this block is dirty, it is block zero, or it is
    actively used by one or more threads. }
  if assigned(rpBlockListTail) or
     (rpBlockNum = 0) or
     ((FRefCount > 0) and ((GetTickCount - FLastAccess) < ffcl_PageLife)) then
    Exit;

  { The page may be re-used if it cannot be sent to temporary storage. }
  Result := (rpReuseMode <> ffrmTempStore);

  if Result then
    Exit
  else begin
    { Otherwise, it can be sent to temp storage. It can be re-used if the page
      is not already in temp storage and temp storage contains room for
      the page. }
    Result := (not rpGetInTempStore);
    if Result then
      if assigned(rpBlockListTail) then
        Result := Result and
                  (TffBaseTempStorage(rpFI^.fiTempStore).HasSpaceFor
                   (2 + rpBlockListTail.TransLevel.Level))             {!!.10}
      else
        Result := Result and (not TffBaseTempStorage(rpFI^.fiTempStore).Full);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.RemoveFromFilePageList;
var
  pc1: PffPageContainer;
  pc2: PffPageContainer;
  pc3: PffPageContainer;
begin
  {$IFDEF RAMPageCheck}
  Log('Page %d: RemoveFromFilePageList',[rpBlockNum]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}

  { Remove self from the list of RAM pages maintained by the RAM pages.
    Remove self from the list of RAM pages maintained by the file
    structure. }
  if Assigned(rpFileNext) then begin
    rpFileNext.rpFilePrev := rpFilePrev;
  end else begin
    if rpFI^.fiPageListTail = Self then
      rpFI^.fiPageListTail := rpFilePrev;
  end;
  if Assigned(rpFilePrev) then begin
    rpFilePrev.rpFileNext := rpFileNext;
  end else begin
    if rpFI^.fiPageListHead = Self then
      rpFI^.fiPageListHead := rpFileNext;
  end;
  rpFileNext := nil;
  rpFilePrev := nil;

  { If this is the header page it was stored it in a special field for quick access }
  if BlockNumber = 0 then begin
    Assert(rpFI^.fiPageZero = Self);
    rpFI^.fiPageZero := nil;
    Exit;
  end;

  { Remove ourselves from the file's RAM pages structure. }
  pc1 := rpFI^.fiPages[TffBlockNum(rpBlockNum)[3]];
  Assert(Assigned(pc1));
  if not Assigned(pc1) then
    Exit;

  pc2 := pc1.pcPages[TffBlockNum(rpBlockNum)[2]];
  Assert(Assigned(pc2));
  if not Assigned(pc2) then
    Exit;

  pc3 := pc2.pcPages[TffBlockNum(rpBlockNum)[1]];
  Assert(Assigned(pc3));
  if not Assigned(pc3) then
    Exit;

  Assert(pc3.pcPages[TffBlockNum(rpBlockNum)[0]] = Self);
  pc3.pcPages[TffBlockNum(rpBlockNum)[0]] := nil;
  Dec(pc3.pcCount);

  { remove the the page container if no longer used }
  if pc3.pcCount = 0 then begin
    { is this the first page container in the list? }
    if not Assigned(pc3.pcPrev) then begin
      { yes... this page container must be the head of the list}
      Assert(rpFI^.fiPageContainerList = pc3);
      rpFI^.fiPageContainerList := pc3.pcNext;
    end else begin
      { no... the previous page container must reference this page container}
      Assert(pc3.pcPrev.pcNext = pc3);
      pc3.pcPrev.pcNext := pc3.pcNext;
    end;
    { is there a page container after this one? }
    if Assigned(pc3.pcNext) then begin
      { yes... the next page container must reference this page container}
      Assert(pc3.pcNext.pcPrev = pc3);
      pc3.pcNext.pcPrev := pc3.pcPrev;
    end;

    { free the page container }
    FFFreeMem(pc3, sizeOf(TffPageContainer));

    { remove this page container from its parent }
    pc2.pcPages[TffBlockNum(rpBlockNum)[1]] := nil;
    Dec(pc2.pcCount);

    { remove the the page container if no longer used }
    if pc2.pcCount = 0 then begin
      { is this the first page container in the list? }
      if not Assigned(pc2.pcPrev) then begin
        { yes... this page container must be the head of the list}
        Assert(rpFI^.fiPageContainerList = pc2);
        rpFI^.fiPageContainerList := pc2.pcNext;
      end else begin
        { no... the previous page container must reference this page container}
        Assert(pc2.pcPrev.pcNext = pc2);
        pc2.pcPrev.pcNext := pc2.pcNext;
      end;
      { is there a page container after this one? }
      if Assigned(pc2.pcNext) then begin
        { yes... the next page container must reference this page container}
        Assert(pc2.pcNext.pcPrev = pc2);
        pc2.pcNext.pcPrev := pc2.pcPrev;
      end;

      { free the page container }
      FFFreeMem(pc2, sizeOf(TffPageContainer));

      { remove this page container from its parent }
      pc1.pcPages[TffBlockNum(rpBlockNum)[2]] := nil;
      Dec(pc1.pcCount);

      { remove the the page container if no longer used }
      if pc1.pcCount = 0 then begin
        { is this the first page container in the list? }
        if not Assigned(pc1.pcPrev) then begin
          { yes... this page container must be the head of the list}
          Assert(rpFI^.fiPageContainerList = pc1);
          rpFI^.fiPageContainerList := pc1.pcNext;
        end else begin
          { no... the previous page container must reference this page container}
          Assert(pc1.pcPrev.pcNext = pc1);
          pc1.pcPrev.pcNext := pc1.pcNext;
        end;
        { is there a page container after this one? }
        if Assigned(pc1.pcNext) then begin
          { yes... the next page container must reference this page container}
          Assert(pc1.pcNext.pcPrev = pc1);
          pc1.pcNext.pcPrev := pc1.pcPrev;
        end;

        { free the page container }
        FFFreeMem(pc1, sizeOf(TffPageContainer));

        { remove this page container from its parent }
        rpFI^.fiPages[TffBlockNum(rpBlockNum)[3]] := nil;
      end;
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.RemoveFromRecycleList;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  rpBufMgr.bmRecycleListHead := rpInUseNext;
  rpInUseNext := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.RemoveFromTransList(aTrans : TffSrTransaction);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if Assigned(rpTransNext) then begin
    rpTransNext.rpTransPrev := rpTransPrev;
  end else begin
    if aTrans.trTransPageListTail = Self then
      aTrans.trTransPageListTail := rpTransPrev;
  end;
  if Assigned(rpTransPrev) then begin
    rpTransPrev.rpTransNext := rpTransNext;
  end else begin
    if aTrans.trTransPageListHead = Self then
      aTrans.trTransPageListHead := rpTransNext;
  end;
  rpTransNext := nil;
  rpTransPrev := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.RemoveFromUseList;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { If this page is not at the tail then make sure the following page
    points back to the page before this page. }
  if Assigned(rpInUseNext) then begin
    rpInUseNext.rpInUsePrev := rpInUsePrev;
  end else begin
    { This page is at the tail.  The tail should now be the page before
      this page. }
    if rpBufMgr.bmInUseListTail = Self then
      rpBufMgr.bmInUseListTail := rpInUsePrev;
  end;

  { The page before this page should point to the page following this page. }
  if Assigned(rpInUsePrev) then begin
    rpInUsePrev.rpInUseNext := rpInUseNext;
  end else begin
    { Otherwise we are at the head of the list so make sure the head points
      to the page following this page. }
    if rpBufMgr.bmInUseListHead = Self then
      rpBufMgr.bmInUseListHead := rpInUseNext;
  end;
  rpInUseNext := nil;
  rpInUsePrev := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.ReadOnlyBlock : PffBlock;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := rpBlock;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.Reusable(var ReuseMode : TffbmPageReuseMode) : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  ReuseMode := rpReuseMode;
  Result := False;

  { Can't be removed if this is block zero or it is actively used by one or
    more threads. }
  if (rpBlockNum = 0) or
     ((FRefCount > 0) and ((GetTickCount - FLastAccess) < ffcl_PageLife)) then
    Exit;

  { Can this page be sent to temporary storage? }
  if (rpReuseMode = ffrmTempStore) then begin
    { Yes. We can re-use the page if it is not already in temporary storage
      & temporary storage contains room for the page & its blocks. }
    Result := (not rpGetInTempStore);
    if Result then
      if assigned(rpBlockListTail) then
        Result := Result and
                  (TffBaseTempStorage(rpFI^.fiTempStore).HasSpaceFor
                   (2 + rpBlockListTail.TransLevel.Level))             {!!.10}
      else
        Result := Result and (not TffBaseTempStorage(rpFI^.fiTempStore).Full);
  end
  else
    { No. Page may be re-used if it is clean. }
    Result := (rpBlockListTail = nil);

  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.Rollback;
var
  aBlock : TffbmModifiedBlock;
begin
  {$IFDEF RAMPageCheck}
  Log('Page %d: Rollback',[rpBlockNum]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Requirement: Must have been dirtied. }
  Assert(assigned(rpBlockListTail));

  { Does this block's nest level match that of the transaction's? }
  if rpGetTransLevel = rpTrans.TransLevel then begin
    { Yes.  Is this nest level zero? }
    if rpTrans.TransLevel.Level = 0 then begin                         {!!.10}
      { Yes. Assume this is the only block in the modified block list.
        Get rid of the modified block. }
      rpBlockListTail.Free;
      rpBlockListTail := nil;
{Begin !!.07}
      if FNew then begin
        RemoveFromFilePageList;
        RemoveFromTransList(rpTrans);
        RemoveFromUseList;
        AddToRecycleList;
        FileInfo := nil;
      end
      else
        RemoveFromTransList(rpTrans);
      rpTrans := nil;
{End !!.07}
    end
    else begin
      { No.  Get rid of the last modified block. }
      aBlock := rpBlockListTail.Prev;
      rpBlockListTail.Free;
      rpBlockListTail := aBlock;
      if not assigned(rpBlockListTail) then
        RemoveFromTransList(rpTrans);
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.rpAllocBlock(aBlockSize : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  rpBlockSize := aBlockSize;
  if (rpBlockSize <> 0) then begin
    rpBlock := rpAllocBlockPrim(rpBlockSize);
    rpHeader := PffBlockCommonHeader(rpBlock);
  end
  else begin
    rpBlock := nil;
    rpHeader := nil;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.rpAllocBlockPrim(aBlockSize : Longint) : PffBlock;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := nil;
  if (aBlockSize <> 0) then begin
    case aBlockSize of
      4 * 1024 : begin
                   if (Pool4K = nil) then
                     Pool4K := TffMemoryPool.Create(4*1024, 1);
                   Result := Pool4K.Alloc;
                 end;
      8 * 1024 : begin
                   if (Pool8K = nil) then
                     Pool8K := TffMemoryPool.Create(8*1024, 1);
                   Result := Pool8K.Alloc;
                 end;
      16* 1024 : begin
                   if (Pool16K = nil) then
                     Pool16K := TffMemoryPool.Create(16*1024, 1);
                   Result := Pool16K.Alloc;
                 end;
      32* 1024 : begin
                   if (Pool32K = nil) then
                     Pool32K := TffMemoryPool.Create(32*1024, 1);
                   Result := Pool32K.Alloc;
                 end;
      64* 1024 : begin
                   if (Pool64K = nil) then
                     Pool64K := TffMemoryPool.Create(64*1024, 1);
                   Result := Pool64K.Alloc;
                 end;
    else
      GetMem(Result, aBlockSize);
    end;{case}
    rpBufMgr.bmIncreaseRAMDetail(aBlockSize);
    FillChar(Result^, aBlockSize, 'F');
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.rpDirty : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := assigned(rpBlockListTail);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.rpFreeBlock(aBlock : PffBlock; aBlockSize : Longint);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if (rpBlockSize <> 0) and assigned(aBlock) then begin
    case aBlockSize of
      4 * 1024 : Pool4K.Dispose(aBlock);
      8 * 1024 : Pool8K.Dispose(aBlock);
      16* 1024 : Pool16K.Dispose(aBlock);
      32* 1024 : Pool32K.Dispose(aBlock);
      64* 1024 : Pool64K.Dispose(aBlock);
    else
      FreeMem(aBlock, aBlockSize);
    end;{case}
    rpBufMgr.bmDecreaseRAMDetail(aBlockSize);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.rpGetInTempStore : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := (rpBlockNumTmp <> ffc_W32NoValue) or
            (rpBlockBits > 0);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function  TffbmRAMPage.rpGetLSN : TffWord32;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Has this page been dirtied by a transaction? }
  if assigned(rpTrans) then
    { Yes.  Return the transaction's LSN. }
    Result := rpTrans.LSN
  else
    { No.  Return the LSN of the read-only block. }
    Result := rpHeader^.bchLSN;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffbmRAMPage.rpGetTransLevel : TffSrTransactionLevel;         {!!.10}
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if assigned(rpBlockListTail) then
    Result := rpBlockListTail.TransLevel
  else
    Result := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.rpRelease(aBlock: PffBlock);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Assert((FRefCount > 0) and
         ((aBlock = rpBlock) or (aBlock = rpBlockListTail.Block)));
  InterlockedDecrement(FRefCount);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.rpReplaceBlock(aNewBlock : PffBlock);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  rpFreeBlock(rpBlock, rpBlockSize);
  rpBlock := aNewBlock;
  rpHeader := PffBlockCommonHeader(rpBlock);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.rpRetrieveFromTemp;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  rpAllocBlock(rpBlockSize);
  TffBaseTempStorage(rpFI^.fiTempStore).ReadBlock(rpBlockNumTmp, rpBlock);
  rpBlockNumTmp := ffc_W32NoValue;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.rpSetBlockSize(aBlockSize : Longint);
var
  aBlock : TffbmModifiedBlock;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Is the read-only page in temporary storage? }
  if rpBlockNumTmp <> ffc_W32NoValue then begin
    TffBaseTempStorage(rpFI^.fiTempStore).ReleaseBlock(rpBlockNumTmp); {!!.01}
    rpBlockNumTmp := ffc_W32NoValue;
  end;

  { Are there any modified blocks? If so, free them. This ensures they
    are removed from temporary storage. }
  while assigned(rpBlockListTail) do begin
    aBlock := rpBlockListTail;
    rpBlockListTail := rpBlockListTail.Prev;
    aBlock.Free;
  end;

  if aBlockSize <> rpBlockSize then begin
    rpFreeBlock(rpBlock, rpBlockSize);
    rpAllocBlock(aBlockSize);
  end
  else
    FillChar(rpBlock^, rpBlockSize, 'F');
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.rpSetLSN(const aLSN : TffWord32);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  rpHeader^.bchLSN := aLSN;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.SendToTempStore;
var
  aBlock : TffbmModifiedBlock;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Requirement: Must be clean & must not already be in temporary
    storage. }
  Assert(assigned(rpBlock));

  { Send the read-only block to temp storage. }
  rpBlockNumTmp := TffBaseTempStorage(rpFI^.fiTempStore).WriteBlock(rpBlock);
  rpFreeBlock(rpBlock, rpBlockSize);
  rpBlock := nil;

  { Send all modified blocks to temp storage. }
  aBlock := rpBlockListTail;
  while assigned(aBlock) do begin
    aBlock.SendToTempStore;
    aBlock := rpBlockListTail.Prev;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffbmRAMPage.rpSetFI(FI : PffFileInfo);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  FNew := False;                                                       {!!.07}
  if (FI <> rpFI) then begin
    { If the file is being set to nil, we need to clear it: it's
      about to be recycled. }
    if (FI = nil) then begin
      BlockSize := 0;
      rpFI := nil;
      rpBlockNum := ffc_W32NoValue;
      rpTrans := nil;
    end
    { If the file is being set to a real fileinfo record, set as
      much data as we can. }
    else begin
      BlockSize := FI^.fiBlockSize;
      rpFI := FI;
      rpBlockNum := ffc_W32NoValue;
      rpTrans := nil;
      if fffaTemporary in FI^.fiAttributes then
        rpReuseMode := ffrmTempStore
      else
        rpReuseMode := ffrmUseAsIs;
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}


{===TffSrTransactionLevel============================================}
constructor TffSrTransactionLevel.Create(aTrans: TffSrTransaction);
begin
  inherited Create;
  tlTransaction := aTrans;
  tlPrev := tlTransaction.trTransLevelListTail;
  tlTransaction.trTransLevelListTail := Self;
  if Assigned(tlPrev) then
    tlLevel := tlPrev.tlLevel + 1
  else
    tlLevel := 0;
end;
{--------}
destructor TffSrTransactionLevel.Destroy;
begin
  tlTransaction.trTransLevelListTail := tlPrev;
  Assert(not Assigned(tlModifiedBlocksHead));
  inherited;
end;
{====================================================================}


{===TffSrTransaction===============================================}
constructor TffSrTransaction.Create(const aDatabaseID : TffDatabaseID;
                                    const aImplicit, readOnly   : boolean);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  FDatabaseID := aDatabaseID;
  FImplicit := aImplicit;
  FJnlFile  := nil;
  FNewSpace := 0;                                                      {!!.11}
  FTransLevel := 0;
  FReadOnly := readOnly;
  FSignature := ffc_SigTransaction;
  FTransMode := tmNormal;
  FLockContainer := nil;
  StartNested;                                                         {!!.10}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffSrTransaction.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if assigned(FLockContainer) then
    FLockContainer.Free;
  EndNested;
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffSrTransaction.AdjustLSN(const Adjustment : TffWord32) : TffWord32;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Assumption: Transaction list & buffer manager data structures have
                been write-locked. }
  FLSN := FLSN - Adjustment;
  Result := FLSN;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.10}
{--------}
procedure TffSrTransaction.StartNested;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  TffSrTransactionLevel.Create(Self);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffSrTransaction.EndNested;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  Assert(Assigned(trTransLevelListTail));
  trTransLevelListTail.Free;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{End !!.10}
{--------}
function TffSrTransaction.trGetNested : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := (TransLevel.Level > 0);                                    {!!.10}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.10}
{--------}
function TffSrTransaction.trGetTransLevel : TffSrTransactionLevel;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  Assert(Assigned(trTransLevelListTail));
  Result := trTransLevelListTail;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{End !!.10}
{--------}
function TffSrTransaction.trGetTransactionID : TffTransID;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  result := TffTransID(Self);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}



{===TffBufferManager=================================================}
constructor TffBufferManager.Create(const ConfigDir : TffPath;
                                    const TempStoreSizeInMB : integer);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
//  bmCommitLSN := High(TffWord32);                                    {Deleted !!.10}
  bmConfigDir := ConfigDir;
  bmInUseListHead := nil;
  bmInUseListTail := nil;
  bmPortal := TffPadlock.Create;                                       {!!.02}
  bmMaxRAM := 10;
  bmMaxRAMDetail.iLow := bmMaxRAM;
  bmMaxRAMDetail.iHigh := 0;
  ffI64MultInt(bmMaxRAMDetail, ffcl_1MB, bmMaxRAMDetail);
  ffInitI64(bmRAMDetail);
  bmRAMUsed := 0;
  bmTempStore := ffcTempStorageClass.Create(bmConfigDir,
                                            TempStoreSizeInMB * ffcl_1MB,
                                            ffcl_64k);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.07}
{--------}
procedure TffBufferManager.bmClearRecycleList;
var
  Temp : TffbmRAMPage;
begin
  while Assigned(bmRecycleListHead) do begin
    Temp := bmRecycleListHead;
    Temp.RemoveFromRecycleList;
    Temp.Free;
  end;
end;
{End !!.07}
{--------}
destructor TffBufferManager.Destroy;
//var                                                                  {Deleted !!.07}
//  Temp : TffbmRAMPage;                                               {Deleted !!.07}
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  bmPortal.Lock;                                                       {!!.02}
  try
    { Free the pages from the recycle list. }
    bmClearRecycleList;                                                {!!.07}

    { All files must be closed before freeing the buffer manager.
      If bmInUseListHead is assigned, files are still open. }
    Assert(not Assigned(bmInUseListHead));
  finally
    bmPortal.Unlock;                                                   {!!.02}
    bmPortal.Free;                                                     {!!.02}
  end;
  bmTempStore.Free;
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function  TffBufferManager.AddBlock(aFI : PffFileInfo;
                                    aTI : PffTransInfo;
                              const aBlockNumber : TffWord32;
                                var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  Temp : TffbmRAMPage;
begin
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    { Is the block already in memory? }
    Temp := bmSearch(aFI, aBlockNumber);

    { If not in memory then bring it into memory. }
    if not Assigned(Temp) then begin
      Temp := bmGetNewRAMPage(aFI, aBlockNumber);
      { If we are in a transaction then make this block part of the
        transaction. }
      if assigned(aTI^.tirTrans) then begin
        Temp.MakeDirty(aTI^.tirTrans);
      end;
    end
    else
      { The block is in memory.  Move it to the end of the InUse list. }
      Temp.MoveToEndOfUseList;

    { Does this file need a reference to temporary storage? }
    if (fffaTemporary in aFI^.fiAttributes) and
       (aFI^.fiTempStore = nil) then
      aFI^.fiTempStore := bmTempStore;

    { Return a modifiable copy of the block. }
    Result := Temp.Block(aTI^.tirTrans, aReleaseMethod);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;{try..finally}
end;
{--------}
function  TffBufferManager.AddFile(aFI : PffFileInfo;
                                   aTI : PffTransInfo;
                             const aMarkHeaderDirty : boolean;
                               var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  Temp : TffbmRAMPage;
  Trans : TffSrTransaction;
begin
  Result := nil;                                                       {!!.13}
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    Temp := bmSearch(aFI, 0);
    if not Assigned(Temp) then begin
      Temp := bmGetNewRAMPage(aFI, 0);
      if not (fffaTemporary in aFI^.fiAttributes) then
{Begin !!.13}
        try
          bmReadBlock(aFI, ffc_W32NoValue, Temp);
        except
          Temp.RemoveFromUseList;
          Temp.RemoveFromFilePageList;
          Temp.Free;
          raise;
        end;
{End !!.13}
    end
    else begin
      if (aFI^.fiBlockSize = 0) then begin
        aFI^.fiBlockSize := Temp.BlockSize;
        aFI^.fiBlockSizeK := Temp.BlockSize div 1024;                  {!!.11}
        aFI^.fiLog2BlockSize := FFCalcLog2BlockSize(Temp.BlockSize);
      end;
    end;
    if aMarkHeaderDirty and (not Temp.DirtiedForTrans(aTI^.tirTrans)) then begin
      Trans := aTI^.tirTrans;
      Temp.MakeDirty(Trans);
    end;

    { Does this file need a reference to temporary storage? }
    if (fffaTemporary in aFI^.fiAttributes) and
       (aFI^.fiTempStore = nil) then
      aFI^.fiTempStore := bmTempStore;

    { Return a modifiable copy of the block. }
    Result := Temp.Block(aTI^.tirTrans, aReleaseMethod);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;{try..finally}
end;
{--------}
procedure TffBufferManager.BeginWrite;
begin
  bmPortal.Lock;                                                       {!!.02}
end;
{--------}
procedure TffBufferManager.bmCommitPrim(aTrans : TffSrTransaction);
var
  aPage, NextPage : TffbmRAMPage;
  CanShove : boolean;
  FirstShove : TffbmRAMPage;
  FlushList : TffFlushList;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Is the transaction nested? }                                       {!!.10}
  if aTrans.TransLevel.Level = 0 then begin                            {!!.10}
    { No. Commit to disk }                                             {!!.10}
    CanShove := true;
    FirstShove := nil;

    { Create list of files that will be needed to be flushed. }
    FlushList := TffFlushList.Create;

{Begin !!.11}
    { Verify there is enough free disk space for the new blocks. }
    aPage := aTrans.trTransPageListHead;
    if (aPage <> nil) and
       (aTrans.FNewSpace > 0) and
       (aTrans.FNewSpace >
        FFGetDiskFreeSpace(ExtractFileDir(aPage.rpFI^.fiName^))) then
        FFRaiseExceptionNoData(EffServerException,
                               ffStrResServer,
                               fferrDiskFull);
{End !!.11}

    { Loop through the pages. }
    while assigned(aPage) do begin
      NextPage := aPage.rpTransNext;
      { If we have a next page and this is page 0, 1, or 2, shove it to the
        end of the transaction page list.  We do this to reduce chances of
        corruption if disk is full. Any new data blocks are written before
        the header block. If a new data block cannot be written then
        we avoid putting a bad record count in block 0. }
      if assigned(NextPage) and
         CanShove and
         (aPage.BlockNumber < 3) then begin
        if aPage = FirstShove then begin
          CanShove := false;
          NextPage := aPage;
        end
        else begin
          aPage.MoveToEndOfTransList;
          if FirstShove = nil then
            FirstShove := aPage;
        end;
      end
      else if aPage.Commit(false) and
         (not (fffaTemporary in aPage.FileInfo^.fiAttributes)) then
        FlushList.Add(aPage.FileInfo);
      aPage := NextPage;
    end;

    { Now flush the files to which we have written. }
    FlushList.Flush(aTrans);
    FlushList.Free;
{Begin !!.10}
  end else begin
    {Yes. Only commit the blocks belonging to the current transaction level }
    while Assigned(aTrans.TransLevel.tlModifiedBlocksHead) do
      aTrans.TransLevel.tlModifiedBlocksHead.RAMPage.Commit(False);
  end;
{End !!.10}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmFailSafeCommit(aTrans : TffSrTransaction);
var
  aPage : TffbmRAMPage;
  FileName  : TffFullFileName;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Get the journal file name for the final deletion. }
  FileName := aTrans.JournalFile^.fiName^;

  { Is this the commit of a nested transaction? }
  if aTrans.TransLevel.Level = 0 then begin                            {!!.10}
    { No. Write out all before- and after-images to journal file.
      We need before-images so that the fail-safe transaction can
      be completely rolled back in the event of power failure. We
      need after-images so that the fail-safe transaction can be
      re-applied. }
    aPage := aTrans.trTransPageListHead;
     while assigned(aPage) do begin
       if (not (fffaTemporary in aPage.FileInfo^.fiAttributes)) then begin
         bmJournalRAMPage(aTrans, aPage, true);
         bmJournalRAMPage(aTrans, aPage, false);
       end;
       aPage := aPage.rpTransNext;
     end;
    { Mark the journal file as complete and close it. }
    bmWriteCompleteJnlHeader(aTrans.JournalFile);
  end;

  { Commit the pages. }
  bmCommitPrim(aTrans);

  { If we get this far all dirty data was force-flushed to disk, so
   delete the journal file (it's no longer needed)}
  if aTrans.TransLevel.Level = 0 then                                  {!!.10}
    try
      FFDeleteFile(FileName);
    except
      {do nothing}
    end;{try..except}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffBufferManager.bmFileRAMPageCount(aFI : PffFileInfo) : Longint;
var
  RAMPage : TffbmRAMPage;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := 0;
  RAMPage := aFI^.fiPageListHead;
  while assigned(RAMPage) do begin
    inc(Result);
    RAMPage := RAMPage.rpFileNext;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffBufferManager.bmGetNewRAMPage(aFI : PffFileInfo;
                                          aBlockNumber : TffWord32) : TffbmRAMPage;
var
  ReuseMode : TffbmPageReuseMode;
  Temp : TffbmRAMPage;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  {$IFDEF RAMPageCheck}
  Log('Entering TffBuffMan.bmGetNewRamPage', []);
  {$ENDIF}
  Result := nil;

  { Check the Recycle list for an available RAM page. }
  if Assigned(bmRecycleListHead) then begin
    Result := bmRecycleListHead;
    Result.RemoveFromRecycleList;
    Result.FileInfo := aFI;
    Result.BlockNumber := aBlockNumber;
  end;

  { If we don't have a recycled page and if adding the new block would push
    us over our maximum RAM limit then try to find a page in the UseList that is
    relatively old and not locked. }
  if (not Assigned(Result)) and bmOverRAMLimit(aFI^.fiBlockSize) then begin
    {$IFDEF RAMPageCheck}
    Log('Looking for reusable RAMPage', []);
    {$ENDIF}
    Temp := bmInUseListHead;
    while Assigned(Temp) do begin
      if Temp.Reusable(ReuseMode) then begin
        Result := Temp;
        Break;
      end;
      Temp := Temp.rpInUseNext;
    end;
    { Did we find a reusable page? }
    if Assigned(Result) then
      { Yes. Can we use it as is? }
      if ReuseMode = ffrmUseAsIs then begin
        { Yes. Update its properties. }
        Result.RemoveFromFilePageList;
        Result.FileInfo := aFI;
        Result.BlockNumber := aBlockNumber;
        Result.RemoveFromUseList;
      end else begin
        {$IFDEF RAMPageCheck}
        Log('Sending reusable page to temp storage.', []);
        {$ENDIF}
        { No. Send it to temporary storage. }
        Result.SendToTempStore;
        Result := nil;
      end;
  end;

  { If didn't have a page to recycle, haven't reached the maximum number of RAM
    pages, or didn't have a re-usable page then create a new RAM page. }
  if (not Assigned(Result)) then begin
    Result := TffbmRAMPage.Create(Self, aFI, aBlockNumber);
    {$IFDEF RAMPageCheck}
    Log('Creating a new RAMPage. RAM used: %d', [bmRAMDetail.ilow]);
    {$ENDIF}
  end;
  { Add it to the buffer manager's InUse list and the file's
    page list. }
  Result.AddToUseList;
  Result.AddToFilePageList;
  {$IFDEF RAMPageCheck}
  Log('Leaving TffBuffMan.bmGetNewRamPage', []);
  {$ENDIF}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffBufferManager.bmGetRAMPage(const anIndex : Longint) : TffbmRAMPage;
var
  Count : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Count := 0;
  Result := bmInUseListHead;
  while assigned(Result) and (Count < anIndex) do begin
    inc(Count);
    Result := Result.rpInUseNext;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffBufferManager.bmGetRecycledCount : Longint;
var
  RAMPage : TffbmRAMPage;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := 0;
  RAMPage := bmRecycleListHead;
  while assigned(RAMPage) do begin
    inc(Result);
    RAMPage := RAMPage.rpInUseNext;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffBufferManager.bmGetTempStoreSize : integer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := bmTempStore.Size div ffcl_1MB;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffBufferManager.bmRAMPageCount : Longint;
var
  RAMPage : TffbmRAMPage;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := 0;
  RAMPage := bmInUseListHead;
  while assigned(RAMPage) do begin
    inc(Result);
    RAMPage := RAMPage.rpInUseNext;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffBufferManager.bmSearch(aFI : PffFileInfo; aBlockNumber : TffWord32) : TffbmRAMPage;
var
  pc1 : PffPageContainer;
  pc2 : PffPageContainer;
  pc3 : PffPageContainer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { is this the header page? }
  if aBlockNumber = 0 then begin
    { yes... it was stored in a special field for faster access }
    Result := aFI^.fiPageZero;
    Exit;
  end;

  pc1 := aFI^.fiPages[TffBlockNum(aBlockNumber)[3]];
  if not Assigned(pc1) then begin
    Result := nil;
    Exit;
  end;
  pc2 := pc1.pcPages[TffBlockNum(aBlockNumber)[2]];
  if not Assigned(pc2) then begin
    Result := nil;
    Exit;
  end;
  pc3 := pc2.pcPages[TffBlockNum(aBlockNumber)[1]];
  if not Assigned(pc3) then begin
    Result := nil;
    Exit;
  end;
  Result := pc3.pcPages[TffBlockNum(aBlockNumber)[0]];
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmSetTempStoreSize(aSizeInMB : integer);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  bmTempStore.Free;
  bmTempStore := ffcTempStorageClass.Create(bmConfigDir,
                                            aSizeInMB * ffcl_1MB, ffcl_64k);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.CommitFileChanges(aFI : PffFileInfo;
                                             aTrans : TffSrTransaction);
var
  aPage : TffbmRAMPage;
  NextPage : TffbmRAMPage;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if assigned(aTrans) then begin
    aPage := aTrans.trTransPageListHead;
    while assigned(aPage) do begin
      if aPage.FileInfo = aFI then begin
         NextPage := aPage.rpTransNext;
         aPage.Commit(True);
         aPage := NextPage;
      end else
        aPage := aPage.rpTransNext;
    end;  { while }
    if not (fffaTemporary in aFI^.fiAttributes) then
      FFForceFlushFile(aFI);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.CommitTransaction(aTrans : TffSrTransaction);
begin
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    if (aTrans <> nil) then begin
      if (aTrans.TransactionMode = tmNormal) then
        bmCommitPrim(aTrans)
      else {TransactionMode = tmFailSafe}
        bmFailSafeCommit(aTrans);
      bmRemoveExcessPages;
    end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;
end;
{--------}
procedure TffBufferManager.CommitTransactionSubset(aTrans : TffSrTransaction);
begin
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    if (aTrans <> nil) then
      bmCommitPrim(aTrans);
    { We typically commit a subset during a long-running operation such as
      pack, reindex, or restructure. Remove the pages associated with this
      transaction. The advantage to this is that we don't squeeze other cursors
      out of the RAM cache. The disadvantage is that we may free up pages that
      we need as we continue the operation. }
    bmRemoveCommittedPages(aTrans);
    bmRemoveExcessPages;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;
end;
{--------}
procedure TffBufferManager.DirtyBlock(aFI : PffFileInfo;
                                      const aBlockNumber : TffWord32;
                                      aTI : PffTransInfo;
                                  var aModifiableBlock : PffBlock);
var
  aModBlockClone : PffBlock;
  aRelMethod : TffReleaseMethod;
  Temp : TffbmRAMPage;
  Trans : TffSrTransaction;
begin
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    { Is the block in memory? }
    Temp := bmSearch(aFI, aBlockNumber);
    { If it is in memory then make it part of the file's transaction. }
    if Assigned(Temp) then begin
      if not Temp.DirtiedForTrans(aTI^.tirTrans) then begin
        Trans := aTI^.tirTrans;
        Temp.MakeDirty(Trans);
      end;
      aModifiableBlock := Temp.Block(aTI^.tirTrans, aRelMethod);
      aModBlockClone := aModifiableBlock;
      aRelMethod(aModBlockClone);
      { Move the page to the end of the InUse list. }
      Temp.MoveToEndOfUseList;
    end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;{try..finally}
end;
{--------}
procedure TffBufferManager.EndWrite;
begin
  bmPortal.Unlock;                                                     {!!.02}
end;
{--------}
procedure TffBufferManager.FlushPools(const blockSizes : TffBlockSizes);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}

  { Time to do a general flush? }                                      {!!.07}
  if blockSizes = [] then begin                                        {!!.01}{!!.07}
    { Free up the recycled list. }                                     {!!.07}
    bmClearRecycleList;                                                {!!.07}
    FFFlushMemPools;                                                   {!!.01}
  end;                                                                 {!!.07}

  if (ffbs4k in blockSizes) and assigned(Pool4k) then
    Pool4k.RemoveUnusedBlocks;

  if (ffbs8k in blockSizes) and assigned(Pool8k) then
    Pool8k.RemoveUnusedBlocks;

  if (ffbs16k in blockSizes) and assigned(Pool16k) then
    Pool16k.RemoveUnusedBlocks;

  if (ffbs32k in blockSizes) and assigned(Pool32k) then
    Pool32k.RemoveUnusedBlocks;

  if (ffbs64k in blockSizes) and assigned(Pool64k) then
    Pool64k.RemoveUnusedBlocks;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function  TffBufferManager.GetBlock(aFI : PffFileInfo;
                              const aBlockNumber : TffWord32;
                                    aTI : PffTransInfo;
                              const aMarkDirty : boolean;
                                var aReleaseMethod : TffReleaseMethod) : PffBlock;
var
  Temp : TffbmRAMPage;
begin
{Begin!!.02}
//  if aMarkDirty then
  bmPortal.Lock;
//  else
//    bmPortal.BeginRead;
{End !!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    { Get the RAM page. }
     Temp := bmGetBlock(aFI, aBlockNumber);

    { If we are to mark it dirty and it has not been marked as part of the
      file's transaction then make it part of the transaction. }
    if aMarkDirty and (not Temp.DirtiedForTrans(aTI^.tirTrans)) then
      Temp.MakeDirty(aTI^.tirTrans);

    Result := Temp.Block(aTI^.tirTrans, aReleaseMethod);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
{Begin !!.02}
//    if aMarkDirty then
    bmPortal.Unlock;
//    else
//      bmPortal.EndRead;
{End !!.02}
  end;{try..finally}
end;
{--------}
function TffBufferManager.GetRAMPage(aFI : PffFileInfo;
                               const aBlockNumber : TffWord32) : TffbmRAMPage;
begin
{Begin !!.05}
  bmPortal.Lock;
  try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                  {!!.03}
    { Get the RAM page. }
    Result := bmGetBlock(aFI, aBlockNumber);
    {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}          {!!.03}
  finally
    bmPortal.Unlock;
  end;
{End !!.05}
end;
{Begin !!.06}
{--------}
function TffBufferManager.GetRAMPageLSN(aRAMPage : TffbmRAMPage) : TffWord32;
begin
  bmPortal.Lock;
  try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                 
    Result := aRAMPage.LSN;
    {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
  finally
    bmPortal.Unlock;
  end;
end;
{--------}
function TffBufferManager.GetRAMPageLSN2(aFI : PffFileInfo;
                                   const aBlockNumber : TffWord32) : TffWord32;
begin
  bmPortal.Lock;
  try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
    { Get the RAM page. }
    Result := bmGetBlock(aFI, aBlockNumber).LSN;
    {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
  finally
    bmPortal.Unlock;
  end;
end;
{End !!.06}
{--------}
function TffBufferManager.bmGetBlock(aFI : PffFileInfo;
                                     aBlockNumber : TffWord32) : TffbmRAMPage;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}

  if (fffaTemporary in aFI^.fiAttributes) and (aFI^.fiTempStore = nil) then
    aFI^.fiTempStore := bmTempStore;

  { Is the block already in memory? }
  Result := bmSearch(aFI, aBlockNumber);
  { If it is not in memory then bring it into memory. }
  if Result = nil then begin
//  if not Assigned(Result) then begin
    Result := bmGetNewRAMPage(aFI, aBlockNumber);
    if not (fffaTemporary in aFI^.fiAttributes) then
{Begin !!.13}
      try
        bmReadBlock(aFI, aBlockNumber, Result);
      except
        Result.RemoveFromUseList;
        Result.RemoveFromFilePageList;
        Result.Free;
        raise;
      end;
{End !!.13}
  end else
    { It is in memory. Move it to the end of the InUse list. }
    Result.MoveToEndOfUseList;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffBufferManager.GetRAM : integer;
begin
  Result := bmRAMUsed;
end;
{--------}
procedure TffBufferManager.HandleLSNrollover;
var
  RAMPage : TffbmRAMPage;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  RAMPage := bmInUseListHead;
  while assigned(RAMPage) do begin
    if not RAMPage.Dirty then
      RAMPage.LSN := 1;
    RAMPage := RAMPage.rpInUseNext;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmDecreaseRAMDetail(const numberBytes : Longint);
var
  tmpI64 : TffInt64;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  ffI64MinusInt(bmRAMDetail, numberBytes, bmRAMDetail);
  ffI64DivInt(bmRAMDetail, ffcl_1MB, tmpI64);
  bmRAMUsed := ffI64ToInt(tmpI64);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmIncreaseRAMDetail(const numberBytes : Longint);
var
  tmpI64 : TffInt64;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  ffI64AddInt(bmRAMDetail, numberBytes, bmRAMDetail);
  ffI64DivInt(bmRAMDetail, ffcl_1MB, tmpI64);
  bmRAMUsed := ffI64ToInt(tmpI64);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmJournalRAMPage(aTrans       : TffSrTransaction;
                                            aRAMPage     : TffbmRAMPage;
                                            aBeforeImage : boolean);
var
  aBlock : PffBlock;
  aReleaseMethod : TffReleaseMethod;
  RecHdr : TffJournalFileRecordHeader;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  FillChar(RecHdr, sizeof(RecHdr), 0);
  with RecHdr, aRAMPage do begin
    jfrhSignature := ffc_SigJnlRecHeader;
    jfrhBlockNumber := BlockNumber;
    jfrhBlockSize := BlockSize;
    jfrhBeforeImg := Longint(ord(aBeforeImage));
    StrCopy(jfrhFileName, @FileInfo^.fiName^[1]);
    FFPositionFileEOF(aTrans.JournalFile);
    FFWriteFileExact(aTrans.JournalFile, sizeof(RecHdr), RecHdr);
    if aBeforeImage then
      FFWriteFileExact(aTrans.JournalFile, BlockSize, ReadOnlyBlock^)
    else begin
      aBlock := Block(aTrans, aReleaseMethod);
      try
        FFWriteFileExact(aTrans.JournalFile, BlockSize, aBlock^);
      finally
        aReleaseMethod(aBlock);
      end;
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.05}
{--------}
procedure TffBufferManager.Lock;
begin
  bmPortal.Lock;
end;
{End !!.05}
{--------}
function TffBufferManager.bmOverRAMLimit(sizeOfNewBlock : Longint) : boolean;
var
  tmpI64 : TffInt64;
begin
  {$IFDEF RAMPageCheck}
  Log('OverRamLimit?',[]);
  Log('  NewBlockSize : %d',[SizeOfNewBlock]);
  Log('  MaxRam       : %d',[bmMaxRAMDetail.ilow]);
  Log('  Current RAM  : %d',[bmRAMDetail.ilow]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Are we already at the limit? }
  Result := (FFCmpI64(bmRAMDetail, bmMaxRAMDetail) = 0);
  { If not then see if this would push us over the limit? }
  if not Result then begin
    ffI64AddInt(bmRamDetail, sizeOfNewBlock, tmpI64);
    Result := (FFCmpI64(tmpI64, bmMaxRAMDetail) > 0);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmReadBlock(aFI          : PffFileInfo;
                                       aBlockNumber : TffWord32;
                                       aRAMPage     : TffbmRAMPage);
var
  aBlock     : PffBlock;
  aReleaseMethod : TffReleaseMethod;
  Header     : TffBlockHeaderFile;
  MaxBlocks  : TffInt64;
  TempI64    : TffInt64;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
   { Note: aBlockNumber = ffc_W32NoValue forces verification of the header, and
     for a header record, we need to calculate the block size first; header
     records are never encrypted. }
  if (aBlockNumber = ffc_W32NoValue) then begin
    TempI64.iLow := 0;
    TempI64.iHigh := 0;
    FFReadFileExactAt(aFI, TempI64, sizeof(Header), Header);
    ffVerifyFileHeaderSignature(aFI, Header.bhfSignature);
    with Header do
      if (bhfSignature <> ffc_SigHeaderBlock) or
         (bhfNextBlock <> ffc_W32NoValue) or
         (bhfThisBlock <> 0) or
         (not FFVerifyBlockSize(bhfBlockSize)) then
        FFRaiseException(EffServerException, ffStrResServer, fferrNotAnFFFile,
                         [aFI^.fiName^]);
    {$IFNDEF SecureServer}
    if (Header.bhfEncrypted = 1) then
      FFRaiseException(EffServerException, ffStrResServer, fferrEncrypted,
                       [aFI^.fiName^]);
    {$ENDIF}
    aFI^.fiBlockSize := Header.bhfBlockSize;
    aFI^.fiBlockSizeK := Header.bhfBlockSize div 1024;                 {!!.11}
    aFI^.fiLog2BlockSize := Header.bhfLog2BlockSize;
    aFI^.fiUsedBlocks := Header.bhfUsedBlocks;
    aFI^.fiEncrypted := (Header.bhfEncrypted = 1);
    aFI^.fiRecordLength := Header.bhfRecordLength;
    aFI^.fiRecLenPlusTrailer := Header.bhfRecLenPlusTrailer;
    aFI^.fiFFVersion := Header.bhfFFVersion;
{Begin !!.11}
    { Verify the table was not created with a newer version of FF. For example,
      it is okay for a 2_11 server to read a 2_06 table but it is *not* okay
      for a 2_10 server to read a 2_11 table. }
    if aFI^.fiFFVersion > ffVersionNumber then
      FFRaiseException(EffServerException, ffStrResServer, fferrTableVersion,
                       [aFI^.fiName^, aFI^.fiFFVersion / 10000.0,
                        FFVersionNumber / 10000.0]);
{End !!.11}

    { Calculate the maximum number of blocks the file may contain.
      D3 max num blocks is 2^31; 2^32 for D4 and 5. }
    ffI64DivInt(FFCalcMaxFileSize(aFI), TffWord32(aFI^.fiBlockSize), MaxBlocks);
    if (ffCmpDW(MaxBlocks.iLow,ffcl_MaxBlocks)) > 0 then
      aFI^.fiMaxBlocks := ffcl_MaxBlocks
    else
      aFI^.fiMaxBlocks := MaxBlocks.iLow;

    aFI^.fiMaxSegSize := FFCalcMaxBLOBSegSize(aFI);
    aRAMPage.BlockSize := Header.bhfBlockSize;
    aBlockNumber := 0;
  end;
  { Read the requested block in its entirety. }
  with aRAMPage do begin
    TempI64.iLow := aBlockNumber;
    TempI64.iHigh := 0;
    ffI64MultInt(TempI64, BlockSize, TempI64);
    { Read the file into the read-only slot. }
    aBlock := Block(nil, aReleaseMethod);
    try
      FFReadDecryptFileExactAt(aFI, TempI64, BlockSize, aBlock^);
    finally
      aReleaseMethod(aBlock);
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmRemoveCommittedPages(const aTran : TffSrTransaction);
var
  BlockSizes : TffBlockSizes;
  LSN : TffWord32;
  NextPage  : TffbmRAMPage;
  RAMPage   : TffbmRAMPage;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}

  { Remove pages marked by the transaction. }
  BlockSizes := [];
  LSN := aTran.LSN;
  RAMPage := aTran.trTransPageListHead;
  while assigned(RAMPage) do begin
    NextPage := RAMPage.rpTransNext;
    { Is this page part of the specified transaction? }
    if (RAMPage.LSN = LSN) and (not RAMPage.Dirty) then begin
      { Yes.  Get rid of the page. }
      Include(BlockSizes, RAMPage.rpBlockSizeEnum);
      RAMPage.RemoveFromTransList(aTran);
      RAMPage.RemoveFromFilePageList;
      RAMPage.RemoveFromUseList;
      RAMPage.Free;
    end;
    { Move to the next page. }
    RAMPage := NextPage;
  end;

  { Tell the memory pools to free up their excess blocks. }
  FlushPools(blockSizes);

  { Flush the semaphore & mutex pools. }
//  FFMutexPool.Flush;
  FFSemPool.Flush;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmRemoveExcessPages;
var
  BlockSizes : TffBlockSizes;
  ExcessRAM : integer;
  NextPage  : TffbmRAMPage;
  RAMPage   : TffbmRAMPage;
  RemoveMode : TffbmPageReuseMode;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}

  { Goal: Make sure the RAM allocated to pages is at or below the MaxRAM
          property. }
  BlockSizes := [];

  { Are we using more RAM than allowed? }
  ExcessRAM := bmRAMUsed - bmMaxRAM;
  if (ExcessRAM > 0) then begin
    { Yes.  See if we can remove any from the recycle list. }
    while assigned(bmRecycleListHead) and (ExcessRAM > 0) do begin
      RAMPage := bmRecycleListHead;
      RAMPage.RemoveFromRecycleList;
      RAMPage.Free;
      ExcessRAM := bmRAMUsed - bmMaxRAM;
    end;

    { Are we still over the limit? }
    if (ExcessRAM > 0) then begin
      { Yes.  See if some InUse pages can be removed. }
      RAMPage := bmInUseListHead;
      while assigned(RAMPage) and (ExcessRAM > 0) do begin
        NextPage := RAMPage.rpInUseNext;
        { Can this page be removed? }
        if RAMPage.Removable(RemoveMode) then begin
          { Yes. Is it to be sent to temporary storage? }
          if RemoveMode = ffrmTempStore then
            { Yes. Do so. }
            RAMPage.SendToTempStore
          else begin
            { No. We can just free it. }
            Include(BlockSizes, RAMPage.rpBlockSizeEnum);
            RAMPage.RemoveFromFilePageList;
            RAMPage.RemoveFromUseList;
            RAMPage.Free;
          end;
          ExcessRAM := bmRAMUsed - bmMaxRAM;
        end;

        { Move to the next page. }
        RAMPage := NextPage;
      end;
    end;

    { We have eliminated some RAM pages.  Tell the memory pools to free up
      their excess blocks. }
    FlushPools(BlockSizes);

    { Flush the semaphore & mutex pools. }
//    FFMutexPool.Flush;
    FFSemPool.Flush;

  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.RemoveFile(aFI : PffFileInfo);
var
  BlockSizes : TffBlockSizes;
  Temp       : TffbmRAMPage;
  Temp2      : TffbmRAMPage;
  t1, t2     : PffPageContainer;
begin
  BlockSizes := [];
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    { Move all RAM pages from the file's page list to the buffer manager's
      Recycle list. }
    Temp := aFI^.fiPageListHead;
    while Assigned(Temp) do
    begin
      Temp2 := Temp.rpFileNext;
      Temp.rpFilePrev := nil;
      Temp.rpFileNext := nil;
      bmRemovePageFromTransaction(Temp);
      Temp.FileInfo := nil;
      Temp.MoveToRecycleList;
      Temp := Temp2;
    end;
    aFI^.fiPageListHead := nil;
    aFI^.fiPageListTail:= nil;

    { Free all of the file's page containers. }
    t1 := aFI^.fiPageContainerList;
    while Assigned(t1) do
    begin
      t2 := t1^.pcNext;
      FFFreeMem(t1, sizeOf(TffPageContainer));
      t1 := t2;
    end;

    FillChar(aFI^.fiPages, SizeOf(aFI^.fiPages), 0);

    Include(BlockSizes, FFMapBlockSize(aFI^.fiBlockSize));
    FlushPools(BlockSizes);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;{try..finally}
end;
{--------}
procedure TffBufferManager.RollbackTransaction(aTrans : TffSrTransaction);
var
  aPage, NextPage : TffbmRAMPage;
  FileName : TffFullFileName;
begin
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    if (aTrans = nil) then                                             {Moved !!.11}
      Exit;                                                            {Moved !!.11}
    { Is the transaction nested? }                                     {!!.10}
    if aTrans.TransLevel.Level = 0 then begin                          {!!.10}
      { No. Rollback all pages in the transaction. }                   {!!.10}

      { For fail safe mode, close and delete the journal file. }
      if //(not aTrans.Nested) and                                     {Deleted !!.10}
         (aTrans.TransactionMode = tmFailSafe) then begin
        try
          FileName := aTrans.JournalFile^.fiName^;
          FFCloseFile(aTrans.JournalFile);
        except
          {do nothing}
        end;{try..except}
        try
          FFDeleteFile(FileName);
        except
          {do nothing}
        end;{try..except}
      end;

      { Rollback all pages involved in the transaction. }
      aPage := aTrans.trTransPageListHead;
      while Assigned(aPage) do begin
        NextPage := aPage.rpTransNext;
        aPage.Rollback;
        aPage := NextPage;
      end;
      bmRemoveExcessPages;
{Begin !!.10}
    end else begin
      {Yes. Only commit the blocks belonging to the current transaction level }
      while Assigned(aTrans.TransLevel.tlModifiedBlocksHead) do
        aTrans.TransLevel.tlModifiedBlocksHead.RAMPage.Rollback;
    end;
{End !!.10}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;{try..finally}
end;
{--------}
procedure TffBufferManager.RollbackTransactionSubset(aTrans : TffSrTransaction);
var
  aPage, NextPage : TffbmRAMPage;
begin
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    if (aTrans <> nil) then begin
      aPage := aTrans.trTransPageListHead;
      while assigned(aPage) do begin
        NextPage := aPage.rpTransNext;
        aPage.Rollback;
        aPage := NextPage;
      end;
     bmRemoveExcessPages;
    end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;
end;
{--------}
procedure TffBufferManager.SetMaxRAM(aNumber : Longint);
begin
  bmPortal.Lock;                                                       {!!.02}
  try
    if (aNumber <> MaxRAM) then begin
      bmMaxRAM := aNumber;
      ffIntToI64(aNumber, bmMaxRAMDetail);
      ffI64MultInt(bmMaxRAMDetail, ffcl_1MB, bmMaxRAMDetail);
    end;
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;
end;
{--------}
procedure TffBufferManager.StartTransaction(aTrans    : TffSrTransaction;
                                      const aFailSafe : Boolean;
                                      const aFileName : TffFullFileName);
var
  JnlFile : PffFileInfo;
begin
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    try
      if aFailSafe then begin
        aTrans.JournalFile := FFAllocFileInfo(aFileName, ffc_ExtForTrans, nil);
        FFOpenFile(aTrans.JournalFile, omReadWrite, smExclusive, True, True);
        bmWriteIncompleteJnlHeader(aTrans.JournalFile);
        aTrans.TransactionMode := tmFailSafe;
      end
      else
        aTrans.TransactionMode := tmNormal;
    except
      if (aTrans.JournalFile <> nil) then begin
        JnlFile := aTrans.JournalFile;
        if FFFileIsOpen(JnlFile) then
          FFCloseFile(JnlFile);
        FFFreeFileInfo(JnlFile);
      end;
      raise;
    end;{try..except}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;{try..finally}
end;
{Begin !!.05}
{--------}
procedure TffBufferManager.Unlock;
begin
  bmPortal.Unlock;
end;
{End !!.05}
{--------}
procedure TffBufferManager.UnlockBlock(aFI          : PffFileInfo;
                                       aBlockNumber : TffWord32);
var
  Temp : TffbmRAMPage;
begin
  bmPortal.Lock;                                                       {!!.02}
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
    Temp := bmSearch(aFI, aBlockNumber);
    if Assigned(Temp) then begin
      Temp.RemoveFromFilePageList;
      bmRemovePageFromTransaction(Temp);
      Temp.FileInfo := nil;
      Temp.MoveToRecycleList;
    end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
  finally
    bmPortal.Unlock;                                                   {!!.02}
  end;{try..finally}
end;
{--------}
procedure TffBufferManager.bmWriteCompleteJnlHeader(aJnlFile : PffFileInfo);
var
  Hdr     : TffJournalFileHeader;
  TempI64 : TffInt64;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Hdr.jfhSignature := ffc_SigJnlHeader;
  Hdr.jfhState := 1;
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  FFWriteFileExactAt(aJnlFile, TempI64, sizeof(Hdr), Hdr);
  FFCloseFile(aJnlFile);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmWriteIncompleteJnlHeader(aJnlFile : PffFileInfo);
var
  Hdr     : TffJournalFileHeader;
  TempI64 : TffInt64;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Hdr.jfhSignature := ffc_SigJnlHeader;
  Hdr.jfhState := 0;
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  FFWriteFileExactAt(aJnlFile, TempI64, sizeof(Hdr), Hdr);
  FFFlushFile(aJnlFile);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffBufferManager.bmRemovePageFromTransaction(aPage: TffbmRAMPage);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  with aPage do begin
    if not rpDirty then
      Exit;
    if not Assigned(rpTrans) then
      Exit;
    aPage.RemoveFromTransList(rpTrans);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{=====================================================================}

{Begin !!.11}
{===TffBaseBLOBResourceMgr============================================}
class function TffBaseBLOBResourceMgr.GetMgr(aFI : PffFileInfo) : TffBaseBLOBResourceMgr;
begin
  if aFI.fiFFVersion <= ffVersion2_10 then
    Result := Tff210BLOBResourceMgr.Create
  else
    Result := TffBLOBResourceMgr.Create;
end;
{--------}
constructor TffBaseBLOBResourceMgr.Create;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  inherited Create;
  brmPadlock := TffPadlock.Create;
  brmSegMgrLoaded := false;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
destructor TffBaseBLOBResourceMgr.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  brmPadLock.Free;
  brmSegmentMgr.Free;
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffBaseBLOBResourceMgr.Commit;
begin
  if brmSegmentMgr <> nil then
    brmSegmentMgr.Commit;
end;
{--------}
procedure TffBaseBLOBResourceMgr.DeleteSegment(aFI        : PffFileInfo;
                                               aTI        : PffTransInfo;
                                         const aSegOffset : TffInt64);
begin
  {segment manager must be loaded before deleting a segment}
  if not brmSegMgrLoaded then
    brmLoadSegMgr(aFI, aTI);
  brmPadLock.Lock;
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
    brmSegmentMgr.DeleteSegment(aFI, aTI, aSegOffset);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
  finally
    brmPadLock.Unlock;
  end;
end;
{--------}
procedure TffBaseBLOBResourceMgr.brmLoadSegMgr(aFI : PffFileInfo;
                                               aTI : PffTransInfo);
begin
  brmPadlock.Lock;
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
    if not brmSegMgrLoaded then begin
      brmSegmentMgr := brmGetSegMgrClass.Create(aFI, aTI);
      brmSegMgrLoaded := True;
    end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
  finally
    brmPadlock.Unlock;
  end;
end;
{--------}
procedure TffBaseBLOBResourceMgr.ListFreeSpace(aFI : PffFileInfo;
                                               aTI : PffTransInfo;
                                         const aInMemory : Boolean;
                                               aStream : TStream);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  if not brmSegMgrLoaded then
    brmLoadSegMgr(aFI, aTI);
  brmSegmentMgr.ListFreeSpace(aFI, aTI, aInMemory, aStream);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffBaseBLOBResourceMgr.Rollback;
begin
  if brmSegmentMgr <> nil then
    brmSegmentMgr.Rollback;
end;
{=====================================================================}

{===TffBLOBResourceMgr================================================}
function TffBLOBResourceMgr.brmGetSegMgrClass : TffBLOBSegmentMgrClass;
begin
  Result := TffBLOBSegmentMgr;
end;
{--------}
function  TffBLOBResourceMgr.NewSegment(aFI             : PffFileInfo;
                                        aTI             : PffTransInfo;
                                    var aSizeNeeded     : TffWord32;
                                  const aMinSizeAllowed : TffWord32)
                                                        : TffInt64;
var
  NewSize,
  NewMinSize : Longint;
begin
  { Segment manager must be loaded before getting a new segment. }
  if not brmSegMgrLoaded then
    brmLoadSegMgr(aFI, aTI);
  brmPadLock.Lock;
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
    {calculate new size based on size of BLOB increment}
    Assert(aSizeNeeded <= aFI^.fiMaxSegSize,
           'Requesting too large segment.');
    NewSize := (((aSizeNeeded + pred(ffc_BLOBSegmentIncrement)) div
                ffc_BLOBSegmentIncrement) * ffc_BLOBSegmentIncrement);
    NewMinSize := (((aMinSizeAllowed + pred(ffc_BLOBSegmentIncrement)) div
                  ffc_BLOBSegmentIncrement) * ffc_BLOBSegmentIncrement);
    if NewMinSize > NewSize then
      NewMinSize := NewSize;
    {look for segment in deleted chain 1st}
    Result := brmSegmentMgr.GetRecycledSeg(aFI, aTI, NewSize, NewMinSize);
    {if aSize segment not available, create a new segment}
    if Result.iLow  = ffc_W32NoValue then
      Result := brmSegmentMgr.GetNewSeg(aFI, aTI, NewSize);
    { Set the final size allocated in the aSizeNeeded parameter. }
    aSizeNeeded := NewSize;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
  finally
    brmPadLock.Unlock;
  end;
end;
{=====================================================================}

{===Tff210BLOBResourceMgr=============================================}
function Tff210BLOBResourceMgr.brmGetSegMgrClass : TffBLOBSegmentMgrClass;
begin
  Result := Tff210BLOBSegmentMgr;
end;
{--------}
function  Tff210BLOBResourceMgr.NewSegment(aFI             : PffFileInfo;
                                           aTI             : PffTransInfo;
                                       var aSizeNeeded     : TffWord32;
                                     const aMinSizeAllowed : TffWord32)
                                           : TffInt64;
var
  NewSize,
  MinSize : Longint;
begin
  { Segment manager must be loaded before getting a new segment. }
  if not brmSegMgrLoaded then
    brmLoadSegMgr(aFI, aTI);
  brmPadLock.Lock;
  try
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
    { Calculate new size based on size of BLOB increment. }
    NewSize := (((aSizeNeeded + pred(ffc_BLOBSegmentIncrement)) div
                ffc_BLOBSegmentIncrement) * ffc_BLOBSegmentIncrement);
    MinSize := NewSize;
    { First, look for segment in deleted chain . }
    Result := brmSegmentMgr.GetRecycledSeg(aFI, aTI, NewSize, MinSize);
    { If aSize segment not available, create a new segment. }
    if Result.iLow  = ffc_W32NoValue then
      Result := brmSegmentMgr.GetNewSeg(aFI, aTI, NewSize);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}          
  finally
    brmPadLock.Unlock;
  end;
end;
{=====================================================================}

{===TffBaseBLOBSegmentMgr=============================================}
constructor TffBaseBLOBSegmentMgr.Create(aFI : PffFileInfo;
                                         aTI : PffTransInfo);
var
  aFHRelMethod  : TffReleaseMethod;
  aSegRelMethod : TffReleaseMethod;
  FileHeader    : PffBlockHeaderFile;
  SegmentOfs    : TffInt64;
  SegmentBlk    : PffBlock;
  SegmentPtr    : PffBLOBSegmentHeaderDel;
  OffsetInBlock : TffWord32;                                         
  ListItem      : TffBLOBSegListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  inherited Create;
  { Fill bsmDelChain with segments. }
  bsmDelChain := TffList.Create;
  bsmTranListHead := nil;
  bsmUseTranList := not (fffaBLOBChainSafe in aFI.fiAttributes);
  { We need the file header to get the deleted segment head. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
                                                aTI,
                                                0,
                                                ffc_ReadOnly,
                                                aFHRelMethod));
  try
    if (FileHeader^.bhfDelBLOBHead.iLow <> ffc_W32NoValue) then begin
      SegmentOfs := FileHeader^.bhfDelBLOBHead;
      bsmDelChain.Sorted := True;

      while (SegmentOfs.iLow <> ffc_W32NoValue) do begin
        SegmentBlk := ReadVfyBlobBlock(aFI,
                                       aTI,
                                       ffc_ReadOnly,
                                       SegmentOfs,
                                       OffsetInBlock,
                                       aSegRelMethod);
        try
          SegmentPtr := @SegmentBlk^[OffsetInBlock];

          { Create a list item for the segment and insert it to the list. }
          ListItem := TffBLOBSegListItem.Create;
          ListItem.Offset := SegmentOfs;
          ListItem.Size := SegmentPtr^.bshSegmentLen;
          bsmDelChain.Insert(ListItem);
          { Get the next segment. }
          SegmentOfs := SegmentPtr^.bshNextSegment;
        finally
          aSegRelMethod(SegmentBlk);
        end;                                                           
      end;
    end; {if}
  finally
    aFHRelMethod(PffBlock(FileHeader));
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
destructor TffBaseBLOBSegmentMgr.Destroy;
var
  aSegItem, aTmpSegItem : TffBLOBSegListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  bsmDelChain.Free;
  { Clear out any remaining items from the transaction list. As of this writing,
    SQL cursors will build up a bunch of stuff within this list & not commit
    it. }
  aSegItem := bsmTranListHead;
  while aSegItem <> nil do begin
    aTmpSegItem := aSegItem.FTranNextItem;
    aSegItem.Free;
    aSegItem := aTmpSegItem;
  end;
  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}         
end;
{--------}
procedure TffBaseBLOBSegmentMgr.bsmAddToTranList(aSegItem : TffBLOBSegListItem;
                                                 anAction : TffBLOBSegAction);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  { Items are always added at the head of the list. }
  aSegItem.FTranNextItem := bsmTranListHead;
  bsmTranListHead := aSegItem;
  aSegItem.FPendingAction := anAction;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffBaseBLOBSegmentMgr.bsmRemoveFromTranList(aSegItem : TffBlobSegListItem);
var
  PrevItem : TffBLOBSegListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}

  PrevItem := bsmTranListHead;

  if (PrevItem = aSegItem) then
    bsmTranListHead := aSegItem.FTranNextItem
  else begin
   { Find the previous segment. }
    while (PrevItem.FTranNextItem <> aSegItem) do
      PrevItem := PrevItem.FTranNextItem;

    { Remove the item from the list. }
    PrevItem.FTranNextItem := aSegItem.FTranNextItem;
  end;

  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffBaseBLOBSegmentMgr.Commit;
var
  CurItem, TmpItem : TffBLOBSegListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  CurItem := bsmTranListHead;
  while CurItem <> nil do begin
    TmpItem := CurItem;
    CurItem := TmpItem.FTranNextItem;
    case TmpItem.FPendingAction of
      bsaAddToList :
        begin
          { Reset item's transaction info & add it to the in-memory
            deleted chain. }
          TmpItem.FPendingAction := bsaNone;
          TmpItem.FTranNextItem := nil;
          bsmDelChain.Insert(TmpItem);
        end;
      bsaDeleteFromList :
        { Item is already removed from list so free the item. }
        TmpItem.Free;
    end;  { case }
  end;
  bsmTranListHead := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffBaseBLOBSegmentMgr.DeleteSegment(aFI        : PffFileInfo;
                                              aTI        : PffTransInfo;
                                        const aSegOffset : TffInt64);
var
  aBLOBRelMethod : TffReleaseMethod;
  aFHRelMethod   : TffReleaseMethod;
  FileHeader     : PffBlockHeaderFile;
  OffsetInBlock  : TffWord32;
  BLOBBlock      : PffBlock;
//  BLOBHeader     : PffBlockHeaderBLOB;                               {Deleted !!.13}
  DelSegPtr      : PffBLOBSegmentHeader;
  BufferItem     : TffBLOBSegListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}

  { Get the file header. }
  FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
                                                aTI,
                                                0,
                                                ffc_MarkDirty,
                                                aFHRelMethod));

  try

    { Grab the segment to be deleted. }
    BlOBBlock := ReadVfyBlobBlock(aFI,
                                  aTI,
                                  ffc_MarkDirty,
                                  aSegOffset,
                                  OffsetInBlock,
                                  aBLOBRelMethod);
    DelSegPtr := @BLOBBlock^[OffsetInBlock];

    { Zero out the segment & mark it as deleted. }
    FillChar(BLOBBlock^[OffsetInBlock + sizeof(TffBLOBSegmentHeaderDel)],
             DelSegPtr^.bshSegmentLen - sizeof(TffBLOBSegmentHeaderDel),
             0);                                                       {!!.13}
    PffBLOBSegmentHeaderDel(DelSegPtr)^.bshSignature := ffc_SigBLOBSegDeleted;

    { Create our list item representing the deleted segment. }
    BufferItem := TffBLOBSegListItem.Create;
    BufferItem.Offset := aSegOffset;
    BufferItem.Size := DelSegPtr^.bshSegmentLen;

    { Assumption: Deleted list is already in memory and contains the entire
      list of deleted BLOB segments. }
    { Is there anything in the deleted list? }
    if (FileHeader^.bhfDelBLOBTail.iLow <> ffc_W32NoValue) then begin

      { Update the segments in the file. }
      bsmAddToDeletedSegChain(aFI,
                              aTI,
                              FileHeader,
                              BufferItem,
                              PffBLOBSegmentHeaderDel(DelSegPtr));

    end else begin
      { Nothing deleted yet.  Make this the first item in the chain. }
      with FileHeader^ do begin
        bhfDelBLOBHead := aSegOffset;
        bhfDelBLOBTail := aSegOffset;
        PffBLOBSegmentHeaderDel(DelSegPtr)^.bshPrevSegment.iLow := ffc_W32NoValue;
        PffBLOBSegmentHeaderDel(DelSegPtr)^.bshPrevSegment.iHigh := ffc_W32NoValue;
        PffBLOBSegmentHeaderDel(DelSegPtr)^.bshNextSegment.iLow := ffc_W32NoValue;
        PffBLOBSegmentHeaderDel(DelSegPtr)^.bshNextSegment.iHigh := ffc_W32NoValue;
      end;
    end;

    { Add the item to the list. }
  if bsmUseTranList then
    bsmAddToTranList(BufferItem, bsaAddToList)
  else
    bsmDelChain.Insert(BufferItem);

    { Decrement the used segment count in the BLOB block. }
//    BLOBHeader := PffBlockHeaderBLOB(BLOBBlock);                            {Deleted !!.13}
//    BLOBHeader^.bhbAssignedSegCount := BLOBHeader^.bhbAssignedSegCount - 1; {Deleted !!.13}
  finally
    aBLOBRelMethod(BLOBBlock);
    aFHRelMethod(PffBlock(FileHeader));
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}          
end;
{--------}
procedure TffBaseBLOBSegmentMgr.bsmAddToDeletedSegChain(aFI         : PffFileInfo;
                                                        aTI         : PffTransInfo;
                                                        aFileHeader : PffBlockHeaderFile;
                                                        aDelSeg     : TffBLOBSegListItem;
                                                        aSegment    : PffBLOBSegmentHeaderDel);
var
  PrevSegment  : PffBLOBSegmentHeaderDel;
  BLOBBlock    : PffBlock;
  OffsetInBlock: TffWord32;
  aRelMethod   : TffReleaseMethod;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  { Assumptions: Deleted list contains at least one segment.
      Segments are sorted by size when first read from disk so it is not
      necessary to maintain sort order on disk. }
  { Get the last segment in the chain. }
  BLOBBlock := ReadVfyBlobBlock(aFI,
                                aTI,
                                ffc_MarkDirty,
                                aFileHeader^.bhfDelBLOBTail,
                                OffsetInBlock,
                                aRelMethod);
  PrevSegment := @BLOBBlock^[OffsetInBlock];

  { Point the last segment to the new deleted segment & vice versa. }
  PrevSegment^.bshNextSegment := aDelSeg.Offset;
  aSegment^.bshNextSegment.iLow := ffc_W32NoValue;
  aSegment^.bshNextSegment.iHigh := ffc_W32NoValue;
  aSegment^.bshPrevSegment := aFileHeader^.bhfDelBLOBTail;
  aRelMethod(BLOBBlock);

  { Mark the new deleted segment as the end of the chain. }
  aFileHeader^.bhfDelBLOBTail := aDelSeg.Offset;

  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
function  TffBaseBLOBSegmentMgr.GetNewSeg(aFI   : PffFileInfo;
                                          aTI   : PffTransInfo;
                                    const aSize : TffWord32) : TffInt64;
var
  BLOBBlock    : PffBlock;
  DelSegHeader : PffBLOBSegmentHeaderDel;
  TempI64      : TffInt64;
  NewSegHeader : PffBLOBSegmentHeader;
  aRelMethod   : TffReleaseMethod;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                 
  { Create a new BLOB block. }
  BLOBBlock := FFTblHlpGetNewBlock(aFI, aTI, aRelMethod);
  try
    PffBlockHeaderBLOB(BLOBBlock)^.bhbSignature := ffc_SigBLOBBlock;
    PffBlockHeaderBLOB(BLOBBlock)^.bhbNextBlock := ffc_W32NoValue;
    PffBlockHeaderBLOB(BLOBBlock)^.bhbLSN := 0;

    { Make a new aSize segment in the block and return its file offset. }
    TempI64.iLow := PffBlockHeaderBLOB(BLOBBlock)^.bhbThisBlock;
    TempI64.iHigh := 0;
    { Set TempI64 to file offset of new segment. }
    ffI64MultInt(TempI64, aFI^.fiBlockSize, TempI64);
    ffI64AddInt(TempI64, sizeof(TffBlockHeaderBLOB), Result);
    NewSegHeader := PffBLOBSegmentHeader(@BLOBBlock^[sizeof(TffBlockHeaderBLOB)]);
    NewSegHeader^.bshSegmentLen := aSize;

    { If there is left over space, make deleted segment and put in deleted
      chain. We must create the deleted seg header now and store it, else we
      won't know how big it is. }
    if aSize < aFI^.fiMaxSegSize then begin
//      PffBlockHeaderBLOB(BLOBBlock)^.bhbAssignedSegCount := 2;        {Deleted !!.13}
      DelSegHeader := PffBLOBSegmentHeaderDel(@BLOBBlock^[ffc_BlockHeaderSizeBLOB + aSize]);
      DelSegHeader^.bshSegmentLen := aFI^.fiMaxSegSize - aSize;
      { Set TempI64 to file offset of deleted segment and add it to deleted
        chain. }
      ffI64AddInt(Result, aSize, TempI64);
      DeleteSegment(aFI, aTI, TempI64);
    end else
    {block only has 1 segment if the new segment was max seg size}
//    PffBlockHeaderBLOB(BLOBBlock)^.bhbAssignedSegCount := 1;         {Deleted !!.13}
  finally
    aRelMethod(BLOBBlock);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffBaseBLOBSegmentMgr.bsmSliceSegment(aFI         : PffFileInfo;
                                                aTI         : PffTransInfo;
                                                aSegOfs     : TffInt64;
                                                aSegSize    : TffWord32;
                                          const aNewSize    : TffWord32;
                                                aInDelChain : Boolean);  
var
  BLOBBlock     : PffBlock;
  BlockNum      : TffWord32;
  DelSegHeader  : PffBLOBSegmentHeaderDel;
  OffsetInBlock : TffWord32;
  TempI64       : TffInt64;
  TempI64b      : TffInt64;
  ThisSeg       : PffBLOBSegmentHeaderDel;
  aRelMethod    : TffReleaseMethod;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  { Post condition: New segment of aSize is always at aSegOfs. }

  { Remove the segment we're slicing from the deleted chain. }
  if (aInDelChain) then
    bsmRemoveFromDeletedChain(aFI, aTI, aSegOfs);

  { Get the segment to be sliced. }
  BLOBBlock := ReadVfyBlobBlock(aFI,
                                aTI,
                                ffc_MarkDirty,
                                aSegOfs,
                                OffsetInBlock,
                                aRelMethod);
  try
    ThisSeg := @BLOBBlock^[OffsetInBlock];

    {increase this blobs used segment count by 2 to keep the count correct -
      one of them will be removed when the unused portion slice segment
      is returned to the deleted segment chain}
//    Inc(PffBlockHeaderBLOB(BLOBBlock)^.bhbAssignedSegCount, 2);      {Deleted !!.13}

    { Set the segment's new size. }
    ThisSeg^.bshSegmentLen := aNewSize;

    { Get the offset for the remainder of the segment that will become a deleted
      segment. }
    ffI64AddInt(aSegOfs, aNewSize, TempI64);
    BlockNum := FFGetBlockNum(aFI, TempI64);
    ffI64MinusInt(TempI64, (BlockNum shl aFI^.fiLog2BlockSize), TempI64);
    DelSegHeader := @BLOBBlock^[TempI64.iLow];

    { Initialize the deleted segment. }
    DelSegHeader^.bshSegmentLen := (aSegSize - aNewSize);
    DelSegHeader^.bshPrevSegment.iLow := ffc_W32NoValue;
    DelSegHeader^.bshPrevSegment.iHigh := ffc_W32NoValue;
    DelSegHeader^.bshNextSegment.iLow := ffc_W32NoValue;
    DelSegHeader^.bshNextSegment.iHigh := ffc_W32NoValue;

    { Put the new unused segment back in the chain. }
    TempI64b.iLow := BlockNum;
    TempI64b.iHigh := 0;
    ffShiftI64L(TempI64b, aFI^.fiLog2BlockSize, TempI64b);
    ffI64AddInt(TempI64b, TempI64.iLow, TempI64);
    DeleteSegment(aFI, aTI, TempI64);                           
  finally
    aRelMethod(BLOBBlock);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffBaseBLOBSegmentMgr.bsmRemoveFromDeletedChain(aFI     : PffFileInfo;
                                                          aTI     : PffTransInfo;
                                                          aSegOfs : TffInt64);
var
  aFileHeader : PffBlockHeaderFile;
  OffsetInBlock    : TffWord32;                                      
  ThisSegBlock     : PffBlock;
  ThisSeg          : PffBLOBSegmentHeaderDel;
  PrevSegBlock     : PffBlock;
  PrevSeg          : PffBLOBSegmentHeaderDel;
  NextSegBlock     : PffBlock;
  NextSeg          : PffBLOBSegmentHeaderDel;
  aFHRelMethod,
  aSegRelMethod,
  aSegRelMethod2   : TffReleaseMethod;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    
  { Assumptions: This segment has already been removed from the in-memory{
                 deleted list. }

  { First get the file header, block 0. }
  aFileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, ffc_MarkDirty,
                                                 aFHRelMethod));
  try
    { Get the block. }
    ThisSegBlock := ReadVfyBlobBlock(aFI,
                                     aTI,
                                     ffc_MarkDirty,
                                     aSegOfs,
                                     OffsetInBlock,
                                     aSegRelMethod);
    try
      ThisSeg := @ThisSegBlock^[OffsetInBlock];

      { Is there a segment before this segment? }
      if ThisSeg^.bshPrevSegment.iLow <> ffc_W32NoValue then begin
        { Yes.  Point the prior segment to the next segment. }
        PrevSegBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                         ThisSeg^.bshPrevSegment, OffsetInBlock,
                                         aSegRelMethod2);
        PrevSeg := @PrevSegBlock^[OffsetInBlock];
        PrevSeg^.bshNextSegment := ThisSeg^.bshNextSegment;

        { If the removed segment was the tail then update the tail on the
          file header. }
        if PrevSeg^.bshNextSegment.iLow = ffc_W32NoValue then
          aFileHeader^.bhfDelBLOBTail := ThisSeg^.bshPrevSegment;

        aSegRelMethod2(PrevSegBlock);

      end else
        { No. This segment was the head.  Update the head on the file header. }
        aFileHeader^.bhfDelBLOBHead := ThisSeg^.bshNextSegment;

      { Is there a segment after this segment? }
      if ThisSeg^.bshNextSegment.iLow <> ffc_W32NoValue then begin
        { Yes.  Point the next segment back to the prior segment. }
        NextSegBlock := ReadVfyBlobBlock(aFI, aTI, ffc_MarkDirty,
                                         ThisSeg^.bshNextSegment, OffsetInBlock,
                                         aSegRelMethod2);
        NextSeg := @NextSegBlock^[OffsetInBlock];
        NextSeg^.bshPrevSegment := ThisSeg^.bshPrevSegment;

        { If the removed segment was the head of the chain then update the head
          in the file header. }
        if NextSeg^.bshPrevSegment.iLow = ffc_W32NoValue then
          aFileHeader^.bhfDelBLOBHead := ThisSeg^.bshNextSegment;

        aSegRelMethod2(NextSegBlock);

      end else
        { No.  This was the tail segment.  Update the tail in the file header. }
        aFileHeader^.bhfDelBLOBTail := ThisSeg^.bshPrevSegment;
    finally
      aSegRelMethod(ThisSegBlock);
    end;
  finally
    aFHRelMethod(PffBlock(aFileHeader));
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure WriteToStream(const aMsg : string; aStream : TStream);
begin
  aStream.Write(aMsg[1], Length(aMsg));
end;
{--------}
procedure TffBaseBLOBSegmentMgr.ListFreeSpace(aFI       : PffFileInfo;
                                              aTI       : PffTransInfo;
                                        const aInMemory : Boolean;
                                              aStream   : TStream);
var
  aRelMethod,
  aFHRelMethod   : TffReleaseMethod;
  anInx          : Longint;
  aSegItem       : TffBLOBSegListItem;
  aSegment       : TffInt64;
  aStr           : string;
  BLOBBlock      : PffBlock;
  DelSegment     : PffBLOBSegmentHeaderDel;
  FileHeader     : PffBlockHeaderFile;
  OffsetInBlock  : TffWord32;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                   

  { Write the segment manager's in-memory list or the list as saved to the
    file? }
  if aInMemory then begin
    { In-memory list. }
    WriteToStream('In-memory deleted chain:' + #13#10, aStream);
    for anInx := 0 to Pred(bsmDelChain.Count) do begin
      aSegItem := TffBLOBSegListItem(bsmDelChain[anInx]);
      aStr := IntToStr(anInx) + ': Size ' + IntToStr(aSegItem.Size) +
              ', Offset ' + IntToStr(aSegItem.Offset.iHigh) +
              ':' + IntToStr(aSegItem.Offset.iLow);
      case aSegItem.FPendingAction of
        bsaAddToList : aStr := aStr + ', add';
        bsaDeleteFromList : aStr := aStr + ', del';
      end;  { case }
      aStr := aStr + #13#10;
      WriteToStream(aStr, aStream);
    end;

    if bsmTranListHead <> nil then begin
      WriteToStream('Transaction list:' + #13#10, aStream);
      aSegItem := bsmTranListHead;
      anInx := 0;
      while aSegItem <> nil do begin
        aStr := Format('%d : Size %d, Offset %d:%d',
                       [anInx, aSegItem.Size, aSegItem.Offset.iHigh,
                        aSegItem.Offset.iLow]);
        aStr := aStr + ', Pending: ';
        case aSegItem.FPendingAction of
          bsaNone : aStr := aStr + 'N/A';
          bsaAddToList : aStr := aStr + 'add';
          bsaDeleteFromList : aStr := aStr + 'del';
        end;  { case }
        aSegItem := aSegItem.FTranNextItem;
        inc(anInx);
        aStr := aStr + #13#10;
        WriteToStream(aStr, aStream);
      end;
    end
    else begin
      WriteToStream(#13#10 + 'Transaction list: EMPTY', aStream);
    end;
  end
  else begin
    { The list as saved to file. Need to walk through the BLOB deleted chain. }
    { Get the file header. }
    FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
                                                  aTI,
                                                  0,
                                                  ffc_ReadOnly,      
                                                  aFHRelMethod));
    try
      { BLOB deleted chain is empty? }
      if FileHeader^.bhfDelBLOBHead.iLow = ffc_W32NoValue then begin
        { Yes. Write blurb & exit. }
        WriteToStream('BLOB deleted chain is empty.', aStream);
        WriteToStream(#0, aStream);
        Exit;
      end;

      { Not empty. Walk through the chain. }
      anInx := 0;
      aSegment := FileHeader^.bhfDelBLOBHead;
      while (aSegment.iLow <> ffc_W32NoValue) do begin
        { Get the block containing the segment. }
        BLOBBlock := ReadVfyBlobBlock(aFI,
                                      aTI,
                                      ffc_ReadOnly,                 
                                      aSegment,
                                      OffsetInBlock,
                                      aRelMethod);
        { Get the segment & write pertinent info to the stream. }
        DelSegment := @BLOBBlock^[OffsetInBlock];
        WriteToStream(Format('%d : Size %d, Offset %d:%d' + #13#10,
                      [anInx, DelSegment^.bshSegmentLen, aSegment.iHigh,
                       aSegment.iLow]), aStream);
        aSegment := DelSegment^.bshNextSegment;
        aRelMethod(BLOBBlock);
        inc(anInx);
      end;
    finally
      aFHRelMethod(PffBlock(FileHeader));
    end;
  end;
  WriteToStream(#0, aStream);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
procedure TffBaseBLOBSegmentMgr.Rollback;
var
  CurItem, TmpItem : TffBLOBSegListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                 
  CurItem := bsmTranListHead;
  while CurItem <> nil do begin
    TmpItem := CurItem;
    CurItem := TmpItem.FTranNextItem;
    case TmpItem.FPendingAction of
      bsaAddToList :
        { The item won't be added to the in-memory deleted chain so free
          the item. }
        TmpItem.Free;
      bsaDeleteFromList :
        begin
          { The item has been removed from the in-memory deleted list. We need
            to reset its transaction info & add it back to the list. }
          TmpItem.FPendingAction := bsaNone;
          TmpItem.FTranNextItem := nil;
          bsmDelChain.Insert(TmpItem);
        end;
    end;  { case }
  end;
  bsmTranListHead := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{====================================================================}

{===TffBLOBSegListItem===============================================}
constructor TffBLOBSegListItem.Create;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  inherited Create;
  fOffset.iLow  := 0;
  fOffset.iHigh := 0;
  fSize := 0;
  MaintainLinks := False;
  FPendingAction := bsaNone;
  FTranNextItem := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{--------}
function TffBLOBSegListItem.Compare(aKey : pointer) : integer;
begin
  Result := FFCmpI32(fSize, Longint(aKey^));
  if Result = 0 then
    Result := 1;
end;
{--------}
function TffBLOBSegListItem.Key : pointer;
begin
  Result := @fSize;
end;
{====================================================================}

{===TffBLOBSegmentMgr================================================}
function TffBLOBSegmentMgr.GetRecycledSeg(aFI             : PffFileInfo;
                                          aTI             : PffTransInfo;
                                      var aSizeNeeded     : Longint;
                                    const aMinSizeAllowed : Longint)
                                                          : TffInt64;
var
//  BLOBBlock    : PffBlock;                                           {Deleted !!.13}
  L, R, M      : Integer;
  OldSegSize   : Integer;
//  aRelMethod   : TffReleaseMethod;                                   {Deleted !!.13}
  SearchSize   : Longint;
  aPrevSegItem,
  aSegItem     : TffBLOBSegListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  { Max TffInt64 returned if segment of aSize not available. }
  Result.iLow := ffc_W32NoValue;
  Result.iHigh := ffc_W32NoValue;

  { Is there a segment in the segment manager's transaction list? }
  if (bsmUseTranList) then begin
    { We are looking for a segment that is being added to the deleted
      segment list and is at least as big as the segment we need. }
    aPrevSegItem := nil;                                               
    aSegItem := bsmTranListHead;
    while (aSegItem <> nil) do begin
      if (aSegItem.FPendingAction = bsaAddTolist) then begin
        if (aSegItem.FSize > aSizeNeeded) then begin
          { Too big so we'll keeep looking. If we don't find a more
            optimum sized segment, we'll use this one. }
          aPrevSegItem := aSegItem;
        end else begin
          if (aSegItem.FSize < aMinSizeAllowed) then
            aSegItem := aPrevSegItem;
          Break;
        end;
      end;

      if (aSegItem.FTranNextItem = nil) then begin
        aSegItem := aPrevSegItem;
        Break;
      end else
        aSegItem := aSegItem.FTranNextItem
    end;

    { Did we find one in the transaction list? }
    if (aSegItem <> nil) then begin
      { Yes. Prepare to return it. }
      Result := aSegItem.FOffset;
      bsmRemoveFromTranList(aSegItem);
      bsmRemoveFromDeletedChain(aFI, aTI, Result);
      { Do we need to slice it down to the correct size? }
      if (aSegItem.FSize > aSizeNeeded) then begin
        bsmSliceSegment(aFI,
                        aTI,
                        Result,
                        aSegItem.FSize,
                        aSizeNeeded,
                        False);
      end else if (aSegItem.FSize < aSizeNeeded) then
        aSizeNeeded := aSegItem.FSize;
      aSegItem.Free;
      Exit;
    end;
   end;

  { We can exit if the list is empty or if there is not a segment big enough
    for the minimum size. }
  if (bsmDelChain.IsEmpty) or                                         
     (Pinteger(bsmDelChain[0].Key)^ < aMinSizeAllowed) then
    Exit;

  { Determine the size of segment to search for. }
  if PInteger(bsmDelChain[0].Key)^ < aSizeNeeded then
    SearchSize := aMinSizeAllowed
  else
    SearchSize := aSizeNeeded;

  { We know the list doesn't contain the exact size we're looking for,
    but it does contain one that we can "slice" to the right size.
    - using a standard binary search, we will slice L - 1}
  L := 0;
  R := pred(bsmDelChain.Count);
  repeat
    M := (L + R) div 2;
    aSegItem := TffBLOBSegListItem(bsmDelChain[M]);
    if (aSegItem.Size < SearchSize) then
      R := M - 1
    else if (aSegItem.Size > SearchSize) then
      L := M + 1
    else {found it} begin
      Result := aSegItem.Offset;
      if bsmUseTranList then begin
        bsmAddToTranList(aSegItem, bsaDeleteFromList);
        bsmDelChain.RemoveAt(M);
      end
      else
        bsmDelChain.DeleteAt(M);
      bsmRemoveFromDeletedChain(aFI, aTI, Result);
      Break;
    end;
  until (L > R);
  if (L > R) and (L > 0) then begin
    {the item just bigger is at L-1}
    dec(L);
    aSegItem := TffBLOBSegListItem(bsmDelChain[L]);
    Result := aSegItem.Offset;
    OldSegSize := aSegItem.Size;
    if bsmUseTranList then begin
      bsmAddToTranList(aSegItem, bsaDeleteFromList);
      bsmDelChain.RemoveAt(L);
    end
    else begin
      bsmDelChain.DeleteAt(L);
    end;
    bsmSliceSegment(aFI,
                    aTI,
                    Result,
                    OldSegSize,
                    SearchSize,
                    True);
  end;
  aSizeNeeded := SearchSize;

  { Get the segment's block & update the used segment count. }
{Begin !!.13}
//  BLOBBlock := ReadVfyBlobBlock3(aFI, aTI, ffc_MarkDirty, Result, aRelMethod);
//  inc(PffBlockHeaderBLOB(BLOBBlock)^.bhbAssignedSegCount);
//  aRelMethod(BLOBBlock);
{End !!.13}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{====================================================================}

{===Tff210BLOBSegmentMgr=============================================}
function Tff210BLOBSegmentMgr.GetRecycledSeg(aFI             : PffFileInfo;
                                             aTI             : PffTransInfo;
                                         var aSizeNeeded     : Longint;
                                       const aMinSizeAllowed : Longint)
                                                             : TffInt64;
var
//  BLOBBlock  : PffBlock;                                             {Deleted !!.13}
  L, R, M    : Integer;
  OldSegSize : Integer;
//  aRelMethod : TffReleaseMethod;                                     {Deleted !!.13}
  aSegItem   : TffBLOBSegListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  { Max TffInt64 returned if segment of aSize not available. }
  Result.iLow := ffc_W32NoValue;
  Result.iHigh := ffc_W32NoValue;

  { Is there a segment in the segment manager's transaction list? }   
  if (bsmUseTranList) then begin
    { We are looking for a segment that is being added to the deleted
      segment list and is at least as big as the segment we need. }
    aSegItem := bsmTranListHead;
    while (aSegItem <> nil) do begin
      if ((aSegItem.FPendingAction = bsaAddToList) and
          (aSegItem.FSize >= aSizeNeeded)) then begin
        Result := aSegItem.FOffset;
        bsmRemoveFromTranList(aSegItem);
        bsmRemoveFromDeletedChain(aFI, aTI, Result);
        { Do we need to slice it down to the correct size? }
        if (aSegItem.FSize > aSizeNeeded) then
          bsmSliceSegment(aFI,
                          aTI,
                          Result,
                          aSegItem.FSize,
                          aSizeNeeded,
                          False);
        aSegItem.Free;
        Exit;
      end;
      aSegItem := aSegItem.FTranNextItem;
    end;
  end;

  if (bsmDelChain.IsEmpty) then
    Exit;
  if (Pinteger(bsmDelChain[0].Key)^ < aSizeNeeded) then
    Exit;

  {we know the list doesn't contain the exact size we're looking for,
   but it does contain one that we can "slice" to the right size.
   - using a standard binary search, we will slice L - 1}
  L := 0;
  R := pred(bsmDelChain.Count);
  repeat
    M := (L + R) div 2;
    aSegItem := TffBLOBSegListItem(bsmDelChain[M]);
    if (aSegItem.Size < aSizeNeeded) then
      R := M - 1
    else if (aSegItem.Size > aSizeNeeded) then
      L := M + 1
    else {found it} begin
      Result := aSegItem.Offset;
      if bsmUseTranList then begin
        bsmAddToTranList(aSegItem, bsaDeleteFromList);
        bsmDelChain.RemoveAt(M);
      end
      else
        bsmDelChain.DeleteAt(M);
      bsmRemoveFromDeletedChain(aFI, aTI, Result);
      Break;
    end;
  until (L > R);
  if (L > R) and (L > 0) then begin
    {the item just bigger is at L-1}
    dec(L);
    aSegItem := TffBLOBSegListItem(bsmDelChain[L]);
    Result := aSegItem.Offset;
    OldSegSize := aSegItem.Size;
    if bsmUseTranList then begin
      bsmAddToTranList(aSegItem, bsaDeleteFromList);
      bsmDelChain.RemoveAt(L);
    end
    else begin
      bsmDelChain.DeleteAt(L);
    end;
    bsmSliceSegment(aFI,
                    aTI,
                    Result,
                    OldSegSize,
                    aSizeNeeded,
                    True);
  end;
  { Get the segment's block & update the used segment count. }
{Begin !!.13}
//  BLOBBlock := ReadVfyBlobBlock3(aFI, aTI, ffc_MarkDirty, Result, aRelMethod);
//  inc(PffBlockHeaderBLOB(BLOBBlock)^.bhbAssignedSegCount);
//  aRelMethod(BLOBBlock);
{End !!.13}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{====================================================================}
{End !!.11}

{===Initialization/Finalization======================================}
procedure FinalizeUnit;
begin
  Pool4k.Free;
  Pool8k.Free;
  Pool16k.Free;
  Pool32k.Free;
  Pool64k.Free;
  ffStrResServer.Free;
  if (EncryptBuffer <> nil) then
    FreeMem(EncryptBuffer, 64*1024);

  {$IFDEF RAMPageCheck}
  aLog.Flush;
  aLog.Free;
  {$ENDIF}
end;
{--------}
procedure InitializeUnit;
begin
  Pool4k := nil;
  Pool8k := nil;
  Pool16k := nil;
  Pool32k := nil;
  Pool64k := nil;
  EncryptBuffer := nil;
  ffStrResServer := nil;
  ffStrResServer := TffStringResource.Create(hInstance, 'FF_SERVER_STRINGS');

  {$IFDEF RAMPageCheck}
  aLog := TffEventLog.Create(nil);
  aLog.FileName := 'RAMPage.log';
  aLog.Enabled := True;
  {$ENDIF}
end;
{--------}

initialization
  InitializeUnit;

finalization
  FinalizeUnit;

end.

