{*********************************************************}
{* FSSQL: Base unit for FSSQL Server                     *}
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

Unit fssrbase;

Interface

Uses
  Windows,
  SysUtils,
  Classes,
  fsconst,
  fshash,
  fsllbase,
  {$IFDEF RAMPageCheck}
  fslllog,
  {$ENDIF}
  fslltemp,
  fssrmgr,
  fsllexcp,
  fsfunInterp,
  fscryptaccess,
  fssrintf;

{$R fssrcnst.res}

Var
  fsStrResServer: TfsStringResource;
  {$IFDEF RAMPageCheck}
  aLog: TffEventLog;
  {$ENDIF}
  TmpVersion: Integer;
  {---Handy constants for readability---}
Const
  fsc_MarkDirty = True;
  fsc_ReadOnly = False;
  fscl_PageLife = 5 * 60 * 1000;
  { A RAM page whose FRefCount > 0 may not be re-used unless the last access
    was 5 or more minutes ago. }

  // for flags record
  frDataRecord = 1;
  frEmptyRecord = 2; // block empty
  frDeleteRecord = 4;
  frUndeletedRecord = 8;
  frProtectDeleteRecord = 16;
  frProtectUpdateRecord = 32;
  frMarkAsBadRecord = 64;
  frVersionRecord = 128; // not yet

  {---Enumerated types---}
Type
  TfsOperation = (fsoNone, fsoRead, fsoInsert, fsoUpdate, fsoDelete);
  TRecoveryEngine = (edtNotUndelete, edtUndeleteIfPossibleNotBlob, edtUndeleteIfPossibleAndBlob, edtUndeleteFull);
  TVersionRecord = (trNotUseVersion, trUseVersion);

Const
  StrRecoveryEngine: Array[TRecoveryEngine] Of String[26] = ('NotUndelete', 'UndeleteIfPossibleNotBlob', 'UndeleteIfPossibleAndBlob',
    'UndeleteFull');

Type
  TffLockPresent = ({Whether a lock is present...}
    lpNotAtAll, {..no, not at all}
    lpYesByUs, {..yes, and by current session}
    lpYesByAnother); {..yes, and by another session}

  {:The types of BLOB segments.
   @enum bstHeader Segment containing BLOB info and first set of lookup entries.
   @enum bstLookup Segment containing additional BLOB lookup entries that
     couldn't fit in the header segment.
   @enum bstContent Segment containing BLOB content. }
  TffBLOBSegment = (bstHeader, bstLookup, bstContent);

  TffTransactionMode = ({Transaction modes for the buffer manager}
    tmOff, {..no transaction active}
    tmNormal, {..non-fail safe transaction in progress}
    tmFailSafe); {..fail safe transaction in progress}

  TffFindKeyAction = ({Find key actions if orig key not found}
    fkaNone, {..do nothing, return error}
    fkaNextKey, {..return next key in index, or error if none}
    fkaPrevKey, {..return previous key in index, or error if none}
    fkaPartialKey, {..key provided is partial, find full key that matches}
    fkaNearestKey); {..return next key, or if none, previous key}

  TffAccessRight = ({user access rights}
    arAdmin, {..administration right}
    arRead, {..read right}
    arInsert, {..insert right}
    arUpdate, {..update right}
    arDelete, {..delete right}

    arReadTable,
    arInsertTable,
    arUpdateTable,
    arDeleteTable,
    arRestruct,
    arDefrag,
    arCopyTable, // dla procedury wewnêtrzej jest obs³uga przez serwer
    arTabSetInc,
    arTabEmpty,

    arReadBase,
    arInsertBase,
    arUpdateBase,
    arDeleteBase,

    arReadBlob,
    arInsertBlob,
    arUpdateBlob,
    arDeleteBlob,

    arProtectRow,
    arxxx,

    arChangePasswd, // zmienic haslo
    arChangeLoginNoRights, // zmienic login ( nazwe)

    arReadRefer, // powi¹zania - referential integrity
    arInsertRefer,
    arUpdateRefer,
    arDeleteRefer,

    arReadView, // View
    arInsertView,
    arUpdateView,
    arDeleteView,

    //future
    arOpenProc, // czy mo¿e widzieæ tabele procedur, triggers, view
    arOpenRefer, // czy mo¿e widzieæ tabele referntial
    arOpenGener, // czy mo¿e widzieæ tabele generation ID
    // future

    arReadProc, // procedury
    arInsertProc,
    arUpdateProc,
    arDeleteProc,
    arExecProc,

    arReadGener, // generatory id
    arInsertGener,
    arUpdateGener,
    arDeleteGener,
    arExecGener,

    arRes1, arRes2, arRes3, arRes4, arRes5, arRes6, arRes7, arRes8);

  TffUserRights = Set Of TffAccessRight;

  {---The FlashFiler primitive file type and buffer manager class---}
  TffBaseBLOBResourceMgr = Class; {..forward declaration}
  TfsBufferManager = Class;

  TffbmRAMPage = Class;

  PffPageArray = ^TffPageArray;
  TffPageArray = Array[Byte] Of Pointer;
  {-This type is used in the TffFileInfo.fiPages structure.
    An element of a leaf array will point to a TffbmRAMpage.
    An element of a node array will point to a TffPageContainer. }

  PffPageContainer = ^TffPageContainer;
  TffPageContainer = Record
    pcNext: PffPageContainer;
    pcPrev: PffPageContainer;
    pcPages: TffPageArray;
    pcCount: Word;
  End;
  {-This type is used in the TffFileInfo.fiPages structure. }

  TffBlockNum = Packed Array[0..3] Of Byte;
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

  TffFileAttributes = Set Of TffFileAttribute;
  PffFileInfo = ^TffFileInfo;
  TffFileInfo = Packed Record {A FSSQL file..}
    fiVerify: TffWord32; {..verification value}
    fiHandle: THandle; {..file handle}
    fiBlockSize: Longint; {..block size--4K, 8K, 16K, 32K, or 64K}
    fiBlockSizeK: Longint; {..block size in kilobytes--4, 8, 16, 32, or 64 } {!!.11}
    fiLog2BlockSize: TffWord32; {..log base 2 of fiBlockSize (12, 13, 14, 15 or 16)}
    fiUsedBlocks: TffWord32; {..number of blocks in file.  We store this
    value here in order to reduce number of
    locks on block 0.  This field is updated
    when a new block is added to the file. }
    fiRecordLength: Longint; {..record length}
    fiRecLenPlusTrailer: Longint; {..record length plus deletion link}
    fiBufMgr: TfsBufferManager; {..the buffer manager being used}
    fiName: PffShStr; {..fully expanded file name}
    fiOpenMode: TffOpenMode; {..open mode}
    fiShareMode: TffShareMode; {..share mode.  Indicates how the file
    has been opened by the server.  The
    server usually opens files in
    smExclusive mode.}
    fiWriteThru: Boolean; {..file has been opened in writethru mode}
    fiForServer: Boolean; {..file is for the server, not the client}
    fiEncrypted: Boolean; {..file is encrypted}
    fiBLOBrscMgr: TffBaseBLOBResourceMgr; {.the resource manager being used} {!!.11}
    fiMaxBlocks: TffWord32; {..max # of blocks for 4 GB file size}
    fiMaxSegSize: TffWord32; {..max size of BLOB segment}
    fiPageListHead: TffbmRAMPage; {..first RAM page in this file's list of
    loaded blocks. }
    fiPageListTail: TffbmRAMPage; {..last RAM page in this file's list of
    loaded blocks. }
    fiPageZero: TffbmRAMPage; {..The TffbmRAMPage for block 0.
    We cache it here since it is frequently-
    requested. }
    fiPageContainerList: PffPageContainer;
    {..the list of page containers used to build
       the fiPages structure.  We maintain
       a separate list of these objects so that
       we can quickly free them when this file
       structure is destroyed. }
    fiPages: TffPageArray; {..The blocks stored in memory as RAM pages.}
    { Note: fiPages is a tree structure having multiple roots.  We use the
            structure to quickly determine whether or not a block is
            loaded in memory. }
    fiRecordLocks: TfsThreadHash64; {..The record locks for this file.  Used by
    the lock manager. }
    fiFSVersion: Longint; {..Version of FS used to create file}
    fiVersionRecord: TVersionRecord;
    bhfAutoInc64Value: Int64;
    fiAutoInc64StepValue: Longint;
    fiExclOwner: TffCursorID; {..if <> fsc_W32NoValue then this is the
    ID of a cursor that has exclusively
    opened this file. }
    fiAttributes: TffFileAttributes;
    {..special attributes of the file. }
    fiTempStore: TFSSpecObject; {..temporary storage used by this file.
    For regular files, this will start off
    as nil and then the buffer manager will
    fill it with the buffer manager's
    temporary storage object. For merge sort
    files, the sorting algorithm will fill
    this field with the file's own
    temporary storage instance. }
  End;

  TfsSrcTransactionLevel = Class; { forward declaration } {!!.10}
  TfsSrcTransaction = Class; { forward declaration }

  PFFBlockCommonHeader = ^TFFBlockCommonHeader;
  TFFBlockCommonHeader = Packed Record
    bchSignature: Longint;
    bchNextBlock: Longint;
    bchThisBlock: TFFWord32;
    bchLSN: TFFWord32;
  End;

  { The following record structure is used to pass transaction-specific
    information to low-level routines in FFTBDATA, FFTBINDX, and FFSRBASE.
    Note that we have to pass around the transaction because its LSN may
    change due to an LSN rollover.  We always want the latest LSN.

    Note: It is included in this unit because it is needed both by FFSRBASE
    and FFSRLOCK. }
  PffTransInfo = ^TffTransInfo;
  TffTransInfo = Packed Record
    tirLockMgr: TFSSpecObject; { Really an instance of TffLockManager. }
    tirTrans: TfsSrcTransaction;
  End;

  { Stored in TffbmRAMPage.rpBlockList.  Helps us track the nesting level
    of each ram page. }
  TffbmModifiedBlock = Class(TFSSpecObject)
  Protected {private}
    mbBlock: PffBlock;
    mbBlockNumTmp: TffWord32;
    {-The block in temporary storage to which this block was written.
      Set to fsc_W32NoValue if not in temp storage. }

    mbTransLevelPrev: TffbmModifiedBlock; {!!.10}
    mbTransLevelNext: TffbmModifiedBlock; {!!.10}

    Function mbGetBlock: PffBlock;
  Protected
    Procedure AddToTransLevel; {!!.10}
    Procedure RemoveFromTransLevel; {!!.10}
  Public
    Prev: TffbmModifiedBlock;
    TransLevel: TfsSrcTransactionLevel;
    RAMPage: TffbmRAMPage;

    Constructor Create(aRAMPage: TffbmRAMPage;
      aPrevBlock: TffbmModifiedBlock;
      aTransLevel: TfsSrcTransactionLevel); {!!.10}
    Destructor Destroy; Override;

    Procedure Copy(aBlock: PffBlock);
    Procedure CopyTo(aBlock: PffBlock);

    Procedure DecreaseTransLevel; {!!.10}

    Procedure FreeBlock;
    { Frees the object's block. }
    Procedure SendToTempStore;
    { Sends the block to temporary storage. }

    Property Block: PffBlock Read mbGetBlock Write mbBlock;

  End;

  PffReleaseMethod = ^TffReleaseMethod;
  TffReleaseMethod = Procedure(Var aBlock: PffBlock) Of Object;
  { The type of method to be called once a thread has finished accessing
    a RAM page. }

  PffReleaseInfo = ^TffReleaseInfo;
  TffReleaseInfo = Packed Record
    BlockPtr: PffBlock;
    MethodVar: TffInt64;
  End;
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
 instance of TfsSrcTransaction maintains a list of TffbmRAMPages that have been
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
  TffbmRAMPage = Class(TFSSpecObject)
  Protected {private}
    FLastAccess: DWORD; {..the time (obtained via GetTickCount) when
    this page was last accessed. }
    FNew: Boolean; {..if True then this is a new file block. } {!!.07}
    FRefCount: Integer; {..the number of times this page has been
    requested. If zero then no threads are
    accessing the page. If greater than zero
    then one or more threads are accessing
    the page.
    The count increments when the page's
    block is retrieved and decrements when the
    Release method is called. }
    rpBlock: PffBlock; {..block data (variably sized)}
    rpBlockBits: TffWord32; {..bits identifying which modified blocks
    are in temporary storage. }
    rpBlockListTail: TffbmModifiedBlock;
    {..the last modified block in this page's
       list of modified blocks. We only need the
       tail because a commit or rollback of the
       page affects the tail. }
    rpBlockNum: TffWord32; {..zero-based block number in file}
    rpBlockSize: Longint; {..sizeof rpBlock}
    rpBlockSizeEnum: TffBlockSize;
    rpBlockNumTmp: TffWord32; {..if not equal to fsc_W32NoValue then this
    block is currently located in temporary
    storage & this is the block in which it
    resides in temporary storage. }
    rpBufMgr: TfsBufferManager;
    {..the buffer mgr with which this page is
       associated }
    rpFI: PffFileInfo; {..the file with which this page is associated}
    rpFileNext: TffbmRAMPage; {..next     ram page in file page list}
    rpFilePrev: TffbmRAMPage; {..previous ram page in file page list}
    rpHeader: PffBlockCommonHeader;
    rpInUseNext: TffbmRAMPage; {..next     ram page in InUse or Recycle list}
    rpInUsePrev: TffbmRAMPage; {..previous ram page in InUse list}
    rpReuseMode: TffbmPageReuseMode; {..indicates how the page may be re-used }
    rpTrans: TfsSrcTransaction; {..server transaction for which the block is dirty}
    rpTransNext: TffbmRAMPage; {..next     ram page in Transaction list}
    rpTransPrev: TffbmRAMPage; {..previous ram page in Transaction list}
  Protected
    Procedure AddToFilePageList;
    {-Adds the page to its file's list of RAM pages. }
    Procedure AddToRecycleList;
    {-Adds the page to the recycle list. }
    Procedure AddToTransList(aTrans: TfsSrcTransaction);
    {-Adds the page to a transaction item's page list. }
    Procedure AddToUseList;
    {-Add the RAM page to the buffer manager's InUse list. }
    Procedure MoveToEndOfTransList;
    {-Moves the RAM page to the end of its transaction's list of RAM
      pages. }
    Procedure MoveToEndOfUseList;
    {-Moves the RAM page to the end of the InUse list.  This is done
      so that the Least Recently Used (LRU) pages appear at the beginning
      of the list. }
    Procedure MoveToRecycleList;
    {-Moves a page from the buffer manager's InUse list to the Recycle
      list. }
    Procedure RemoveFromFilePageList;
    {-Removes the page from its file's list of RAM pages. }
    Procedure RemoveFromRecycleList;
    {-Removes the page from the recycle list. }
    Procedure RemoveFromTransList(aTrans: TfsSrcTransaction);
    {-Removes the page from a transaction item's page list. }
    Procedure RemoveFromUseList;
    {-Remove the RAM page from the buffer manager's InUse list. }
    Procedure rpAllocBlock(aBlockSize: Longint);
    {-Allocates a new read-only block. }
    Function rpAllocBlockPrim(aBlockSize: Longint): PffBlock;
    {-Carries out the actual allocation of a block. }
    Function rpDirty: boolean;
    {-If returns True then this block is dirty. }
    Procedure rpFreeBlock(aBlock: PffBlock; aBlockSize: Longint);
    {-Frees a specific block. }
    Function rpGetInTempStore: boolean;
    {-If the block is in temporary storage then returns True otherwise
      returns False. }
    Function rpGetLSN: TffWord32;
    {-If no transaction has dirtied the block then returns the LSN of the
      read-only block.  Otherwise returns the LSN of the most recent
      version. }
    Function rpGetTransLevel: TfsSrcTransactionLevel; {!!.10}
    {-Returns nest level of last transaction to modify this page. }
    Procedure rpRelease(aBlock: PffBlock);
    {-Alternative to Release method that does not nil the input parameter. }
    Procedure rpReplaceBlock(aNewBlock: PffBlock);
    {-Replaces the read-only block with another block. }
    Procedure rpRetrieveFromTemp;
    {-Retrieves the read-only block from temp storage. }
    Procedure rpSetBlockSize(aBlockSize: Longint);
    Procedure rpSetFI(FI: PffFileInfo);
    Procedure rpSetLSN(Const aLSN: TffWord32);
    {-Sets the LSN of a RAM page that has not been modified by a
      transaction. }
  Public
    Constructor Create(aBufMgr: TfsBufferManager; aFI: PffFileInfo;
      Const aBlockNumber: TffWord32);
    Destructor Destroy; Override;

    Function Block(aTrans: TfsSrcTransaction;
      Var aReleaseMethod: TffReleaseMethod): PffBlock;
    { Returns a copy of the file block.  If the transaction requesting the
      block previously modified the block then this routine returns the
      last modified version of the block.  If the block has not been
      modified by the transaction or the aTrans parameter is nil, the
      read-only copy of the block is returned.

      Once the requesting thread has finished with the block, it must call
      the procedure specified by aReleaseMethod. }

    Function Commit(forceWrite: boolean): boolean;
    { Commits a changed RAM page.  If forceWrite is True then changes are
      committed to disk regardless of nesting level and this function
      returns True.

      If forceWrite is False then the following logic is used:
        If the nesting level is greater than zero then this merely
        decrements the TransLevel of the RAM page and returns False.
        Otherwise, it writes the RAM page to disk and returns True. }

    Function DirtiedForTrans(aTrans: TfsSrcTransaction): boolean;
    { Returns True if this block has been modified by the transaction.
      This function returns True only if the following is true:
        1. aTrans is a transaction.
        2. The block is marked as dirty.
        3. The block's LSN matches the transaction's LSN.
        4. The block's nesting level matches the transaction's nesting
           level. }

    Procedure MakeClean;

    Procedure MakeDirty(aTrans: TfsSrcTransaction);

    Function ReadOnlyBlock: PffBlock;
    { Returns the page's read-only block. }

    Procedure Release(Var aBlock: PffBlock);
    { Use this method to tell the buffer manager that a thread is
      no longer using a ram page. Every retrieval of a page must be
      accompanied by a call to this method, otherwise the buffer manager
      will not re-use the ram page as soon as it normally would. }

    Function Removable(Var RemoveMode: TffbmPageReuseMode): boolean;
    { Use this method to determine if a RAM page may be removed from
      the buffer manager.

      If returns False then this page may not be removed.

      If returns True then this page may be removed.  Look at the
      RemoveMode parameter to determine how it may be removed. If it
      returns ffrmUseAsIs then you may free the page. If it returns
      ffrmTempStore then the page may be moved to temporary storage,
      which removes its data block from memory. Do not free a page that
      returns a mode of ffrmTempStore. }

    Function Reusable(Var ReuseMode: TffbmPageReuseMode): boolean;
    { Use this method to determine if the RAM page may be re-used.

      If returns False then this page may not be re-used.

      If returns True then this page may be re-used. Look at the
      ReuseMode parameter to determine how it may be reused. If it returns
      ffrmUseAsIs then you may use this RAM page instance as is. If it
      returns ffrmTempStore then you may send the RAM page to temporary
      storage and create a new RAM page to take its place.  Do not free
      or re-use the instance of a RAM page that returns a mode of
      ffrmTempStore. }

    Procedure Rollback;
    { Rolls back the most recent changes to the RAM page.  Assumes that
      a transaction has modified the page. }

    Procedure SendToTempStore;
    { Use this method to send a RAM page to temp storage. }

    Property BlockNumber: TffWord32 Read rpBlockNum Write rpBlockNum;

    Property BlockSize: Longint Read rpBlockSize Write rpSetBlockSize;

    Property Dirty: boolean Read rpDirty;
    { If returns True then the page has been modified by a transaction.
      The Block method returns the modified block to the transaction
      that dirtied the page. The Block method returns the read-only block
      to all other threads. }

    Property FileInfo: PffFileInfo Read rpFI Write rpSetFI;

    Property InTempStore: boolean Read rpGetInTempStore;
    { If returns True then this block is currently in temporary storage. }

    Property LastAccess: DWORD Read FLastAccess;
    { The time, obtained via GetTickCount, when this page was last
      accessed by a thread. }

    Property LSN: TffWord32 Read rpGetLSN Write rpSetLSN;
    { Log Sequence Number (LSN) of the last transaction to modify the
      RAM page.  A RAM page already loaded into memory can be re-used
      if its LSN is less than the buffer manager's CommitLSN. }

    Property TransLevel: TfsSrcTransactionLevel Read rpGetTransLevel; {!!.10}
    { The nesting level of the page.  If -1 then this block has not been
      modified by a transaction.  If zero then only one transaction
      has started and modified this block.  If >= 1 then there are one or
      more nested transactions. }

{Begin !!.07}
    Property New: Boolean Read FNew Write FNew;
    { Indicates whether this page represents a new file block (i.e., just
      added to the file). }
{End !!.07}

    Property RefCount: Integer Read FRefCount;
    { The number of times a thread has requested this page. If this
      property returns zero then no threads are currently accessing the
      page. If this property returns a value greater than zero then
      one or more threads are reading the contents of the page. }

    Property ReuseMode: TffbmPageReuseMode Read rpReuseMode;
    { Use this property to determine the page's reuse mode. }

  End;

  {---Transaction types---}
  TfsSrcTransactionLevel = Class(TFSSpecObject)
  Protected {private}
    tlPrev: TfsSrcTransactionLevel;
    tlLevel: Integer;
    tlTransaction: TfsSrcTransaction;

    tlModifiedBlocksHead: TffbmModifiedBlock;
    tlModifiedBlocksTail: TffbmModifiedBlock;
  Public
    Constructor Create(aTrans: TfsSrcTransaction);
    Destructor Destroy; Override;

    Property Level: Integer Read tlLevel;
  End;

  { This class represents an active transaction within a folder (i.e.,
    directory).
    A transaction maintains a list of the RAM pages that have been dirtied
    by the transaction. }
  TfsSrcTransaction = Class(TfsSelfListItem)
  Protected {private}
    FCorrupt: boolean;
    FDatabaseID: TffDatabaseID;
    FImplicit: boolean;
    FJnlFile: PffFileInfo;
    FLSN: TffWord32;
    FNewSpace: Integer; {!!.11}
    FTransLevel: Integer;
    FReadOnly: boolean;
    FSignature: Longint;
    FTransMode: TffTransactionMode;
    FLockContainer: TFSSpecListItem;
    FListEventsTransaction: TFSSpecStringList;
    FListEventsGlobalTransaction: TFSSpecStringList;
    trTransLevelListTail: TfsSrcTransactionLevel;
    fVariables: TIntVariables;

    trTransPageListHead: TffbmRAMPage;
    {-The first RAM page associated with this transaction. }
    trTransPageListTail: TffbmRAMPage;
    {-The last RAM page associated with this transaction. }

  Protected
    Function trGetNested: boolean;
    Function trGetTransactionID: TffTransID;
    Function trGetTransLevel: TfsSrcTransactionLevel; {!!.10}
  Public
    Constructor Create(Const aDatabaseID: TffDatabaseID;
      Const aImplicit, ReadOnly: boolean);
    Destructor Destroy; Override;

    Function AdjustLSN(Const Adjustment: TffWord32): TffWord32;
    { Adjusts the transaction's LSN.  The adjusted LSN is then applied to
      each RAM page dirtied by the transaction.  Returns the new LSN of
      the transaction. }

    Procedure StartNested; {!!.10}
    { Increases the nesting level of the transaction }{!!.10}
    Procedure EndNested; {!!.10}
    { Decreases the nesting level of the transaction }{!!.10}

    Property DatabaseID: TffDatabaseID Read FDatabaseID;
    Property IsCorrupt: boolean Read FCorrupt Write FCorrupt;
    Property IsImplicit: boolean Read FImplicit;
    Property IsReadOnly: boolean Read FReadOnly Write FReadOnly; {!!.06}
    Property JournalFile: PffFileInfo
      Read FJnlFile Write FJnlFile;
    { If TransactionMode = tmFailSafe then this property identifies
      the journal file. }

    Property LSN: TffWord32 Read FLSN Write FLSN;
    { The Log Sequence Number of this transaction.  In the future,
      this number will reflect the position within the log file of
      the transaction's next log record.
      For now, this is a static number assigned when the transaction
      is created. }

    Property Nested: boolean Read trGetNested;
    { Returns True if the transaction is nested. }

{Begin !!.11}
    Property NewSpace: Integer Read FNewspace Write FNewSpace;
    { # of kb in free space required for blocks added by this transaction. }
{End !!.11}

    Property TransLevel: TfsSrcTransactionLevel Read trGetTransLevel; {!!.10}
    { The nesting level of the transaction.  For a non-nested transaction,
      this property returns zero.  For a transaction that has been nested
      1 level, this property returns one, and so on. }

    Property TransactionID: TffTransID Read trGetTransactionID;
    { The unique ID of the transaction.  This will be unique across all
      transactions on an FF server. }

    Property TransactionMode: TffTransactionMode
      Read FTransMode Write FTransMode;
    { Indicates whether this is a normal or failsafe transaction. }

    Property TransLockContainer: TFSSpecListItem
      Read FLockContainer Write FLockContainer;

    Property ListEventsTransaction: TFSSpecStringList Read fListEventsTransaction Write fListEventsTransaction;
    Property ListEventsGlobalTransaction: TFSSpecStringList Read fListEventsGlobalTransaction Write fListEventsGlobalTransaction;
    Property Variables: TIntVariables Read fVariables Write fVariables;
  End;

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
  TfsBufferManager = Class(TFSSpecObject)
  Protected {private}
    bmConfigDir: String;
    bmInUseListHead: TffbmRAMPage;
    bmInUseListTail: TffbmRAMPage;
    bmRecycleListHead: TffbmRAMPage;
    {Begin !!.02}
    bmPortal: TfsPadlock; { Provides thread-safe access
    to data structures. }
{End !!.02}
    bmMaxRAM: Longint; { Max number of megabytes for cache. }
    bmMaxRAMDetail: TffInt64; { Max number of bytes for cache.  For comparisons. }
    bmRAMDetail: TffInt64; { Number of bytes used.  For comparisons. }
    bmRAMUsed: Longint; { Number of megabytes used.  For status. }
    bmLRUValue: TffWord32; { The latest LRU value.  Indicator for
    when the block was last used. }
    bmLRULockValue: TffWord32; { The LRU value of the last started
    transaction.  Used to indicate the point
    after which blocks may not be re-used. }
    bmTempStore: TfsBaseTempStorage;

    bmEncryptTempStorage: boolean;
    bmClearCachePerCount: Longint;
    bmClearCachePerCountT: Longint; // tmp
    bmClearCacheIfUpdate: Boolean;

  Protected
    Function GetRAM: Integer;
    Procedure SetMaxRAM(aNumber: Longint);

    Procedure bmClearRecycleList; Virtual; {!!.07}
    Procedure bmCommitPrim(aTrans: TfsSrcTransaction);
    Function bmRAMPageCount: Longint;
    Function bmFileRAMPageCount(aFI: PffFileInfo): Longint;
    Procedure bmFailSafeCommit(aTrans: TfsSrcTransaction);
    Function bmGetBlock(aFI: PffFileInfo; aBlockNumber: TffWord32; aOperation: TfsOperation): TffbmRAMPage;
    { Find a block in the internal data structure.  If the block is not
      already in memory then retrieve it. }

    Function bmGetNewRAMPage(aFI: PffFileInfo; aBlockNumber: TffWord32; aOperation: TfsOperation): TffbmRAMPage;
    { Obtains a new RAM page. It tries to reuse a recycled page. If none is
      available then it checks to see if adding a new page would push it
      over the RAM limit. If it would push the buffer manager over the RAM
      limit then it looks for a page that may be re-used. If one is found
      then the page is re-used. If none is found a new page is created
      from scratch. When the next transaction ends, the buffer manager tries
      to remove the excess page(s). }

    Function bmGetRAMPage(Const anIndex: Longint): TffbmRAMPage;
    {-Returns a specific RAM page managed by the buffer manager. }

    Function bmGetRecycledCount: Longint;
    {-Returns the total # of RAM pages in the recycled list. }

    Function bmGetTempStoreSize: Integer;
    {-Returns the size of temporary storage in megabytes. }

    Procedure bmJournalRAMPage(aTrans: TfsSrcTransaction;
      aRAMPage: TffbmRAMPage; aBeforeImage: boolean);
    Function bmOverRAMLimit(sizeOfNewBlock: Longint): boolean;
    {-Used to determine if adding a new block of the specified size would
      push the buffer manager over its RAM limit. }

    Procedure bmReadBlock(aFI: PffFileInfo; aBlockNumber: TffWord32;
      aRAMPage: TffbmRAMPage);
    {-Reads the specified block from the file, placing it into aRAMPage.
      If aBlockNumber is set to fsc_W32NoValue then this method reads
      block zero of the file, transferring information from the header
      block into the aFI structure. }

    Function bmSearch(aFI: PffFileInfo; aBlockNumber: TffWord32): TffbmRAMPage;
    { Determines if the page specified by aBlockNumber is already in
      memory. }

    Procedure bmSetTempStoreSize(aSizeInMB: Integer);
    { Changes the size of temporary storage. Note that this method may
      be used only when the temporary storage has not been written to.
      That's because this method does not handle transfer of data from
      existing temporary storage to the newly-size temporary storage. }

    Procedure bmDecreaseRAMDetail(Const numberBytes: Longint);
    Procedure bmIncreaseRAMDetail(Const numberBytes: Longint);
    Procedure bmWriteCompleteJnlHeader(aJnlFile: PffFileInfo);
    Procedure bmWriteIncompleteJnlHeader(aJnlFile: PffFileInfo);
    Procedure bmRemovePageFromTransaction(aPage: TffbmRAMPage);
  Public

    Constructor Create(Const TempPath: String;
      Const TempStoreSizeInMB: Integer; Const aEncryptTempStorage: boolean);
    Destructor Destroy; Override;

    Function AddBlock(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Const aBlockNumber: TffWord32;
      Var aReleaseMethod: TffReleaseMethod;
      aOperation: TfsOperation): PffBlock;
    { Adds a new block to the specified file (i.e., increases the size
      of the file). }

    Function AddFile(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Const aMarkHeaderDirty: boolean;
      Var aReleaseMethod: TffReleaseMethod): PffBlock;
    { Adds a file to the buffer manager's list of managed files.  }
    Procedure bmRemoveCommittedPages(Const aTran: TfsSrcTransaction);
    { Called after committing a transaction subset, this procedure removes
      the RAM pages associated with the specified transaction. }
    Procedure bmRemoveExcessPages;
    { Called after a commit or rollback, this method removes RAM pages
      from the cache if the amount of memory occupied by the RAM pages
      exceeds the MaxRAM property. }

    Procedure BeginWrite;
    { Must be called prior to accessing the buffer manager's internal
      data structures.  This method is public due to its being used by
      TfsSrcTransaction. }

    Procedure DirtyBlock(aFI: PffFileInfo;
      Const aBlockNumber: TffWord32;
      aTI: PffTransInfo;
      Var aModifiableBlock: PffBlock);
    { Marks a block as modified by the specified transaction.  The
      transaction's LSN (as specified in parameter aTI) is written to
      the block. Returns the modifiable copy of the block. Any method
      calling this function *MUST* use the returned block instead of
      the current block. }

    Procedure EndWrite;
    { Must be called after finished accessing the buffer manager's internal
      data structures.  Must be preceded by a call to BeginWrite.
      This method is public due to its use by TfsSrcTransaction. }

    Procedure FlushPools(Const blockSizes: TffBlockSizes);
    { Use this method to have the buffer manager flush any unused blocks
      from the memory pools. aBlockSize contains enumerated values
      representing the memory pools that are to be flushed. Only those
      memory pools having an enumerated value in blockSizes are flushed. }

    Function GetBlock(aFI: PffFileInfo;
      Const aBlockNumber: TffWord32;
      aTI: PffTransInfo;
      Const aMarkDirty: boolean;
      Var aReleaseMethod: TffReleaseMethod;
      aOperation: TfsOperation): PffBlock;
    { Retrieves a block from a file.  If the block is already in the
      RAM cache then it is retrieved from the cache otherwise it is
      retrieved from the physical file and stored in the RAM cache. }

    Function GetRAMPage(aFI: PffFileInfo;
      Const aBlockNumber: TffWord32): TffbmRAMPage;
    { Retrieves the RAM page for a specific block in a file. }

{Begin !!.10}
    Function GetRAMPageLSN(aRAMPage: TffbmRAMPage): TffWord32;
    { Retrieve the LSN of the specified RAM page. }

    Function GetRAMPageLSN2(aFI: PffFileInfo;
      Const aBlockNumber: TffWord32): TffWord32;
    { Retrieves the RAM page for a specific block in a file. }
{End !!.10}

    Procedure HandleLSNRollover;
    { Called when the transaction manager rolls over its LSN.  For each
      RAM page that is not associated with a transaction, the buffer
      manager resets the LSN of that RAM page to 1. }

    Procedure Lock; {!!.05}
    Procedure Unlock; {!!.05}

    Procedure RemoveFile(aFI: PffFileInfo);
    { Moves a file's RAM pages to the buffer manager's Recycle list and
      frees the structure used to index the file's RAM pages. }

    Procedure RemoveFile2(aFI: PffFileInfo);

    Procedure UnlockBlock(aFI: PffFileInfo; aBlockNumber: TffWord32);
    { This function recycles a page, removing it from the header list
      (i.e., page of file header blocks) or file list and from a
      transaction list if the block is associated with a transaction.

      Currently, this function is not called from the engine. }

    Procedure CommitFileChanges(aFI: PffFileInfo; aTrans: TfsSrcTransaction);
    { Use this method to commit changes to a file that is being closed
      in the midst of a transaction. }
    Procedure CommitTransaction(aTrans: TfsSrcTransaction);
    Procedure CommitTransactionSubset(aTrans: TfsSrcTransaction);
    Procedure RollbackTransaction(aTrans: TfsSrcTransaction);
    Procedure RollbackTransactionSubset(aTrans: TfsSrcTransaction);
    Procedure StartTransaction(aTrans: TfsSrcTransaction;
      Const aFailSafe: boolean;
      Const aFileName: TffFullFileName);

    Property TempPath: String Read bmConfigDir Write bmConfigDir;
    { The server engine's configuration directory.  Passed on to temporary
      storage. }
    Property EncryptTempStorage: boolean Read bmEncryptTempStorage Write bmEncryptTempStorage;

    Property MaxRAM: Integer Read bmMaxRAM Write SetMaxRAM;
    { The maximum amount of RAM the buffer manager may allocate to hold
      RAM pages. }

    Property ClearCachePerCount: Longint Read bmClearCachePerCount Write bmClearCachePerCount;
    Property ClearCacheIfUpdate: Boolean Read bmClearCacheIfUpdate Write bmClearCacheIfUpdate;

    Property RAMPageCount: Longint Read bmRAMPageCount;
    { Returns the number of RAM pages being managed by the buffer
      manager. }
    Property RAMPages[Const aIndex: Longint]: TffbmRAMPage
    Read bmGetRAMPage;
    { Use this property to access the RAM pages managed by the buffer
      manager. This property is base zero. The upper bound is
      pred(RAMPageCount).

      Note: This property is for unit testing purposes only. The buffer
      manager uses a sequential search to find the specified RAM page
      so accessing this property could lead to poor performance. }

    Property RAMUsed: Integer Read GetRAM;
    { The total amount of RAM allocated to RAM pages by the buffer
      manager.  Note that this property is not thread-safe. It returns
      whatever value is available at the time and does not worry about
      the value being modified while it is being read. }

    Property RecycledCount: Longint Read bmGetRecycledCount;
    { Returns the total number of RAM pages in the recycled list. }

    Property TempStoreSize: Integer Read bmGetTempStoreSize
      Write bmSetTempStoreSize;
    { Gets and sets the size of temporary storage, in MegaBytes (MB).
      Note that you should never change the size of temporary storage
      after temporary storage has already been written to.  This is
      because the change routine does not transfer blocks already
      written to temp storage from the existing temp storage to the
      new temp storage. }
  End;

  {---Primitive file access: procedural types, vars---}
  TffCloseFilePrim = Procedure(aFI: PffFileInfo);
  {-to close a file}
  TffFlushFilePrim = Procedure(aFI: PffFileInfo);
  {-to flush a file}
  TffGetPositionFilePrim = Function(aFI: PffFileInfo): TffInt64;
  {-to return the position of the file cursor}
  TffOpenFilePrim = Function(aName: PAnsiChar;
    aOpenMode: TffOpenMode;
    aShareMode: TffShareMode;
    aWriteThru: boolean;
    aCreateFile: boolean): THandle;
  {-to open/create file}
  TffPositionFilePrim = Procedure(aFI: PffFileInfo; Const aOffset: TffInt64);
  {-to position file cursor}
  TffPositionFileEOFPrim = Function(aFI: PffFileInfo): TffInt64;
  {-to position file cursor at EOF, returning file size}
  TffReadFilePrim = Function(aFI: PffFileInfo; aToRead: TffWord32; Var aBuffer): TffWord32;
  {-to read from file, returning bytes read}
  TffSetEOFPrim = Procedure(aFI: PffFileInfo; Const aOffset: TffInt64);
  {-to truncate/extend file}
  TffSleepPrim = Procedure(MilliSecs: Longint);
  {-to sleep/delay a period of time}
  TffWriteFilePrim = Function(aFI: PffFileInfo; aToWrite: TffWord32; Const aBuffer): TffWord32;
  {-to write to file, returning bytes written}

  TTableType = (ttData, ttDataList, ttUser, ttSystem);
  TArray92 = Array[1..92] Of Char;
  Tctr8 = Array[1..8] Of Byte;
  Tctr8c = Array[1..8] Of char;
  Tctr16c = Array[1..16] Of char;
  Tctr16 = Array[1..16] Of Byte;

  TInfoSec1 = Packed Record
    bhfPasswdTable: Tctr16c;
    bhfPasswdTableBlanc: Tctr16c;
    bhfPasswdT: Tctr8c;
  End;
  TInfoSec2 = Packed Record
    bhfPasswdRest: Tctr16c;
    bhfPasswdRestBlanc: Tctr16c;
    bhfPasswdT: Tctr8c;
  End;
  TInfoSec3 = Packed Record
    bhfSignatureDBID: Tctr16c;
    bhfSignatureDBIDB: Tctr16c;
    bhfPasswdT: Tctr8c;
  End;
  TInfoSec4 = Packed Record
    bhfUserSignature: Tctr16c;
    bhfUserSignatureB: Tctr16c;
    bhfPasswdT: Tctr8c;
  End;
  TAllocPageType = (aptNone, aptUser, aptAuto);
  {---Type definitions of the different block headers---}
  {   Note: all block headers start with a signature, a next block field,
            a this block field, and a log sequence number field}
  PffBlockHeaderFile = ^TffBlockHeaderFile;
  TffBlockHeaderFile = Packed Record {Block header for file}
    bhfSignature: Longint;
    bhfNextBlock: TffWord32; {should always be -1}
    bhfThisBlock: TffWord32; {should be equal to this block number}
    bhfLSN: TffWord32; {highest LSN of any block in the table;
    updated each time a non-readonly
    transaction is committed}
    bhfBlockSize: Longint; {size of blocks in bytes (4K, 8K, 16K, 32K, 64K)}
    bhfEncrypted: Longint; {0-not encrypted, 1-encrypted}
    bhfLog2BlockSize: TffWord32; {log base 2 of bhfBlockSize (12, 13, 14, 15 or 16)}
    bhfUsedBlocks: TffWord32; {number of blocks in file}
    bhfAvailBlocks: Longint; {number of free blocks}
    bhf1stFreeBlock: TffWord32; {number of first free block, or -1}
    bhfRecordCount: TffWord32; {number of records in file}
    bhfDelRecCount: TffWord32; {number of deleted records in file}
    bhf1stDelRec: TffInt64; {offset of 1st deleted record, or -1}
    bhfRecordLength: Longint; {record length}
    bhfRecLenPlusTrailer: Longint; {record length plus deletion link}
    bhfRecsPerBlock: TffWord32; {number of records per block}
    bhf1stDataBlock: TffWord32; {first data block, or -1}
    bhfLastDataBlock: TffWord32; {last data block, or -1}
    bhfBLOBCount: TffWord32; {number of BLOBs in file}
    bhfDelBLOBHead: TffInt64; {file-relative offset of deleted BLOB chain head}
    bhfDelBLOBTail: TffInt64; {file-relative offset of deleted BLOB chain tail}
    bhfAutoInc64Value: Int64; {Last used autoinc value}
    bhfIndexCount: Longint; {number of indexes}
    bhfHasSeqIndex: Longint; {0-no seq access index; 1-has seq access index}
    bhfIndexHeader: TffWord32; {block number of index header}
    bhfFieldCount: Longint; {number of fields}
    bhfDataDict: TffWord32; {data dictionary stream, or 0}
    bhfFSVersion: Longint; {FSSQL Version this file was created with}

    bhfres3: Byte;
    bhfTableFlags: Word;
    bhfres1: Byte;
    bhfMaxRecords: TffWord32;
    bhfAutoInc64StepValue: Longint;
    // 899
    bhfres2: Word;
    bhfUseSecureProtectRecord: Byte; // sprpasswddelete 1, 2, 4...
    bhfReservedC: TArray92;

    bhCodePage: Longint;
    bhOEMCodePage: Longint;
    bhLocale: Integer;

    bhfInfoSec1: TInfoSec1;
    bhfInfoSec2: TInfoSec2;
    bhfInfoSec3: TInfoSec3;
    bhfInfoSec4: TInfoSec4;
    bhfLastRecordID: Int64;
    bhfEngineDeleteType: TRecoveryEngine;
    bhfLastRec: TffInt64; {offset of last record, or -1}
    bhfLastCountRec: TffWord32;
    bhfVersionRecord: TVersionRecord;

    bhfAllocPageCount: Longint;
    bhfBeforeEndAllocCount: Longint;
    bhfAllocPageType: TAllocPageType;

    bhfReserved4: Array[1..601] Of Byte;
  End;

  PffBlockHeaderData = ^TffBlockHeaderData;
  TffBlockHeaderData = Packed Record {Block header for data}
    bhdSignature: Longint;
    bhdNextBlock: TffWord32; {number of next block in chain, or -1}
    bhdThisBlock: TffWord32; {should be equal to this block number}
    bhdLSN: TffWord32; {log sequence number}
    bhdRecCount: TffWord32; {number of records in block, =bhfRecsPerBlock}
    bhdRecLength: Longint; {record length, =bhfRecordLength}
    bhdNextDataBlock: TffWord32; {number of next data block}
    bhdPrevDataBlock: TffWord32; {number of previous data block}
  End;

  PffBlockHeaderIndex = ^TffBlockHeaderIndex;
  TffBlockHeaderIndex = Packed Record {Block header for index}
    bhiSignature: Longint;
    bhiNextBlock: TffWord32; {number of next block in chain, or -1}
    bhiThisBlock: TffWord32; {should be equal to this block number}
    bhiLSN: TffWord32; {log sequence number}
    bhiBlockType: Byte; {0=header, 1=btree page}
    bhiIsLeafPage: boolean; {0=internal btree page, 1=leaf btree page}
    bhiNodeLevel: Byte; {node level (leaves are 1, increments)}
    bhiKeysAreRefs: boolean; {true if keys are reference numbers}
    bhiIndexNum: Word; {index number to which page belongs}
    bhiKeyLength: Word; {length of each key}
    bhiKeyCount: Longint; {current number of keys in page}
    bhiMaxKeyCount: Longint; {maximum number of keys in page}
    bhiPrevPageRef: TffWord32; {previous page reference !!MUST BE HERE!!}
  End;

  PffBlockHeaderBLOB = ^TffBlockHeaderBLOB;
  TffBlockHeaderBLOB = Packed Record {Block header for BLOB}
    bhbSignature: Longint;
    bhbNextBlock: TffWord32; {number of next block in chain, or -1}
    bhbThisBlock: TffWord32; {should be equal to this block number}
    bhbLSN: TffWord32; {log sequence number}
    bhbAssignedSegCount: TffWord32; {number of segments in a BLOB block; this
    field is not maintained as of v2.13 }
    bhbReserved: Array[0..3] Of Longint;
  End;

  PffBlockHeaderStream = ^TffBlockHeaderStream;
  TffBlockHeaderStream = Packed Record {Block header for stream}
    bhsSignature: Longint;
    bhsNextBlock: TffWord32; {number of next block in chain, or -1}
    bhsThisBlock: TffWord32; {should be equal to this block number}
    bhsLSN: TffWord32; {log sequence number}
    bhsNextStrmBlock: TffWord32; {next stream block in chain, or -1}
    bhsStreamType: Longint; {user-defined type of stream}
    bhsStreamLength: Longint; {length of stream (only in first block)}
    bhsOwningStream: Longint; {number of stream that owns block}
  End;

  PffBLOBHeader = ^TffBLOBHeader;
  TffBLOBHeader = Packed Record {Header for BLOBs}
    bbhSignature: Byte; {..'H' for header segment, 'D' for deleted   !!.01
    BLOB}{!!.01}
    bhbFiller: Byte; {..used to align bytes in memory}
    bbhSegmentLen: Word; {..length of this segment}
    bbhBLOBLength: TffWord32; {..length of BLOB in bytes} {!!.06}
    bbhSegCount: Longint; {..number of segments,
    -1 for file BLOBs, -2 for BLOB links }
    bbh1stLookupSeg: TffInt64; {..file-relative offset of 1st lookup segment,
    -1 for file BLOBs}
  End;

  PffIndexHeader = ^TffIndexHeader;
  TffIndexHeader = Packed Record {Header for index data}
    bihIndexKeyLen: Array[0..pred(fscl_MaxIndexes)] Of Word;
    {..key lengths for each index}
    bihIndexFlags: Array[0..pred(fscl_MaxIndexes)] Of Byte;
    {..flags for each index}
    bihIndexKeyCount: Array[0..pred(fscl_MaxIndexes)] Of Longint;
    {..number of keys for each index}
    bihIndexRoot: Array[0..pred(fscl_MaxIndexes)] Of TffWord32;
    {..root page for each index}
    bihIndexPageCount: Array[0..pred(fscl_MaxIndexes)] Of Longint;
    {..number of pages for each index}
  End;

  PffBLOBLookupEntry = ^TffBLOBLookupEntry;
  TffBLOBLookupEntry = Packed Record {Lookup entry for BLOB}
    bleSegmentOffset: TffInt64; {File-relative offset of segment}
    bleContentLength: TffWord32; {Length of the content, may be < length} {!!.11}
    {of segment}
  End;

  PffBLOBSegmentHeader = ^TffBLOBSegmentHeader;
  TffBLOBSegmentHeader = Packed Record {Segment header for active BLOB}
    bshSignature: Byte; {'C' for content, 'D' for deleted,
    'L' for lookup segments}
    bshFiller: Byte; {aligns bytes in memory}
    bshSegmentLen: Word; {Length of this segment}
    bshParentBLOB: TffInt64; {File-relative offset of header
    segment, or -1}
    bshNextSegment: TffInt64; {File-relative offset of next segment,
    or -1}
  End;

  PffBLOBSegmentHeaderDel = ^TffBLOBSegmentHeaderDel;
  TffBLOBSegmentHeaderDel = Packed Record {Segment header for deleted BLOB}
    bshSignature: Byte; {'D' for deleted}
    bshFiller: Byte; {aligns bytes in memory}
    bshSegmentLen: Word; {Length of this segment}
    bshNextSegment: TffInt64; {File-relative offset of next segment,
    or -1}
    bshPrevSegment: TffInt64; {File-relative offset of prev segment,
    or -1}
  End;

  {Begin !!.03}
  TffBLOBSegAction = (bsaNone, bsaAddToList, bsaDeleteFromList);
  {End !!.03}

  TffBLOBSegListItem = Class(TFSSpecListItem)
  Protected
    FSize: Longint;
    FOffset: TffInt64;
    {Begin !!.03}
    FPendingAction: TffBLOBSegAction;
    { Identifies the action to be taken upon the list item pending the
      commit or rollback of the current transaction. }
    FTranNextItem: TffBLOBSegListItem;
    { The next BLOB segment list item modified by the current transaction.
      Allows for quick iteration through modified segments. }
{End !!.03}
  Public
    Constructor Create;
    Function Compare(aKey: pointer): Integer; Override;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise}
    Function Key: pointer; Override;
    {-return a pointer to this item's key}

    Property Size: Longint Read fSize Write fSize;
    { The total size of the segment including header information. }

    Property Offset: TffInt64 Read fOffset Write fOffset;
    { The offset of the segment within the file. }
  End;

  {Begin !!.11}
  TffBaseBLOBSegmentMgr = Class(TFSSpecObject)
    { Base class representing a BLOB segment manager. The segment manager
      carries out the dirty work of managing an internal free segment list for
      instances of TffBaseBLOBResourceMgr. }
  Protected
    bsmUseTranList: Boolean;
    bsmDelChain: TFSNormalList;
    bsmDelChainSize: Integer; { defaults to ciDelChainSize }
    bsmTranListHead: TffBLOBSegListItem;
    Procedure bsmAddToDeletedSegChain(aFI: PffFileInfo;
      aTI: PffTransInfo;
      aFileHeader: PffBlockHeaderFile;
      aDelSeg: TffBLOBSegListItem;
      aSegment: PffBLOBSegmentHeaderDel);
    {-Inserts the deleted segment into the deleted chain within the
      physical file. }

    Procedure bsmAddToTranList(aSegItem: TffBLOBSegListItem;
      anAction: TffBLOBSegAction);
    { Adds a segment list item to the list of items modified by the current
      transaction. }

    Procedure bsmRemoveFromTranList(aSegItem: TffBlobSegListItem);
    Procedure bsmSliceSegment(aFI: PffFileInfo;
      aTI: PffTransInfo;
      aSegOfs: TffInt64;
      aSegSize: TffWord32;
      Const aNewSize: TffWord32;
      aInDelChain: Boolean);
    {makes two smaller deleted segments from a larger one}
    Procedure bsmRemoveFromDeletedChain(aFI: PffFileInfo;
      aTI: PffTransInfo;
      aSegOfs: TffInt64);
    {removes segment from deleted chain and updates file header}
  Public
    Constructor Create(aFI: PffFileInfo; aTI: PffTransInfo);
    Destructor Destroy; Override;

    Procedure Commit; Virtual;
    Procedure DeleteSegment(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Const aSegOffset: TffInt64); Virtual;
    Function GetNewSeg(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Const aSize: TffWord32): TffInt64; Virtual;
    Function GetRecycledSeg(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Var aSizeNeeded: Longint;
      Const aMinSizeAllowed: Longint)
      : TffInt64; Virtual; Abstract;
    Procedure ListFreeSpace(aFI: PffFileInfo; aTI: PffTransInfo;
      Const aInMemory: Boolean;
      aStream: TStream); Virtual;
    Procedure Rollback; Virtual;
  End;

  TffBLOBSegmentMgr = Class(TffBaseBLOBSegmentMgr)
    { This version of the BLOB segment manager supports the improved nesting
      algorithm that makes use of available segments even if they are smaller
      than the requested size. }
  Public
    Function GetRecycledSeg(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Var aSizeNeeded: Longint;
      Const aMinSizeAllowed: Longint)
      : TffInt64; Override;
  End;

  Tff210BLOBSegmentMgr = Class(TffBaseBLOBSegmentMgr)
    { This version of the BLOB segment manager supports tables created prior
      to version 2.1.0.1. }
  Public
    Function GetRecycledSeg(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Var aSizeNeeded: Longint;
      Const aMinSizeAllowed: Longint)
      : TffInt64; Override;
  End;

  TffBLOBSegmentMgrClass = Class Of TffBaseBLOBSegmentMgr;
  TffBLOBResourceMgrClass = Class Of TffBaseBLOBResourceMgr;

  TffBaseBLOBResourceMgr = Class(TFSSpecObject)
    { Base class is used by a table to manage the creation and
      deletion of BLOB segments.  One instance of a concrete subclass
      should be created per table. }
  Private
    brmPadlock: TfsPadlock;
    { Used to ensure only one thread actually tries to create a BLOB
      segment manager. }
  Protected
    brmDelChainSize: Integer; { defaults to ciDelChainSize }
    brmSegmentMgr: TffBaseBLOBSegmentMgr;
    brmSegMgrLoaded: boolean;

    Function brmGetSegMgrClass: TffBLOBSegmentMgrClass; Virtual; Abstract;
    Procedure brmLoadSegMgr(aFI: PffFileInfo; aTI: PffTransInfo); Virtual;
  Public
    Constructor Create; Virtual;
    Destructor Destroy; Override;

    Class Function GetMgr(aFI: PffFileInfo): TffBaseBLOBResourceMgr;
    { Determines which BLOB resource manager implementation should be used
      for the specified file. }

    Procedure Commit; Virtual;
    Procedure DeleteSegment(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Const aSegOffset: TffInt64); Virtual;
    { Use this method to delete an existing segment once it is no longer needed.
      This class will zero out the segment and place it in the recycle list.
      @param aFI The file containing the segment.
      @param segOffset The offset of the existing segment within the file. }

    Function NewSegment(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Var aSizeNeeded: TffWord32;
      Const aMinSizeAllowed: TffWord32): TffInt64; Virtual; Abstract;
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

    Procedure ListFreeSpace(aFI: PffFileInfo; aTI: PffTransInfo;
      Const aInMemory: Boolean;
      aStream: TStream); Virtual;
    Procedure Rollback; Virtual;
  End;

  TffBLOBResourceMgr = Class(TffBaseBLOBResourceMgr)
    { This version of the BLOB resource manager supports the improved nesting
      algorithm that makes use of available segments even if they are smaller
      than the requested size. }
  Protected
    Function brmGetSegMgrClass: TffBLOBSegmentMgrClass; Override;
  Public
    Function NewSegment(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Var aSizeNeeded: TffWord32;
      Const aMinSizeAllowed: TffWord32)
      : TffInt64; Override;
  End;

  Tff210BLOBResourceMgr = Class(TffBaseBLOBResourceMgr)
    { This version of the BLOB resource manager supports tables created prior
      to version 2.1.0.1. }
  Protected
    Function brmGetSegMgrClass: TffBLOBSegmentMgrClass; Override;
  Public
    Function NewSegment(aFI: PffFileInfo;
      aTI: PffTransInfo;
      Var aSizeNeeded: TffWord32;
      Const aMinSizeAllowed: TffWord32)
      : TffInt64; Override;
  End;
  {End !!.11}

Var
  FFCloseFilePrim: TffCloseFilePrim;
  {-Primitive routine to close a file}
  FFFlushFilePrim: TffFlushFilePrim;
  {-Primitive routine to flush a file}
  FFGetPositionFilePrim: TffGetPositionFilePrim;
  {-Primitive routine to get position of file cursor}
  FFOpenFilePrim: TffOpenFilePrim;
  {-Primitive routine to open/create a file}
  FFPositionFilePrim: TffPositionFilePrim;
  {-Primitive routine to position file cursor}
  FFPositionFileEOFPrim: TffPositionFileEOFPrim;
  {-Primitive routine to position file cursor at EOF, returning file size}
  FFReadFilePrim: TffReadFilePrim;
  {-Primitive routine to read from file, returning bytes read}
  FFSetEOFPrim: TffSetEOFPrim;
  {-Primitive routine to truncate/extend file}
  FFSleepPrim: TffSleepPrim;
  {-Primitive routine to sleep/delay a period of time}
  FFWriteFilePrim: TffWriteFilePrim;
  {-Primitive routine to write to file, returning bytes written}

Const
  fsc_QuestRights: TffUserRights =
  [
    arRead,
    arReadTable,
    arReadBase,
    arReadBlob,
    arReadView,
    arReadProc,
    arExecProc,
    arReadGener,
    arExecGener];

  fsc_AdminRights: TffUserRights =
  [
    arAdmin,
    arRead,
    arInsert,
    arUpdate,
    arDelete,
    arReadTable,
    arInsertTable,
    arUpdateTable,
    arDeleteTable,
    arRestruct,
    arDefrag,
    arCopyTable,
    arTabSetInc,
    arTabEmpty,
    arReadBase,
    arInsertBase,
    arUpdateBase,
    arDeleteBase,
    arReadBlob,
    arInsertBlob,
    arUpdateBlob,
    arDeleteBlob,
    arProtectRow,
    arxxx,
    arChangePasswd,
    arReadRefer,
    arInsertRefer,
    arUpdateRefer,
    arDeleteRefer,
    arReadView,
    arInsertView,
    arUpdateView,
    arDeleteView,
    arOpenProc,
    arOpenRefer,
    arOpenGener,
    arReadProc,
    arInsertProc,
    arUpdateProc,
    arDeleteProc,
    arExecProc,
    arReadGener,
    arInsertGener,
    arUpdateGener,
    arDeleteGener,
    arExecGener,
    arRes1, arRes2, arRes3, arRes4, arRes5, arRes6, arRes7, arRes8];

  fsc_UserRights: TffUserRights =
  [
    arRead,
    arInsert,
    arUpdate,
    arDelete,
    arReadTable,
    arUpdateTable,
    arReadBase,
    arUpdateBase,
    arReadBlob,
    arInsertBlob,
    arUpdateBlob,
    arDeleteBlob,
    arReadView,
    arReadProc,
    arExecProc,
    arReadGener,
    arExecGener];

  {---constants for the file data---}
Const
  {signatures}
  fsc_SigHeaderBlock = $7FFFFFF4;
  fsc_SigHeaderBlockv1 = $38464646;
  fsc_SigDataBlock = $38444646;
  fsc_SigIndexBlock = $38494646;
  fsc_SigBLOBBlock = $38424646;
  fsc_SigStreamBlock = $38534646;
  fsc_SigFreeBlock = $34414544;
  fsc_SigJnlHeader = $3846464A;
  fsc_SigJnlRecHeader = $3852464A;
  fsc_SigDictStream = $44434944;

  {block header sizes}
  fsc_BlockHeaderSizeHeader = sizeof(TffBlockHeaderFile);
  (*fsc_BlockHeaderSizeData   = sizeof(TffBlockHeaderData); moved to FFLLBASE *)
  fsc_BlockHeaderSizeIndex = sizeof(TffBlockHeaderIndex);
  fsc_BlockHeaderSizeBLOB = sizeof(TffBlockHeaderBLOB);
  fsc_BlockHeaderSizeStream = sizeof(TffBlockHeaderStream);

  {BLOB-specific constants}
  fsc_BLOBHeaderSize = sizeof(TffBLOBHeader);
  fsc_BLOBBlockTypeHeader = 0;
  fsc_BLOBBlockTypeSeg = 1;
  fsc_BLOBSegmentHeaderSize = sizeof(TffBLOBSegmentHeader);
  fsc_BLOBLookupEntrySize = sizeof(TffBLOBLookupEntry);
  fsc_BLOBSegmentIncrement = 64;

  {Index-specific constants}
  fsc_InxBlockTypeHeader = 0;
  fsc_InxBlockTypeBtreePage = 1;
  fsc_InxFlagAllowDups = 1;
  fsc_InxFlagKeysAreRefs = 2;

  {BLOB segment signatures}
  fsc_SigBLOBSegHeader = $48;
  fsc_SigBLOBSegContent = $43;
  fsc_SigBLOBSegDeleted = $44;
  fsc_SigBLOBSegLookup = $4C;

  ciDelChainSize = 20; { Default # of entries in deleted chain linked list. }
  ciSegmentMultiple = 64; { Size increment for segments. }

  {---Journal file header types---}
Type
  TffJournalFileHeader = Packed Record {journal file header}
    jfhSignature: Longint; {..signature}
    jfhState: Longint; {..0=incomplete transaction, 1=complete}
  End;

  TffJournalFileRecordHeader = Packed Record {journal file record header}
    jfrhSignature: Longint; {..signature: 'TFRH'}
    jfrhBlockNumber: TffWord32; {..block number in file}
    jfrhBlockSize: Longint; {..size of block}
    jfrhBeforeImg: Longint; {..0=after image, 1=before image}
    jfrhFileName: TffMaxPathZ; {..file name}
  End;

  {---Verification routines---}
Function FFVerifyBLOBNr(Const aBLOBNr: TffInt64;
  aLog2BlockSize: Longint): boolean;
{-Verify a BLOB number to be valid}
Function FFVerifyIndexCount(IndexCount: Longint): boolean;
{-Verify number of indexes to be between 0 and 255}
Function FFVerifyRefNr(Const aRefNr: TffInt64;
  aLog2BlockSize: Longint;
  aRecLenPlusTrailer: TffWord32): boolean;
{-Verify a record's RefNr to be valid}

{---Internal File Info routines---}
Function FFAllocFileInfo(Const aName: TffFullFileName;
  Const aExt: TffExtension;
  aBufMgr: TfsBufferManager): PffFileInfo;
{-Allocate a file information record for file with name aName}
Procedure FFChangeFileInfo(aFI: PffFileInfo;
  Const aNewName: TffFullFileName;
  Const aExt: TffExtension);
{-Change a file information record for a new name aName
  Note: file must be closed}
Procedure FFFreeFileInfo(Var aFI: PffFileInfo);
{-Free a file information record}
Procedure FFVerifyFileHeaderSignature(aFI: PffFileInfo; Const signature: Longint);
{-Verify a file has a valid file header}
Procedure FFVerifyFileInfo(aFI: PffFileInfo; IsOpen: boolean);
{-Verify a file information record to be valid and open/closed}

{---File Access Routines---}
Procedure FFCloseFile(aFI: PffFileInfo);
{-Close file aFI}
{ Exception raised if close call fails}
Function FFFileIsOpen(aFI: PffFileInfo): boolean;
{-Return true if the file aFI is open}
{ All exceptions are trapped and generate a result of False}
Procedure FFFlushFile(aFI: PffFileInfo);
{-Flushes file aFI}
{ Exception raised if flush call fails}
Procedure FFForceFlushFile(aFI: PffFileInfo);
{-Flushes file aFI by closing and reopening it}
{ Exception raised if anything fails}
Function FFGetPositionFile(aFI: PffFileInfo): TffInt64;
{-Get position (offset from start) of file pointer of file aFI}
{ Exception raised if seek call fails}
Function FFGetFileSize(aFI: PffFileInfo): TffInt64;
{-Get size of file aFI}
{ Exception raised if seek call fails}
Procedure FFOpenFile(aFI: PffFileInfo;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aWriteThru: boolean;
  aCreateFile: boolean);
{-Allocate new file aFI, open it}
{ Exception raised if open call fails, if out of memory}
Procedure FFPositionFile(aFI: PffFileInfo;
  Const aOffset: TffInt64);
{-Position file pointer of file aFI at aOffset}
{ Exception raised if seek call fails}
Function FFPositionFileEOF(aFI: PffFileInfo): TffInt64;
{-Position file pointer of file aFI at EOF, return file length}
{ Exception raised if seek call fails}
Function FFReadFile(aFI: PffFileInfo;
  aToRead: TffWord32;
  Var aBuffer): TffWord32;
{-Read aToRead bytes from file aFI into aBuffer, return bytes read}
{ Exception raised if read call fails}
Procedure FFReadFileExact(aFI: PffFileInfo;
  Const aToRead: TffWord32;
  Var aBuffer);
{-Read exactly aToRead bytes from file aFI into aBuffer}
{ Exception raised if not exactly aToRead bytes read}
Procedure FFReadFileExactAt(aFI: PffFileInfo;
  Const aOffset: TffInt64;
  aToRead: TffWord32;
  Var aBuffer);
{-Read exactly aToRead bytes from file aFI at position aOffset into aBuffer}
Procedure FFSetEOF(aFI: PffFileInfo;
  Const aOffset: TffInt64);
{-Truncates/extends file aFI to position aOffset}
Function FFWriteFile(aFI: PffFileInfo;
  aToWrite: TffWord32;
  Const aBuffer): TffWord32;
{-Write aToWrite bytes to file aFI from aBuffer, return bytes written}
{ Exception raised if write call fails}
Procedure FFWriteFileExact(aFI: PffFileInfo;
  aToWrite: TffWord32;
  Const aBuffer);
{-Write exactly aToWrite bytes to file aFI from aBuffer}
{ Exception raised if not exactly aToWrite bytes written}
Procedure FFWriteFileExactAt(aFI: PffFileInfo;
  Const aOffset: TffInt64;
  aToWrite: TffWord32;
  Const aBuffer);
{-Write exactly aToWrite bytes to file aFI at position aOffset from aBuffer}
Function FFCalcMaxFileSize(aFI: PffFileInfo): TffInt64;
{-Calculate maximum file size for a table}
Function FFCalcMaxBLOBSegSize(aFI: PffFileInfo): TffWord32;
{-Calculate maximum BLOB segment size}

{---Encrypted File Access Routines---}
Procedure FFReadDecryptFileExact(aFI: PffFileInfo;
  aToRead: TffWord32;
  Var aBuffer);
{-Read/decrypt exactly aToRead bytes from file aFI into aBuffer}
{ Exception raised if not exactly aToRead bytes read}
Procedure FFReadDecryptFileExactAt(aFI: PffFileInfo;
  Const aOffset: TffInt64;
  aToRead: TffWord32;
  Var aBuffer);
{-Read/decrypt exactly aToRead bytes from file aFI at position
  aOffset into aBuffer}
Procedure FFWriteEncryptFileExact(aFI: PffFileInfo;
  aToWrite: TffWord32;
  Var aBuffer);
{-Write/encrypt exactly aToWrite bytes to file aFI from aBuffer}
{ Exception raised if not exactly aToWrite bytes written}
Procedure FFWriteEncryptFileExactAt(aFI: PffFileInfo;
  Const aOffset: TffInt64;
  aToWrite: TffWord32;
  Var aBuffer);
{-Write/encrypt exactly aToWrite bytes to file aFI at position
  aOffset from aBuffer}

{---File Management Routines---}
Procedure FFDeleteFile(Const FileName: TffFullFileName);
{-Delete file FileName}
Procedure FFCopyFile(Const FromFileName, ToFileName: TffFullFileName);
{-Copy file FromFileName to file ToFileName, overwrite if exists}
Procedure FFRenameFile(Const OldFileName, NewFileName: TffFullFileName);
{-Rename file OldFileName to NewFileName}

{---Retry Management---}
Procedure FFCheckRemainingTime; {!!.02}
{ Determines if the operation has timed out. }{!!.02}
Function FFGetRetry: DWORD;
{ Returns the end time of the operation. }
Function FFGetRemainingTime: Longint; {!!.01}
{ Returns the # of milliseconds until the operation times out. }
Procedure FFSetRetry(Const aTimeout: DWORD);
{ Sets the end time of the operation. aTimeout is the number of milliseconds
  the current operation has to complete. }

{---Utility Routines---}
Function FFCalcLog2BlockSize(Const BlockSize: Longint): TffWord32;
Function FFCalcMaxLookupEntries(LookupSegPtr: PffBLOBSegmentHeader): TffWord32; {!!.11}
Function FFGetBlockNum(aFI: PffFileInfo;
  Const anOffset: TffInt64): TffWord32;
Function FFAllocReleaseInfo(aBlock: PffBlock;
  aMethod: TffInt64): PffReleaseInfo;
Procedure FFDeallocReleaseInfo(aReleaseInfo: PffReleaseInfo);

Implementation

Uses
  fssrblob,
  fssrlock,
  fstablehelper,
  fsserverclass;

Const
  VerificationValue = $DF16FABB; //$FF15FABB;
  ciReopenSleep: DWORD = 25; {!!.06}
  { # of milliseconds to sleep before attempting to reopen a file.
    Used in FFForceFlushFile. On W2K machines, it is possible for the OS
    to consider the file open even though it was just previously closed.
    Not sure why this happens. This behavior has been seen by at least one
    other person outside TurboPower and waiting for the OS to flush the
    closed file seems to be the only answer. }

{ Signatures }
  fsc_SigTransaction = $71544646;

Var
  Pool4K: TfsMemoryPool; {Block pool - 4K}
  Pool8K: TfsMemoryPool; {Block pool - 8K}
  Pool16K: TfsMemoryPool; {Block pool - 16K}
  Pool32K: TfsMemoryPool; {Block pool - 32K}
  Pool64K: TfsMemoryPool; {Block pool - 64K}
  EncryptBuffer: PffByteArray; {for encryption}

Type
  PFIBlockKey = ^TFIBlockKey;
  TFIBlockKey = Record
    FI: PffFileInfo;
    BN: TffWord32;
  End;

  {$IFDEF RAMPageCheck}

Procedure Log(aMsg: String; args: Array Of Const);
Begin
  If aLog <> Nil Then
    aLog.WriteStringFmt(aMsg, args);
End;
{$ENDIF}

{===File Management Routines=========================================}
{$I FsSRBASE.INC}
{====================================================================}

{===Retry Management=================================================}

Threadvar
  fftv_RetryUntil: DWORD;
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

Procedure FFCheckRemainingTime;
Var
  RetryUntil: DWORD;
  TickCount: DWORD;
Begin
  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left? }
  If (RetryUntil < TickCount) Or
    ((RetryUntil - TickCount) < 10) Then
    { No. }
    FSRaiseExceptionNoData(EfsServerException, fsStrResServer,
      fserrGeneralTimeout);
End;
{End !!.02}
{--------}

Function FFGetRemainingTime: Longint; {!!.01}
Begin
  If (fftv_RetryUntil = fscl_INFINITE) Or {!!.01} {!!.06}
  (fftv_RetryUntil = 0) Then {!!.01}
    Result := 0 {!!.01}
  Else If fftv_RetryUntil < GetTickCount Then {!!.02}
    Result := 1 {!!.02}
  Else {!!.01}
    Result := fftv_RetryUntil - GetTickCount;
End;
{--------}

Function FFGetRetry: DWORD;
Begin
  Result := fftv_RetryUntil;
End;
{--------}

Procedure FFSetRetry(Const aTimeout: DWORD);
{-Sets the retry limit for the current thread.  Assumes that
  aTimeout is specified in milliseconds.  The retry limit is
  stored in variable fftv_RetryUntil (unit FFSRBASE).  The retry
  limit is used when acquiring table & record locks.

  This routine should be called in the public methods of
  TffServerEngine.  If a public method is sending a notification
  to extenders, the calling of this routine should occur before
  the extender notification as the extender may be doing something
  that involves table & record locking. }
Begin
  If aTimeout <= 0 Then
    fftv_RetryUntil := fscl_INFINITE {!!.06}
  Else
    fftv_RetryUntil := GetTickCount + aTimeout;
End;
{====================================================================}

{===Utility routines=================================================}

Function FFCalcLog2BlockSize(Const BlockSize: Longint): TffWord32;
Begin
  Case BlockSize Of
    4 * 1024: Result := 12;
    8 * 1024: Result := 13;
    16 * 1024: Result := 14;
    32 * 1024: Result := 15;
    Else
      Result := 16;
  End; {case}
End;
{--------}

Function FFCalcMaxLookupEntries(LookupSegPtr: PffBLOBSegmentHeader): TffWord32; {!!.11}
Begin
  Result := ((LookupSegPtr^.bshSegmentLen - sizeof(TffBLOBSegmentHeader))
    Div sizeof(TffBLOBLookupEntry));
End;
{--------}

Function FFGetBlockNum(aFI: PffFileInfo;
  Const anOffset: TffInt64): TffWord32;
{ Returns the block number for the specified file offset. }
Var
  TempI64: TffInt64;
Begin
  ffShiftI64R(anOffset, aFI^.fiLog2BlockSize, TempI64);
  Result := TempI64.iLow;
End;
{--------}

Function FFAllocReleaseInfo(aBlock: PffBlock;
  aMethod: TffInt64): PffReleaseInfo;
Begin
  FFGetMem(Result, SizeOf(TffReleaseInfo));
  Result^.BlockPtr := aBlock;
  Result^.MethodVar := aMethod;
End;
{--------}

Procedure FFDeallocReleaseInfo(aReleaseInfo: PffReleaseInfo);
Begin
  TffReleaseMethod(aReleaseInfo^.MethodVar)(aReleaseInfo^.BlockPtr);
  FFFreeMem(aReleaseInfo, SizeOf(TffReleaseInfo));
End;
{====================================================================}

{===Verification routines for BLOB segments==========================}

Function FFVerifyBLOBNr(Const aBLOBNr: TffInt64;
  aLog2BlockSize: Longint): boolean;
{Note: a BLOB number is a file-offset to a BLOB header}
Var
  Offset: TffInt64;
  TempI64: TffInt64;
Begin
  Result := False;
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  {BLOB Number can't be = 0}
  If (ffCmpI64(aBLOBNr, TempI64) <> 0) Then
    Begin
      ffShiftI64R(aBLOBNr, aLog2BlockSize, Offset);
      ffShiftI64L(Offset, aLog2BlockSize, Offset);
      ffI64AddInt(Offset, fsc_BlockHeaderSizeBLOB, Offset);
      ffI64MinusI64(aBLOBNr, Offset, Offset);
      If (ffCmpI64(Offset, TempI64) = 0) Then
        Result := True
      Else If (ffCmpI64(Offset, TempI64) > 0) Then
        Begin
          ffI64DivInt(Offset, fsc_BLOBSegmentIncrement, TempI64);
          ffI64MultInt(TempI64, fsc_BLOBSegmentIncrement, TempI64);
          If ffCmpI64(Offset, TempI64) = 0 Then
            Result := True;
        End; {if..else}
    End;
End;
{--------}

Function FFVerifyIndexCount(IndexCount: Longint): boolean;
Begin
  Result := (IndexCount And $FFFFFF00) = 0;
End;
{--------}

Function FFVerifyRefNr(Const aRefNr: TffInt64;
  aLog2BlockSize: Longint;
  aRecLenPlusTrailer: TffWord32): boolean;
Var
  Offset: TffInt64;
  TempI64: TffInt64;
Begin
  Result := False;
  TempI64.iLow := 0;
  TempI64.iHigh := 0;
  If (ffCmpI64(aRefNr, TempI64) <> 0) Then
    Begin
      ffShiftI64R(aRefNr, aLog2BlockSize, TempI64);
      ffShiftI64L(TempI64, aLog2BlockSize, Offset);
      ffI64MinusInt(aRefNr, Offset.iLow, TempI64);
      ffI64MinusInt(TempI64, fsc_BlockHeaderSizeData, Offset);
      If (Offset.iLow = 0) Then
        Result := True
      Else If (Offset.iLow > 0) Then
        If (((Offset.iLow Div aRecLenPlusTrailer) * aRecLenPlusTrailer) = Offset.iLow) Then
          Result := True;
    End;
End;
{====================================================================}

{===Fileblock info routines==========================================}

Procedure FFFreeFileInfo(Var aFI: PffFileInfo);
Begin
  If Assigned(aFI) Then
    Begin
      With aFI^ Do
        Begin
          FFShStrFree(fiName);
        End;
      FFFreeMem(aFI, sizeof(TffFileInfo));
    End;
End;
{--------}

Procedure FFChangeFileInfo(aFI: PffFileInfo;
  Const aNewName: TffFullFileName;
  Const aExt: TffExtension);
Var
  S: TffFullFileName;
Begin
  FFVerifyFileInfo(aFI, False);
  With aFI^ Do
    Begin
      FFShStrFree(fiName);
      S := FFForceExtension(FFExpandFileName(aNewName), aExt);
      fiName := FFShStrAlloc(S);
    End;
End;
{--------}

Function FFAllocFileInfo(Const aName: TffFullFileName;
  Const aExt: TffExtension;
  aBufMgr: TfsBufferManager): PffFileInfo;
Var
  S: String;
Begin
  FFGetMem(Result, sizeof(TffFileInfo));
  Try
    FillChar(Result^, sizeof(TffFileInfo), 0);
    With Result^ Do
      Begin
        fiVerify := VerificationValue;
        fiHandle := INVALID_HANDLE_VALUE;
        S := FFForceExtension(FFExpandFileName(aName), aExt);
        fiName := FFShStrAlloc(S);
        fiBufMgr := aBufMgr;
        fiMaxBlocks := 0;
        fiRecordLocks := Nil;
        fiExclOwner := fsc_W32NoValue;
        fiAttributes := [];
        fiTempStore := Nil;
      End;
  Except
    FFFreeFileInfo(Result);
    Raise;
  End; {try..except}
End;
{--------}

Procedure FFVerifyFileHeaderSignature(aFI: PffFileInfo; Const signature: Longint);
Begin
  If signature <> fsc_SigHeaderBlock Then
    {Not a FS File header}
    FSRaiseExceptionNoData(EfsServerException, fsStrResServer, fserrNotAnFFFile);
End;
{--------}

Procedure FFVerifyFileInfo(aFI: PffFileInfo; IsOpen: boolean);
Begin
  If IsOpen Then {should be open}
    Begin
      If Assigned(aFI) And
        (aFI^.fiVerify = VerificationValue) And
        Assigned(aFI^.fiName) And
        (aFI^.fiHandle <> INVALID_HANDLE_VALUE) Then
        Exit;
      FSRaiseExceptionNoData(EfsServerException, fsStrResServer, fserrBadStruct);
    End
  Else {should be closed}
    Begin
      If Assigned(aFI) And
        (aFI^.fiVerify = VerificationValue) And
        Assigned(aFI^.fiName) And
        (aFI^.fiHandle = INVALID_HANDLE_VALUE) Then
        Exit;
      FSRaiseExceptionNoData(EfsServerException, fsStrResServer, fserrBadStruct);
    End
End;
{====================================================================}

{===File access routines=============================================}

Procedure FFCloseFile(aFI: PffFileInfo);
Begin
  FFVerifyFileInfo(aFI, True);
  If Not (fffaTemporary In aFI^.fiAttributes) Then
    FFCloseFilePrim(aFI);
  With aFI^ Do
    Begin
      fiHandle := INVALID_HANDLE_VALUE;
      fiBLOBrscMgr.Free;
      fiBLOBrscMgr := Nil;
      fiRecordLocks.Free;
      fiRecordLocks := Nil;
    End;
End;
{--------}

Function FFFileIsOpen(aFI: PffFileInfo): boolean;
Begin
  Try
    FFVerifyFileInfo(aFI, True);
    Result := aFI^.fiHandle <> INVALID_HANDLE_VALUE;
  Except
    Result := False;
  End; {try..except}
End;
{--------}

Procedure FFFlushFile(aFI: PffFileInfo);
Begin
  FFVerifyFileInfo(aFI, True);
  If Not (fffaTemporary In aFI^.fiAttributes) Then
    FFFlushFilePrim(aFI);
End;
{--------}

Procedure FFForceFlushFile(aFI: PffFileInfo);
Begin
  FFVerifyFileInfo(aFI, True);
  If Not (fffaTemporary In aFI^.fiAttributes) Then
    Begin
      FFCloseFilePrim(aFI);
      With aFI^ Do
        {Begin !!.05}
        Try
          fiHandle := FFOpenFilePrim(@fiName^[1], fiOpenMode, fiShareMode,
            False, False);
        Except
          { Re-attempt in event of failure. The failure could have occurred
            due to a timing issue (i.e., OS still thinks file is open). }
          Sleep(ciReopenSleep); {!!.06}
          fiHandle := FFOpenFilePrim(@fiName^[1], fiOpenMode, fiShareMode,
            False, False);
        End;
      {End !!.05}
    End;
End;
{--------}

Function FFGetPositionFile(aFI: PffFileInfo): TffInt64;
Begin
  FFVerifyFileInfo(aFI, True);
  Result := FFGetPositionFilePrim(aFI);
End;
{--------}

Function FFGetFileSize(aFI: PffFileInfo): TffInt64;
Var
  CurPos: TffInt64;
Begin
  FFVerifyFileInfo(aFI, True);
  CurPos := FFGetPositionFilePrim(aFI);
  Result := FFPositionFileEOFPrim(aFI);
  FFPositionFilePrim(aFI, CurPos);
End;
{--------}

Procedure FFOpenFile(aFI: PffFileInfo;
  aOpenMode: TffOpenMode;
  aShareMode: TffShareMode;
  aWriteThru: boolean;
  aCreateFile: boolean);
Var
  Attr: Integer;
Begin
  FFVerifyFileInfo(aFI, False);
  With aFI^ Do
    Begin
      { Is this a temporary file? }
      If fffaTemporary In fiAttributes Then
        { Yes. Obtain a fake file handle. }
        fiHandle := THandle(aFI)
      Else
        Begin
          { No. Are we creating the file? }
          If Not aCreateFile Then
            Begin
              { No. Is the existing file marked read-only? }
              {$IFDEF DCC6OrLater}
              {$WARN SYMBOL_PLATFORM OFF}
              {$ENDIF}
              Attr := FileGetAttr(fiName^);
              If ((Attr And faReadOnly) <> 0) Then
                Begin
                  { Yes. Force the file to be opened in read-only shared mode. }
                  aOpenMode := omReadOnly;
                  aShareMode := smShared; {!!.10}
                End;
            End;
          {$IFDEF DCC6OrLater}
          {$WARN SYMBOL_PLATFORM ON}
          {$ENDIF}
          fiHandle := FFOpenFilePrim(@fiName^[1], aOpenMode, aShareMode, aWriteThru, aCreateFile);
        End;
      fiOpenMode := aOpenMode;
      fiShareMode := aShareMode;
      fiWriteThru := aWriteThru;
    End;
End;
{--------}

Procedure FFPositionFile(aFI: PffFileInfo;
  Const aOffset: TffInt64);
Begin
  FFVerifyFileInfo(aFI, True);
  FFPositionFilePrim(aFI, aOffset);
End;
{--------}

Function FFPositionFileEOF(aFI: PffFileInfo): TffInt64;
Begin
  FFVerifyFileInfo(aFI, True);
  Result := FFPositionFileEOFPrim(aFI);
End;
{--------}

Function FFReadFile(aFI: PffFileInfo;
  aToRead: TffWord32;
  Var aBuffer): TffWord32;
Begin
  FFVerifyFileInfo(aFI, True);
  Result := FFReadFilePrim(aFI, aToRead, aBuffer);
End;
{--------}

Procedure FFReadFileExact(aFI: PffFileInfo;
  Const aToRead: TffWord32;
  Var aBuffer);
Begin
  FFVerifyFileInfo(aFI, True);
  If FFReadFilePrim(aFI, aToRead, aBuffer) <> aToRead Then
    Begin
      FSRaiseException(EfsServerException, fsStrResServer, fserrReadExact, [aFI^.fiName^, aToRead]);
    End;
End;
{--------}

Procedure FFReadFileExactAt(aFI: PffFileInfo;
  Const aOffset: TffInt64;
  aToRead: TffWord32;
  Var aBuffer);
Begin
  {note: this routine is not thread safe: the file handle is
   available to many threads, and the file pointer is handle-
   relative not thread-relative}
  FFVerifyFileInfo(aFI, True);
  FFPositionFilePrim(aFI, aOffset);
  If FFReadFilePrim(aFI, aToRead, aBuffer) <> aToRead Then
    Begin
      FSRaiseException(EfsServerException, fsStrResServer, fserrReadExact, [aFI^.fiName^, aToRead]);
    End;
End;
{--------}

Procedure FFSetEOF(aFI: PffFileInfo;
  Const aOffset: TffInt64);
Begin
  FFVerifyFileInfo(aFI, True);
  FFSetEOFPrim(aFI, aOffset);
End;
{--------}

Function FFWriteFile(aFI: PffFileInfo;
  aToWrite: TffWord32;
  Const aBuffer): TffWord32;
Begin
  FFVerifyFileInfo(aFI, True);
  Result := FFWriteFilePrim(aFI, aToWrite, aBuffer);
End;
{--------}

Procedure FFWriteFileExact(aFI: PffFileInfo;
  aToWrite: TffWord32;
  Const aBuffer);
Begin
  FFVerifyFileInfo(aFI, True);
  If (FFWriteFilePrim(aFI, aToWrite, aBuffer) <> aToWrite) Then
    Begin
      FSRaiseException(EfsServerException, fsStrResServer, fserrWriteExact, [aFI^.fiName^, aToWrite]);
    End;
End;
{--------}

Procedure FFWriteFileExactAt(aFI: PffFileInfo;
  Const aOffset: TffInt64;
  aToWrite: TffWord32;
  Const aBuffer);
Begin
  {note: this routine is not thread safe: the file handle is
   available to many threads, and the file pointer is handle-
   relative not thread-relative}
  FFVerifyFileInfo(aFI, True);
  FFPositionFilePrim(aFI, aOffset);
  If (FFWriteFilePrim(aFI, aToWrite, aBuffer) <> aToWrite) Then
    Begin
      FSRaiseException(EfsServerException, fsStrResServer, fserrWriteExact, [aFI^.fiName^, aToWrite]);
    End;
End;
{--------}

Function FFCalcMaxFileSize(aFI: PffFileInfo): TffInt64;
Var
  MaxFileNameLen: DWord;
  FileSysFlags: Dword;
  FileSysName: Array[0..MAX_PATH - 1] Of AnsiChar;
  VolumeName: Array[0..MAX_PATH - 1] Of AnsiChar;
  OSVersion: TOSVersionInfo;
  OSNumber: Byte;
  FileDrive: String;
Begin
  OSVersion.dwOSVersionInfoSize := SizeOf(OSVersion);
  GetVersionEx(OSVersion);
  If OSVersion.dwPlatformId = 1 Then
    Begin
      If OSVersion.dwMinorVersion = 0 Then
        OSNumber := 1 {Win95}
      Else
        OSNumber := 2; {Win98}
    End
  Else {OSVersion.dwPlatformID = 2}
    Begin
      If OSVersion.dwMajorVersion = 3 Then
        OSNumber := 3 {WinNT 3.51}
      Else If OSVersion.dwMajorVersion = 4 Then
        OSNumber := 4 {WinNT 4}
      Else
        OSNumber := 5; {Win2K}
    End;
  FileDrive := PChar(ExtractFileDrive(aFI^.fiName^));
  FileDrive := FileDrive + '\';
  If GetVolumeInformation(PChar(FileDrive), VolumeName, Length(VolumeName), Nil, Maxfilenamelen, FileSysFlags, FileSysName, SizeOf(
    FileSysName)) Then
    Begin
      {!! check on other possibilites for types of filesystems}
      If FileSysName = 'FAT32' Then
        Begin
          If OSNumber = 5 Then
            Begin
              {Win2K max FAT32 partition = 8TB, but only 4GB files}
              Result.iLow := fscl_FourGigabytes;
              Result.iHigh := 0;
            End
          Else
            Begin
              {Win95/98 max FAT32 partition size = (4GB - 2 bytes)}
              Result.iLow := fscl_FourGigabytes;
              Result.iHigh := 0;
            End;
        End
      Else If FileSysName = 'NTFS' Then
        Begin
          {NTFS max file size is 2^64}
          Result.iLow := fsc_W32NoValue;
          Result.iHigh := fsc_W32NoValue;
        End
      Else If FileSysName = 'FAT16' Then
        Begin
          If OSNumber >= 4 Then
            Begin
              {NT max FAT16 partition = 4GB; Max File Size = 2GB }
              Result.iLow := fscl_TwoGigabytes;
              Result.iHigh := 0;
            End
          Else
            Begin
              {Win95/98 max FAT16 partition = 2GB}
              Result.iLow := fscl_TwoGigabytes;
              Result.iHigh := 0;
            End;
        End
      Else If FileSysName = 'CDFS' Then
        Begin
          {Can't write to a CD-ROM drive}
          Result.iLow := 0;
          Result.iHigh := 0;
        End
      Else If FileSysName = 'FAT' Then
        Begin
          If FileDrive = 'A:\' Then
            Begin
              {1.44 floppy}
              Result.iLow := fscl_MaxHDFloppy;
              Result.iHigh := 0;
            End
          Else
            Begin
              {Any other FAT drive}
              Result.iLow := fscl_TwoGigabytes;
              Result.iHigh := 0;
            End;
        End;
    End
  Else
    Begin
      Result.iLow := 0;
      Result.iHigh := 0;
    End;
End;
{--------}

Function FFCalcMaxBLOBSegSize(aFI: PffFileInfo): TffWord32;
Begin
  {calc max segment size: excluding the segment header}
  Result := (((aFI^.fiBlockSize - fsc_BlockHeaderSizeBLOB - fsc_BLOBSegmentHeaderSize)
    Div fsc_BLOBSegmentIncrement) * fsc_BLOBSegmentIncrement);
End;
{====================================================================}

{===Encrypted file routines==========================================}

Procedure FFReadDecryptFileExact(aFI: PffFileInfo;
  aToRead: TffWord32;
  Var aBuffer);
Begin
  FFReadFileExact(aFI, aToRead, aBuffer);
  If aFI^.fiEncrypted Then
    If aFI^.fiForServer Then
      FFDecodeBlockServer(@aBuffer, aToRead, 0)
    Else
      FFDecodeBlock(@aBuffer, aToRead, 0);
End;
{--------}

Procedure FFReadDecryptFileExactAt(aFI: PffFileInfo;
  Const aOffset: TffInt64;
  aToRead: TffWord32;
  Var aBuffer);
Var
  tmpOffset: TffWord32;
Begin
  FFReadFileExactAt(aFI, aOffset, aToRead, aBuffer);
  tmpOffset := aOffset.iLow;
  If ((aOffset.iHigh <> 0) Or (tmpOffset <> 0)) And aFI^.fiEncrypted Then
    If aFI^.fiForServer Then
      FFDecodeBlockServer(@aBuffer, aToRead, tmpOffset)
    Else
      FFDecodeBlock(@aBuffer, aToRead, tmpOffset);
End;
{--------}

Procedure FFWriteEncryptFileExact(aFI: PffFileInfo;
  aToWrite: TffWord32;
  Var aBuffer);
Begin
  FFVerifyFileInfo(aFI, True);
  If (EncryptBuffer = Nil) Then
    GetMem(EncryptBuffer, 64 * 1024);
  Move(aBuffer, EncryptBuffer^, aToWrite);
  If aFI^.fiEncrypted Then
    If aFI^.fiForServer Then
      FFCodeBlockServer(EncryptBuffer, aToWrite, 0)
    Else
      FFCodeBlock(EncryptBuffer, aToWrite, 0);
  If (FFWriteFilePrim(aFI, aToWrite, EncryptBuffer^) <> aToWrite) Then
    Begin
      FSRaiseException(EfsServerException, fsStrResServer, fserrWriteExact, [aFI^.fiName^, aToWrite]);
    End;
End;
{--------}

Procedure FFWriteEncryptFileExactAt(aFI: PffFileInfo;
  Const aOffset: TffInt64;
  aToWrite: TffWord32;
  Var aBuffer);
Var
  tmpOffset: TffWord32;
Begin
  FFVerifyFileInfo(aFI, True);
  tmpOffset := aOffset.iLow;
  If (EncryptBuffer = Nil) Then
    GetMem(EncryptBuffer, 64 * 1024);
  Move(aBuffer, EncryptBuffer^, aToWrite);
  If ((aOffset.iHigh <> 0) Or (tmpOffset <> 0)) And aFI^.fiEncrypted Then
    If aFI^.fiForServer Then
      FFCodeBlockServer(EncryptBuffer, aToWrite, tmpOffset)
    Else
      FFCodeBlock(EncryptBuffer, aToWrite, tmpOffset);
  FFPositionFilePrim(aFI, aOffset);
  If (FFWriteFilePrim(aFI, aToWrite, EncryptBuffer^) <> aToWrite) Then
    Begin
      FSRaiseException(EfsServerException, fsStrResServer, fserrWriteExact, [aFI^.fiName^, aToWrite]);
    End;
End;
{====================================================================}

{===Manager for list of files to flush===============================}
Type
  TffFlushList = Class(TFSSpecObject)
  Protected
    FList: TfsVCLList;
    Function GetCount: Integer;
  Public
    Constructor Create;
    Destructor Destroy; Override;
    Function Add(FI: PffFileInfo): boolean;
    Procedure Flush(aTrans: TfsSrcTransaction);
    Property Count: Integer Read GetCount;
  End;
  {--------}

Constructor TffFlushList.Create;
Begin
  Inherited Create;
  FList := TfsVCLList.Create;
End;
{--------}

Destructor TffFlushList.Destroy;
Begin
  FList.Free;
  Inherited Destroy;
End;
{--------}

Function TffFlushList.Add(FI: PffFileInfo): boolean;
Var
  i: Integer;
Begin
  { SPW - 11/7/2000 - Note that this routine is as optimized as possible.
    Turns out that accessing List[i] is about 4 times faster than accessing
    Items[i].  I tried replacing the use of TList with other list classes
    declared in FFLLBASE but it turns out they run slower than TList, even
    though the same kind of code is being executed.
    Interestingly, using TList.Items is much faster than TfsVCLList.Items even
    though the TList.Get method is being called in either case.  We haven't
    been able to figure out why.  Regardless, using TList.List or
    TfsVCLList.List gives us the fastest performance in this situation. }
  Result := False;
  For i := 0 To pred(Count) Do
    If FList.List[i] = pointer(FI) Then
      Exit;
  FList.Add(pointer(FI));
  Result := True;
End;
{--------}

Procedure TffFlushList.Flush(aTrans: TfsSrcTransaction);
Var
  CurrFile: PffFileInfo;
  Inx: Integer;
Begin
  For Inx := 0 To Pred(FList.Count) Do
    Begin
      CurrFile := PffFileInfo(FList[Inx]);
      {if block 0's LSN is less than the LSN of the current transaction,
       we need to change block 0's LSN to the current transaction's LSN}
      With CurrFile^ Do
        Begin
          If fiPageZero.LSN < aTrans.LSN Then
            Begin
              fiPageZero.MakeDirty(aTrans);
              fiPageZero.LSN := aTrans.LSN;
              fiPageZero.Commit(False);
            End;
        End;
      If aTrans.TransactionMode = tmFailSafe Then {!!.12}
        FFFlushFile(CurrFile); {!!.12}
      FFForceFlushFile(CurrFile);
    End;
End;
{--------}

Function TffFlushList.GetCount: Integer;
Begin
  Result := FList.Count;
End;
{====================================================================}

{===TffbmModifiedBlock=================================================}

Constructor TffbmModifiedBlock.Create(aRAMPage: TffbmRAMPage;
  aPrevBlock: TffbmModifiedBlock;
  aTransLevel: TfsSrcTransactionLevel); {!!.10}
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Assert(assigned(aRAMPage));
    Inherited Create;
    RAMPage := aRAMPage;
    TransLevel := aTransLevel;
    mbBlock := RAMPage.rpAllocBlockPrim(RAMPage.BlockSize);
    mbBlockNumTmp := fsc_W32NoValue;
    Prev := aPrevBlock;
    AddToTransLevel; {!!.10}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TffbmModifiedBlock.Destroy;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { The modified block may have been used to replace another block.  If the
      block is still available to us, free it. }
    If assigned(Block) Then
      RAMPage.rpFreeBlock(Block, RAMPage.BlockSize);
    Inherited Destroy;
    RemoveFromTransLevel; {!!.10}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmModifiedBlock.Copy(aBlock: PffBlock);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Move(aBlock^, Block^, RAMPage.BlockSize);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmModifiedBlock.CopyTo(aBlock: PffBlock);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Move(Block^, aBlock^, RAMPage.BlockSize);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.10}
{--------}

Procedure TffbmModifiedBlock.DecreaseTransLevel;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    RemoveFromTransLevel;
    TransLevel := TransLevel.tlPrev;
    Assert(Assigned(TransLevel));
    AddToTransLevel;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{End !!.10}
{--------}

Procedure TffbmModifiedBlock.FreeBlock;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    RAMPage.rpFreeBlock(Block, RAMPage.BlockSize);
    Block := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmModifiedBlock.mbGetBlock: PffBlock;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}

    If mbBlockNumTmp <> fsc_W32NoValue Then
      Begin
        Assert(mbBlock = Nil, 'Modified block still in memory');
        Assert(assigned(RAMPage.FileInfo^.fiTempStore), 'Temp storage not assigned');
        mbBlock := RAMPage.rpAllocBlockPrim(RAMPage.BlockSize);
        TfsBaseTempStorage(RAMPage.FileInfo^.fiTempStore).ReadBlock(mbBlockNumTmp, mbBlock);
        If TransLevel.Level < SizeOf(TffWord32) * 8 Then {!!.10}
          FFClearBit(@RAMPage.rpBlockBits, TransLevel.Level); {!!.10}
        mbBlockNumTmp := fsc_W32NoValue;
      End;
    Result := mbBlock;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.10}
{--------}

Procedure TffbmModifiedBlock.AddToTransLevel;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    mbTransLevelPrev := TransLevel.tlModifiedBlocksTail;
    TransLevel.tlModifiedBlocksTail := Self;

    { If there was a tail, make sure the old tail points to this page. }
    If Assigned(mbTransLevelPrev) Then
      mbTransLevelPrev.mbTransLevelNext := Self;

    { If this is the first page in the list, put self in the
      head position. }
    If Not Assigned(TransLevel.tlModifiedBlocksHead) Then
      TransLevel.tlModifiedBlocksHead := Self;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffbmModifiedBlock.RemoveFromTransLevel;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    { If this page is not at the tail then make sure the following page
      points back to the page before this page. }
    If Assigned(mbTransLevelNext) Then
      Begin
        mbTransLevelNext.mbTransLevelPrev := mbTransLevelPrev;
      End
    Else
      Begin
        { This page is at the tail.  The tail should now be the page before
          this page. }
        If TransLevel.tlModifiedBlocksTail = Self Then
          TransLevel.tlModifiedBlocksTail := mbTransLevelPrev;
      End;

    { The page before this page should point to the page following this page. }
    If Assigned(mbTransLevelPrev) Then
      Begin
        mbTransLevelPrev.mbTransLevelNext := mbTransLevelNext;
      End
    Else
      Begin
        { Otherwise we are at the head of the list so make sure the head points
          to the page following this page. }
        If TransLevel.tlModifiedBlocksHead = Self Then
          TransLevel.tlModifiedBlocksHead := mbTransLevelNext;
      End;
    mbTransLevelNext := Nil;
    mbTransLevelPrev := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{End !!.10}
{--------}

Procedure TffbmModifiedBlock.SendToTempStore;
Var
  aTmpStore: TfsBaseTempStorage;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Assert(mbBlockNumTmp = fsc_W32NoValue, 'Modified block already in temp store');
    aTmpStore := TfsBaseTempStorage(RAMPage.FileInfo^.fiTempStore);
    If Not aTmpStore.Full Then
      Begin
        mbBlockNumTmp := aTmpStore.WriteBlock(mbBlock);
        RAMPage.rpFreeBlock(mbBlock, RAMPage.BlockSize);
        mbBlock := Nil;
        If TransLevel.Level < SizeOf(TffWord32) * 8 Then {!!.10}
          FFSetBit(@RAMPage.rpBlockBits, TransLevel.Level); {!!.10}
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{===TffbmRAMPage=====================================================}

Constructor TffbmRAMPage.Create(aBufMgr: TfsBufferManager; aFI: PffFileInfo;
  Const aBlockNumber: TffWord32);
Begin
  {$IFDEF RAMPageCheck}
  Log('Create RAMPage %d', [aBlockNumber]);
  {$ENDIF}
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    FNew := True; {!!.11}
    rpBlockBits := 0;
    rpBlockListTail := Nil;
    rpBufMgr := aBufMgr;
    rpFI := aFI;
    rpBlockNum := aBlockNumber;
    rpBlockNumTmp := fsc_W32NoValue;
    BlockSize := aFI^.fiBlockSize;
    rpBlockSizeEnum := FFMapBlockSize(aFI^.fiBlockSize);
    FLastAccess := fscl_INFINITE; {!!.06}
    FRefCount := 0;
    If fffaTemporary In aFI^.fiAttributes Then
      rpReuseMode := ffrmTempStore
    Else
      rpReuseMode := ffrmUseAsIs;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TffbmRAMPage.Destroy;
Var
  aBlock: TffbmModifiedBlock;
Begin
  {$IFDEF RAMPageCheck}
  Log('Free RAMPage %d', [rpBlockNum]);
  {$ENDIF}
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Is the read-only block in temporary storage? }
    If rpBlockNumTmp <> fsc_W32NoValue Then
      Begin
        { Yes. Retrieve. }
        rpAllocBlock(rpBlockSize);
        TfsBaseTempStorage(rpFI^.fiTempStore).ReadBlock(rpBlockNumTmp, rpBlock);
      End;

    { Free the block. }
    BlockSize := 0;
    While assigned(rpBlockListTail) Do
      Begin
        aBlock := rpBlockListTail;
        rpBlockListTail := rpBlockListTail.Prev;
        aBlock.Free;
      End;
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.AddToFilePageList;
Var
  pc1: PffPageContainer;
  pc2: PffPageContainer;
  pc3: PffPageContainer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Insert self into the list of RAM pages maintained by the
      RAM pages themselves.  Add the page to the tail of the list of RAM
      pages maintained by the file structure. }
    rpFilePrev := rpFI^.fiPageListTail;
    rpFI^.fiPageListTail := Self;
    If Assigned(rpFilePrev) Then
      rpFilePrev.rpFileNext := Self;
    { If this page is the first in the list then update
      the file's head pointer. }
    If Not Assigned(rpFI^.fiPageListHead) Then
      rpFI^.fiPageListHead := Self;

    { If this is the header page store it in a special field for quick access }
    If BlockNumber = 0 Then
      Begin
        Assert(Not Assigned(rpFI^.fiPageZero));
        rpFI^.fiPageZero := Self;
        Exit;
      End;

    { Walk through the tree to the spot where this page should be located. }
    pc1 := rpFI^.fiPages[TffBlockNum(rpBlockNum)[3]];
    If Not Assigned(pc1) Then
      Begin
        FFGetMem(pc1, sizeOf(TffPageContainer));
        FillChar(pc1^, SizeOf(pc1^), 0);
        pc1.pcNext := rpFI^.fiPageContainerList;
        If Assigned(pc1.pcNext) Then
          Begin
            Assert(Not Assigned(pc1.pcNext.pcPrev));
            pc1.pcNext.pcPrev := pc1;
          End;
        rpFI^.fiPageContainerList := pc1;
        rpFI^.fiPages[TffBlockNum(rpBlockNum)[3]] := pc1;
      End;

    pc2 := pc1.pcPages[TffBlockNum(rpBlockNum)[2]];
    If Not Assigned(pc2) Then
      Begin
        FFGetMem(pc2, sizeOf(TffPageContainer));
        FillChar(pc2^, SizeOf(pc2^), 0);
        pc2.pcNext := rpFI^.fiPageContainerList;
        If Assigned(pc2.pcNext) Then
          Begin
            Assert(Not Assigned(pc2.pcNext.pcPrev));
            pc2.pcNext.pcPrev := pc2;
          End;
        rpFI^.fiPageContainerList := pc2;
        pc1.pcPages[TffBlockNum(rpBlockNum)[2]] := pc2;
        Inc(pc1.pcCount);
      End;

    pc3 := pc2.pcPages[TffBlockNum(rpBlockNum)[1]];
    If Not Assigned(pc3) Then
      Begin
        FFGetMem(pc3, sizeOf(TffPageContainer));
        FillChar(pc3^, SizeOf(pc3^), 0);
        pc3.pcNext := rpFI^.fiPageContainerList;
        If Assigned(pc3.pcNext) Then
          Begin
            Assert(Not Assigned(pc3.pcNext.pcPrev));
            pc3.pcNext.pcPrev := pc3;
          End;
        rpFI^.fiPageContainerList := pc3;
        pc2.pcPages[TffBlockNum(rpBlockNum)[1]] := pc3;
        Inc(pc2.pcCount);
      End;

    { Add self to the leaf node. }
    Assert(Not Assigned(pc3.pcPages[TffBlockNum(rpBlockNum)[0]]));
    pc3.pcPages[TffBlockNum(rpBlockNum)[0]] := Self;
    Inc(pc3.pcCount);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.AddToRecycleList;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Assumption: rpInUsePrev already set to nil. }
    rpInUseNext := rpBufMgr.bmRecycleListHead;
    rpBufMgr.bmRecycleListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.AddToTransList(aTrans: TfsSrcTransaction);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Assert(assigned(aTrans));
    rpTransPrev := aTrans.trTransPageListTail;
    aTrans.trTransPageListTail := Self;
    If Assigned(rpTransPrev) Then
      rpTransPrev.rpTransNext := Self;
    If Not Assigned(aTrans.trTransPageListHead) Then
      aTrans.trTransPageListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.AddToUseList;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    rpInUsePrev := rpBufMgr.bmInUseListTail;
    rpBufMgr.bmInUseListTail := Self;

    { If there was a tail, make sure the old tail points to this page. }
    If Assigned(rpInUsePrev) Then
      rpInUsePrev.rpInUseNext := Self;

    { If this is the first page in the list, put self in the
      head position. }
    If Not Assigned(rpBufMgr.bmInUseListHead) Then
      rpBufMgr.bmInUseListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.Block(aTrans: TfsSrcTransaction;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
{$IFDEF RAMPageCheck}
Var
  PStr: Array[0..8] Of char;
  {$ENDIF}
Begin
  {$IFDEF RAMPageCheck}
  Log('Page %d: TffbmRAMPage.Block', [rpBlockNum]);
  {$ENDIF}
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { No transaction specified or this is a transaction other than the one
      that has modified this block? }
    If (Not assigned(aTrans)) Or (rpTrans <> aTrans) Then
      Begin
        { Yes.  Is the read-only block currently in temporary storage? }
        If rpBlockNumTmp <> fsc_W32NoValue Then
          { Yes. Retrieve from temp storage. }
          rpRetrieveFromTemp;
        { Return the read-only block. }
        Result := rpBlock;
        {$IFDEF RAMPageCheck}
        FFPointerAsHex(PStr, Result);
        Log('Page %d: Acq read-only block, ref Count %d, address %s',
          [rpBlockNum, FRefCount + 1, PStr]);
        {$ENDIF}
      End
    Else
      Begin
        { No.  Return the most-recent modification. }
        Result := rpBlockListTail.Block;
        {$IFDEF RAMPageCheck}
        FFPointerAsHex(PStr, Result);
        Log('Page %d: Acq modified block, ref count %d, address %s',
          [rpBlockNum, FRefCount + 1, PStr]);
        {$ENDIF}
      End;

    { Ensure the ram page is looking at the header of the retrieved block.}
    rpHeader := PffBlockCommonHeader(Result);

    { Increment the reference count. }
    InterlockedIncrement(FRefCount);
    aReleaseMethod := Self.Release;
    FLastAccess := GetTickCount;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.Commit(forceWrite: boolean): boolean;
Var
  anItem: TffbmModifiedBlock;
  aPrevItem: TffbmModifiedBlock;
  TempI64: TffInt64;
  {$IFDEF RAMPageCheck}
  PStr, PStr2: Array[0..8] Of char;
  {$ENDIF}
Begin
  {$IFDEF RAMPageCheck}
  Log('Commit page %d', [rpBlockNum]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Assumption: If transaction is being committed to disk then the transaction
      has obtained write access to the table content. }

    { Requirement: Must have been modified. }
    Assert(rpDirty);

    { Was the read-only block written to temporary storage? }
    If rpBlockNumTmp <> fsc_W32NoValue Then
      { Yes. Restore the read-only block so that things work out properly. }
      rpRetrieveFromTemp;

    { Assume we are not committing to disk. }
    Result := False;

    { Are we forcing commit to disk? }
    If forceWrite Then
      Begin
        { Yes.  Copy the most recently modified block to the read-only block. }
        rpReplaceBlock(rpBlockListTail.Block);
        rpHeader^.bchLSN := rpTrans.LSN;
        rpBlockListTail.Block := Nil;

        { If this is not a temporary file then write to disk. }
        If Not (fffaTemporary In rpFI^.fiAttributes) Then
          Begin
            TempI64.iLow := BlockNumber;
            TempI64.iHigh := 0;
            FFI64MultInt(TempI64, BlockSize, TempI64);
            FFWriteEncryptFileExactAt(FileInfo, TempI64, rpBlockSize, rpBlock^);
          End;

        { Get rid of all modified block versions. }
        While assigned(rpBlockListTail) Do
          Begin
            anItem := rpBlockListTail;
            rpBlockListTail := rpBlockListTail.Prev;
            anItem.Free;
          End;
        RemoveFromTransList(rpTrans);
        MakeClean;
        FNew := False; {!!.07}
        Result := True;
      End
    Else If rpGetTransLevel = rpTrans.TransLevel Then
      { Yes. Do we have more than one modified block? }
      If assigned(rpBlockListTail.Prev) Then
        Begin
          { Yes. Is the previous block one nest level behind the most recent
            block? }
          aPrevItem := rpBlockListTail.Prev;
          If aPrevItem.TransLevel = (rpBlockListTail.TransLevel.tlPrev) Then
            Begin
              { Yes.  Replace the previous block with the most recent block. }
              aPrevItem.FreeBlock;
              aPrevItem.Block := rpBlockListTail.Block;
              rpBlockListTail.Block := Nil;
              { Delete the most recent block. }
              rpBlockListTail.Free;
              rpBlockListTail := aPrevItem;
            End
          Else
            { No.  The previous block is two or more levels below us.  Decrement the
             nest level of the most recent block. }
            rpBlockListTail.DecreaseTransLevel; {!!.10}
        End
      Else
        Begin
          { No.  We have only 1 modified block. Is this block ready to be written
            to disk?  }
          If rpBlockListTail.TransLevel.Level = 0 Then
            Begin {!!.10}
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
              rpBlockListTail.Block := Nil;
              rpHeader^.bchLSN := rpTrans.LSN;

              { If this is not a temporary file then write to disk. }
              If Not (fffaTemporary In rpFI^.fiAttributes) Then
                Begin
                  TempI64.iLow := BlockNumber;
                  TempI64.iHigh := 0;
                  FFI64MultInt(TempI64, BlockSize, TempI64);
                  FFWriteEncryptFileExactAt(FileInfo, TempI64, rpBlockSize, rpBlock^);
                End;

              { Get rid of the modified block since it is no longer needed. }
              rpBlockListTail.Free;
              rpBlockListTail := Nil;
              RemoveFromTransList(rpTrans);
              MakeClean;
              FNew := False; {!!.07}
              Result := True;
            End
          Else
            rpBlockListTail.DecreaseTransLevel; {!!.10}
        End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.DirtiedForTrans(aTrans: TfsSrcTransaction): Boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := assigned(aTrans) And
      assigned(rpBlockListTail) And
      (rpTrans = aTrans) And
      (rpGetTransLevel = aTrans.TransLevel);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.MakeClean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    rpBlockListTail := Nil;
    rpTrans := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.MakeDirty(aTrans: TfsSrcTransaction);
Var
  anItem: TffbmModifiedBlock;
  aDatabase: TfsSrcDatabase;
Begin
  {$IFDEF RAMPageCheck}
  Log('Page %d: MakeDirty', [rpBlockNum]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Assumption: If already marked dirty then never marked dirty by a different
      transaction. }
    // jeœli jest jakaœ rozpoczêta lub ta sama transakcja to generoj bl¹d
    //tu powinna czekac na zakonczenie poprzedniej
    aDataBase := Nil;
    If (aTrans.FDatabaseID > 0) And (Not (aTrans.FDatabaseID = 4294967295)) Then
      aDatabase := TfsSrcDatabase(aTrans.FDatabaseID);
    If aDatabase <> Nil Then
      Begin
        If aDatabase.RecLocking In [tlPessimisticNoWait, tlPessimisticWait] Then
          Assert((rpTrans = Nil) Or (aTrans = rpTrans));
      End
    Else
      Assert((rpTrans = Nil) Or (aTrans = rpTrans));

    { Is this block already dirty? }
    If assigned(rpBlockListTail) Then
      Begin
        { Yes.  Does the transaction have a higher nesting level? }
        If rpGetTransLevel.Level < aTrans.TransLevel.Level Then
          Begin {!!.10}
            { Yes. Make a copy of the last modified block and add it to the list
              of modified blocks.  Assumption: There is at least one modified block
              in the modified block list. }
            anItem := TffbmModifiedBlock.Create(Self, rpBlockListTail, aTrans.TransLevel);

            { Copy the last modified block. }
            anItem.Copy(rpBlockListTail.Block);

            { Add the block to the list. }
            rpBlockListTail := anItem;
          End;
      End
    Else
      Begin
        { No.  Record the transaction. }
        rpTrans := aTrans;

        { Make a copy of the read-only block and add it to the modified block
          list. }
        rpBlockListTail := TffbmModifiedBlock.Create(Self, Nil, aTrans.TransLevel);
        { Is the read-only block currently in temporary storage? }
        If rpBlockNumTmp <> fsc_W32NoValue Then
          { Yes. Retrieve from temp storage. }
          rpRetrieveFromTemp;
        rpBlockListTail.Copy(rpBlock);
        AddToTransList(aTrans);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.MoveToEndOfTransList;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { If this page is followed by another page, the following page
      should point back to the page before this page. }
    If Assigned(rpTransNext) Then
      Begin
        rpTransNext.rpTransPrev := rpTransPrev;
      End
    Else
      { Otherwise this page is already at the end of the list so do nothing. }
      Exit;

    { If a page precedes this page then it should point to the page following
      this page. }
    If Assigned(rpTransPrev) Then
      Begin
        rpTransPrev.rpTransNext := rpTransNext;
      End
    Else
      Begin
        { Otherwise we are at the head of the list so the head should point to
          the page following this page. }
        If rpTrans.trTransPageListHead = Self Then
          rpTrans.trTransPageListHead := rpTransNext;
      End;

    { The page at the end of the list should now point to this page. }
    rpTransPrev := rpTrans.trTransPageListTail;
    rpTrans.trTransPageListTail := Self;
    rpTransNext := Nil;
    If Assigned(rpTransPrev) Then
      rpTransPrev.rpTransNext := Self;
    If Not Assigned(rpTrans.trTransPageListHead) Then
      rpTrans.trTransPageListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.MoveToEndOfUseList;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {Begin !!.01}
      { Already at end of list? }
    If rpInUseNext = Nil Then
      { Yes. Exit. }
      Exit;

    { Point the following page to the page before this page. }
    rpInUseNext.rpInUsePrev := rpInUsePrev;

    { If a page precedes this page then it should point to the page following
      this page. }
    If Assigned(rpInUsePrev) Then
      Begin
        rpInUsePrev.rpInUseNext := rpInUseNext;
      End
    Else
      Begin
        { Otherwise we are at the head of the list so the head should point to
          the page following this page. }
        If rpBufMgr.bmInUseListHead = Self Then
          rpBufMgr.bmInUseListHead := rpInUseNext;
      End;

    { The page at the end of the list should now point to this page. }
    rpInUsePrev := rpBufMgr.bmInUseListTail;
    rpBufMgr.bmInUseListTail := Self;
    rpInUseNext := Nil;
    If Assigned(rpInUsePrev) Then
      rpInUsePrev.rpInUseNext := Self;
    If rpBufMgr.bmInUseListHead = Nil Then
      rpBufMgr.bmInUseListHead := Self;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.MoveToRecycleList;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    RemoveFromUseList;
    AddToRecycleList;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.Release(Var aBlock: PffBlock);
{$IFDEF RAMPageCheck}
Var
  Pstr: Array[0..8] Of char;
  {$ENDIF}
Begin
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
  Assert((FRefCount > 0) And
    ((aBlock = rpBlock) Or
    (assigned(rpBlockListTail) And {!!.10}
    ((aBlock = rpBlockListTail.Block) Or {!!.10}
    (assigned(rpBlockListTail.Prev) And {!!.10}
    (aBlock = rpBlockListTail.Prev.Block)))))); {!!.10}
  aBlock := Nil;
  InterlockedDecrement(FRefCount);
  //  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
End;
{--------}

Function TffbmRAMPage.Removable(Var RemoveMode: TffbmPageReuseMode): boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    RemoveMode := rpReuseMode;
    Result := False;

    { Can't be removed if this block is dirty, it is block zero, or it is
      actively used by one or more threads. }
    If assigned(rpBlockListTail) Or
      (rpBlockNum = 0) Or
      ((FRefCount > 0) And ((GetTickCount - FLastAccess) < fscl_PageLife)) Then
      Exit;

    { The page may be re-used if it cannot be sent to temporary storage. }
    Result := (rpReuseMode <> ffrmTempStore);

    If Result Then
      Exit
    Else
      Begin
        { Otherwise, it can be sent to temp storage. It can be re-used if the page
          is not already in temp storage and temp storage contains room for
          the page. }
        Result := (Not rpGetInTempStore);
        If Result Then
          If assigned(rpBlockListTail) Then
            Result := Result And
              (TfsBaseTempStorage(rpFI^.fiTempStore).HasSpaceFor
              (2 + rpBlockListTail.TransLevel.Level)) {!!.10}
          Else
            Result := Result And (Not TfsBaseTempStorage(rpFI^.fiTempStore).Full);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.RemoveFromFilePageList;
Var
  pc1: PffPageContainer;
  pc2: PffPageContainer;
  pc3: PffPageContainer;
Begin
  {$IFDEF RAMPageCheck}
  Log('Page %d: RemoveFromFilePageList', [rpBlockNum]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}

    { Remove self from the list of RAM pages maintained by the RAM pages.
      Remove self from the list of RAM pages maintained by the file
      structure. }
    If Assigned(rpFileNext) Then
      Begin
        rpFileNext.rpFilePrev := rpFilePrev;
      End
    Else
      Begin
        If rpFI^.fiPageListTail = Self Then
          rpFI^.fiPageListTail := rpFilePrev;
      End;
    If Assigned(rpFilePrev) Then
      Begin
        rpFilePrev.rpFileNext := rpFileNext;
      End
    Else
      Begin
        If rpFI^.fiPageListHead = Self Then
          rpFI^.fiPageListHead := rpFileNext;
      End;
    rpFileNext := Nil;
    rpFilePrev := Nil;

    { If this is the header page it was stored it in a special field for quick access }
    If BlockNumber = 0 Then
      Begin
        Assert(rpFI^.fiPageZero = Self);
        rpFI^.fiPageZero := Nil;
        Exit;
      End;

    { Remove ourselves from the file's RAM pages structure. }
    pc1 := rpFI^.fiPages[TffBlockNum(rpBlockNum)[3]];
    Assert(Assigned(pc1));
    If Not Assigned(pc1) Then
      Exit;

    pc2 := pc1.pcPages[TffBlockNum(rpBlockNum)[2]];
    Assert(Assigned(pc2));
    If Not Assigned(pc2) Then
      Exit;

    pc3 := pc2.pcPages[TffBlockNum(rpBlockNum)[1]];
    Assert(Assigned(pc3));
    If Not Assigned(pc3) Then
      Exit;

    Assert(pc3.pcPages[TffBlockNum(rpBlockNum)[0]] = Self);
    pc3.pcPages[TffBlockNum(rpBlockNum)[0]] := Nil;
    Dec(pc3.pcCount);

    { remove the the page container if no longer used }
    If pc3.pcCount = 0 Then
      Begin
        { is this the first page container in the list? }
        If Not Assigned(pc3.pcPrev) Then
          Begin
            { yes... this page container must be the head of the list}
            Assert(rpFI^.fiPageContainerList = pc3);
            rpFI^.fiPageContainerList := pc3.pcNext;
          End
        Else
          Begin
            { no... the previous page container must reference this page container}
            Assert(pc3.pcPrev.pcNext = pc3);
            pc3.pcPrev.pcNext := pc3.pcNext;
          End;
        { is there a page container after this one? }
        If Assigned(pc3.pcNext) Then
          Begin
            { yes... the next page container must reference this page container}
            Assert(pc3.pcNext.pcPrev = pc3);
            pc3.pcNext.pcPrev := pc3.pcPrev;
          End;

        { free the page container }
        FFFreeMem(pc3, sizeOf(TffPageContainer));

        { remove this page container from its parent }
        pc2.pcPages[TffBlockNum(rpBlockNum)[1]] := Nil;
        Dec(pc2.pcCount);

        { remove the the page container if no longer used }
        If pc2.pcCount = 0 Then
          Begin
            { is this the first page container in the list? }
            If Not Assigned(pc2.pcPrev) Then
              Begin
                { yes... this page container must be the head of the list}
                Assert(rpFI^.fiPageContainerList = pc2);
                rpFI^.fiPageContainerList := pc2.pcNext;
              End
            Else
              Begin
                { no... the previous page container must reference this page container}
                Assert(pc2.pcPrev.pcNext = pc2);
                pc2.pcPrev.pcNext := pc2.pcNext;
              End;
            { is there a page container after this one? }
            If Assigned(pc2.pcNext) Then
              Begin
                { yes... the next page container must reference this page container}
                Assert(pc2.pcNext.pcPrev = pc2);
                pc2.pcNext.pcPrev := pc2.pcPrev;
              End;

            { free the page container }
            FFFreeMem(pc2, sizeOf(TffPageContainer));

            { remove this page container from its parent }
            pc1.pcPages[TffBlockNum(rpBlockNum)[2]] := Nil;
            Dec(pc1.pcCount);

            { remove the the page container if no longer used }
            If pc1.pcCount = 0 Then
              Begin
                { is this the first page container in the list? }
                If Not Assigned(pc1.pcPrev) Then
                  Begin
                    { yes... this page container must be the head of the list}
                    Assert(rpFI^.fiPageContainerList = pc1);
                    rpFI^.fiPageContainerList := pc1.pcNext;
                  End
                Else
                  Begin
                    { no... the previous page container must reference this page container}
                    Assert(pc1.pcPrev.pcNext = pc1);
                    pc1.pcPrev.pcNext := pc1.pcNext;
                  End;
                { is there a page container after this one? }
                If Assigned(pc1.pcNext) Then
                  Begin
                    { yes... the next page container must reference this page container}
                    Assert(pc1.pcNext.pcPrev = pc1);
                    pc1.pcNext.pcPrev := pc1.pcPrev;
                  End;

                { free the page container }
                FFFreeMem(pc1, sizeOf(TffPageContainer));

                { remove this page container from its parent }
                rpFI^.fiPages[TffBlockNum(rpBlockNum)[3]] := Nil;
              End;
          End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.RemoveFromRecycleList;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    rpBufMgr.bmRecycleListHead := rpInUseNext;
    rpInUseNext := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.RemoveFromTransList(aTrans: TfsSrcTransaction);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If Assigned(rpTransNext) Then
      Begin
        rpTransNext.rpTransPrev := rpTransPrev;
      End
    Else
      Begin
        If aTrans.trTransPageListTail = Self Then
          aTrans.trTransPageListTail := rpTransPrev;
      End;
    If Assigned(rpTransPrev) Then
      Begin
        rpTransPrev.rpTransNext := rpTransNext;
      End
    Else
      Begin
        If aTrans.trTransPageListHead = Self Then
          aTrans.trTransPageListHead := rpTransNext;
      End;
    rpTransNext := Nil;
    rpTransPrev := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.RemoveFromUseList;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { If this page is not at the tail then make sure the following page
      points back to the page before this page. }
    If Assigned(rpInUseNext) Then
      Begin
        rpInUseNext.rpInUsePrev := rpInUsePrev;
      End
    Else
      Begin
        { This page is at the tail.  The tail should now be the page before
          this page. }
        If rpBufMgr.bmInUseListTail = Self Then
          rpBufMgr.bmInUseListTail := rpInUsePrev;
      End;

    { The page before this page should point to the page following this page. }
    If Assigned(rpInUsePrev) Then
      Begin
        rpInUsePrev.rpInUseNext := rpInUseNext;
      End
    Else
      Begin
        { Otherwise we are at the head of the list so make sure the head points
          to the page following this page. }
        If rpBufMgr.bmInUseListHead = Self Then
          rpBufMgr.bmInUseListHead := rpInUseNext;
      End;
    rpInUseNext := Nil;
    rpInUsePrev := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.ReadOnlyBlock: PffBlock;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := rpBlock;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.Reusable(Var ReuseMode: TffbmPageReuseMode): boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    ReuseMode := rpReuseMode;
    Result := False;

    { Can't be removed if this is block zero or it is actively used by one or
      more threads. }
    If (rpBlockNum = 0) Or
      ((FRefCount > 0) And ((GetTickCount - FLastAccess) < fscl_PageLife)) Then
      Exit;

    { Can this page be sent to temporary storage? }
    If (rpReuseMode = ffrmTempStore) Then
      Begin
        { Yes. We can re-use the page if it is not already in temporary storage
          & temporary storage contains room for the page & its blocks. }
        Result := (Not rpGetInTempStore);
        If Result Then
          If assigned(rpBlockListTail) Then
            Result := Result And
              (TfsBaseTempStorage(rpFI^.fiTempStore).HasSpaceFor
              (2 + rpBlockListTail.TransLevel.Level)) {!!.10}
          Else
            Result := Result And (Not TfsBaseTempStorage(rpFI^.fiTempStore).Full);
      End
    Else
      { No. Page may be re-used if it is clean. }
      Result := (rpBlockListTail = Nil);

  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.Rollback;
Var
  aBlock: TffbmModifiedBlock;
Begin
  {$IFDEF RAMPageCheck}
  Log('Page %d: Rollback', [rpBlockNum]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Requirement: Must have been dirtied. }
    Assert(assigned(rpBlockListTail));

    { Does this block's nest level match that of the transaction's? }
    If rpGetTransLevel = rpTrans.TransLevel Then
      Begin
        { Yes.  Is this nest level zero? }
        If rpTrans.TransLevel.Level = 0 Then
          Begin {!!.10}
            { Yes. Assume this is the only block in the modified block list.
              Get rid of the modified block. }
            rpBlockListTail.Free;
            rpBlockListTail := Nil;
            {Begin !!.07}
            If FNew Then
              Begin
                RemoveFromFilePageList;
                RemoveFromTransList(rpTrans);
                RemoveFromUseList;
                AddToRecycleList;
                FileInfo := Nil;
              End
            Else
              RemoveFromTransList(rpTrans);
            rpTrans := Nil;
            {End !!.07}
          End
        Else
          Begin
            { No.  Get rid of the last modified block. }
            aBlock := rpBlockListTail.Prev;
            rpBlockListTail.Free;
            rpBlockListTail := aBlock;
            If Not assigned(rpBlockListTail) Then
              RemoveFromTransList(rpTrans);
          End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.rpAllocBlock(aBlockSize: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    rpBlockSize := aBlockSize;
    If (rpBlockSize <> 0) Then
      Begin
        rpBlock := rpAllocBlockPrim(rpBlockSize);
        rpHeader := PffBlockCommonHeader(rpBlock);
      End
    Else
      Begin
        rpBlock := Nil;
        rpHeader := Nil;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.rpAllocBlockPrim(aBlockSize: Longint): PffBlock;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := Nil;
    If (aBlockSize <> 0) Then
      Begin
        Case aBlockSize Of
          4 * 1024:
            Begin
              If (Pool4K = Nil) Then
                Pool4K := TfsMemoryPool.Create(4 * 1024, 1);
              Result := Pool4K.Alloc;
            End;
          8 * 1024:
            Begin
              If (Pool8K = Nil) Then
                Pool8K := TfsMemoryPool.Create(8 * 1024, 1);
              Result := Pool8K.Alloc;
            End;
          16 * 1024:
            Begin
              If (Pool16K = Nil) Then
                Pool16K := TfsMemoryPool.Create(16 * 1024, 1);
              Result := Pool16K.Alloc;
            End;
          32 * 1024:
            Begin
              If (Pool32K = Nil) Then
                Pool32K := TfsMemoryPool.Create(32 * 1024, 1);
              Result := Pool32K.Alloc;
            End;
          64 * 1024:
            Begin
              If (Pool64K = Nil) Then
                Pool64K := TfsMemoryPool.Create(64 * 1024, 1);
              Result := Pool64K.Alloc;
            End;
          Else
            GetMem(Result, aBlockSize);
        End; {case}
        rpBufMgr.bmIncreaseRAMDetail(aBlockSize);
        FillChar(Result^, aBlockSize, 'F');
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.rpDirty: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := assigned(rpBlockListTail);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.rpFreeBlock(aBlock: PffBlock; aBlockSize: Longint);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If (rpBlockSize <> 0) And assigned(aBlock) Then
      Begin
        Case aBlockSize Of
          4 * 1024: Pool4K.Dispose(aBlock);
          8 * 1024: Pool8K.Dispose(aBlock);
          16 * 1024: Pool16K.Dispose(aBlock);
          32 * 1024: Pool32K.Dispose(aBlock);
          64 * 1024: Pool64K.Dispose(aBlock);
          Else
            FreeMem(aBlock, aBlockSize);
        End; {case}
        rpBufMgr.bmDecreaseRAMDetail(aBlockSize);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.rpGetInTempStore: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := (rpBlockNumTmp <> fsc_W32NoValue) Or
      (rpBlockBits > 0);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.rpGetLSN: TffWord32;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Has this page been dirtied by a transaction? }
    If assigned(rpTrans) Then
      { Yes.  Return the transaction's LSN. }
      Result := rpTrans.LSN
    Else
      { No.  Return the LSN of the read-only block. }
      Result := rpHeader^.bchLSN;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TffbmRAMPage.rpGetTransLevel: TfsSrcTransactionLevel; {!!.10}
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If assigned(rpBlockListTail) Then
      Result := rpBlockListTail.TransLevel
    Else
      Result := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.rpRelease(aBlock: PffBlock);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Assert((FRefCount > 0) And
      ((aBlock = rpBlock) Or (aBlock = rpBlockListTail.Block)));
    InterlockedDecrement(FRefCount);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.rpReplaceBlock(aNewBlock: PffBlock);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    rpFreeBlock(rpBlock, rpBlockSize);
    rpBlock := aNewBlock;
    rpHeader := PffBlockCommonHeader(rpBlock);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.rpRetrieveFromTemp;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    rpAllocBlock(rpBlockSize);
    TfsBaseTempStorage(rpFI^.fiTempStore).ReadBlock(rpBlockNumTmp, rpBlock);
    rpBlockNumTmp := fsc_W32NoValue;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.rpSetBlockSize(aBlockSize: Longint);
Var
  aBlock: TffbmModifiedBlock;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Is the read-only page in temporary storage? }
    If rpBlockNumTmp <> fsc_W32NoValue Then
      Begin
        TfsBaseTempStorage(rpFI^.fiTempStore).ReleaseBlock(rpBlockNumTmp); {!!.01}
        rpBlockNumTmp := fsc_W32NoValue;
      End;

    { Are there any modified blocks? If so, free them. This ensures they
      are removed from temporary storage. }
    While assigned(rpBlockListTail) Do
      Begin
        aBlock := rpBlockListTail;
        rpBlockListTail := rpBlockListTail.Prev;
        aBlock.Free;
      End;

    If aBlockSize <> rpBlockSize Then
      Begin
        rpFreeBlock(rpBlock, rpBlockSize);
        rpAllocBlock(aBlockSize);
      End
    Else
      FillChar(rpBlock^, rpBlockSize, 'F');
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.rpSetLSN(Const aLSN: TffWord32);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    rpHeader^.bchLSN := aLSN;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.SendToTempStore;
Var
  aBlock: TffbmModifiedBlock;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Requirement: Must be clean & must not already be in temporary
      storage. }
    Assert(assigned(rpBlock));

    { Send the read-only block to temp storage. }
    rpBlockNumTmp := TfsBaseTempStorage(rpFI^.fiTempStore).WriteBlock(rpBlock);
    rpFreeBlock(rpBlock, rpBlockSize);
    rpBlock := Nil;

    { Send all modified blocks to temp storage. }
    aBlock := rpBlockListTail;
    While assigned(aBlock) Do
      Begin
        aBlock.SendToTempStore;
        aBlock := rpBlockListTail.Prev;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TffbmRAMPage.rpSetFI(FI: PffFileInfo);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    FNew := False; {!!.07}
    If (FI <> rpFI) Then
      Begin
        { If the file is being set to nil, we need to clear it: it's
          about to be recycled. }
        If (FI = Nil) Then
          Begin
            BlockSize := 0;
            rpFI := Nil;
            rpBlockNum := fsc_W32NoValue;
            rpTrans := Nil;
          End
            { If the file is being set to a real fileinfo record, set as
              much data as we can. }
        Else
          Begin
            BlockSize := FI^.fiBlockSize;
            rpFI := FI;
            rpBlockNum := fsc_W32NoValue;
            rpTrans := Nil;
            If fffaTemporary In FI^.fiAttributes Then
              rpReuseMode := ffrmTempStore
            Else
              rpReuseMode := ffrmUseAsIs;
          End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{===TfsSrcTransactionLevel============================================}

Constructor TfsSrcTransactionLevel.Create(aTrans: TfsSrcTransaction);
Begin
  Inherited Create;
  tlTransaction := aTrans;
  tlPrev := tlTransaction.trTransLevelListTail;
  tlTransaction.trTransLevelListTail := Self;
  If Assigned(tlPrev) Then
    tlLevel := tlPrev.tlLevel + 1
  Else
    tlLevel := 0;
End;
{--------}

Destructor TfsSrcTransactionLevel.Destroy;
Begin
  tlTransaction.trTransLevelListTail := tlPrev;
  Assert(Not Assigned(tlModifiedBlocksHead));
  Inherited;
End;
{====================================================================}

{===TfsSrcTransaction===============================================}

Constructor TfsSrcTransaction.Create(Const aDatabaseID: TffDatabaseID;
  Const aImplicit, ReadOnly: boolean);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    FDatabaseID := aDatabaseID;
    FImplicit := aImplicit;
    FJnlFile := Nil;
    FNewSpace := 0; {!!.11}
    FTransLevel := 0;
    FReadOnly := ReadOnly;
    FSignature := fsc_SigTransaction;
    FTransMode := tmNormal;
    FLockContainer := Nil;
    FListEventsTransaction := TFSSpecStringList.Create;
    FListEventsTransaction.CaseSensitive := False;
    FListEventsGlobalTransaction := TFSSpecStringList.Create;
    FListEventsGlobalTransaction.CaseSensitive := False;
    fVariables := TIntVariables.Create;
    StartNested; {!!.10}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TfsSrcTransaction.Destroy;
Var
  parentDB: TfsSrcDatabase;
  ParentServer: TfsServer;
  i: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If assigned(FLockContainer) Then
      FLockContainer.Free;
    If assigned(FListEventsTransaction) Then
      ListEventsTransaction.free;
    If assigned(FListEventsGlobalTransaction) Then
      Begin
        //delete event if exists server
        Try
          parentDB := Nil;
          If (FDatabaseID > 0) And (Not (FDatabaseID = 4294967295)) Then
            parentDB := TfsSrcDatabase(FDatabaseID);
          If parentDB <> Nil Then
            Begin
              ParentServer := parentDB.Engine;
              If ParentServer <> Nil Then
                Begin
                  For i := 0 To ListEventsGlobalTransaction.Count - 1 Do
                    Begin
                      If ParentServer.ListEventsTransaction.Exists(ListEventsGlobalTransaction.Strings[i]) Then
                        ParentServer.ListEventsTransaction.Delete(ListEventsGlobalTransaction.Strings[i]);
                    End;
                End;
            End;
        Except
        End;
        ListEventsGlobalTransaction.free;
      End;
    fVariables.free;
    EndNested;
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsSrcTransaction.AdjustLSN(Const Adjustment: TffWord32): TffWord32;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Assumption: Transaction list & buffer manager data structures have
                  been write-locked. }
    FLSN := FLSN - Adjustment;
    Result := FLSN;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.10}
{--------}

Procedure TfsSrcTransaction.StartNested;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    TfsSrcTransactionLevel.Create(Self);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TfsSrcTransaction.EndNested;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    Assert(Assigned(trTransLevelListTail));
    trTransLevelListTail.Free;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{End !!.10}
{--------}

Function TfsSrcTransaction.trGetNested: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := (TransLevel.Level > 0); {!!.10}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.10}
{--------}

Function TfsSrcTransaction.trGetTransLevel: TfsSrcTransactionLevel;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    Assert(Assigned(trTransLevelListTail));
    Result := trTransLevelListTail;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{End !!.10}
{--------}

Function TfsSrcTransaction.trGetTransactionID: TffTransID;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := TffTransID(Self);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{===TfsBufferManager=================================================}

Constructor TfsBufferManager.Create(Const TempPath: String;
  Const TempStoreSizeInMB: Integer; Const aEncryptTempStorage: boolean);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    bmClearCachePerCountT := 0;

    bmClearCachePerCount := 0;
    bmClearCacheIfUpdate := False;

    bmConfigDir := TempPath;
    bmEncryptTempStorage := aEncryptTempStorage;
    bmInUseListHead := Nil;
    bmInUseListTail := Nil;
    bmPortal := TfsPadlock.Create; {!!.02}
    bmMaxRAM := 10;
    bmMaxRAMDetail.iLow := bmMaxRAM;
    bmMaxRAMDetail.iHigh := 0;
    ffI64MultInt(bmMaxRAMDetail, fscl_1MB, bmMaxRAMDetail);
    ffInitI64(bmRAMDetail);
    bmRAMUsed := 0;
    bmTempStore := fscTempStorageClass.Create(bmConfigDir,
      TempStoreSizeInMB * fscl_1MB,
      fscl_64k,
      aEncryptTempStorage);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.07}
{--------}

Procedure TfsBufferManager.bmClearRecycleList;
Var
  Temp: TffbmRAMPage;
Begin
  While Assigned(bmRecycleListHead) Do
    Begin
      Temp := bmRecycleListHead;
      Temp.RemoveFromRecycleList;
      Temp.Free;
    End;
End;
{End !!.07}
{--------}

Destructor TfsBufferManager.Destroy;
//var                                                                  {Deleted !!.07}
//  Temp : TffbmRAMPage;                                               {Deleted !!.07}
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    bmPortal.Lock; {!!.02}
    Try
      { Free the pages from the recycle list. }
      bmClearRecycleList; {!!.07}

      { All files must be closed before freeing the buffer manager.
        If bmInUseListHead is assigned, files are still open. }
      Assert(Not Assigned(bmInUseListHead));
    Finally
      bmPortal.Unlock; {!!.02}
      bmPortal.Free; {!!.02}
    End;
    bmTempStore.Free;
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.AddBlock(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aBlockNumber: TffWord32;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation): PffBlock;
Var
  Temp: TffbmRAMPage;
Begin
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF}
      {If bmClearCacheIfUpdate Then
        If aOperation In [fsoInsert, fsoUpdate, fsoDelete] Then
          If bmClearCachePerCount > 0 Then
            Begin
              If bmClearCachePerCountT >= bmClearCachePerCount Then
                bmRemoveExcessPages;
              Inc(bmClearCachePerCountT);
            End; }
      { Is the block already in memory? }
      Temp := bmSearch(aFI, aBlockNumber);

      { If not in memory then bring it into memory. }
      If Not Assigned(Temp) Then
        Begin
          Temp := bmGetNewRAMPage(aFI, aBlockNumber, aOperation);
          { If we are in a transaction then make this block part of the
            transaction. }
          If assigned(aTI^.tirTrans) Then
            Begin
              Temp.MakeDirty(aTI^.tirTrans);
            End;
        End
      Else
        { The block is in memory.  Move it to the end of the InUse list. }
        Temp.MoveToEndOfUseList;

      { Does this file need a reference to temporary storage? }
      If (fffaTemporary In aFI^.fiAttributes) And
        (aFI^.fiTempStore = Nil) Then
        aFI^.fiTempStore := bmTempStore;

      { Return a modifiable copy of the block. }
      Result := Temp.Block(aTI^.tirTrans, aReleaseMethod);
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End; {try..finally}
End;
{--------}

Function TfsBufferManager.AddFile(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aMarkHeaderDirty: boolean;
  Var aReleaseMethod: TffReleaseMethod): PffBlock;
Var
  Temp: TffbmRAMPage;
  Trans: TfsSrcTransaction;
Begin
  Result := Nil; {!!.13}
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      Temp := bmSearch(aFI, 0);
      If Not Assigned(Temp) Then
        Begin
          Temp := bmGetNewRAMPage(aFI, 0, fsoNone);
          If Not (fffaTemporary In aFI^.fiAttributes) Then
            {Begin !!.13}
            Try
              bmReadBlock(aFI, fsc_W32NoValue, Temp);
            Except
              Temp.RemoveFromUseList;
              Temp.RemoveFromFilePageList;
              Temp.Free;
              Raise;
            End;
          {End !!.13}
        End
      Else
        Begin
          If (aFI^.fiBlockSize = 0) Then
            Begin
              aFI^.fiBlockSize := Temp.BlockSize;
              aFI^.fiBlockSizeK := Temp.BlockSize Div 1024; {!!.11}
              aFI^.fiLog2BlockSize := FFCalcLog2BlockSize(Temp.BlockSize);
            End;
        End;
      If aMarkHeaderDirty And (Not Temp.DirtiedForTrans(aTI^.tirTrans)) Then
        Begin
          Trans := aTI^.tirTrans;
          Temp.MakeDirty(Trans);
        End;

      { Does this file need a reference to temporary storage? }
      If (fffaTemporary In aFI^.fiAttributes) And
        (aFI^.fiTempStore = Nil) Then
        aFI^.fiTempStore := bmTempStore;

      { Return a modifiable copy of the block. }
      Result := Temp.Block(aTI^.tirTrans, aReleaseMethod);
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End; {try..finally}
End;
{--------}

Procedure TfsBufferManager.BeginWrite;
Begin
  bmPortal.Lock; {!!.02}
End;
{--------}

Procedure TfsBufferManager.bmCommitPrim(aTrans: TfsSrcTransaction);
Var
  aPage, NextPage: TffbmRAMPage;
  CanShove: boolean;
  FirstShove: TffbmRAMPage;
  FlushList: TffFlushList;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Is the transaction nested? }{!!.10}
    If aTrans.TransLevel.Level = 0 Then
      Begin {!!.10}
        { No. Commit to disk }{!!.10}
        CanShove := True;
        FirstShove := Nil;

        { Create list of files that will be needed to be flushed. }
        FlushList := TffFlushList.Create;

        {Begin !!.11}
            { Verify there is enough free disk space for the new blocks. }
        aPage := aTrans.trTransPageListHead;
        If (aPage <> Nil) And
          (aTrans.FNewSpace > 0) And
          (aTrans.FNewSpace >
          FFGetDiskFreeSpace(ExtractFileDir(aPage.rpFI^.fiName^))) Then
          FSRaiseExceptionNoData(EfsServerException,
            fsStrResServer,
            fserrDiskFull);
        {End !!.11}

            { Loop through the pages. }
        While assigned(aPage) Do
          Begin
            NextPage := aPage.rpTransNext;
            { If we have a next page and this is page 0, 1, or 2, shove it to the
              end of the transaction page list.  We do this to reduce chances of
              corruption if disk is full. Any new data blocks are written before
              the header block. If a new data block cannot be written then
              we avoid putting a bad record count in block 0. }
            If assigned(NextPage) And
              CanShove And
              (aPage.BlockNumber < 3) Then
              Begin
                If aPage = FirstShove Then
                  Begin
                    CanShove := False;
                    NextPage := aPage;
                  End
                Else
                  Begin
                    aPage.MoveToEndOfTransList;
                    If FirstShove = Nil Then
                      FirstShove := aPage;
                  End;
              End
            Else If aPage.Commit(False) And
              (Not (fffaTemporary In aPage.FileInfo^.fiAttributes)) Then
              FlushList.Add(aPage.FileInfo);
            aPage := NextPage;
          End;

        { Now flush the files to which we have written. }
        FlushList.Flush(aTrans);
        FlushList.Free;
        {Begin !!.10}
      End
    Else
      Begin
        {Yes. Only commit the blocks belonging to the current transaction level }
        While Assigned(aTrans.TransLevel.tlModifiedBlocksHead) Do
          aTrans.TransLevel.tlModifiedBlocksHead.RAMPage.Commit(False);
      End;
    {End !!.10}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmFailSafeCommit(aTrans: TfsSrcTransaction);
Var
  aPage: TffbmRAMPage;
  FileName: TffFullFileName;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Get the journal file name for the final deletion. }
    FileName := aTrans.JournalFile^.fiName^;

    { Is this the commit of a nested transaction? }
    If aTrans.TransLevel.Level = 0 Then
      Begin {!!.10}
        { No. Write out all before- and after-images to journal file.
          We need before-images so that the fail-safe transaction can
          be completely rolled back in the event of power failure. We
          need after-images so that the fail-safe transaction can be
          re-applied. }
        aPage := aTrans.trTransPageListHead;
        While assigned(aPage) Do
          Begin
            If (Not (fffaTemporary In aPage.FileInfo^.fiAttributes)) Then
              Begin
                bmJournalRAMPage(aTrans, aPage, True);
                bmJournalRAMPage(aTrans, aPage, False);
              End;
            aPage := aPage.rpTransNext;
          End;
        { Mark the journal file as complete and close it. }
        bmWriteCompleteJnlHeader(aTrans.JournalFile);
      End;

    { Commit the pages. }
    bmCommitPrim(aTrans);

    { If we get this far all dirty data was force-flushed to disk, so
     delete the journal file (it's no longer needed)}
    If aTrans.TransLevel.Level = 0 Then {!!.10}
      Try
        FFDeleteFile(FileName);
      Except
        {do nothing}
      End; {try..except}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.bmFileRAMPageCount(aFI: PffFileInfo): Longint;
Var
  RAMPage: TffbmRAMPage;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := 0;
    RAMPage := aFI^.fiPageListHead;
    While assigned(RAMPage) Do
      Begin
        inc(Result);
        RAMPage := RAMPage.rpFileNext;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.bmGetNewRAMPage(aFI: PffFileInfo;
  aBlockNumber: TffWord32; aOperation: TfsOperation): TffbmRAMPage;
Var
  ReuseMode: TffbmPageReuseMode;
  Temp: TffbmRAMPage;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {$IFDEF RAMPageCheck}
    Log('Entering TffBuffMan.bmGetNewRamPage', []);
    {$ENDIF}
    Result := Nil;

    { Check the Recycle list for an available RAM page. }
    If Assigned(bmRecycleListHead) Then
      Begin
        Result := bmRecycleListHead;
        Result.RemoveFromRecycleList;
        Result.FileInfo := aFI;
        Result.BlockNumber := aBlockNumber;
      End;

    { If we don't have a recycled page and if adding the new block would push
      us over our maximum RAM limit then try to find a page in the UseList that is
      relatively old and not locked. }
    If (Not Assigned(Result)) And bmOverRAMLimit(aFI^.fiBlockSize) Then
      Begin
        {$IFDEF RAMPageCheck}
        Log('Looking for reusable RAMPage', []);
        {$ENDIF}
        Temp := bmInUseListHead;
        While Assigned(Temp) Do
          Begin
            If Temp.Reusable(ReuseMode) Then
              Begin
                Result := Temp;
                Break;
              End;
            Temp := Temp.rpInUseNext;
          End;
        { Did we find a reusable page? }
        If Assigned(Result) Then
          { Yes. Can we use it as is? }
          If ReuseMode = ffrmUseAsIs Then
            Begin
              { Yes. Update its properties. }
              Result.RemoveFromFilePageList;
              Result.FileInfo := aFI;
              Result.BlockNumber := aBlockNumber;
              Result.RemoveFromUseList;
            End
          Else
            Begin
              {$IFDEF RAMPageCheck}
              Log('Sending reusable page to temp storage.', []);
              {$ENDIF}
              { No. Send it to temporary storage. }
              Result.SendToTempStore;
              Result := Nil;
            End;
      End;

    { If didn't have a page to recycle, haven't reached the maximum number of RAM
      pages, or didn't have a re-usable page then create a new RAM page. }
    If (Not Assigned(Result)) Then
      Begin
        Result := TffbmRAMPage.Create(Self, aFI, aBlockNumber);
        {$IFDEF RAMPageCheck}
        Log('Creating a new RAMPage. RAM used: %d', [bmRAMDetail.ilow]);
        {$ENDIF}
      End;
    { Add it to the buffer manager's InUse list and the file's
      page list. }
    Result.AddToUseList;
    Result.AddToFilePageList;
    {$IFDEF RAMPageCheck}
    Log('Leaving TffBuffMan.bmGetNewRamPage', []);
    {$ENDIF}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.bmGetRAMPage(Const anIndex: Longint): TffbmRAMPage;
Var
  Count: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Count := 0;
    Result := bmInUseListHead;
    While assigned(Result) And (Count < anIndex) Do
      Begin
        inc(Count);
        Result := Result.rpInUseNext;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.bmGetRecycledCount: Longint;
Var
  RAMPage: TffbmRAMPage;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := 0;
    RAMPage := bmRecycleListHead;
    While assigned(RAMPage) Do
      Begin
        inc(Result);
        RAMPage := RAMPage.rpInUseNext;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.bmGetTempStoreSize: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := bmTempStore.Size Div fscl_1MB;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.bmRAMPageCount: Longint;
Var
  RAMPage: TffbmRAMPage;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := 0;
    RAMPage := bmInUseListHead;
    While assigned(RAMPage) Do
      Begin
        inc(Result);
        RAMPage := RAMPage.rpInUseNext;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.bmSearch(aFI: PffFileInfo; aBlockNumber: TffWord32): TffbmRAMPage;
Var
  pc1: PffPageContainer;
  pc2: PffPageContainer;
  pc3: PffPageContainer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { is this the header page? }
    If aBlockNumber = 0 Then
      Begin
        { yes... it was stored in a special field for faster access }
        Result := aFI^.fiPageZero;
        Exit;
      End;

    pc1 := aFI^.fiPages[TffBlockNum(aBlockNumber)[3]];
    If Not Assigned(pc1) Then
      Begin
        Result := Nil;
        Exit;
      End;
    pc2 := pc1.pcPages[TffBlockNum(aBlockNumber)[2]];
    If Not Assigned(pc2) Then
      Begin
        Result := Nil;
        Exit;
      End;
    pc3 := pc2.pcPages[TffBlockNum(aBlockNumber)[1]];
    If Not Assigned(pc3) Then
      Begin
        Result := Nil;
        Exit;
      End;
    Result := pc3.pcPages[TffBlockNum(aBlockNumber)[0]];
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmSetTempStoreSize(aSizeInMB: Integer);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    bmTempStore.Free;
    bmTempStore := fscTempStorageClass.Create(bmConfigDir,
      aSizeInMB * fscl_1MB, fscl_64k, bmEncryptTempStorage);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.CommitFileChanges(aFI: PffFileInfo;
  aTrans: TfsSrcTransaction);
Var
  aPage: TffbmRAMPage;
  NextPage: TffbmRAMPage;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If assigned(aTrans) Then
      Begin
        aPage := aTrans.trTransPageListHead;
        While assigned(aPage) Do
          Begin
            If aPage.FileInfo = aFI Then
              Begin
                NextPage := aPage.rpTransNext;
                aPage.Commit(True);
                aPage := NextPage;
              End
            Else
              aPage := aPage.rpTransNext;
          End; { while }
        If Not (fffaTemporary In aFI^.fiAttributes) Then
          FFForceFlushFile(aFI);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;

Procedure TfsBufferManager.CommitTransaction(aTrans: TfsSrcTransaction);
Begin
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      If (aTrans <> Nil) Then
        Begin
          If (aTrans.TransactionMode = tmNormal) Then
            bmCommitPrim(aTrans)
          Else {TransactionMode = tmFailSafe}
            bmFailSafeCommit(aTrans);

          bmRemoveExcessPages;
        End;
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End;
End;
{--------}

Procedure TfsBufferManager.CommitTransactionSubset(aTrans: TfsSrcTransaction);
Begin
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      If (aTrans <> Nil) Then
        bmCommitPrim(aTrans);
      { We typically commit a subset during a long-running operation such as
        pack, reindex, or restructure. Remove the pages associated with this
        transaction. The advantage to this is that we don't squeeze other cursors
        out of the RAM cache. The disadvantage is that we may free up pages that
        we need as we continue the operation. }
      bmRemoveCommittedPages(aTrans);
      bmRemoveExcessPages;
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End;
End;
{--------}

Procedure TfsBufferManager.DirtyBlock(aFI: PffFileInfo;
  Const aBlockNumber: TffWord32;
  aTI: PffTransInfo;
  Var aModifiableBlock: PffBlock);
Var
  aModBlockClone: PffBlock;
  aRelMethod: TffReleaseMethod;
  Temp: TffbmRAMPage;
  Trans: TfsSrcTransaction;
Begin
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      { Is the block in memory? }
      Temp := bmSearch(aFI, aBlockNumber);
      { If it is in memory then make it part of the file's transaction. }
      If Assigned(Temp) Then
        Begin
          If Not Temp.DirtiedForTrans(aTI^.tirTrans) Then
            Begin
              Trans := aTI^.tirTrans;
              Temp.MakeDirty(Trans);
            End;
          aModifiableBlock := Temp.Block(aTI^.tirTrans, aRelMethod);
          aModBlockClone := aModifiableBlock;
          aRelMethod(aModBlockClone);
          { Move the page to the end of the InUse list. }
          Temp.MoveToEndOfUseList;
        End;
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End; {try..finally}
End;
{--------}

Procedure TfsBufferManager.EndWrite;
Begin
  bmPortal.Unlock; {!!.02}
End;
{--------}

Procedure TfsBufferManager.FlushPools(Const blockSizes: TffBlockSizes);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}

    { Time to do a general flush? }{!!.07}
    If blockSizes = [] Then
      Begin {!!.01} {!!.07}
        { Free up the recycled list. }{!!.07}
        bmClearRecycleList; {!!.07}
        FFFlushMemPools; {!!.01}
      End; {!!.07}

    If (ffbs4k In blockSizes) And assigned(Pool4k) Then
      Pool4k.RemoveUnusedBlocks;

    If (ffbs8k In blockSizes) And assigned(Pool8k) Then
      Pool8k.RemoveUnusedBlocks;

    If (ffbs16k In blockSizes) And assigned(Pool16k) Then
      Pool16k.RemoveUnusedBlocks;

    If (ffbs32k In blockSizes) And assigned(Pool32k) Then
      Pool32k.RemoveUnusedBlocks;

    If (ffbs64k In blockSizes) And assigned(Pool64k) Then
      Pool64k.RemoveUnusedBlocks;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.GetBlock(aFI: PffFileInfo;
  Const aBlockNumber: TffWord32;
  aTI: PffTransInfo;
  Const aMarkDirty: boolean;
  Var aReleaseMethod: TffReleaseMethod;
  aOperation: TfsOperation): PffBlock;
Var
  Temp: TffbmRAMPage;
Begin
  bmPortal.Lock;
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      { Get the RAM page. }
      Temp := bmGetBlock(aFI, aBlockNumber, aOperation);
      { If we are to mark it dirty and it has not been marked as part of the
        file's transaction then make it part of the transaction. }
      If aMarkDirty And (Not Temp.DirtiedForTrans(aTI^.tirTrans)) Then
        Temp.MakeDirty(aTI^.tirTrans);
      Result := Temp.Block(aTI^.tirTrans, aReleaseMethod);
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock;
  End; {try..finally}
End;
{--------}

Function TfsBufferManager.GetRAMPage(aFI: PffFileInfo;
  Const aBlockNumber: TffWord32): TffbmRAMPage;
Begin
  {Begin !!.05}
  bmPortal.Lock;
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      { Get the RAM page. }
      Result := bmGetBlock(aFI, aBlockNumber, fsoNone);
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock;
  End;
  {End !!.05}
End;
{Begin !!.06}
{--------}

Function TfsBufferManager.GetRAMPageLSN(aRAMPage: TffbmRAMPage): TffWord32;
Begin
  bmPortal.Lock;
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF}
      Result := aRAMPage.LSN;
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF}
  Finally
    bmPortal.Unlock;
  End;
End;
{--------}

Function TfsBufferManager.GetRAMPageLSN2(aFI: PffFileInfo;
  Const aBlockNumber: TffWord32): TffWord32;
Begin
  bmPortal.Lock;
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF}
      { Get the RAM page. }
      Result := bmGetBlock(aFI, aBlockNumber, fsoNone).LSN;
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF}
  Finally
    bmPortal.Unlock;
  End;
End;
{End !!.06}
{--------}
// obsluga

Function TfsBufferManager.bmGetBlock(aFI: PffFileInfo;
  aBlockNumber: TffWord32; aOperation: TfsOperation): TffbmRAMPage;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}

    If (fffaTemporary In aFI^.fiAttributes) And (aFI^.fiTempStore = Nil) Then
      aFI^.fiTempStore := bmTempStore;
    If bmClearCacheIfUpdate Then
      If aOperation In [fsoInsert, fsoUpdate, fsoDelete] Then
        If bmClearCachePerCount > 0 Then
          Begin
            If bmClearCachePerCountT >= bmClearCachePerCount Then
              bmRemoveExcessPages;
            Inc(bmClearCachePerCountT);
          End;
    { Is the block already in memory? }
    Result := bmSearch(aFI, aBlockNumber);
    { If it is not in memory then bring it into memory. }
    If Result = Nil Then
      Begin

        //  if not Assigned(Result) then begin
        Result := bmGetNewRAMPage(aFI, aBlockNumber, fsoNone);
        If Not (fffaTemporary In aFI^.fiAttributes) Then
          {Begin !!.13}
          Try
            bmReadBlock(aFI, aBlockNumber, Result);
          Except
            Result.RemoveFromUseList;
            Result.RemoveFromFilePageList;
            Result.Free;
            Raise;
          End;
        {End !!.13}
      End
    Else
      { It is in memory. Move it to the end of the InUse list. }
      Result.MoveToEndOfUseList;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsBufferManager.GetRAM: Integer;
Begin
  Result := bmRAMUsed;
End;
{--------}

Procedure TfsBufferManager.HandleLSNrollover;
Var
  RAMPage: TffbmRAMPage;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    RAMPage := bmInUseListHead;
    While assigned(RAMPage) Do
      Begin
        If Not RAMPage.Dirty Then
          RAMPage.LSN := 1;
        RAMPage := RAMPage.rpInUseNext;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmDecreaseRAMDetail(Const numberBytes: Longint);
Var
  tmpI64: TffInt64;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    ffI64MinusInt(bmRAMDetail, numberBytes, bmRAMDetail);
    ffI64DivInt(bmRAMDetail, fscl_1MB, tmpI64);
    bmRAMUsed := ffI64ToInt(tmpI64);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmIncreaseRAMDetail(Const numberBytes: Longint);
Var
  tmpI64: TffInt64;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    ffI64AddInt(bmRAMDetail, numberBytes, bmRAMDetail);
    ffI64DivInt(bmRAMDetail, fscl_1MB, tmpI64);
    bmRAMUsed := ffI64ToInt(tmpI64);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmJournalRAMPage(aTrans: TfsSrcTransaction;
  aRAMPage: TffbmRAMPage;
  aBeforeImage: boolean);
Var
  aBlock: PffBlock;
  aReleaseMethod: TffReleaseMethod;
  RecHdr: TffJournalFileRecordHeader;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    FillChar(RecHdr, sizeof(RecHdr), 0);
    With RecHdr, aRAMPage Do
      Begin
        jfrhSignature := fsc_SigJnlRecHeader;
        jfrhBlockNumber := BlockNumber;
        jfrhBlockSize := BlockSize;
        jfrhBeforeImg := Longint(ord(aBeforeImage));
        StrCopy(jfrhFileName, @FileInfo^.fiName^[1]);
        FFPositionFileEOF(aTrans.JournalFile);
        FFWriteFileExact(aTrans.JournalFile, sizeof(RecHdr), RecHdr);
        If aBeforeImage Then
          FFWriteFileExact(aTrans.JournalFile, BlockSize, ReadOnlyBlock^)
        Else
          Begin
            aBlock := Block(aTrans, aReleaseMethod);
            Try
              FFWriteFileExact(aTrans.JournalFile, BlockSize, aBlock^);
            Finally
              aReleaseMethod(aBlock);
            End;
          End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.05}
{--------}

Procedure TfsBufferManager.Lock;
Begin
  bmPortal.Lock;
End;
{End !!.05}
{--------}

Function TfsBufferManager.bmOverRAMLimit(sizeOfNewBlock: Longint): boolean;
Var
  tmpI64: TffInt64;
Begin
  {$IFDEF RAMPageCheck}
  Log('OverRamLimit?', []);
  Log('  NewBlockSize : %d', [SizeOfNewBlock]);
  Log('  MaxRam       : %d', [bmMaxRAMDetail.ilow]);
  Log('  Current RAM  : %d', [bmRAMDetail.ilow]);
  {$ENDIF}

  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Are we already at the limit? }
    Result := (FFCmpI64(bmRAMDetail, bmMaxRAMDetail) = 0);
    { If not then see if this would push us over the limit? }
    If Not Result Then
      Begin
        ffI64AddInt(bmRamDetail, sizeOfNewBlock, tmpI64);
        Result := (FFCmpI64(tmpI64, bmMaxRAMDetail) > 0);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmReadBlock(aFI: PffFileInfo;
  aBlockNumber: TffWord32;
  aRAMPage: TffbmRAMPage);
Var
  aBlock: PffBlock;
  aReleaseMethod: TffReleaseMethod;
  Header: TffBlockHeaderFile;
  MaxBlocks: TffInt64;
  TempI64: TffInt64;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Note: aBlockNumber = fsc_W32NoValue forces verification of the header, and
      for a header record, we need to calculate the block size first; header
      records are never encrypted. }
    If (aBlockNumber = fsc_W32NoValue) Then
      Begin
        TempI64.iLow := 0;
        TempI64.iHigh := 0;
        FFReadFileExactAt(aFI, TempI64, sizeof(Header), Header);
        ffVerifyFileHeaderSignature(aFI, Header.bhfSignature);
        With Header Do
          If (bhfSignature <> fsc_SigHeaderBlock) Or
            (bhfNextBlock <> fsc_W32NoValue) Or
            (bhfThisBlock <> 0) Or
            (Not FFVerifyBlockSize(bhfBlockSize)) Then
            FSRaiseException(EfsServerException, fsStrResServer, fserrNotAnFFFile,
              [aFI^.fiName^]);

        aFI^.fiBlockSize := Header.bhfBlockSize;
        aFI^.fiBlockSizeK := Header.bhfBlockSize Div 1024; {!!.11}
        aFI^.fiLog2BlockSize := Header.bhfLog2BlockSize;
        aFI^.fiUsedBlocks := Header.bhfUsedBlocks;
        aFI^.fiEncrypted := (Header.bhfEncrypted = 1);
        aFI^.fiRecordLength := Header.bhfRecordLength;
        aFI^.fiRecLenPlusTrailer := Header.bhfRecLenPlusTrailer;
        aFI^.fiVersionRecord := Header.bhfVersionRecord;
        aFI^.fiFSVersion := Header.bhfFSVersion;
        tmpVersion := aFI^.fiFSVersion;

        If aFI^.fiFSVersion > fsVersionNumber Then
          FSRaiseException(EfsServerException, fsStrResServer, fserrTableVersion,
            [aFI^.fiName^, aFI^.fiFSVersion,
            FsVersionNumber]);

        { Calculate the maximum number of blocks the file may contain.
          D3 max num blocks is 2^31; 2^32 for D4 and 5. }
        ffI64DivInt(FFCalcMaxFileSize(aFI), TffWord32(aFI^.fiBlockSize), MaxBlocks);
        If (ffCmpDW(MaxBlocks.iLow, fscl_MaxBlocks)) > 0 Then
          aFI^.fiMaxBlocks := fscl_MaxBlocks
        Else
          aFI^.fiMaxBlocks := MaxBlocks.iLow;

        aFI^.fiMaxSegSize := FFCalcMaxBLOBSegSize(aFI);
        aRAMPage.BlockSize := Header.bhfBlockSize;
        aBlockNumber := 0;
      End;
    { Read the requested block in its entirety. }
    With aRAMPage Do
      Begin
        TempI64.iLow := aBlockNumber;
        TempI64.iHigh := 0;
        ffI64MultInt(TempI64, BlockSize, TempI64);
        { Read the file into the read-only slot. }
        aBlock := Block(Nil, aReleaseMethod);
        Try
          FFReadDecryptFileExactAt(aFI, TempI64, BlockSize, aBlock^);
        Finally
          aReleaseMethod(aBlock);
        End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmRemoveCommittedPages(Const aTran: TfsSrcTransaction);
Var
  BlockSizes: TffBlockSizes;
  LSN: TffWord32;
  NextPage: TffbmRAMPage;
  RAMPage: TffbmRAMPage;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}

    { Remove pages marked by the transaction. }
    BlockSizes := [];
    LSN := aTran.LSN;
    RAMPage := aTran.trTransPageListHead;
    While assigned(RAMPage) Do
      Begin
        NextPage := RAMPage.rpTransNext;
        { Is this page part of the specified transaction? }
        If (RAMPage.LSN = LSN) And (Not RAMPage.Dirty) Then
          Begin
            { Yes.  Get rid of the page. }
            Include(BlockSizes, RAMPage.rpBlockSizeEnum);
            RAMPage.RemoveFromTransList(aTran);
            RAMPage.RemoveFromFilePageList;
            RAMPage.RemoveFromUseList;
            RAMPage.Free;
          End;
        { Move to the next page. }
        RAMPage := NextPage;
      End;

    { Tell the memory pools to free up their excess blocks. }
    FlushPools(blockSizes);

    { Flush the semaphore & mutex pools. }
  //  FFMutexPool.Flush;
    FFSemPool.Flush;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmRemoveExcessPages;
Var
  BlockSizes: TffBlockSizes;
  ExcessRAM: Integer;
  NextPage: TffbmRAMPage;
  RAMPage: TffbmRAMPage;
  RemoveMode: TffbmPageReuseMode;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}

    { Goal: Make sure the RAM allocated to pages is at or below the MaxRAM
            property. }
    BlockSizes := [];

    { Are we using more RAM than allowed? }
    ExcessRAM := bmRAMUsed - bmMaxRAM;
    If (ExcessRAM > 0) Then
      Begin
        { Yes.  See if we can remove any from the recycle list. }
        While assigned(bmRecycleListHead) And (ExcessRAM > 0) Do
          Begin
            RAMPage := bmRecycleListHead;
            RAMPage.RemoveFromRecycleList;
            RAMPage.Free;
            ExcessRAM := bmRAMUsed - bmMaxRAM;
          End;

        { Are we still over the limit? }
        If (ExcessRAM > 0) Then
          Begin
            { Yes.  See if some InUse pages can be removed. }
            RAMPage := bmInUseListHead;
            While assigned(RAMPage) And (ExcessRAM > 0) Do
              Begin
                NextPage := RAMPage.rpInUseNext;
                { Can this page be removed? }
                If RAMPage.Removable(RemoveMode) Then
                  Begin
                    { Yes. Is it to be sent to temporary storage? }
                    If RemoveMode = ffrmTempStore Then
                      { Yes. Do so. }
                      RAMPage.SendToTempStore
                    Else
                      Begin
                        { No. We can just free it. }
                        Include(BlockSizes, RAMPage.rpBlockSizeEnum);
                        RAMPage.RemoveFromFilePageList;
                        RAMPage.RemoveFromUseList;
                        RAMPage.Free;
                      End;
                    ExcessRAM := bmRAMUsed - bmMaxRAM;
                  End;

                { Move to the next page. }
                RAMPage := NextPage;
              End;
          End;

        { We have eliminated some RAM pages.  Tell the memory pools to free up
          their excess blocks. }
        FlushPools(BlockSizes);

        { Flush the semaphore & mutex pools. }
    //    FFMutexPool.Flush;
        FFSemPool.Flush;

      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.RemoveFile(aFI: PffFileInfo);
Var
  BlockSizes: TffBlockSizes;
  Temp: TffbmRAMPage;
  Temp2: TffbmRAMPage;
  t1, t2: PffPageContainer;
Begin
  BlockSizes := [];
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      { Move all RAM pages from the file's page list to the buffer manager's
        Recycle list. }
      Temp := aFI^.fiPageListHead;
      While Assigned(Temp) Do
        Begin
          Temp2 := Temp.rpFileNext;
          Temp.rpFilePrev := Nil;
          Temp.rpFileNext := Nil;
          bmRemovePageFromTransaction(Temp);
          Temp.FileInfo := Nil;
          Temp.MoveToRecycleList;
          Temp := Temp2;
        End;
      aFI^.fiPageListHead := Nil;
      aFI^.fiPageListTail := Nil;

      { Free all of the file's page containers. }
      t1 := aFI^.fiPageContainerList;
      While Assigned(t1) Do
        Begin
          t2 := t1^.pcNext;
          FFFreeMem(t1, sizeOf(TffPageContainer));
          t1 := t2;
        End;

      FillChar(aFI^.fiPages, SizeOf(aFI^.fiPages), 0);

      Include(BlockSizes, FFMapBlockSize(aFI^.fiBlockSize));
      FlushPools(BlockSizes);
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End; {try..finally}
End;
{--------}

//test

Procedure TfsBufferManager.RemoveFile2(aFI: PffFileInfo);
Var
  BlockSizes: TffBlockSizes;
  Temp: TffbmRAMPage;
  Temp2: TffbmRAMPage;
  t1, t2: PffPageContainer;
Begin
  BlockSizes := [];
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      { Move all RAM pages from the file's page list to the buffer manager's
        Recycle list. }
      Temp := aFI^.fiPageListHead;
      While Assigned(Temp) Do
        Begin
          Temp2 := Temp.rpFileNext;
          Temp.rpFilePrev := Nil;
          Temp.rpFileNext := Nil;
          bmRemovePageFromTransaction(Temp);
          Temp.FileInfo := Nil;
          Temp.MoveToRecycleList;
          Temp := Temp2;
        End;
      aFI^.fiPageListHead := Nil;
      aFI^.fiPageListTail := Nil;

      { Free all of the file's page containers. }
      t1 := aFI^.fiPageContainerList;
      While Assigned(t1) Do
        Begin
          t2 := t1^.pcNext;
          FFFreeMem(t1, sizeOf(TffPageContainer));
          t1 := t2;
        End;

      FillChar(aFI^.fiPages, SizeOf(aFI^.fiPages), 0);

      Include(BlockSizes, FFMapBlockSize(aFI^.fiBlockSize));
      FlushPools(BlockSizes);
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End; {try..finally}
End;
{--------}

Procedure TfsBufferManager.RollbackTransaction(aTrans: TfsSrcTransaction);
Var
  aPage, NextPage: TffbmRAMPage;
  FileName: TffFullFileName;
Begin
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      If (aTrans = Nil) Then {Moved !!.11}
        Exit; {Moved !!.11}
      { Is the transaction nested? }{!!.10}
      If aTrans.TransLevel.Level = 0 Then
        Begin {!!.10}
          { No. Rollback all pages in the transaction. }{!!.10}

          { For fail safe mode, close and delete the journal file. }
          If //(not aTrans.Nested) and                                     {Deleted !!.10}
          (aTrans.TransactionMode = tmFailSafe) Then
            Begin
              Try
                FileName := aTrans.JournalFile^.fiName^;
                FFCloseFile(aTrans.JournalFile);
              Except
                {do nothing}
              End; {try..except}
              Try
                FFDeleteFile(FileName);
              Except
                {do nothing}
              End; {try..except}
            End;

          { Rollback all pages involved in the transaction. }
          aPage := aTrans.trTransPageListHead;
          While Assigned(aPage) Do
            Begin
              NextPage := aPage.rpTransNext;
              aPage.Rollback;
              aPage := NextPage;
            End;
          bmRemoveExcessPages;
          {Begin !!.10}
        End
      Else
        Begin
          {Yes. Only commit the blocks belonging to the current transaction level }
          While Assigned(aTrans.TransLevel.tlModifiedBlocksHead) Do
            aTrans.TransLevel.tlModifiedBlocksHead.RAMPage.Rollback;
        End;
      {End !!.10}
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End; {try..finally}
End;
{--------}

Procedure TfsBufferManager.RollbackTransactionSubset(aTrans: TfsSrcTransaction);
Var
  aPage, NextPage: TffbmRAMPage;
Begin
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      If (aTrans <> Nil) Then
        Begin
          aPage := aTrans.trTransPageListHead;
          While assigned(aPage) Do
            Begin
              NextPage := aPage.rpTransNext;
              aPage.Rollback;
              aPage := NextPage;
            End;
          bmRemoveExcessPages;
        End;
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End;
End;
{--------}

Procedure TfsBufferManager.SetMaxRAM(aNumber: Longint);
Begin
  bmPortal.Lock; {!!.02}
  Try
    If (aNumber <> MaxRAM) Then
      Begin
        bmMaxRAM := aNumber;
        ffIntToI64(aNumber, bmMaxRAMDetail);
        ffI64MultInt(bmMaxRAMDetail, fscl_1MB, bmMaxRAMDetail);
      End;
  Finally
    bmPortal.Unlock; {!!.02}
  End;
End;
{--------}

Procedure TfsBufferManager.StartTransaction(aTrans: TfsSrcTransaction;
  Const aFailSafe: Boolean;
  Const aFileName: TffFullFileName);
Var
  JnlFile: PffFileInfo;
Begin
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      Try
        If aFailSafe Then
          Begin
            aTrans.JournalFile := FFAllocFileInfo(aFileName, fsc_ExtForTrans, Nil);
            FFOpenFile(aTrans.JournalFile, omReadWrite, smExclusive, True, True);
            bmWriteIncompleteJnlHeader(aTrans.JournalFile);
            aTrans.TransactionMode := tmFailSafe;
          End
        Else
          aTrans.TransactionMode := tmNormal;
      Except
        If (aTrans.JournalFile <> Nil) Then
          Begin
            JnlFile := aTrans.JournalFile;
            If FFFileIsOpen(JnlFile) Then
              FFCloseFile(JnlFile);
            FFFreeFileInfo(JnlFile);
          End;
        Raise;
      End; {try..except}
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End; {try..finally}
End;
{Begin !!.05}
{--------}

Procedure TfsBufferManager.Unlock;
Begin
  bmPortal.Unlock;
End;
{End !!.05}
{--------}

Procedure TfsBufferManager.UnlockBlock(aFI: PffFileInfo;
  aBlockNumber: TffWord32);
Var
  Temp: TffbmRAMPage;
Begin
  bmPortal.Lock; {!!.02}
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF} {!!.03}
      Temp := bmSearch(aFI, aBlockNumber);
      If Assigned(Temp) Then
        Begin
          Temp.RemoveFromFilePageList;
          bmRemovePageFromTransaction(Temp);
          Temp.FileInfo := Nil;
          Temp.MoveToRecycleList;
        End;
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF} {!!.03}
  Finally
    bmPortal.Unlock; {!!.02}
  End; {try..finally}
End;
{--------}

Procedure TfsBufferManager.bmWriteCompleteJnlHeader(aJnlFile: PffFileInfo);
Var
  Hdr: TffJournalFileHeader;
  TempI64: TffInt64;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Hdr.jfhSignature := fsc_SigJnlHeader;
    Hdr.jfhState := 1;
    TempI64.iLow := 0;
    TempI64.iHigh := 0;
    FFWriteFileExactAt(aJnlFile, TempI64, sizeof(Hdr), Hdr);
    FFCloseFile(aJnlFile);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmWriteIncompleteJnlHeader(aJnlFile: PffFileInfo);
Var
  Hdr: TffJournalFileHeader;
  TempI64: TffInt64;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Hdr.jfhSignature := fsc_SigJnlHeader;
    Hdr.jfhState := 0;
    TempI64.iLow := 0;
    TempI64.iHigh := 0;
    FFWriteFileExactAt(aJnlFile, TempI64, sizeof(Hdr), Hdr);
    FFFlushFile(aJnlFile);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsBufferManager.bmRemovePageFromTransaction(aPage: TffbmRAMPage);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    With aPage Do
      Begin
        If Not rpDirty Then
          Exit;
        If Not Assigned(rpTrans) Then
          Exit;
        aPage.RemoveFromTransList(rpTrans);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{=====================================================================}

{Begin !!.11}
{===TffBaseBLOBResourceMgr============================================}

Class Function TffBaseBLOBResourceMgr.GetMgr(aFI: PffFileInfo): TffBaseBLOBResourceMgr;
Begin
  Result := TffBLOBResourceMgr.Create;
End;
{--------}

Constructor TffBaseBLOBResourceMgr.Create;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    Inherited Create;
    brmPadlock := TfsPadlock.Create;
    brmSegMgrLoaded := False;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Destructor TffBaseBLOBResourceMgr.Destroy;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    brmPadLock.Free;
    brmSegmentMgr.Free;
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBResourceMgr.Commit;
Begin
  If brmSegmentMgr <> Nil Then
    brmSegmentMgr.Commit;
End;
{--------}

Procedure TffBaseBLOBResourceMgr.DeleteSegment(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aSegOffset: TffInt64);
Begin
  {segment manager must be loaded before deleting a segment}
  If Not brmSegMgrLoaded Then
    brmLoadSegMgr(aFI, aTI);
  brmPadLock.Lock;
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF}
      brmSegmentMgr.DeleteSegment(aFI, aTI, aSegOffset);
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF}
  Finally
    brmPadLock.Unlock;
  End;
End;
{--------}

Procedure TffBaseBLOBResourceMgr.brmLoadSegMgr(aFI: PffFileInfo;
  aTI: PffTransInfo);
Begin
  brmPadlock.Lock;
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF}
      If Not brmSegMgrLoaded Then
        Begin
          brmSegmentMgr := brmGetSegMgrClass.Create(aFI, aTI);
          brmSegMgrLoaded := True;
        End;
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF}
  Finally
    brmPadlock.Unlock;
  End;
End;
{--------}

Procedure TffBaseBLOBResourceMgr.ListFreeSpace(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aInMemory: Boolean;
  aStream: TStream);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    If Not brmSegMgrLoaded Then
      brmLoadSegMgr(aFI, aTI);
    brmSegmentMgr.ListFreeSpace(aFI, aTI, aInMemory, aStream);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBResourceMgr.Rollback;
Begin
  If brmSegmentMgr <> Nil Then
    brmSegmentMgr.Rollback;
End;
{=====================================================================}

{===TffBLOBResourceMgr================================================}

Function TffBLOBResourceMgr.brmGetSegMgrClass: TffBLOBSegmentMgrClass;
Begin
  Result := TffBLOBSegmentMgr;
End;
{--------}

Function TffBLOBResourceMgr.NewSegment(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aSizeNeeded: TffWord32;
  Const aMinSizeAllowed: TffWord32)
  : TffInt64;
Var
  NewSize,
    NewMinSize: Longint;
Begin
  { Segment manager must be loaded before getting a new segment. }
  If Not brmSegMgrLoaded Then
    brmLoadSegMgr(aFI, aTI);
  brmPadLock.Lock;
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF}
      {calculate new size based on size of BLOB increment}
      Assert(aSizeNeeded <= aFI^.fiMaxSegSize,
        'Requesting too large segment.');
      NewSize := (((aSizeNeeded + pred(fsc_BLOBSegmentIncrement)) Div
        fsc_BLOBSegmentIncrement) * fsc_BLOBSegmentIncrement);
      NewMinSize := (((aMinSizeAllowed + pred(fsc_BLOBSegmentIncrement)) Div
        fsc_BLOBSegmentIncrement) * fsc_BLOBSegmentIncrement);
      If NewMinSize > NewSize Then
        NewMinSize := NewSize;
      {look for segment in deleted chain 1st}
      Result := brmSegmentMgr.GetRecycledSeg(aFI, aTI, NewSize, NewMinSize);
      {if aSize segment not available, create a new segment}
      If Result.iLow = fsc_W32NoValue Then
        Result := brmSegmentMgr.GetNewSeg(aFI, aTI, NewSize);
      { Set the final size allocated in the aSizeNeeded parameter. }
      aSizeNeeded := NewSize;
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF}
  Finally
    brmPadLock.Unlock;
  End;
End;
{=====================================================================}

{===Tff210BLOBResourceMgr=============================================}

Function Tff210BLOBResourceMgr.brmGetSegMgrClass: TffBLOBSegmentMgrClass;
Begin
  Result := Tff210BLOBSegmentMgr;
End;
{--------}

Function Tff210BLOBResourceMgr.NewSegment(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aSizeNeeded: TffWord32;
  Const aMinSizeAllowed: TffWord32)
  : TffInt64;
Var
  NewSize,
    MinSize: Longint;
Begin
  { Segment manager must be loaded before getting a new segment. }
  If Not brmSegMgrLoaded Then
    brmLoadSegMgr(aFI, aTI);
  brmPadLock.Lock;
  Try
    {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
    Try
      {$ENDIF}
      { Calculate new size based on size of BLOB increment. }
      NewSize := (((aSizeNeeded + pred(fsc_BLOBSegmentIncrement)) Div
        fsc_BLOBSegmentIncrement) * fsc_BLOBSegmentIncrement);
      MinSize := NewSize;
      { First, look for segment in deleted chain . }
      Result := brmSegmentMgr.GetRecycledSeg(aFI, aTI, NewSize, MinSize);
      { If aSize segment not available, create a new segment. }
      If Result.iLow = fsc_W32NoValue Then
        Result := brmSegmentMgr.GetNewSeg(aFI, aTI, NewSize);
    {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
    End;
    {$ENDIF}
  Finally
    brmPadLock.Unlock;
  End;
End;
{=====================================================================}

{===TffBaseBLOBSegmentMgr=============================================}

Constructor TffBaseBLOBSegmentMgr.Create(aFI: PffFileInfo;
  aTI: PffTransInfo);
Var
  aFHRelMethod: TffReleaseMethod;
  aSegRelMethod: TffReleaseMethod;
  FileHeader: PffBlockHeaderFile;
  SegmentOfs: TffInt64;
  SegmentBlk: PffBlock;
  SegmentPtr: PffBLOBSegmentHeaderDel;
  OffsetInBlock: TffWord32;
  ListItem: TffBLOBSegListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    Inherited Create;
    { Fill bsmDelChain with segments. }
    bsmDelChain := TFSNormalList.Create;
    bsmTranListHead := Nil;
    bsmUseTranList := Not (fffaBLOBChainSafe In aFI.fiAttributes);
    { We need the file header to get the deleted segment head. }
    FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
      aTI,
      0,
      fsc_ReadOnly,
      aFHRelMethod, fsoNone));
    Try
      If (FileHeader^.bhfDelBLOBHead.iLow <> fsc_W32NoValue) Then
        Begin
          SegmentOfs := FileHeader^.bhfDelBLOBHead;
          bsmDelChain.Sorted := True;

          While (SegmentOfs.iLow <> fsc_W32NoValue) Do
            Begin
              SegmentBlk := ReadVfyBlobBlock(aFI,
                aTI,
                fsc_ReadOnly,
                SegmentOfs,
                OffsetInBlock,
                aSegRelMethod);
              Try
                SegmentPtr := @SegmentBlk^[OffsetInBlock];

                { Create a list item for the segment and insert it to the list. }
                ListItem := TffBLOBSegListItem.Create;
                ListItem.Offset := SegmentOfs;
                ListItem.Size := SegmentPtr^.bshSegmentLen;
                bsmDelChain.Insert(ListItem);
                { Get the next segment. }
                SegmentOfs := SegmentPtr^.bshNextSegment;
              Finally
                aSegRelMethod(SegmentBlk);
              End;
            End;
        End; {if}
    Finally
      aFHRelMethod(PffBlock(FileHeader));
    End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Destructor TffBaseBLOBSegmentMgr.Destroy;
Var
  aSegItem, aTmpSegItem: TffBLOBSegListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    bsmDelChain.Free;
    { Clear out any remaining items from the transaction list. As of this writing,
      SQL cursors will build up a bunch of stuff within this list & not commit
      it. }
    aSegItem := bsmTranListHead;
    While aSegItem <> Nil Do
      Begin
        aTmpSegItem := aSegItem.FTranNextItem;
        aSegItem.Free;
        aSegItem := aTmpSegItem;
      End;
    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBSegmentMgr.bsmAddToTranList(aSegItem: TffBLOBSegListItem;
  anAction: TffBLOBSegAction);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    { Items are always added at the head of the list. }
    aSegItem.FTranNextItem := bsmTranListHead;
    bsmTranListHead := aSegItem;
    aSegItem.FPendingAction := anAction;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBSegmentMgr.bsmRemoveFromTranList(aSegItem: TffBlobSegListItem);
Var
  PrevItem: TffBLOBSegListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}

    PrevItem := bsmTranListHead;

    If (PrevItem = aSegItem) Then
      bsmTranListHead := aSegItem.FTranNextItem
    Else
      Begin
        { Find the previous segment. }
        While (PrevItem.FTranNextItem <> aSegItem) Do
          PrevItem := PrevItem.FTranNextItem;

        { Remove the item from the list. }
        PrevItem.FTranNextItem := aSegItem.FTranNextItem;
      End;

  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBSegmentMgr.Commit;
Var
  CurItem, TmpItem: TffBLOBSegListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    CurItem := bsmTranListHead;
    While CurItem <> Nil Do
      Begin
        TmpItem := CurItem;
        CurItem := TmpItem.FTranNextItem;
        Case TmpItem.FPendingAction Of
          bsaAddToList:
            Begin
              { Reset item's transaction info & add it to the in-memory
                deleted chain. }
              TmpItem.FPendingAction := bsaNone;
              TmpItem.FTranNextItem := Nil;
              bsmDelChain.Insert(TmpItem);
            End;
          bsaDeleteFromList:
            { Item is already removed from list so free the item. }
            TmpItem.Free;
        End; { case }
      End;
    bsmTranListHead := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBSegmentMgr.DeleteSegment(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aSegOffset: TffInt64);
Var
  aBLOBRelMethod: TffReleaseMethod;
  aFHRelMethod: TffReleaseMethod;
  FileHeader: PffBlockHeaderFile;
  OffsetInBlock: TffWord32;
  BLOBBlock: PffBlock;
  //  BLOBHeader     : PffBlockHeaderBLOB;                               {Deleted !!.13}
  DelSegPtr: PffBLOBSegmentHeader;
  BufferItem: TffBLOBSegListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}

    { Get the file header. }
    FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
      aTI,
      0,
      fsc_MarkDirty,
      aFHRelMethod, fsoNone));

    Try

      { Grab the segment to be deleted. }
      BlOBBlock := ReadVfyBlobBlock(aFI,
        aTI,
        fsc_MarkDirty,
        aSegOffset,
        OffsetInBlock,
        aBLOBRelMethod);
      DelSegPtr := @BLOBBlock^[OffsetInBlock];

      { Zero out the segment & mark it as deleted. }
      FillChar(BLOBBlock^[OffsetInBlock + sizeof(TffBLOBSegmentHeaderDel)],
        DelSegPtr^.bshSegmentLen - sizeof(TffBLOBSegmentHeaderDel),
        0); {!!.13}
      PffBLOBSegmentHeaderDel(DelSegPtr)^.bshSignature := fsc_SigBLOBSegDeleted;

      { Create our list item representing the deleted segment. }
      BufferItem := TffBLOBSegListItem.Create;
      BufferItem.Offset := aSegOffset;
      BufferItem.Size := DelSegPtr^.bshSegmentLen;

      { Assumption: Deleted list is already in memory and contains the entire
        list of deleted BLOB segments. }
      { Is there anything in the deleted list? }
      If (FileHeader^.bhfDelBLOBTail.iLow <> fsc_W32NoValue) Then
        Begin

          { Update the segments in the file. }
          bsmAddToDeletedSegChain(aFI,
            aTI,
            FileHeader,
            BufferItem,
            PffBLOBSegmentHeaderDel(DelSegPtr));

        End
      Else
        Begin
          { Nothing deleted yet.  Make this the first item in the chain. }
          With FileHeader^ Do
            Begin
              bhfDelBLOBHead := aSegOffset;
              bhfDelBLOBTail := aSegOffset;
              PffBLOBSegmentHeaderDel(DelSegPtr)^.bshPrevSegment.iLow := fsc_W32NoValue;
              PffBLOBSegmentHeaderDel(DelSegPtr)^.bshPrevSegment.iHigh := fsc_W32NoValue;
              PffBLOBSegmentHeaderDel(DelSegPtr)^.bshNextSegment.iLow := fsc_W32NoValue;
              PffBLOBSegmentHeaderDel(DelSegPtr)^.bshNextSegment.iHigh := fsc_W32NoValue;
            End;
        End;

      { Add the item to the list. }
      If bsmUseTranList Then
        bsmAddToTranList(BufferItem, bsaAddToList)
      Else
        bsmDelChain.Insert(BufferItem);

      { Decrement the used segment count in the BLOB block. }
  //    BLOBHeader := PffBlockHeaderBLOB(BLOBBlock);                            {Deleted !!.13}
  //    BLOBHeader^.bhbAssignedSegCount := BLOBHeader^.bhbAssignedSegCount - 1; {Deleted !!.13}
    Finally
      aBLOBRelMethod(BLOBBlock);
      aFHRelMethod(PffBlock(FileHeader));
    End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBSegmentMgr.bsmAddToDeletedSegChain(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aFileHeader: PffBlockHeaderFile;
  aDelSeg: TffBLOBSegListItem;
  aSegment: PffBLOBSegmentHeaderDel);
Var
  PrevSegment: PffBLOBSegmentHeaderDel;
  BLOBBlock: PffBlock;
  OffsetInBlock: TffWord32;
  aRelMethod: TffReleaseMethod;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    { Assumptions: Deleted list contains at least one segment.
        Segments are sorted by size when first read from disk so it is not
        necessary to maintain sort order on disk. }
    { Get the last segment in the chain. }
    BLOBBlock := ReadVfyBlobBlock(aFI,
      aTI,
      fsc_MarkDirty,
      aFileHeader^.bhfDelBLOBTail,
      OffsetInBlock,
      aRelMethod);
    PrevSegment := @BLOBBlock^[OffsetInBlock];

    { Point the last segment to the new deleted segment & vice versa. }
    PrevSegment^.bshNextSegment := aDelSeg.Offset;
    aSegment^.bshNextSegment.iLow := fsc_W32NoValue;
    aSegment^.bshNextSegment.iHigh := fsc_W32NoValue;
    aSegment^.bshPrevSegment := aFileHeader^.bhfDelBLOBTail;
    aRelMethod(BLOBBlock);

    { Mark the new deleted segment as the end of the chain. }
    aFileHeader^.bhfDelBLOBTail := aDelSeg.Offset;

  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Function TffBaseBLOBSegmentMgr.GetNewSeg(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aSize: TffWord32): TffInt64;
Var
  BLOBBlock: PffBlock;
  DelSegHeader: PffBLOBSegmentHeaderDel;
  TempI64: TffInt64;
  NewSegHeader: PffBLOBSegmentHeader;
  aRelMethod: TffReleaseMethod;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    { Create a new BLOB block. }
    BLOBBlock := FFTblHlpGetNewBlock(aFI, aTI, aRelMethod, fsoNone);
    Try
      PffBlockHeaderBLOB(BLOBBlock)^.bhbSignature := fsc_SigBLOBBlock;
      PffBlockHeaderBLOB(BLOBBlock)^.bhbNextBlock := fsc_W32NoValue;
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
      If aSize < aFI^.fiMaxSegSize Then
        Begin
          //      PffBlockHeaderBLOB(BLOBBlock)^.bhbAssignedSegCount := 2;        {Deleted !!.13}
          DelSegHeader := PffBLOBSegmentHeaderDel(@BLOBBlock^[fsc_BlockHeaderSizeBLOB + aSize]);
          DelSegHeader^.bshSegmentLen := aFI^.fiMaxSegSize - aSize;
          { Set TempI64 to file offset of deleted segment and add it to deleted
            chain. }
          ffI64AddInt(Result, aSize, TempI64);
          DeleteSegment(aFI, aTI, TempI64);
        End
      Else
        {block only has 1 segment if the new segment was max seg size}
    //    PffBlockHeaderBLOB(BLOBBlock)^.bhbAssignedSegCount := 1;         {Deleted !!.13}
    Finally
      aRelMethod(BLOBBlock);
    End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBSegmentMgr.bsmSliceSegment(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aSegOfs: TffInt64;
  aSegSize: TffWord32;
  Const aNewSize: TffWord32;
  aInDelChain: Boolean);
Var
  BLOBBlock: PffBlock;
  BlockNum: TffWord32;
  DelSegHeader: PffBLOBSegmentHeaderDel;
  OffsetInBlock: TffWord32;
  TempI64: TffInt64;
  TempI64b: TffInt64;
  ThisSeg: PffBLOBSegmentHeaderDel;
  aRelMethod: TffReleaseMethod;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    { Post condition: New segment of aSize is always at aSegOfs. }

    { Remove the segment we're slicing from the deleted chain. }
    If (aInDelChain) Then
      bsmRemoveFromDeletedChain(aFI, aTI, aSegOfs);

    { Get the segment to be sliced. }
    BLOBBlock := ReadVfyBlobBlock(aFI,
      aTI,
      fsc_MarkDirty,
      aSegOfs,
      OffsetInBlock,
      aRelMethod);
    Try
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
      ffI64MinusInt(TempI64, (BlockNum Shl aFI^.fiLog2BlockSize), TempI64);
      DelSegHeader := @BLOBBlock^[TempI64.iLow];

      { Initialize the deleted segment. }
      DelSegHeader^.bshSegmentLen := (aSegSize - aNewSize);
      DelSegHeader^.bshPrevSegment.iLow := fsc_W32NoValue;
      DelSegHeader^.bshPrevSegment.iHigh := fsc_W32NoValue;
      DelSegHeader^.bshNextSegment.iLow := fsc_W32NoValue;
      DelSegHeader^.bshNextSegment.iHigh := fsc_W32NoValue;

      { Put the new unused segment back in the chain. }
      TempI64b.iLow := BlockNum;
      TempI64b.iHigh := 0;
      ffShiftI64L(TempI64b, aFI^.fiLog2BlockSize, TempI64b);
      ffI64AddInt(TempI64b, TempI64.iLow, TempI64);
      DeleteSegment(aFI, aTI, TempI64);
    Finally
      aRelMethod(BLOBBlock);
    End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBSegmentMgr.bsmRemoveFromDeletedChain(aFI: PffFileInfo;
  aTI: PffTransInfo;
  aSegOfs: TffInt64);
Var
  aFileHeader: PffBlockHeaderFile;
  OffsetInBlock: TffWord32;
  ThisSegBlock: PffBlock;
  ThisSeg: PffBLOBSegmentHeaderDel;
  PrevSegBlock: PffBlock;
  PrevSeg: PffBLOBSegmentHeaderDel;
  NextSegBlock: PffBlock;
  NextSeg: PffBLOBSegmentHeaderDel;
  aFHRelMethod,
    aSegRelMethod,
    aSegRelMethod2: TffReleaseMethod;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    { Assumptions: This segment has already been removed from the in-memory{
                   deleted list. }

    { First get the file header, block 0. }
    aFileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI, aTI, 0, fsc_MarkDirty,
      aFHRelMethod, fsoNone));
    Try
      { Get the block. }
      ThisSegBlock := ReadVfyBlobBlock(aFI,
        aTI,
        fsc_MarkDirty,
        aSegOfs,
        OffsetInBlock,
        aSegRelMethod);
      Try
        ThisSeg := @ThisSegBlock^[OffsetInBlock];

        { Is there a segment before this segment? }
        If ThisSeg^.bshPrevSegment.iLow <> fsc_W32NoValue Then
          Begin
            { Yes.  Point the prior segment to the next segment. }
            PrevSegBlock := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
              ThisSeg^.bshPrevSegment, OffsetInBlock,
              aSegRelMethod2);
            PrevSeg := @PrevSegBlock^[OffsetInBlock];
            PrevSeg^.bshNextSegment := ThisSeg^.bshNextSegment;

            { If the removed segment was the tail then update the tail on the
              file header. }
            If PrevSeg^.bshNextSegment.iLow = fsc_W32NoValue Then
              aFileHeader^.bhfDelBLOBTail := ThisSeg^.bshPrevSegment;

            aSegRelMethod2(PrevSegBlock);

          End
        Else
          { No. This segment was the head.  Update the head on the file header. }
          aFileHeader^.bhfDelBLOBHead := ThisSeg^.bshNextSegment;

        { Is there a segment after this segment? }
        If ThisSeg^.bshNextSegment.iLow <> fsc_W32NoValue Then
          Begin
            { Yes.  Point the next segment back to the prior segment. }
            NextSegBlock := ReadVfyBlobBlock(aFI, aTI, fsc_MarkDirty,
              ThisSeg^.bshNextSegment, OffsetInBlock,
              aSegRelMethod2);
            NextSeg := @NextSegBlock^[OffsetInBlock];
            NextSeg^.bshPrevSegment := ThisSeg^.bshPrevSegment;

            { If the removed segment was the head of the chain then update the head
              in the file header. }
            If NextSeg^.bshPrevSegment.iLow = fsc_W32NoValue Then
              aFileHeader^.bhfDelBLOBHead := ThisSeg^.bshNextSegment;

            aSegRelMethod2(NextSegBlock);

          End
        Else
          { No.  This was the tail segment.  Update the tail in the file header. }
          aFileHeader^.bhfDelBLOBTail := ThisSeg^.bshPrevSegment;
      Finally
        aSegRelMethod(ThisSegBlock);
      End;
    Finally
      aFHRelMethod(PffBlock(aFileHeader));
    End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure WriteToStream(Const aMsg: String; aStream: TStream);
Begin
  aStream.Write(aMsg[1], Length(aMsg));
End;
{--------}

Procedure TffBaseBLOBSegmentMgr.ListFreeSpace(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Const aInMemory: Boolean;
  aStream: TStream);
Var
  aRelMethod,
    aFHRelMethod: TffReleaseMethod;
  anInx: Longint;
  aSegItem: TffBLOBSegListItem;
  aSegment: TffInt64;
  aStr: String;
  BLOBBlock: PffBlock;
  DelSegment: PffBLOBSegmentHeaderDel;
  FileHeader: PffBlockHeaderFile;
  OffsetInBlock: TffWord32;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}

    { Write the segment manager's in-memory list or the list as saved to the
      file? }
    If aInMemory Then
      Begin
        { In-memory list. }
        WriteToStream('In-memory deleted chain:' + #13#10, aStream);
        For anInx := 0 To Pred(bsmDelChain.Count) Do
          Begin
            aSegItem := TffBLOBSegListItem(bsmDelChain[anInx]);
            aStr := IntToStr(anInx) + ': Size ' + IntToStr(aSegItem.Size) +
              ', Offset ' + IntToStr(aSegItem.Offset.iHigh) +
              ':' + IntToStr(aSegItem.Offset.iLow);
            Case aSegItem.FPendingAction Of
              bsaAddToList: aStr := aStr + ', add';
              bsaDeleteFromList: aStr := aStr + ', del';
            End; { case }
            aStr := aStr + #13#10;
            WriteToStream(aStr, aStream);
          End;

        If bsmTranListHead <> Nil Then
          Begin
            WriteToStream('Transaction list:' + #13#10, aStream);
            aSegItem := bsmTranListHead;
            anInx := 0;
            While aSegItem <> Nil Do
              Begin
                aStr := Format('%d : Size %d, Offset %d:%d',
                  [anInx, aSegItem.Size, aSegItem.Offset.iHigh,
                  aSegItem.Offset.iLow]);
                aStr := aStr + ', Pending: ';
                Case aSegItem.FPendingAction Of
                  bsaNone: aStr := aStr + 'N/A';
                  bsaAddToList: aStr := aStr + 'add';
                  bsaDeleteFromList: aStr := aStr + 'del';
                End; { case }
                aSegItem := aSegItem.FTranNextItem;
                inc(anInx);
                aStr := aStr + #13#10;
                WriteToStream(aStr, aStream);
              End;
          End
        Else
          Begin
            WriteToStream(#13#10 + 'Transaction list: EMPTY', aStream);
          End;
      End
    Else
      Begin
        { The list as saved to file. Need to walk through the BLOB deleted chain. }
        { Get the file header. }
        FileHeader := PffBlockHeaderFile(FFBMGetBlock(aFI,
          aTI,
          0,
          fsc_ReadOnly,
          aFHRelMethod, fsoNone));
        Try
          { BLOB deleted chain is empty? }
          If FileHeader^.bhfDelBLOBHead.iLow = fsc_W32NoValue Then
            Begin
              { Yes. Write blurb & exit. }
              WriteToStream('BLOB deleted chain is empty.', aStream);
              WriteToStream(#0, aStream);
              Exit;
            End;

          { Not empty. Walk through the chain. }
          anInx := 0;
          aSegment := FileHeader^.bhfDelBLOBHead;
          While (aSegment.iLow <> fsc_W32NoValue) Do
            Begin
              { Get the block containing the segment. }
              BLOBBlock := ReadVfyBlobBlock(aFI,
                aTI,
                fsc_ReadOnly,
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
            End;
        Finally
          aFHRelMethod(PffBlock(FileHeader));
        End;
      End;
    WriteToStream(#0, aStream);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Procedure TffBaseBLOBSegmentMgr.Rollback;
Var
  CurItem, TmpItem: TffBLOBSegListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    CurItem := bsmTranListHead;
    While CurItem <> Nil Do
      Begin
        TmpItem := CurItem;
        CurItem := TmpItem.FTranNextItem;
        Case TmpItem.FPendingAction Of
          bsaAddToList:
            { The item won't be added to the in-memory deleted chain so free
              the item. }
            TmpItem.Free;
          bsaDeleteFromList:
            Begin
              { The item has been removed from the in-memory deleted list. We need
                to reset its transaction info & add it back to the list. }
              TmpItem.FPendingAction := bsaNone;
              TmpItem.FTranNextItem := Nil;
              bsmDelChain.Insert(TmpItem);
            End;
        End; { case }
      End;
    bsmTranListHead := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{====================================================================}

{===TffBLOBSegListItem===============================================}

Constructor TffBLOBSegListItem.Create;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    Inherited Create;
    fOffset.iLow := 0;
    fOffset.iHigh := 0;
    fSize := 0;
    MaintainLinks := False;
    FPendingAction := bsaNone;
    FTranNextItem := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{--------}

Function TffBLOBSegListItem.Compare(aKey: pointer): Integer;
Begin
  Result := FFCmpI32(fSize, Longint(aKey^));
  If Result = 0 Then
    Result := 1;
End;
{--------}

Function TffBLOBSegListItem.Key: pointer;
Begin
  Result := @fSize;
End;
{====================================================================}

{===TffBLOBSegmentMgr================================================}

Function TffBLOBSegmentMgr.GetRecycledSeg(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aSizeNeeded: Longint;
  Const aMinSizeAllowed: Longint)
  : TffInt64;
Var
  //  BLOBBlock    : PffBlock;                                           {Deleted !!.13}
  L, R, M: Integer;
  OldSegSize: Integer;
  //  aRelMethod   : TffReleaseMethod;                                   {Deleted !!.13}
  SearchSize: Longint;
  aPrevSegItem,
    aSegItem: TffBLOBSegListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    { Max TffInt64 returned if segment of aSize not available. }
    Result.iLow := fsc_W32NoValue;
    Result.iHigh := fsc_W32NoValue;

    { Is there a segment in the segment manager's transaction list? }
    If (bsmUseTranList) Then
      Begin
        { We are looking for a segment that is being added to the deleted
          segment list and is at least as big as the segment we need. }
        aPrevSegItem := Nil;
        aSegItem := bsmTranListHead;
        While (aSegItem <> Nil) Do
          Begin
            If (aSegItem.FPendingAction = bsaAddTolist) Then
              Begin
                If (aSegItem.FSize > aSizeNeeded) Then
                  Begin
                    { Too big so we'll keeep looking. If we don't find a more
                      optimum sized segment, we'll use this one. }
                    aPrevSegItem := aSegItem;
                  End
                Else
                  Begin
                    If (aSegItem.FSize < aMinSizeAllowed) Then
                      aSegItem := aPrevSegItem;
                    Break;
                  End;
              End;

            If (aSegItem.FTranNextItem = Nil) Then
              Begin
                aSegItem := aPrevSegItem;
                Break;
              End
            Else
              aSegItem := aSegItem.FTranNextItem
          End;

        { Did we find one in the transaction list? }
        If (aSegItem <> Nil) Then
          Begin
            { Yes. Prepare to return it. }
            Result := aSegItem.FOffset;
            bsmRemoveFromTranList(aSegItem);
            bsmRemoveFromDeletedChain(aFI, aTI, Result);
            { Do we need to slice it down to the correct size? }
            If (aSegItem.FSize > aSizeNeeded) Then
              Begin
                bsmSliceSegment(aFI,
                  aTI,
                  Result,
                  aSegItem.FSize,
                  aSizeNeeded,
                  False);
              End
            Else If (aSegItem.FSize < aSizeNeeded) Then
              aSizeNeeded := aSegItem.FSize;
            aSegItem.Free;
            Exit;
          End;
      End;

    { We can exit if the list is empty or if there is not a segment big enough
      for the minimum size. }
    If (bsmDelChain.IsEmpty) Or
      (Pinteger(bsmDelChain[0].Key)^ < aMinSizeAllowed) Then
      Exit;

    { Determine the size of segment to search for. }
    If PInteger(bsmDelChain[0].Key)^ < aSizeNeeded Then
      SearchSize := aMinSizeAllowed
    Else
      SearchSize := aSizeNeeded;

    { We know the list doesn't contain the exact size we're looking for,
      but it does contain one that we can "slice" to the right size.
      - using a standard binary search, we will slice L - 1}
    L := 0;
    R := pred(bsmDelChain.Count);
    Repeat
      M := (L + R) Div 2;
      aSegItem := TffBLOBSegListItem(bsmDelChain[M]);
      If (aSegItem.Size < SearchSize) Then
        R := M - 1
      Else If (aSegItem.Size > SearchSize) Then
        L := M + 1
      Else {found it}
        Begin
          Result := aSegItem.Offset;
          If bsmUseTranList Then
            Begin
              bsmAddToTranList(aSegItem, bsaDeleteFromList);
              bsmDelChain.RemoveAt(M);
            End
          Else
            bsmDelChain.DeleteAt(M);
          bsmRemoveFromDeletedChain(aFI, aTI, Result);
          Break;
        End;
    Until (L > R);
    If (L > R) And (L > 0) Then
      Begin
        {the item just bigger is at L-1}
        dec(L);
        aSegItem := TffBLOBSegListItem(bsmDelChain[L]);
        Result := aSegItem.Offset;
        OldSegSize := aSegItem.Size;
        If bsmUseTranList Then
          Begin
            bsmAddToTranList(aSegItem, bsaDeleteFromList);
            bsmDelChain.RemoveAt(L);
          End
        Else
          Begin
            bsmDelChain.DeleteAt(L);
          End;
        bsmSliceSegment(aFI,
          aTI,
          Result,
          OldSegSize,
          SearchSize,
          True);
      End;
    aSizeNeeded := SearchSize;

    { Get the segment's block & update the used segment count. }
  {Begin !!.13}
  //  BLOBBlock := ReadVfyBlobBlock3(aFI, aTI, fsc_MarkDirty, Result, aRelMethod);
  //  inc(PffBlockHeaderBLOB(BLOBBlock)^.bhbAssignedSegCount);
  //  aRelMethod(BLOBBlock);
  {End !!.13}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{====================================================================}

{===Tff210BLOBSegmentMgr=============================================}

Function Tff210BLOBSegmentMgr.GetRecycledSeg(aFI: PffFileInfo;
  aTI: PffTransInfo;
  Var aSizeNeeded: Longint;
  Const aMinSizeAllowed: Longint)
  : TffInt64;
Var
  //  BLOBBlock  : PffBlock;                                             {Deleted !!.13}
  L, R, M: Integer;
  OldSegSize: Integer;
  //  aRelMethod : TffReleaseMethod;                                     {Deleted !!.13}
  aSegItem: TffBLOBSegListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    { Max TffInt64 returned if segment of aSize not available. }
    Result.iLow := fsc_W32NoValue;
    Result.iHigh := fsc_W32NoValue;

    { Is there a segment in the segment manager's transaction list? }
    If (bsmUseTranList) Then
      Begin
        { We are looking for a segment that is being added to the deleted
          segment list and is at least as big as the segment we need. }
        aSegItem := bsmTranListHead;
        While (aSegItem <> Nil) Do
          Begin
            If ((aSegItem.FPendingAction = bsaAddToList) And
              (aSegItem.FSize >= aSizeNeeded)) Then
              Begin
                Result := aSegItem.FOffset;
                bsmRemoveFromTranList(aSegItem);
                bsmRemoveFromDeletedChain(aFI, aTI, Result);
                { Do we need to slice it down to the correct size? }
                If (aSegItem.FSize > aSizeNeeded) Then
                  bsmSliceSegment(aFI,
                    aTI,
                    Result,
                    aSegItem.FSize,
                    aSizeNeeded,
                    False);
                aSegItem.Free;
                Exit;
              End;
            aSegItem := aSegItem.FTranNextItem;
          End;
      End;

    If (bsmDelChain.IsEmpty) Then
      Exit;
    If (Pinteger(bsmDelChain[0].Key)^ < aSizeNeeded) Then
      Exit;

    {we know the list doesn't contain the exact size we're looking for,
     but it does contain one that we can "slice" to the right size.
     - using a standard binary search, we will slice L - 1}
    L := 0;
    R := pred(bsmDelChain.Count);
    Repeat
      M := (L + R) Div 2;
      aSegItem := TffBLOBSegListItem(bsmDelChain[M]);
      If (aSegItem.Size < aSizeNeeded) Then
        R := M - 1
      Else If (aSegItem.Size > aSizeNeeded) Then
        L := M + 1
      Else {found it}
        Begin
          Result := aSegItem.Offset;
          If bsmUseTranList Then
            Begin
              bsmAddToTranList(aSegItem, bsaDeleteFromList);
              bsmDelChain.RemoveAt(M);
            End
          Else
            bsmDelChain.DeleteAt(M);
          bsmRemoveFromDeletedChain(aFI, aTI, Result);
          Break;
        End;
    Until (L > R);
    If (L > R) And (L > 0) Then
      Begin
        {the item just bigger is at L-1}
        dec(L);
        aSegItem := TffBLOBSegListItem(bsmDelChain[L]);
        Result := aSegItem.Offset;
        OldSegSize := aSegItem.Size;
        If bsmUseTranList Then
          Begin
            bsmAddToTranList(aSegItem, bsaDeleteFromList);
            bsmDelChain.RemoveAt(L);
          End
        Else
          Begin
            bsmDelChain.DeleteAt(L);
          End;
        bsmSliceSegment(aFI,
          aTI,
          Result,
          OldSegSize,
          aSizeNeeded,
          True);
      End;
    { Get the segment's block & update the used segment count. }
  {Begin !!.13}
  //  BLOBBlock := ReadVfyBlobBlock3(aFI, aTI, fsc_MarkDirty, Result, aRelMethod);
  //  inc(PffBlockHeaderBLOB(BLOBBlock)^.bhbAssignedSegCount);
  //  aRelMethod(BLOBBlock);
  {End !!.13}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{====================================================================}
{End !!.11}

{===Initialization/Finalization======================================}

Procedure FinalizeUnit;
Begin
  Pool4k.Free;
  Pool8k.Free;
  Pool16k.Free;
  Pool32k.Free;
  Pool64k.Free;
  fsStrResServer.Free;
  If (EncryptBuffer <> Nil) Then
    FreeMem(EncryptBuffer, 64 * 1024);

  {$IFDEF RAMPageCheck}
  aLog.Flush;
  aLog.Free;
  {$ENDIF}
End;
{--------}

Procedure InitializeUnit;
Begin
  Pool4k := Nil;
  Pool8k := Nil;
  Pool16k := Nil;
  Pool32k := Nil;
  Pool64k := Nil;
  EncryptBuffer := Nil;
  fsStrResServer := Nil;
  fsStrResServer := TfsStringResource.Create(hInstance, 'FS_SERVER_STRINGS');

  {$IFDEF RAMPageCheck}
  aLog := TffEventLog.Create(Nil);
  aLog.FileName := 'RAMPage.lg';
  aLog.Enabled := True;
  {$ENDIF}
End;
{--------}

Initialization
  InitializeUnit;

Finalization
  FinalizeUnit;

End.

