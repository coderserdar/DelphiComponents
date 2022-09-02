{*********************************************************}
{* FSSQL:      Database lock manager                     *}
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

{.$DEFINE LockLogging}
{$DEFINE UseLockContainerPool}

Unit fssrlock;

Interface
Uses
  {$IFDEF LockLogging}
  fslllog,
  {$ENDIF}
  SysUtils,
  Classes,
  Windows,
  Forms,
  fsllbase,
  fsllthrd,
  fshash,
  fssrbase,
  fssrbde,
  fsllexcp,
  fsconst;

Resourcestring
  ffcLockNone = 'None';
  ffcLockIntentS = 'Intent Shared';
  ffcLockIntentX = 'Intent Exclusive';
  ffcLockShare = 'Share';
  ffcLockSIX = 'Shared Intent Exclusive';
  ffcLockUpdate = 'Update';
  ffcLockExclusive = 'Exclusive';
  {$IFDEF LockLogging}
  ffcDurationInstant = 'Instant';
  ffcDurationShort = 'Short';
  ffcDurationCommit = 'Commit';
  ffcRStatusGranted = 'Granted';
  ffcRStatusTimeout = 'Timeout';
  ffcRStatusDeadlock = 'Deadlock';
  ffcRStatusRejected = 'Rejected';
  {$ENDIF}

Type
  { This type is used to signify the type of lock requested, or the mode of
    a granted group within the lock manager.

      ffsltNone      The resource is not locked.
      ffsltIntentS   Intent Shared (IS) lock.
      ffsltIntentX   Intent Exclusive (IX) lock.
      ffsltShare     Share lock.
      ffsltSIX       Share Intent Exclusive (SIX) lock.
      ffsltUpdate    Update lock.  This lock is handy when a resource has to
                     be read with the intent of updating the resource.  The
                     Update lock is granted even if there are other Share locks
                     on the resource.  When the Update lock is granted, it
                     prevents all other locks from being granted on the
                     resource.  When the resource is to be modified, it must
                     be converted to an ffsltExclusive lock.
      ffsltExclusive Exclusive lock.
  }
  TfsSrcLockType = (ffsltNone, ffsltIntentS, ffsltIntentX, ffsltShare,
    ffsltSIX, ffsltUpdate, ffsltExclusive);

  { The result of a lock request.

     fflrsGranted  The lock was granted.
     fflrsTimeOut  The lock request timed out.
     fflrsRejected The lock request was rejected.  This is typically
                   returned when the request is for an instant duration
                   lock and the lock could not be granted.
  }
  TffLockRequestStatus = (fflrsGranted, fflrsTimeout, fflrsRejected);

  { The status of a lock request within a lock's request queue.

      fflsNone     No status. Typically used when the request is first
                   issued.
      fflsGranted  The lock has been granted to the request.
      fflsWaiting  The request is waiting for the lock to be granted.
      fflsWaitingConv The request is waiting for a conversion request to be
                      granted.
  }
  TffLockStatus = (fflsRejected, fflsGranted, fflsWaiting, fflsWaitingConv);

  TfsLockListItem = Class(TFSSpecListItem)
  Private
    FConversion: Boolean;
    {Begin !!.10}
    FPrimaryID: TffBaseID;
    { The unique ID of the server-side object requesting a lock. The object
      may be a client, session, database, or cursor. }
    F2ndaryID: TffBaseID;
    { F2ndaryID is used for record locks. If it has the value zero then
      a cursor associated with database FPrimaryID edited and released the
      record. It is okay for a cursor associated with the same database to
      lock the record. If non-zero then a cursor associated with database
      FPrimaryID currently has the record locked. It is not okay to lock
      the record for another cursor associated with the same database. }
{End !!.10}
    FLockType: TfsSrcLockType;
    FEvent: TFSNormalEvent;
    FRefCount: Integer;
    FStatus: TffLockStatus;
    FTransaction: TfsSrcTransaction;
  Protected
  Public
    Constructor Create(Const aKey: TffBaseID); {!!.10}
    {-create the list item; aKey is its access/sort key}
    Function Compare(aKey: pointer): Integer; Override;
    {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise}
    Function Key: pointer; Override;
    {-return a pointer to this item's key: it'll be a pointer to a
      cursorID }

    Property LockType: TfsSrcLockType
      Read FLockType Write FLockType;
    {Begin !!.10}
    Property PrimaryID: TffBaseID
      Read FPrimaryID;
    Property SecondaryID: TffBaseID
      Read F2ndaryID;
    {End !!.10}
    Property Status: TffLockStatus
      Read FStatus Write FStatus;
    Property Transaction: TfsSrcTransaction
      Read FTransaction Write FTransaction;
    Property Event: TFSNormalEvent
      Read FEvent Write FEvent;
    Property RefCount: Integer
      Read FRefCount Write FRefCount;
    Property Conversion: Boolean
      Read FConversion Write FConversion;
  End;

  TfsLockQueue = Class(TFSSpecQueue)
  Public
    Function Peek: TFSSpecListItem;
    { Retrieve a pointer to the first item in the list }
    Procedure EnqueuePriority(anItem: TFSSpecListItem);
    { Queue and item, but place it first in the queue }
  End;

  { One TfsLockContainer is required for each ResourceID, no two resources
    share a LockContainer. TfsLockContainer is responsible for maintaining the
    following:

    1. A list of granted lock requests.
    2. A queue of waiting lock requests.
    3. A queue of conversion lock requests. }
  TfsLockContainer = Class(TFSSpecThreadList)
  Protected
    FWaitQueue: TfsLockQueue;
    FWaitConversionQueue: TfsLockQueue;
    Function AddLock(Const Granted: Boolean;
      Const Conditional: Boolean;
      LockItem: TfsLockListItem): TffLockStatus;
    Function LastLock: TfsLockListItem;
    Procedure ProcessQueue;
    Procedure ProcessLockConversion(Const aCursorID: TffCursorID;
      aLockListItem: TfsLockListItem);
  Public
    Constructor Create; Override;
    Destructor Destroy; Override;
    Function IsEmpty: boolean;
    Procedure RelaxRecordLock(Const aDatabaseID: TffCursorID); {!!.10}
    Procedure ReleaseCursorLock(Const aCursorID: TffCursorID;
      Var aRefCount: Integer);
    { Release the specified cursor lock.  If the lock's reference count
      is greater than one then the reference count is decremented. }
    Procedure CountCursorLock(Const aCursorID: TffCursorID;
      Var aRefCount: Integer);
    Procedure ReleaseCursorLockAll(Const aCursorID: TffCursorID);
    Procedure ReleaseWaitingConversion(Const RequestorID: TffBaseID); {!!.10}
    Procedure ReleaseWaitingLock(Const RequestorID: TffBaseID); {!!.10}
    Function RequestLock(Const LockType: TfsSrcLockType;
      Const Conditional: Boolean;
      Const Transaction: TfsSrcTransaction;
      Const RequestorID: TffBaseID; {!!.10}
      Var WaitEvent: TFSNormalEvent): TffLockStatus;
    {Begin !!.10}
            { Used to request all types of locks excluding record locks. }
    Function RequestRecLock(Const LockType: TfsSrcLockType;
      Const Conditional: Boolean;
      Const Transaction: TfsSrcTransaction;
      Const ReqPrimaryID,
      ReqSecondaryID: TffBaseID;
      Var WaitEvent: TFSNormalEvent): TffLockStatus;
    { Used to request record locks. }
{End !!.10}
{Begin !!.03}
    Function SimpleDeadlock: Boolean;
    { See if a simple deadlock situation may occur. Assumes that this
      method is called only when a) the specified lock container has
      granted locks to more than one transaction, b) the specified
      Transaction has been granted a share lock, & c) the specified
      transaction plans to request an Exclusive lock but has not
      submitted the request at the time of this method call. }
{End !!.03}
    Function SummaryMode: TfsSrcLockType;
  End;

  TfsLockManager = Class(TFSSpecObject)
  Protected
    {$IFDEF LockLogging}
    FEventLog: TFSNormalEventLog;
    {$ENDIF}
    FTableLocks: TfsThreadHash;
    FTransactions: TFSSpecThreadList;
    FStartWithLock: TfsPadlock; {!!.10}
    Procedure DisposeLockList(Sender: TfsBaseHashTable; aData: Pointer);
    Procedure DisposeRecordLockList(Sender: TfsBaseHashTable; aData: Pointer);
    Procedure ReleaseContentLockAll(Container: TfsLockContainer;
      Transaction: TfsSrcTransaction);

  Public
    Constructor Create;
    Destructor Destroy; Override;

    Function AcquireClientLock(Const Container: TfsLockContainer;
      Const CursorID: TffCursorID;
      Const Timeout: TffWord32;
      Const LockType: TfsSrcLockType): TffLockRequestStatus;
    { Use this method to obtain client locks on a server table
      (e.g., TfsTable.LockTable).  Container is the table's client lock
      container. Cursor is the cursor requesting the lock. }

    Function AcquireContentLock(Const Container: TfsLockContainer;
      Const ParentTable: TFSSpecObject;
      Const Transaction: TfsSrcTransaction;
      Const Conditional: Boolean;
      Const Timeout: TffWord32;
      Const LockType: TfsSrcLockType): TffLockRequestStatus;
    { Use this method to acquire a content lock on a server table for a
      transaction.  Container is the table's content lock container.
      Transaction is the transaction requesting the content lock. }

    Function AcquireRecordLock(Const ResourceID: TffInt64;
      Const FI: PffFileInfo;
      Const LockType: TfsSrcLockType;
      Const Conditional: Boolean;
      Const Timeout: TffWord32;
      Const Transaction: TfsSrcTransaction;
      Const DatabaseID: TffDatabaseID; {!!.10}
      Const CursorID: TffCursorID;
      Const TrLock: TfsDataBaseRecLocking): TffLockRequestStatus;
    { CursorID requests a RecordLock of type LockType on ResourceID/FileID
      for Duration. }

    Function AcquireTableLock(Const ResourceID: TffWord32;
      Const LockType: TfsSrcLockType;
      Const Conditional: Boolean;
      Const Timeout: TffWord32;
      Const CursorID: TffCursorID): TffLockRequestStatus;
    { CursorID requests a TableLock of type LockType on ResourceID
      for Duration. }

    Procedure GetWaitingRecordLocks(Const ResourceID: TffInt64;
      Const FI: PffFileInfo;
      Const Transaction: TfsSrcTransaction;
      Var WaitingLocks: TfsPointerList);

    Function HasClientLock(Const Container: TfsLockContainer;
      Const CursorID: TffCursorID): boolean;
    { Returns True if the client has any kind of client lock on the table. }

    Function IsContentLockedBy(Const Container: TfsLockContainer;
      Const Transaction: TfsSrcTransaction): boolean;
    { Does the specified transaction have any kind of content lock on the
      table?  Returns True if the transaction has any kind of content lock. }

    Function IsTableLockedBy(Const aResourceID: TffWord32;
      Const aCursorID: TffCursorID;
      Const aLockType: TfsSrcLockType): Boolean;
    { Returns True if a lock of the specified type was granted to the
      specified cursor. }

    Function IsRecordLocked(Const aResourceID: TffInt64;
      Const aFI: PffFileInfo): Boolean;
    { Returns True if the record is locked.  Assumption: FF only requests
      Exclusive record locks. }

    Function WhoRecordLocked(Const aResourceID: TffInt64;
      Const aFI: PffFileInfo): String;
      
    Function TableLockGranted(Const ResourceID: Longint): TfsSrcLockType;
    { Returns the summary mode for table ResourceID. If a lock is not
      present, this routine returns ffslNone. }

    Function RecordLockGranted(Const ResourceID: TffInt64;
      Const FI: PffFileInfo): TfsSrcLockType;
    { Returns the summary mode for record ResourceID. If a lock is not
      present, this routine returns ffslNone. }

{Begin !!.10}
    Procedure RelaxRecordLock(Const ResourceID: TffInt64;
      Const FI: PffFileInfo;
      Const DatabaseID: TffDatabaseID);
    { Called after a successful insert, update, or delete so that another
      cursor within the same transaction may obtain a record lock on the
      same record. }
{End !!.10}

    Procedure ReleaseClientLock(Const Container: TfsLockContainer;
      Const CursorID: TffCursorID);

    Procedure ReleaseClientLockAll(Const Container: TfsLockContainer;
      Const CursorID: TffCursorID);
    Procedure ReleaseClientW(Const Container: TfsLockContainer;
      Const CursorID: TffCursorID);
    Procedure ReleaseClientWC(Const Container: TfsLockContainer;
      Const CursorID: TffCursorID);

    Procedure ReleaseContentLock(Const Container: TfsLockContainer;
      Const Transaction: TfsSrcTransaction);
    Procedure ReleaseContentW(Const Container: TfsLockContainer;
      Const Transaction: TfsSrcTransaction);
    Procedure ReleaseContentWC(Const Container: TfsLockContainer;
      Const Transaction: TfsSrcTransaction);

    Procedure ReleaseRecordLock(Const ResourceID: TffInt64;
      Const FI: PffFileInfo;
      Const Transaction: TfsSrcTransaction;
      Const DatabaseID: TffDatabaseID); {!!.10}

    Function FindMyRecordLock(Const ResourceID: TffInt64;
      Const FI: PffFileInfo;
      Const Transaction: TfsSrcTransaction;
      Const DatabaseID: TffDatabaseID): Longint;

    Procedure ReleaseRecordLockAll(Const ResourceID: TffInt64;
      Const FI: PffFileInfo;
      Const Transaction: TfsSrcTransaction;
      Const DatabaseID: TffDatabaseID); {!!.10}
    { Releases a record lock for a cursor regardless of the record lock's
      reference count.
      Parameters:
        ResourceID - The reference number of the record that was locked.
        FI - The file containing the locked record.
        Transaction - The transaction in which the record was locked.
        CursorID - The cursor locking the record. }

    Procedure RelRecLockIterator(aKey: TffInt64; aData: pointer;
      Const cookie1, cookie2, cookie3: TffWord32);
    { Used to free record locks held by a transaction. }

    Procedure ReleaseRecordW(Const ResourceID: TffInt64;
      Const FI: PffFileInfo;
      Const DatabaseID: TffDatabaseID); {!!.10}

    Procedure ReleaseRecordWC(Const ResourceID: TffInt64;
      Const FI: PffFileInfo;
      Const DatabaseID: TffDatabaseID); {!!.10}

    Procedure ReleaseTableLock(Const ResourceID: TffWord32;
      Const CursorID: TffCursorID);

    Procedure ReleaseTableLockAll(Const aResourceID: Longint;
      Const aCursorID: TffCursorID);

    Procedure ReleaseTableW(Const ResourceID: TffWord32;
      Const CursorID: TffCursorID);

    Procedure ReleaseTableWC(Const ResourceID: TffWord32;
      Const CursorID: TffCursorID);

    Procedure ReleaseTransactionLocks(Const Transaction: TfsSrcTransaction;
      Const RecordsOnly: boolean);
    { Call this method when committing a transaction.  Transaction is the
      transaction whose locks are to be freed.  If only record locks are to
      be freed, set RecordsOnly to True.  If both content and record locks
      are to be freed, set RecordsOnly to False. }
    Procedure ReleaseTransactionLocksInTable(aFI: PffFileInfo; Const Transaction: TfsSrcTransaction;
      Const RecordsOnly: boolean);
    {Begin !!.10}
    Property StartWithLock: TfsPadlock Read FStartWithLock;
    { Used by the TffServerEngine.TransactionStartWith method. }
{End !!.10}

  End;

  { This class tracks what record locks and content locks have been acquired
    by a transaction over the course of the transaction's life.  Each
    transaction has its own TfsTransContainer.

    Only the thread carrying out an operation in the context of the
    transaction will access the transaction's TfsTransContainer therefore
    the TfsTransContainer does not need to be threadsafe.

    Each TfsTransContainer contains a list of content locks.  Each content
    "lock" is a reference to the TfsLockContainer of a TfsSrcTable.  A lock
    is added to the TfsLockContainer when the transaction is granted a content
    lock.  The list of content locks in this class allows us to quickly
    reference the table-specific lock containers when removing the
    content locks on a per-transaction basis.

    Each element in the list of content locks not only points to a table's
    lock container but the element's ExtraData item is also a reference to
    the table itself. }
  TfsTransContainer = Class(TFSSpecListItem)
  Protected
    FContentLocks: TFSNormalList;
    { List of  }
    FLockManager: TfsLockManager;
    FRecordLocks: TFSNormalList;
    FTransaction: TfsSrcTransaction;

    Procedure AddContentLock(Container: TfsLockContainer;
      ParentTable: TFSSpecObject;
      LockType: TfsSrcLockType);
    Procedure AddRecordLock(Const FI: PffFileInfo;
      Const CursorID: TffCursorID;
      Const ResourceID: TffInt64);
    Procedure RemoveContentLock(Container: TfsLockContainer);
    Procedure RemoveRecordLock(Const FI: PffFileInfo;
      Const ResourceID: TffInt64);
    Function tcGetContentCount: Longint;
    Function tcGetContentContainer(Const aInx: Longint): TfsLockContainer;
    Function tcGetContentLockType(Const aInx: Longint): TfsSrcLockType;
    Function tcGetContentTable(Const aInx: Longint): TFSSpecObject;
    Function tcGetFileCount: Longint;
    Function tcGetFiles(Const aInx: Longint): TFSSpecW32ListItem;

  Public
    Constructor Create(Const aKey: TfsSrcTransaction);
    { Create the list item; aKey is its access/sort key. }

    Destructor Destroy; Override;

    Function Compare(aKey: pointer): Integer; Override;
    { Compare Self's key to aKey: return <0 if aKey < Self's, 0 if
      equal, >0 otherwise. }

    Function Key: pointer; Override;
    { Return a pointer to this item's key: it'll be a pointer to a
      TffInt64. }

    Function TableContentLockType(Container: TfsLockContainer): TfsSrcLockType;
    { Returns the type of lock held by the transaction on a table's content.
      Container is the table's lock container. If the transaction does not
      have a content lock for the table then this function returns
      ffsltNone. }

    Property ContentCount: Longint
      Read tcGetContentCount;
    { Number of tables for which the transaction has been granted a
      content lock. }

    Property ContentContainer[Const aInx: Longint]: TfsLockContainer
    Read tcGetContentContainer;
    { Returns the lock container associated with a particular content
      lock.  aInx is base zero.  The upper bound of aInx is
      pred(ContentCount). }

    Property ContentLockType[Const aInx: Longint]: TfsSrcLockType
    Read tcGetContentLockType;
    { Returns the type of lock held by a transaction on a table's content.
      This list is base zero. Use pred(ContentCount) to determine the
      upper bound. }

    Property ContentTable[Const aInx: Longint]: TFSSpecObject
    Read tcGetContentTable;
    { Returns the TfsSrcTable (viewed as a TFSSpecObject) at the specified
      index.  This list is base zero.  Use pred(ContentCount) to
      determine the upper bound. }

    Property FileCount: Longint
      Read tcGetFileCount;
    { Returns the number of files for which the transaction has obtained
      record locks. }

    Property Files[Const aInx: Longint]: TFSSpecW32ListItem
    Read tcGetFiles;
    { Returns the data structure holding record locks for a particular file.
      aInx is base zero.  The upper bound is pred(FileCount). }

    Property LockManager: TfsLockManager
      Read FLockManager Write FLockManager;

    Property Transaction: TfsSrcTransaction
      Read FTransaction Write FTransaction;
  End;

Function FFMapLockToName(aLockType: TfsSrcLockType): String;
{ Translates a lock type into a lock name. }

{$IFDEF LockLogging}
Function FFMapRequestStatusToName(aStatus: TffLockRequestStatus): String;
{ Translates a request status type into a name. }
{$ENDIF}
Const
  fsc_DefaultDeadlockFreq = 5000;
  { The default deadlock detection frequency, in milliseconds.  A value of
    5000 means deadlock detection will occur every 5 seconds. }

{Begin !!.01}
  {$IFDEF UseLockContainerPool}
Type
  TfsLockContainerPool = Class(TObject)
  Protected
    FList: TfsPointerList;
    FRetainCount: Integer;
    FPadLock: TfsPadlock;
  Public
    Constructor Create(Const InitialCount, RetainCount: Integer);
    Destructor Destroy; Override;
    Procedure Flush;
    Function Get: TfsLockContainer;
    Procedure Put(Const aLockContainer: TfsLockContainer);
  End;

Var
  FSLockContainerPool: TfsLockContainerPool;
  {$ENDIF}
  {End !!.01}

Implementation

{$IFDEF LockLogging}
Var
  Log: TFSNormalEventLog;
  {$ENDIF}

  {$IFDEF LockLogging}
Const
  { Logging constants }
  csResourceID = '  ResourceID    : %8d';
  csResourceID64 = '  ResourceID    : %8d:%8d';
  csLockContainer = '  LockContainer : %8d';
  csLockType = '  LockType      : %s';
  csDuration = '  Duration      : %s';
  csTransaction = '  Transaction   : %8d';
  csConditional = '  Conditional   : %s'; {!!.10}
  csCursorID = '  CursorID      : %8d';
  csDatabaseID = '  DatabaseID    : %8d'; {!!.10}
  csTimeout = '  Timeout       : %d';
  csFI = '  FI            : %8d';
  csLockRequestStatus = '  LRequestStat  : %s';
  {$ENDIF}

Const
  { Identifies whether or not a requested lock mode is compatible with an
    existing (i.e., granted) lock mode.  The first dimension of the array
    is the granted mode.  The second dimension of the array is the requested
    mode. }

  ffca_LockCompatibility: Array[TfsSrcLockType, TfsSrcLockType] Of Boolean =
  ({None   IntS   IntX   Share  SIX    Updt   Excl }
    {ffsltNone}(True, True, True, True, True, True, True),
    {ffslIntentS}(True, True, True, True, True, False, False),
    {ffsltIntentX }(True, True, True, False, False, False, False),
    {ffsltShare}(True, True, False, True, False, False, False),
    {ffsltSIX}(True, True, False, False, False, False, False),
    {ffsltUpdate}(True, False, False, True, False, False, False),
    {ffsltExclusive}(True, False, False, False, False, False, False)
    );

  { This lock conversion matrix is used to determine the new lock
    type when a lock conversion is necessary. }

  ffca_LockConversion: Array[TfsSrcLockType, TfsSrcLockType] Of TfsSrcLockType =
  (
    {ffsltNone}(ffsltNone, ffsltIntentS, ffsltIntentX,
    ffsltShare, ffsltSIX, ffsltUpdate,
    ffsltExclusive),

    {ffsltIntentS}(ffsltIntentS, ffsltIntentS, ffsltIntentX,
    ffsltShare, ffsltSIX, ffsltUpdate,
    ffsltExclusive),

    {ffsltIntentX}(ffsltIntentX, ffsltIntentX, ffsltIntentX,
    ffsltSIX, ffsltSIX, ffsltExclusive,
    ffsltExclusive),

    {ffsltShare}(ffsltShare, ffsltShare, ffsltSIX,
    ffsltShare, ffsltSIX, ffsltUpdate,
    ffsltExclusive),

    {ffsltSIX}(ffsltSIX, ffsltSIX, ffsltSIX,
    ffsltSIX, ffsltSIX, ffsltSIX,
    ffsltExclusive),

    {ffsltUpdate}(ffsltUpdate, ffsltUpdate, ffsltExclusive,
    ffsltUpdate, ffsltSIX, ffsltUpdate,
    ffsltExclusive),

    {ffsltExclusive}(ffsltExclusive, ffsltExclusive, ffsltExclusive,
    ffsltExclusive, ffsltExclusive, ffsltExclusive,
    ffsltExclusive)
    );

Type
  TfsWaitForListItem = Class(TfsIntListItem)
  Private
    FWaitingTrans: TfsSrcTransaction;
  Public
    Property WaitingTrans: TfsSrcTransaction
      Read FWaitingTrans Write FWaitingTrans;
  End;

  {Begin Move !!.01}
  {$IFDEF UseLockContainerPool}
  //type
  //  TfsLockContainerPool = class(TObject)
  //  protected
  //    FList : TfsPointerList;
  //    FRetainCount : Integer;
  //    FPadLock : TfsPadlock;
  //  public
  //    constructor Create(const InitialCount, RetainCount : Integer);
  //    destructor Destroy; override;
  //    function Get : TfsLockContainer;
  //    procedure Put(const aLockContainer : TfsLockContainer);
  //  end;

  //var
  //  FSLockContainerPool : TfsLockContainerPool;
  {$ENDIF}
  {End Move !!.01}

  {===TfsLockManager===================================================}

Constructor TfsLockManager.Create;
Begin
  Inherited Create;
  FTableLocks := TfsThreadHash.Create(fsc_Size59);
  FTableLocks.OnDisposeData := DisposeLockList;

  FTransactions := TFSSpecThreadList.Create;
  FTransactions.Sorted := True;

  FStartWithLock := TfsPadlock.Create; {!!.10}

  {$IFDEF LockLogging}
  FEventLog := TFSNormalEventLog.Create(Nil);
  FEventLog.FileName := ExtractFilePath(application.ExeName) + 'LOCK.LG';
  FEventLog.Enabled := True;
  FEventLog.WriteStrings(['******************************',
    '******************************',
      Format('Lock Manager Started: %12D', [GetTickCount])]);
  Log := FEventLog;
  {$ENDIF}
End;
{--------}

Destructor TfsLockManager.Destroy;
Begin
  FTableLocks.Clear;
  FTableLocks.Free;
  FTableLocks := Nil;

  FTransactions.Free;
  FTransactions := Nil;

  FStartWithLock.Free; {!!.10}

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    Format('Lock Manager Stopped: %12D', [GetTickCount])]);
  FEventLog.Free;
  {$ENDIF}

  Inherited Destroy;
End;
{--------}

Function TfsLockManager.AcquireClientLock(Const Container: TfsLockContainer;
  Const CursorID: TffCursorID;
  Const Timeout: TffWord32;
  Const LockType: TfsSrcLockType): TffLockRequestStatus;
Var
  LockStatus: TffLockStatus;
  WaitEvent: TFSNormalEvent;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  FEventLog.WriteStrings(['',
    '========================================',
      'AcquireClientLock.BEGIN',
      Format(csLockContainer, [Longint(Container)]),
    Format(csCursorID, [CursorID])]);
  {$ENDIF}
  Result := fflrsGranted;
  Container.BeginWrite;
  Try
    { Add the request to the queue. }
    LockStatus := Container.RequestLock(LockType,
      False,
      Nil,
      CursorID,
      WaitEvent);
  Finally
    Container.EndWrite;
  End;

  If LockStatus = fflsGranted Then
    Result := fflrsGranted
  Else If LockStatus = fflsRejected Then
    Result := fflrsRejected
  Else { waiting }
    { The lock is now in the queue. At this point we must pause the thread
      until the lock is granted. The WaitEvent local var is passed to the
      TfsLockContainer.RequestLock method. This keeps us from creating
      an instance of TFSNormalEvent unnecessarily. The container is responsible
      for the create operation if it is necessary}
    Try
      Try
        WaitEvent.WaitFor(Timeout);
        Result := fflrsGranted;
      Except
        On E: EfsException Do
          Begin
            If E.ErrorCode = fserrReplyTimeout Then
              Result := fflrsTimeout
            Else
              Result := fflrsRejected;
            If LockStatus = fflsWaiting Then
              ReleaseClientW(Container, CursorID)
            Else
              ReleaseClientWC(Container, CursorID);
          End
        Else
          Begin
            If LockStatus = fflsWaiting Then
              ReleaseClientW(Container, CursorID)
            Else
              ReleaseClientWC(Container, CursorID);
            Raise;
          End;
      End
    Finally
      WaitEvent.Free;
      WaitEvent := Nil;
    End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('AcquireClientLock - Time: %12D', [GetTickCount - StartTime]),
      Format(csLockContainer, [Longint(Container)]),
    Format(csTimeout, [Timeout]),
      Format(csCursorID, [CursorID]),
      Format(csLockType, [FFMapLockToName(LockType)]),
    Format(csLockRequestStatus, [FFMapRequestStatusToName(Result)])]);
  {$ENDIF}
End;
{--------}

Function TfsLockManager.AcquireContentLock(Const Container: TfsLockContainer;
  Const ParentTable: TFSSpecObject;
  Const Transaction: TfsSrcTransaction;
  Const Conditional: Boolean;
  Const Timeout: TffWord32;
  Const LockType: TfsSrcLockType): TffLockRequestStatus;
Var
  aCursorID: TffCursorID;
  LockStatus: TffLockStatus;
  TransContainer: TfsTransContainer;
  WaitEvent: TFSNormalEvent;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  FEventLog.WriteStrings(['',
    '========================================',
      'AcquireContentLock.BEGIN',
      Format(csLockContainer, [Longint(Container)]),
    Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}
  Result := fflrsGranted;
  aCursorID := TffCursorID(Transaction);
  Container.BeginWrite;
  Try
    { Add the request to the queue. }
    LockStatus := Container.RequestLock(LockType, Conditional, Transaction,
      aCursorID, WaitEvent);
  Finally
    Container.EndWrite;
  End;

  If LockStatus = fflsGranted Then
    Result := fflrsGranted
  Else If LockStatus = fflsRejected Then
    Result := fflrsRejected
  Else { waiting }
    { The lock is now in the queue. At this point we must pause the thread
      until the lock is granted. The WaitEvent local var is passed to the
      TfsLockContainer.RequestLock method. This keeps us from creating
      an instance of TFSNormalEvent unnecessarily. The container is responsible
      for the create operation if it is necessary}
    Try
      Try
        {$IFDEF LockLogging}
        FEventLog.WriteStrings(['',
          '========================================',
            Format('AcquireContentLock - Waiting: %d', [aCursorID])]);
        {$ENDIF}
        WaitEvent.WaitFor(Timeout);
        Result := fflrsGranted;
      Except
        On E: EfsException Do
          Begin
            If E.ErrorCode = fserrReplyTimeout Then
              Result := fflrsTimeout
            Else
              Result := fflrsRejected;
            If LockStatus = fflsWaiting Then
              ReleaseContentW(Container, Transaction)
            Else
              ReleaseContentWC(Container, Transaction);
          End
        Else
          Begin
            If LockStatus = fflsWaiting Then
              ReleaseContentW(Container, Transaction)
            Else
              ReleaseContentWC(Container, Transaction);
            Raise;
          End;
      End
    Finally
      WaitEvent.Free;
      WaitEvent := Nil;
    End;

  If Result = fflrsGranted Then
    Begin
      { Add the new lock to the transaction list. }
      FTransactions.BeginWrite;
      Try
        TransContainer := TfsTransContainer(Transaction.TransLockContainer);
        If Not Assigned(TransContainer) Then
          Begin
            TransContainer := TfsTransContainer.Create(Transaction);
            TransContainer.LockManager := Self;
            Transaction.TransLockContainer := TransContainer;
            FTransactions.Insert(TransContainer);
          End;
        TransContainer.AddContentLock(Container, ParentTable, LockType);
      Finally
        FTransactions.EndWrite;
      End;
    End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('AcquireContentLock - Time: %12D', [GetTickCount - StartTime]),
      Format(csLockContainer, [Longint(Container)]),
    Format(csTimeout, [Timeout]),
      Format(csTransaction, [Transaction.TransactionID]),
      Format(csLockType, [FFMapLockToName(LockType)]),
    Format(csLockRequestStatus, [FFMapRequestStatusToName(Result)])]);
  {$ENDIF}
End;
{--------}

Function TfsLockManager.AcquireRecordLock(Const ResourceID: TffInt64;
  Const FI: PffFileInfo;
  Const LockType: TfsSrcLockType;
  Const Conditional: Boolean;
  Const Timeout: TffWord32;
  Const Transaction: TfsSrcTransaction;
  Const DatabaseID: TffDatabaseID; {!!.10}
  Const CursorID: TffCursorID;
  Const TrLock: TfsDataBaseRecLocking): TffLockRequestStatus;
Var
  TransContainer: TfsTransContainer;
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
  LockStatus: TffLockStatus;
  WaitEvent: TFSNormalEvent;
  bCreateNew, bCreateNew1: boolean;
  {$IFDEF LockLogging}
  IsCond: String;
  StartTime: DWORD;
  {$ENDIF}

  Procedure AddToTransact;
  Begin
    If Result = fflrsGranted Then
      Begin
        { Is a transaction active? }
        If assigned(Transaction) Then
          Begin
            { Yes. Add the new lock to the transaction list. }
            FTransactions.BeginWrite;
            Try
              TransContainer := TfsTransContainer(Transaction.TransLockContainer);
              If Not Assigned(TransContainer) Then
                Begin
                  TransContainer := TfsTransContainer.Create(Transaction);
                  TransContainer.LockManager := Self;
                  Transaction.TransLockContainer := TransContainer;
                  FTransactions.Insert(TransContainer);
                End;

              TransContainer.AddRecordLock(FI, DatabaseID, ResourceID); {!!.10}
            Finally
              FTransactions.EndWrite;
            End;
          End;
      End;
  End;

  Procedure LockUntil;
  Begin
    { The lock is now in the queue. At this point we must pause the thread
    until the lock is granted. The WaitEvent local var is passed to the
    TfsLockContainer.RequestLock method. This keeps us from creating
    an instance of TFSNormalEvent unnecessarily. The container is responsible
    for the create operation if it is necessary}
    Try
      Try
        {$IFDEF LockLogging}
        FEventLog.WriteStrings(['AcquireRecordLock: WaitEvent.WaitFor',
          Format(csCursorID, [CursorID])]);
        {$ENDIF}
        WaitEvent.WaitFor(Timeout);
        Result := fflrsGranted;
      Except
        On E: EfsException Do
          Begin
            Case E.ErrorCode Of
              fserrReplyTimeout: Result := fflrsTimeout;
              fserrWaitFailed: Result := fflrsRejected;
            End;
            {$IFDEF LockLogging}
            FEventLog.WriteStrings(['AcquireRecordLock: lock request timed out or rejected',
              Format(csCursorID, [CursorID])]);
            {$ENDIF}
            If LockStatus = fflsWaiting Then
              ReleaseRecordW(ResourceID, FI, DatabaseID) {!!.10}
            Else
              ReleaseRecordWC(ResourceID, FI, DatabaseID); {!!.10}
          End
        Else
          Begin
            If LockStatus = fflsWaiting Then
              ReleaseRecordW(ResourceID, FI, DatabaseID) {!!.10}
            Else
              ReleaseRecordWC(ResourceID, FI, DatabaseID); {!!.10}
            Raise;
          End;
      End
    Finally
      {$IFDEF LockLogging}
      FEventLog.WriteStrings(['AcquireRecordLock: WaitEvent.Free',
        Format(csCursorID, [CursorID])]);
      {$ENDIF}
      WaitEvent.Free;
      WaitEvent := Nil;
    End;
  End;

  Procedure LockStsRec;
  Begin
    If LockStatus = fflsGranted Then
      Result := fflrsGranted
    Else If LockStatus = fflsRejected Then
      Result := fflrsRejected
    Else If (LockStatus = fflsWaiting) Then
      Begin
        LockUntil;
      End;
  End;

  Procedure AddLog;
  Begin
    {$IFDEF LockLogging}
    If Conditional Then
      IsCond := 'Yes'
    Else
      IsCond := 'No';
    {Begin !!.06}
    If Transaction = Nil Then
      FEventLog.WriteStrings(['',
        '========================================',
          Format('AcquireRecordLock END - Time: %12D', [GetTickCount - StartTime]),
          Format(csFI, [FI^.fiHandle]),
          Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
          Format(csLockType, [FFMapLockToName(LockType)]),
        Format(csConditional, [isCond]), {!!.10}
        Format(csTimeout, [Timeout]),
          Format(csTransaction, [0]),
          Format(csCursorID, [CursorID]),
          Format(csLockRequestStatus, [FFMapRequestStatusToName(Result)])])
    Else
      FEventLog.WriteStrings(['',
        '========================================',
          Format('AcquireRecordLock END - Time: %12D', [GetTickCount - StartTime]),
          Format(csFI, [FI^.fiHandle]),
          Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
          Format(csLockType, [FFMapLockToName(LockType)]),
        Format(csConditional, [isCond]), {!!.10}
        Format(csTimeout, [Timeout]),
          Format(csTransaction, [Transaction.TransactionID]),
          Format(csCursorID, [CursorID]),
          Format(csLockRequestStatus, [FFMapRequestStatusToName(Result)])]);
    {End !!.06}
    {$ENDIF}
  End;
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  WaitEvent := Nil;
  Result := fflrsRejected;

  {$IFDEF LockLogging}
  If Transaction = Nil Then
    FEventLog.WriteStrings(['',
      '========================================',
        Format('AcquireRecordLock BEGIN - Time: %12D', [GetTickCount - StartTime]),
        Format(csFI, [FI^.fiHandle]),
        Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
        Format(csTransaction, [0]),
        Format(csCursorID, [CursorID])])
  Else
    FEventLog.WriteStrings(['',
      '========================================',
        Format('AcquireRecordLock BEGIN - Time: %12D', [GetTickCount - StartTime]),
        Format(csFI, [FI^.fiHandle]),
        Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
        Format(csTransaction, [Transaction.TransactionID]),
        Format(csCursorID, [CursorID])]);
  {End !!.06}
  {$ENDIF}

  //tlOptimisticNoWait, tlOptimisticWait,tlPessimisticNoWait, tlPessimisticWait
  If TrLock = tlOptimisticNoWait Then // no wait
    Begin
      //Result := fflrsGranted;
      //Exit;
      If IsRecordLocked(ResourceID, Fi) Then
        Result := fflrsRejected
      Else
        Result := fflrsGranted;
      AddLog;
      Exit;
    End;

  { Find the LockContainer List, create one if it does not exist. }
  LockContainerList := FI^.fiRecordLocks;
  bCreateNew := False;
  If Not Assigned(LockContainerList) Then
    Begin
      bCreateNew := True;
      LockContainerList := TfsThreadHash64.Create(fsc_Size257);
      LockContainerList.OnDisposeData := DisposeLockList;
      FI^.fiRecordLocks := LockContainerList;
    End;

  { We need write access to the container list before we can find/create
    the LockContainer. }
  LockContainerList.BeginWrite;

  { Has a lock already been created for this table resource? }
  Try
    { Find the lock container, create one if it does not exist. }
    LockContainer := Nil;
    LockContainer := LockContainerList.Get(ResourceID);
    bCreateNew1 := False;
    If Not Assigned(LockContainer) Then
      Begin
        bCreateNew1 := True;
        {$IFDEF UseLockContainerPool}
        LockContainer := FSLockContainerPool.Get;
        {$ELSE}
        LockContainer := TfsLockContainer.Create;
        {$ENDIF}

        { Add the new lock container to the internal list }
        If TrLock In [tlPessimisticNoWait, tlPessimisticWait] Then
          LockContainerList.Add(ResourceID, LockContainer);
      End;

    { We need write access to the container before we can queue the lock }
    LockContainer.BeginWrite;
  Finally
    LockContainerList.EndWrite;
  End;

  Try
    { We need to add the request to the queue. }
    LockStatus := LockContainer.RequestRecLock(LockType, {!!.10}
      Conditional,
      Transaction,
      DatabaseID, {!!.10}
      CursorID,
      WaitEvent);
  Finally
    LockContainer.EndWrite;
  End;

  If TrLock = tlOptimisticWait Then
    Begin
      If LockStatus = fflsGranted Then
        Result := fflrsGranted
      Else
        Begin
          Try
            LockUntil;
          Finally
            If Assigned(LockContainer) And bCreateNew1 Then
              LockContainer.Free;
            If Assigned(LockContainerList) And bCreateNew Then
              LockContainerList.free;
          End;
        End;
      AddLog;
      Exit;
    End;
  If TrLock = tlOptimisticWait Then Exit;

  If TrLock In [tlPessimisticNoWait] Then
    Begin
      If LockStatus = fflsGranted Then
        Result := fflrsGranted
      Else
        Begin
          If LockStatus = fflsWaiting Then
            ReleaseRecordW(ResourceID, FI, DatabaseID)
          Else
            ReleaseRecordWC(ResourceID, FI, DatabaseID);
          Result := fflrsRejected;
        End;
    End
  Else
    LockStsRec;

  AddToTransact;
  AddLog;
End;
{--------}

Function TfsLockManager.AcquireTableLock(Const ResourceID: TffWord32;
  Const LockType: TfsSrcLockType;
  Const Conditional: Boolean;
  Const Timeout: TffWord32;
  Const CursorID: TffCursorID): TffLockRequestStatus;
Var
  LockContainer: TfsLockContainer;
  LockStatus: TffLockStatus;
  WaitEvent: TFSNormalEvent;
  {$IFDEF LockLogging}
  IsCond: String;
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  WaitEvent := Nil;
  Result := fflrsRejected;
  { Has a lock already been created for this table resource? }
  FTableLocks.BeginWrite;
  Try
    { Find the lock container, create one if it does not exist. }
    LockContainer := FTableLocks.Get(ResourceID);
    If Not Assigned(LockContainer) Then
      Begin
        {$IFDEF UseLockContainerPool}
        LockContainer := FSLockContainerPool.Get;
        {$ELSE}
        LockContainer := TfsLockContainer.Create;
        {$ENDIF}

        { Add the new lock container to the internal list }
        FTableLocks.Add(ResourceID, LockContainer);
      End;

    { We need write access to the container before we can queue the lock }
    LockContainer.BeginWrite;
  Finally
    FTableLocks.EndWrite;
  End;

  Try
    { We need to add the request to the queue. }
    LockStatus := LockContainer.RequestLock(LockType,
      Conditional,
      Nil,
      CursorID,
      WaitEvent);
  Finally
    LockContainer.EndWrite;
  End;

  If (LockStatus = fflsWaiting) Then
    { The lock is now in the queue. At this point we must pause the thread
      until the lock is granted. The WaitEvent local var is passed to the
      TfsLockContainer.RequestLock method. This keeps us from creating
      an instance of TFSNormalEvent unnecessarily. The container is responsible
      for the create operation if it is necessary}
    Try
      Try
        WaitEvent.WaitFor(Timeout);
        Result := fflrsGranted;
      Except
        On E: EfsException Do
          Begin
            Case E.ErrorCode Of
              fserrReplyTimeout: Result := fflrsTimeout;
              fserrWaitFailed: Result := fflrsRejected;
            End;
            If LockStatus = fflsWaiting Then
              ReleaseTableW(ResourceID, CursorID)
            Else
              ReleaseTableWC(ResourceID, CursorID);
          End
        Else
          Begin
            ReleaseTableLock(ResourceID, CursorID);
            Raise;
          End;
      End;
    Finally
      WaitEvent.Free;
      WaitEvent := Nil;
    End
  Else If LockStatus = fflsGranted Then
    Result := fflrsGranted
  Else
    Result := fflrsRejected;
  {$IFDEF LockLogging}
  If Conditional Then
    isCond := 'Yes'
  Else
    isCond := 'No';
  FEventLog.WriteStrings(['========================================',
    Format('AcquireTableLock - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID, [ResourceID]),
      Format(csLockType, [FFMapLockToName(LockType)]),
    Format(csConditional, [isCond]), {!!.10}
    Format(csTimeout, [Timeout]),
      Format(csCursorID, [CursorID]),
      Format(csLockRequestStatus, [FFMapRequestStatusToName(Result)])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.DisposeLockList(Sender: TfsBaseHashTable;
  aData: Pointer);
Var
  LockList: TfsLockContainer;
  Index: Integer;
Begin
  If Assigned(aData) Then
    Begin
      LockList := TfsLockContainer(aData);

      { Free the items in the list. }
      For Index := Pred(LockList.Count) Downto 0 Do
        LockList.DeleteAt(Index);
      {$IFDEF UseLockContainerPool}
      FSLockContainerPool.Put(LockList);
      {$ELSE}
      LockList.Free;
      {$ENDIF}
    End;
End;
{--------}

Procedure TfsLockManager.DisposeRecordLockList(Sender: TfsBaseHashTable;
  aData: Pointer);
Var
  LockList: TfsThreadHash;
Begin
  If Assigned(aData) Then
    Begin
      LockList := TfsThreadHash(aData);
      { Free the items in the list. }
      LockList.Clear;
      LockList.Free;
    End;
End;
{--------}

Procedure TfsLockManager.GetWaitingRecordLocks(Const ResourceID: TffInt64;
  Const FI: PffFileInfo;
  Const Transaction: TfsSrcTransaction;
  Var WaitingLocks: TfsPointerList);
Var
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
  LockIdx: Integer;
  LockItem: TfsLockListItem;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  If Not Assigned(LockContainerList) Then Exit;

  LockContainerList.BeginRead;
  Try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    If Not Assigned(LockContainer) Then Exit;

    LockContainer.BeginRead;
    Try
      For LockIdx := 0 To Pred(LockContainer.Count) Do
        Begin
          LockItem := TfsLockListItem(LockContainer.Items[LockIdx]);
          If LockItem.Status = fflsWaiting Then
            WaitingLocks.Append(pointer(LockItem));
        End;
    Finally
      LockContainer.EndRead;
    End;
  Finally
    LockContainerList.EndRead;
  End;
  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('GetWaitingRecordLocks - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
      Format(csFI, [FI^.fiHandle]),
      Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}
End;
{--------}

Function TfsLockManager.IsRecordLocked(Const aResourceID: TffInt64;
  Const aFI: PffFileInfo): Boolean;
Var
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
  {$IFDEF LockLogging}
  LockStatus: String;
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  Result := False;

  { Find the LockContainerList }
  LockContainerList := aFI^.fiRecordLocks;
  If Not Assigned(LockContainerList) Then Exit;

  LockContainerList.BeginRead;
  Try
    { Find the lock container }
    LockContainer := LockContainerList.Get(AResourceID);
    If Not Assigned(LockContainer) Then Exit;
    Result := (LockContainer.Count > 0);
  Finally
    LockContainerList.EndRead;
  End;

  {$IFDEF LockLogging}
  If Not Result Then
    LockStatus := 'Not ';
  FEventLog.WriteStrings(['',
    '========================================',
      Format('IsRecordLocked - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID64, [aResourceID.iLow, aResourceID.iHigh]),
      Format(csFI, [aFI^.fiHandle]),
      Format('  %sLocked', [LockStatus])]);
  {$ENDIF}
End;

Function TfsLockManager.WhoRecordLocked(Const aResourceID: TffInt64;
  Const aFI: PffFileInfo): String;
Var
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
  {$IFDEF LockLogging}
  LockStatus: String;
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  Result := '';

  { Find the LockContainerList }
  LockContainerList := aFI^.fiRecordLocks;
  If Not Assigned(LockContainerList) Then Exit;

  LockContainerList.BeginRead;
  Try
    { Find the lock container }
    LockContainer := LockContainerList.Get(AResourceID);
    If Not Assigned(LockContainer) Then Exit;
    //Result := (LockContainer.Count > 0);
  Finally
    LockContainerList.EndRead;
  End;

  {$IFDEF LockLogging}
  If Result = '' Then
    LockStatus := 'Not ';
  FEventLog.WriteStrings(['',
    '========================================',
      Format('WhoRecordLocked - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID64, [aResourceID.iLow, aResourceID.iHigh]),
      Format(csFI, [aFI^.fiHandle]),
      Format('  %sLocked', [LockStatus])]);
  {$ENDIF}
End;
{--------}

Function TfsLockManager.HasClientLock(Const Container: TfsLockContainer;
  Const CursorID: TffCursorID): boolean;
Begin
  Container.BeginRead;
  Try
    Result := (Container.fflIndexPrim(CursorID) <> -1);
  Finally
    Container.EndRead;
  End;
End;
{--------}

Function TfsLockManager.IsContentLockedBy(Const Container: TfsLockContainer;
  Const Transaction: TfsSrcTransaction): boolean;
Begin
  Container.BeginRead;
  Try
    Result := (Container.fflIndexPrim(TffCursorID(Transaction)) <> -1);
  Finally
    Container.EndRead;
  End;
End;
{--------}

Function TfsLockManager.IsTableLockedBy(Const AResourceID: TffWord32;
  Const aCursorID: TffCursorID;
  Const ALockType: TfsSrcLockType): Boolean;
Var
  ItemIndex: Longint;
  LockContainer: TfsLockContainer;
  {$IFDEF LockLogging}
  LockStatus: String;
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  Result := False;
  FTableLocks.BeginRead;
  Try
    { Find the lock container }
    LockContainer := FTableLocks.Get(AResourceID);
    If Not Assigned(LockContainer) Then Exit;

    LockContainer.BeginRead;
    Try
      With LockContainer Do
        Begin
          ItemIndex := fflIndexPrim(aCursorID);
          If ItemIndex <> -1 Then
            With TfsLockListItem(Items[ItemIndex]) Do
              Result := ALockType = LockType;
        End;
    Finally
      LockContainer.EndRead;
    End;

  Finally
    FTableLocks.EndRead;
  End;
  {$IFDEF LockLogging}
  If Not Result Then
    LockStatus := 'Not ';
  FEventLog.WriteStrings(['',
    '========================================',
      Format('IsTableLockedBy - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID, [aResourceID]),
      Format(csCursorID, [aCursorID]),
      Format(csLockType, [FFMapLockToName(aLockType)]),
    Format('  %sLocked', [LockStatus])]);
  {$ENDIF}
End;
{--------}

Function TfsLockManager.RecordLockGranted(Const ResourceID: TffInt64;
  Const FI: PffFileInfo): TfsSrcLockType;
Var
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  Result := ffsltNone;

  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  If Not Assigned(LockContainerList) Then Exit;

  LockContainerList.BeginRead;
  Try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    If Not Assigned(LockContainer) Then Exit;

    LockContainer.BeginRead;
    Try
      Result := LockContainer.SummaryMode;
    Finally
      LockContainer.EndRead;
    End;

    { Remove the lock container if it is empty }
    If LockContainer.Count = 0 Then
      LockContainerList.Remove(ResourceID);

  Finally
    LockContainerList.EndRead;
  End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('RecordLockGranted - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
      Format(csFI, [FI^.fiHandle]),
      Format(csLockType, [FFMapLockToName(Result)])]);
  {$ENDIF}
End;
{Begin !!.10}
{--------}

Procedure TfsLockManager.RelaxRecordLock(Const ResourceID: TffInt64;
  Const FI: PffFileInfo;
  Const DatabaseID: TffDatabaseID);
Var
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  If Not Assigned(LockContainerList) Then Exit;

  LockContainerList.BeginWrite;
  Try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    If Not Assigned(LockContainer) Then Exit;

    LockContainer.BeginWrite;
    Try
      LockContainer.RelaxRecordLock(DatabaseID);
    Finally
      LockContainer.EndWrite;
    End;

  Finally
    LockContainerList.EndWrite;
  End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('RelaxRecordLock - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
      Format(csFI, [FI^.fiHandle]),
      Format(csDatabaseID, [DatabaseID])]);
  {$ENDIF}
End;
{End !!.10}
{--------}

Procedure TfsLockManager.ReleaseClientLock(Const Container: TfsLockContainer;
  Const CursorID: TffCursorID);
Var
  RefCount: Integer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin

  Assert(assigned(Container));

  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  FEventLog.WriteStrings(['',
    '========================================',
      'ReleaseClientLock.BEGIN',
      Format(csLockContainer, [Longint(Container)]),
    Format(csCursorID, [CursorID])]);
  {$ENDIF}

  Container.BeginWrite;
  Try
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
      '========================================',
        'ReleaseClientLock.ReleaseCursorLock pre',
        Format(csLockContainer, [Longint(Container)]),
      Format(csCursorID, [CursorID])]);
    {$ENDIF}
    Container.ReleaseCursorLock(CursorID, RefCount);

    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
      '========================================',
        'ReleaseClientLock.ReleaseCursorLock post',
        Format(csLockContainer, [Longint(Container)]),
      Format(csCursorID, [CursorID])]);
    {$ENDIF}
  Finally
    Container.EndWrite;
  End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseClientLock - Time: %12D', [GetTickCount - StartTime]),
      Format(csLockContainer, [Longint(Container)]),
    Format(csCursorID, [CursorID])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.ReleaseClientLockAll(Const Container: TfsLockContainer;
  Const CursorID: TffCursorID);
{$IFDEF LockLogging}
Var
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}

  Container.BeginWrite;
  Try
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
      '========================================',
        'ReleaseClientLockAll',
        Format(csCursorID, [CursorID]),
        Format(csLockContainer, [Longint(Container)]),
      Format('# container items: %d', [Container.Count])]);
    {$ENDIF}
    Container.ReleaseCursorLockAll(CursorID);
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
      '========================================',
        'ReleaseClientLockAll - after purge',
        Format(csCursorID, [CursorID]),
        Format('# container items: %d', [Container.Count])]);
    {$ENDIF}
  Finally
    Container.EndWrite;
  End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseClientLockAll - Time: %12D', [GetTickCount - StartTime]),
      Format(csCursorID, [CursorID]),
      Format(csLockContainer, [Longint(Container)])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.ReleaseClientW(Const Container: TfsLockContainer;
  Const CursorID: TffCursorID);
Begin
  Assert(assigned(Container));
  Container.BeginWrite;
  Try
    Container.ReleaseWaitingLock(CursorID);
  Finally
    Container.EndWrite;
  End;
End;
{--------}

Procedure TfsLockManager.ReleaseClientWC(Const Container: TfsLockContainer;
  Const CursorID: TffCursorID);
Begin
  Assert(assigned(Container));
  Container.BeginWrite;
  Try
    Container.ReleaseWaitingConversion(CursorID);
  Finally
    Container.EndWrite;
  End;
End;
{--------}

Procedure TfsLockManager.ReleaseContentLock(Const Container: TfsLockContainer;
  Const Transaction: TfsSrcTransaction);
Var
  RefCount: Integer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
  TransContainer: TfsTransContainer;
Begin

  Assert(assigned(Container));
  Assert(assigned(Transaction));

  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  FEventLog.WriteStrings(['',
    '========================================',
      'ReleaseContentLock.BEGIN',
      Format(csLockContainer, [Longint(Container)]),
    Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}

  Container.BeginWrite;
  Try
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
      '========================================',
        'ReleaseContentLock.ReleaseCursorLock pre',
        Format(csLockContainer, [Longint(Container)]),
      Format(csTransaction, [Transaction.TransactionID])]);
    {$ENDIF}
    Container.ReleaseCursorLock(TffCursorID(Transaction), RefCount);

    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
      '========================================',
        'ReleaseContentLock.ReleaseCursorLock post',
        Format(csLockContainer, [Longint(Container)]),
      Format(csTransaction, [Transaction.TransactionID])]);
    {$ENDIF}
    { Remove the lock from the transaction list }
    If RefCount = 0 Then
      { Is a transaction active? }
      If assigned(Transaction) Then
        Begin
          FTransactions.BeginWrite;
          Try
            TransContainer := TfsTransContainer(Transaction.TransLockContainer);
            If Assigned(TransContainer) Then
              TransContainer.RemoveContentLock(Container);
          Finally
            FTransactions.EndWrite;
          End;
        End;
  Finally
    Container.EndWrite;
  End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseContentLock - Time: %12D', [GetTickCount - StartTime]),
      Format(csLockContainer, [Longint(Container)]),
    Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.ReleaseContentLockAll(Container: TfsLockContainer;
  Transaction: TfsSrcTransaction);
{$IFDEF LockLogging}
Var
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}

  Container.BeginWrite;
  Try
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
      '========================================',
        'ReleaseContentLockAll',
        Format(csTransaction, [Longint(Transaction)]),
      Format(csLockContainer, [Longint(Container)]),
      Format('# container items: %d', [Container.Count])]);
    {$ENDIF}
    Container.ReleaseCursorLockAll(TffCursorID(Transaction));
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
      '========================================',
        'ReleaseContentLockAll - after purge',
        Format('# container items: %d', [Container.Count])]);
    {$ENDIF}
  Finally
    Container.EndWrite;
  End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseContentLockAll - Time: %12D', [GetTickCount - StartTime]),
      Format(csTransaction, [Longint(Transaction)]),
    Format(csLockContainer, [Longint(Container)])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.ReleaseContentW(Const Container: TfsLockContainer;
  Const Transaction: TfsSrcTransaction);
Begin

  Assert(assigned(Container));
  Assert(assigned(Transaction));

  Container.BeginWrite;
  Try
    Container.ReleaseWaitingLock(TffCursorID(Transaction));
  Finally
    Container.EndWrite;
  End;
End;
{--------}

Procedure TfsLockManager.ReleaseContentWC(Const Container: TfsLockContainer;
  Const Transaction: TfsSrcTransaction);
Begin

  Assert(assigned(Container));
  Assert(assigned(Transaction));

  Container.BeginWrite;
  Try
    Container.ReleaseWaitingConversion(TffCursorID(Transaction));
  Finally
    Container.EndWrite;
  End;
End;
{--------}

Procedure TfsLockManager.ReleaseRecordLock(Const ResourceID: TffInt64;
  Const FI: PffFileInfo;
  Const Transaction: TfsSrcTransaction;
  Const DatabaseID: TffDatabaseID); {!!.10}
Var
  TransContainer: TfsTransContainer;
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
  RefCount: Integer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  If Not Assigned(LockContainerList) Then Exit;

  LockContainerList.BeginWrite;
  Try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    If Not Assigned(LockContainer) Then Exit;

    LockContainer.BeginWrite;
    Try
      LockContainer.ReleaseCursorLock(DatabaseID, RefCount);
    Finally
      LockContainer.EndWrite;
    End;

    { Remove the lock from the transaction list }
    If RefCount = 0 Then
      { Is a transaction active? }
      If assigned(Transaction) Then
        Begin
          FTransactions.BeginWrite;
          Try
            TransContainer := TfsTransContainer(Transaction.TransLockContainer);
            If Assigned(TransContainer) Then
              TransContainer.RemoveRecordLock(FI, ResourceID);
          Finally
            FTransactions.EndWrite;
          End;
        End;

    { Remove the lock container if it is empty }
    If LockContainer.IsEmpty Then
      LockContainerList.Remove(ResourceID);

  Finally
    LockContainerList.EndWrite;
  End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseRecordLock - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
      Format(csFI, [FI^.fiHandle]),
      Format(csTransaction, [Transaction.TransactionID]),
      Format(csDatabaseID, [DatabaseID])]);
  {$ENDIF}
End;

Function TfsLockManager.FindMyRecordLock(Const ResourceID: TffInt64;
  Const FI: PffFileInfo;
  Const Transaction: TfsSrcTransaction;
  Const DatabaseID: TffDatabaseID): Longint;
Var
  TransContainer: TfsTransContainer;
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
  RefCount: Integer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  Result := 0;
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  If Not Assigned(LockContainerList) Then Exit;

  LockContainerList.BeginWrite;
  Try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    If Not Assigned(LockContainer) Then Exit;

    LockContainer.BeginWrite;
    Try
      LockContainer.CountCursorLock(DatabaseID, RefCount);
      Result := RefCount;
    Finally
      LockContainer.EndWrite;
    End;

    { Remove the lock from the transaction list }
    If RefCount = 0 Then
      { Is a transaction active? }
      If assigned(Transaction) Then
        Begin
          FTransactions.BeginWrite;
          Try
            TransContainer := TfsTransContainer(Transaction.TransLockContainer);
            If Assigned(TransContainer) Then
              RefCount := TransContainer.GetRefCount;
            Result := RefCount;
          Finally
            FTransactions.EndWrite;
          End;
        End;
  Finally
    LockContainerList.EndWrite;
  End;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseRecordLock - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
      Format(csFI, [FI^.fiHandle]),
      Format(csTransaction, [Transaction.TransactionID]),
      Format(csDatabaseID, [DatabaseID])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.ReleaseRecordLockAll(Const ResourceID: TffInt64;
  Const FI: PffFileInfo;
  Const Transaction: TfsSrcTransaction;
  Const DatabaseID: TffDatabaseID); {!!.10}
Var
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { We might need to release the lock container, and lock container list so
    we get request full access to the list }
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;

  { Find the lock container }
  LockContainer := LockContainerList.Get(ResourceID);
  If Not Assigned(LockContainer) Then Exit;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseRecordLockAll - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
      Format(csFI, [FI^.fiHandle]),
      Format(csTransaction, [Transaction.TransactionID]),
      Format(csDatabaseID, [DatabaseID])]);
  {$ENDIF}

  LockContainer.BeginWrite;
  Try
    LockContainer.ReleaseCursorLockAll(DatabaseID); {!!.10}
  Finally
    LockContainer.EndWrite;
  End;

  { Remove the lock container if it is empty }
  If LockContainer.Count = 0 Then
    LockContainerList.Remove(ResourceID);

End;
{--------}

Procedure TfsLockManager.ReleaseRecordW(Const ResourceID: TffInt64;
  Const FI: PffFileInfo;
  Const DatabaseID: TffDatabaseID); {!!.10}
Var
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
Begin
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  If Not Assigned(LockContainerList) Then Exit;

  LockContainerList.BeginWrite;
  Try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    If Not Assigned(LockContainer) Then Exit;

    LockContainer.BeginWrite;
    Try
      LockContainer.ReleaseWaitingLock(DatabaseID); {!!.10}
    Finally
      LockContainer.EndWrite;
    End;

  Finally
    LockContainerList.EndWrite;
  End;

End;
{--------}

Procedure TfsLockManager.ReleaseRecordWC(Const ResourceID: TffInt64;
  Const FI: PffFileInfo;
  Const DatabaseID: TffDatabaseID); { !!.10}
Var
  LockContainerList: TfsThreadHash64;
  LockContainer: TfsLockContainer;
Begin
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  If Not Assigned(LockContainerList) Then Exit;

  LockContainerList.BeginWrite;
  Try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    If Not Assigned(LockContainer) Then Exit;

    LockContainer.BeginWrite;
    Try
      LockContainer.ReleaseWaitingConversion(DatabaseID); {!!.10}
    Finally
      LockContainer.EndWrite;
    End;

  Finally
    LockContainerList.EndWrite;
  End;

End;
{--------}

Procedure TfsLockManager.ReleaseTableLock(Const ResourceID: TffWord32;
  Const CursorID: TffCursorID);

Var
  LockContainer: TfsLockContainer;
  RefCount: Integer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { We might need to release the lock container, so we request full access
    to the list }
  FTableLocks.BeginWrite;
  Try
    { Find the lock container }
    LockContainer := FTableLocks.Get(ResourceID);

    If Not Assigned(LockContainer) Then
      Exit;

    LockContainer.BeginWrite;
    Try
      LockContainer.ReleaseCursorLock(CursorID, RefCount);
    Finally
      LockContainer.EndWrite;
    End;

  Finally
    FTableLocks.EndWrite;
  End;
  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseTableLock - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID, [ResourceID]),
      Format(csCursorID, [CursorID])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.ReleaseTableLockAll(Const aResourceID: Integer;
  Const aCursorID: TffCursorID);
Var
  LockContainer: TfsLockContainer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { We might need to release the lock container, so we get request full access
    to the list }
  FTableLocks.BeginWrite;
  Try
    { Find the lock container }
    LockContainer := FTableLocks.Get(AResourceID);
    If Not Assigned(LockContainer) Then Exit;

    LockContainer.BeginWrite;
    Try
      LockContainer.ReleaseCursorLockAll(aCursorID);
    Finally
      LockContainer.EndWrite;
    End;

    { Remove the lock container if it is empty }
    If LockContainer.IsEmpty Then
      FTableLocks.Remove(AResourceID);

  Finally
    FTableLocks.EndWrite;
  End;
  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseTableLockAll - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID, [aResourceID]),
      Format(csCursorID, [aCursorID])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.ReleaseTableW(Const ResourceID: TffWord32;
  Const CursorID: TffCursorID);

Var
  LockContainer: TfsLockContainer;
Begin
  { We might need to release the lock container, so we request full access
    to the list }
  FTableLocks.BeginWrite;
  Try
    { Find the lock container }
    LockContainer := FTableLocks.Get(ResourceID);

    If Not Assigned(LockContainer) Then
      Exit;

    LockContainer.BeginWrite;
    Try
      LockContainer.ReleaseWaitingLock(CursorID);
    Finally
      LockContainer.EndWrite;
    End;

  Finally
    FTableLocks.EndWrite;
  End;
End;
{--------}

Procedure TfsLockManager.ReleaseTableWC(Const ResourceID: TffWord32;
  Const CursorID: TffCursorID);

Var
  LockContainer: TfsLockContainer;
Begin
  { We might need to release the lock container, so we request full access
    to the list }
  FTableLocks.BeginWrite;
  Try
    { Find the lock container }
    LockContainer := FTableLocks.Get(ResourceID);

    If Not Assigned(LockContainer) Then
      Exit;

    LockContainer.BeginWrite;
    Try
      LockContainer.ReleaseWaitingConversion(CursorID);
    Finally
      LockContainer.EndWrite;
    End;

  Finally
    FTableLocks.EndWrite;
  End;
End;
{--------}

Procedure TfsLockManager.ReleaseTransactionLocks(Const Transaction: TfsSrcTransaction;
  Const RecordsOnly: boolean);
Var
  FI: PffFileInfo;
  FileInx: Longint;
  FileItem: TFSSpecW32ListItem;
  ResList: TfsHash64;
  TransContainer: TfsTransContainer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  TransContainer := TfsTransContainer(Transaction.TransLockContainer);

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      'ReleaseTransactionLocks - Start',
      Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}

  If assigned(TransContainer) Then
    Begin
      { Release record locks first. }
      For FileInx := 0 To pred(TransContainer.FileCount) Do
        Begin
          FileItem := TransContainer.Files[FileInx];
          FI := PffFileInfo(FileItem.KeyAsInt);
          If assigned(FI^.fiRecordLocks) Then
            Begin
              FI^.fiRecordLocks.BeginWrite;
              Try
                ResList := TfsHash64(FileItem.ExtraData);
                ResList.Iterate(RelRecLockIterator, TffWord32(FileItem),
                  TffWord32(Transaction), 0);
                ResList.Free;
              Finally
                FI^.fiRecordLocks.EndWrite;
              End;
            End; { if have record locks }
          FileItem.ExtraData := Nil;
        End;

      { Release content locks next. }
      If Not RecordsOnly Then
        For FileInx := 0 To pred(TransContainer.ContentCount) Do
          ReleaseContentLockAll
            (TransContainer.ContentContainer[FileInx], Transaction);
    End; { if have transaction container  }

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseTransactionLocks - Time: %12D', [GetTickCount - StartTime]),
      Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.ReleaseTransactionLocksInTable(aFI: PffFileInfo; Const Transaction: TfsSrcTransaction;
  Const RecordsOnly: boolean);
Var
  FI: PffFileInfo;
  FileInx: Longint;
  FileItem: TFSSpecW32ListItem;
  ResList: TfsHash64;
  TransContainer: TfsTransContainer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  TransContainer := TfsTransContainer(Transaction.TransLockContainer);

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      'ReleaseTransactionLocks - Start',
      Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}

  If assigned(TransContainer) Then
    Begin
      { Release record locks first. }
      For FileInx := 0 To pred(TransContainer.FileCount) Do
        Begin
          FileItem := TransContainer.Files[FileInx];
          FI := PffFileInfo(FileItem.KeyAsInt);
          If FI = aFi Then
            Begin
              If assigned(FI^.fiRecordLocks) Then
                Begin
                  FI^.fiRecordLocks.BeginWrite;
                  Try
                    ResList := TfsHash64(FileItem.ExtraData);
                    ResList.Iterate(RelRecLockIterator, TffWord32(FileItem),
                      TffWord32(Transaction), 0);
                    ResList.Free;
                  Finally
                    FI^.fiRecordLocks.EndWrite;
                  End;
                End; { if have record locks }
            End;
          FileItem.ExtraData := Nil;
        End;

      { Release content locks next. }
      If Not RecordsOnly Then
        For FileInx := 0 To pred(TransContainer.ContentCount) Do
          ReleaseContentLockAll
            (TransContainer.ContentContainer[FileInx], Transaction);
    End; { if have transaction container  }

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('ReleaseTransactionLocks - Time: %12D', [GetTickCount - StartTime]),
      Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}
End;
{--------}

Procedure TfsLockManager.RelRecLockIterator(aKey: TffInt64;
  aData: pointer;
  Const cookie1, cookie2, cookie3: TffWord32);
Var
  FileItem: TFSSpecW32ListItem;
  Transaction: TfsSrcTransaction;
Begin
  { Assumptions:
    aKey    = record reference number (i.e., ID of locked resource)
    aData   = database ID
    Cookie1 = fileItem
    Cookie2 = transaction
    Cookie3 = nothing of value }
  FileItem := TFSSpecW32ListItem(cookie1);
  Transaction := TfsSrcTransaction(cookie2);
  ReleaseRecordLockAll(aKey,
    PffFileInfo(FileItem.KeyValue),
    Transaction,
    TffDatabaseID(aData)); {!!.10}
End;
{--------}

Function TfsLockManager.TableLockGranted(Const ResourceID: Integer): TfsSrcLockType;
Var
  LockContainer: TfsLockContainer;
  {$IFDEF LockLogging}
  StartTime: DWORD;
  {$ENDIF}
Begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  FTableLocks.BeginRead;
  Try
    { Find the lock container }
    LockContainer := FTableLocks.Get(ResourceID);
    If Not Assigned(LockContainer) Then
      Begin
        Result := ffsltNone;
        Exit;
      End;

    Result := LockContainer.SummaryMode;
  Finally
    FTableLocks.EndRead;
  End;
  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
    '========================================',
      Format('TableLockGranted - Time: %12D', [GetTickCount - StartTime]),
      Format(csResourceID, [ResourceID]),
      Format(csLockType, [FFMapLockToName(Result)])]);
  {$ENDIF}
End;
{====================================================================}

{===TfsLockContainer=================================================}

Constructor TfsLockContainer.Create;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    Sorted := True;
    FWaitQueue := TfsLockQueue.Create;
    FWaitConversionQueue := TfsLockQueue.Create;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TfsLockContainer.Destroy;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    FWaitQueue.Free;
    FWaitQueue := Nil;

    FWaitConversionQueue.Free;
    FWaitConversionQueue := Nil;

    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsLockContainer.AddLock(Const Granted: Boolean;
  Const Conditional: Boolean;
  LockItem: TfsLockListItem
  ): TffLockStatus;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If Granted Then
      { Normal Lock is granted, so add it to the list }
      If Not LockItem.Conversion Then
        Begin
          Insert(LockItem);
          Result := fflsGranted;
        End
      Else
        Begin
          { Grant a conversion lock }
          With TfsLockListItem(Items[fflIndexPrim(LockItem.PrimaryID)]) Do {!!.10}
            LockType := LockItem.LockType;
          LockItem.Free;
          Result := fflsGranted;
          Exit;
        End
    Else If (Not Granted) And (Not Conditional) Then
      Begin
        { A waiting lock is added to the list }
        If LockItem.Conversion Then
          Begin
            { Add the lock item to the conversion list }
            FWaitConversionQueue.Enqueue(LockItem);
            Result := fflsWaitingConv;
          End
        Else
          Begin
            { Append lock request to the queue }
            FWaitQueue.Enqueue(LockItem);
            Result := fflsWaiting;
          End;
      End
    Else
      { Since a conditional lock could not be acquired instantly, the request
        is rejected }
      Result := fflsRejected;

    LockItem.Status := Result;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsLockContainer.LastLock: TfsLockListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := TfsLockListItem(Self.Items[Pred(Count)]);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsLockContainer.ProcessLockConversion(Const aCursorID: TffCursorID;
  aLockListItem: TfsLockListItem);
Var
  anItem: TfsLockListItem;
  LockIndex: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {$IFDEF LockLogging}
    Log.WriteStrings(['',
      '========================================',
        Format('LockContainer.ProcessLockConversion: CU %d', [aCursorID])]);
    {$ENDIF}
    { Retrieve the existing lock }
    LockIndex := fflIndexPrim(aCursorID);

    { Convert the granted lock & wake it up. }
    If LockIndex <> -1 Then
      Begin
        anItem := TfsLockListItem(Items[LockIndex]);
        anItem.LockType := aLockListItem.LockType;
        anItem.Event.SignalEvent;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsLockContainer.ProcessQueue;
Var
  anItem: TfsLockListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {$IFDEF LockLogging}
    Log.WriteStrings(['',
      '========================================',
        Format('LockContainer.ProcessQueue: %d', [Longint(Self)])]);
    {$ENDIF}
    { Process all pending conversion requests first}
    While Assigned(TfsLockListItem(FWaitConversionQueue.Peek)) Do
      With TfsLockListItem(FWaitConversionQueue.Peek) Do
        If ffca_LockCompatibility[LockType, SummaryMode] Then
          Begin

            {$IFDEF LockLogging}
            Log.WriteStrings(['',
              Format('LockContainer.SumMode: %s', [FFMapLockToName(SummaryMode)])]);
            {$ENDIF}

            { Compatible waiting lock found, we must now grant it }
            anItem := TfsLockListItem(FWaitConversionQueue.Dequeue);
            anItem.Status := fflsGranted;

            { If a lock conversion request has been encountered, we
              must finalize the conversion operation }
            ProcessLockConversion(PrimaryID, anItem); {!!.10}

            anItem.Free;
          End
        Else
          Exit;

    {$IFDEF LockLogging}
    Log.WriteStrings(['',
      Format('Middle LockContainer.SumMode: %s', [FFMapLockToName(SummaryMode)])]);
    {$ENDIF}

    { Check for normal locks }
    While Assigned(TfsLockListItem(FWaitQueue.Peek)) Do
      With TfsLockListItem(FWaitQueue.Peek) Do
        If ffca_LockCompatibility[LockType, SummaryMode] Then
          Begin
            {$IFDEF LockLogging}
            Log.WriteStrings(['',
              Format('LockContainer.SumMode: %s', [FFMapLockToName(SummaryMode)])]);
            {$ENDIF}
            { Compatible waiting lock found, we must now move it to the granted
              list & mark it as granted. }
            anItem := TfsLockListItem(FWaitQueue.Dequeue);
            anItem.Status := fflsGranted;
            Insert(anItem);
            anItem.Event.SignalEvent;
          End
        Else
          { incompatible waiting lock found }
          Exit;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsLockContainer.IsEmpty: boolean;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := (Count = 0) And
      (FWaitQueue.Count = 0) And
      (FWaitConversionQueue.Count = 0);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.10}
{--------}

Procedure TfsLockContainer.RelaxRecordLock(Const aDatabaseID: TffCursorID);
Var
  anItem: TfsLockListItem;
  ItemIndex: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    {$IFDEF LockLogging}
    Log.WriteStrings(['',
      '========================================',
        Format('LockContainer.RelaxRecordLock: %d, DB %d',
        [Longint(Self), aDatabaseID])]);
    {$ENDIF}
    ItemIndex := fflIndexPrim(aDatabaseID);
    If ItemIndex <> -1 Then
      Begin
        anItem := TfsLockListItem(Items[ItemIndex]);
        anItem.F2ndaryID := 0;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{End !!.10}
{--------}

Procedure TfsLockContainer.ReleaseWaitingConversion(Const RequestorID: TffBaseID); {!!.10}
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    FWaitConversionQueue.Delete(RequestorID); {!!.10}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsLockContainer.ReleaseWaitingLock(Const RequestorID: TffBaseID); {!!.10}
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    FWaitQueue.Delete(RequestorID); {!!.10}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsLockContainer.ReleaseCursorLock(Const aCursorID: TffCursorID;
  Var aRefCount: Integer);
Var
  anItem: TfsLockListItem;
  ItemIndex: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {$IFDEF LockLogging}
    Log.WriteStrings(['',
      '========================================',
        Format('LockContainer.ReleaseCursorLock: %d, Cursor %d',
        [Longint(Self), aCursorID])]);
    {$ENDIF}
    ItemIndex := fflIndexPrim(aCursorID);
    If ItemIndex <> -1 Then
      Begin
        anItem := TfsLockListItem(Items[ItemIndex]);
        If anItem.RefCount > 1 Then
          Begin
            anItem.RefCount := anItem.RefCount - 1;
            aRefCount := anItem.RefCount;
          End
        Else
          Begin
            aRefCount := 0;
            DeleteAt(ItemIndex);
            ProcessQueue;
          End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsLockContainer.CountCursorLock(Const aCursorID: TffCursorID;
  Var aRefCount: Integer);
Var
  anItem: TfsLockListItem;
  ItemIndex: Longint;
Begin
  aRefCount := 0;
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {$IFDEF LockLogging}
    Log.WriteStrings(['',
      '========================================',
        Format('LockContainer.CountCursorLock: %d, Cursor %d',
        [Longint(Self), aCursorID])]);
    {$ENDIF}
    ItemIndex := fflIndexPrim(aCursorID);
    If ItemIndex <> -1 Then
      Begin
        anItem := TfsLockListItem(Items[ItemIndex]);
        aRefCount := anItem.RefCount;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsLockContainer.ReleaseCursorLockAll(Const aCursorID: TffCursorID);
Var
  ItemIndex: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    {$IFDEF LockLogging}
    Log.WriteStrings(['',
      '========================================',
        Format('LockContainer.ReleaseCursorLockAll: %d, cursor %d',
        [Longint(Self), aCursorID])]);
    {$ENDIF}
    ItemIndex := fflIndexPrim(aCursorID);
    If ItemIndex <> -1 Then
      With TfsLockListItem(Items[ItemIndex]) Do
        Begin
          DeleteAt(ItemIndex);
          ProcessQueue;
        End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsLockContainer.RequestLock(Const LockType: TfsSrcLockType;
  Const Conditional: Boolean;
  Const Transaction: TfsSrcTransaction;
  Const RequestorID: TffBaseID; {!!.10}
  Var WaitEvent: TFSNormalEvent
  ): TffLockStatus;
Var
  CvtLockItem: TfsLockListItem;
  CvtOnlyItem: boolean;
  ItemIndex: Longint;
  LockItem: TfsLockListItem;
  {$IFDEF LockLogging}
  TranID: TffTransID;
  {$ENDIF}
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    CvtOnlyItem := False;
    ItemIndex := fflIndexPrim(RequestorID); {!!.10}
    {$IFDEF LockLogging}
    If Transaction = Nil Then
      TranID := 0
    Else
      TranID := Transaction.TransactionID;
    {$ENDIF}
    If ItemIndex <> -1 Then
      Begin
        { If a lock item already exists for this transaction, then we need to
          see if it is compatible }
        LockItem := TfsLockListItem(Items[ItemIndex]);
        assert(LockItem.Status = fflsGranted);
        If LockItem.LockType >= LockType Then
          Begin
            { The lock is compatible, so we increment the lock's RefCount }
            {$IFDEF LockLogging}
            Log.WriteStringFmt('ReqLock.Compatible, TR %d, CU %d', {!!.10}
              [TranID, RequestorID]); {!!.10}
            {$ENDIF}
            LockItem.RefCount := LockItem.RefCount + 1;
            Result := fflsGranted;
            Exit;
          End
        Else
          Begin
            {$IFDEF LockLogging}
            Log.WriteStringFmt('ReqLock.Incompatible, TR %d, CU %d', {!!.10}
              [TranID, RequestorID]); {!!.10}
            {$ENDIF}
            { The LockTypes are not compatible, so we must convert the existing Lock }

            { To facilitate the lock conversion, we add a new lock request into the
              queue. Once the lock is granted, any existing locks for the same
              transaction will be removed, and this lock will take it's place. }

            { If there is only 1 item in the queue then we are that item and the
              conversion can be granted automatically.  Set a flag so we know this
              later on. }
            CvtOnlyItem := (Count = 1);

            { Create and Initialize the lock item }
            CvtLockItem := TfsLockListItem.Create(RequestorID); {!!.10}
            CvtLockItem.LockType := ffca_LockConversion[LockItem.LockType, LockType];
            CvtLockItem.Transaction := Transaction;
            CvtLockItem.RefCount := 1;
            CvtLockItem.Conversion := True;
            CvtLockItem.MaintainLinks := False;
            LockItem.Conversion := True;
            { Mark the granted lock's conversion flag so that if we know to pull
              a lock item out of the wait conversion queue if the lock request
              times out. }

          { We are done with the original lock item, so we will set it (the var)
            to the new converted lock item. When this new lock is granted, any
            existing locks for this transaction will be removed. }
            LockItem := CvtLockItem;
          End;
      End
    Else
      Begin
        { Create and Initialize the lock item }
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.CreateLockItem, TR %d, CU %d',
          [TranID, RequestorID]); {!!.10}
        {$ENDIF}
        LockItem := TfsLockListItem.Create(RequestorID); {!!.10}
        LockItem.LockType := LockType;
        LockItem.Transaction := Transaction;
        LockItem.RefCount := 1;
        LockItem.Conversion := False;
        LockItem.MaintainLinks := False;
      End;

    { If there are no items in the queue or we are the only item in the queue
      and we happen to be a conversion request then grant the lock. }
    If (Count = 0) Or CvtOnlyItem Then
      Begin
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.OnlyItem, TR %d, CU %d',
          [TranID, RequestorID]); {!!.10}
        {$ENDIF}
        Result := AddLock(True, Conditional, LockItem);
        Exit;
      End;

    { If the last lock is waiting, then make the new lock wait in line }
    If (FWaitQueue.Count > 0) Then
      Begin
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.MakeWait, TR %d, CU %d',
          [TranID, RequestorID]); {!!.10}
        {$ENDIF}
        Result := AddLock(False, Conditional, LockItem)
      End
    Else If ffca_LockCompatibility[LockType, SummaryMode] Then
      Begin
        { No locks are waiting, the summary mode is compatible, so add a granted
          lock. }
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.CompatibleWithSumMode, TR %d, CU %d',
          [TranID, RequestorID]); {!!.10}
        {$ENDIF}
        Result := AddLock(True, Conditional, LockItem)
      End
    Else
      Begin
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.LastOption, TR %d, CU %d',
          [TranID, RequestorID]); {!!.10}
        {$ENDIF}
        Result := AddLock(False, Conditional, LockItem);
      End;

    If Result In [fflsWaiting, fflsWaitingConv] Then
      Begin
        { We need to create the waitfor event }
        WaitEvent := TFSNormalEvent.Create;
        LockItem.Event := WaitEvent;
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.CreateWaitEvent, TR %d, CU %d',
          [TranID, RequestorID]); {!!.10}
        {$ENDIF}
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{Begin !!.10}
{--------}

Function TfsLockContainer.RequestRecLock(Const LockType: TfsSrcLockType;
  Const Conditional: Boolean;
  Const Transaction: TfsSrcTransaction;
  Const ReqPrimaryID,
  ReqSecondaryID: TffBaseID;
  Var WaitEvent: TFSNormalEvent
  ): TffLockStatus;
Var
  CvtLockItem: TfsLockListItem;
  CvtOnlyItem: boolean;
  ItemIndex: Longint;
  LockItem: TfsLockListItem;
  {$IFDEF LockLogging}
  TranID: TffTransID;
  {$ENDIF}
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF}
    CvtOnlyItem := False;
    { Look for an existing lock held by the database. }
    ItemIndex := fflIndexPrim(ReqPrimaryID);
    {$IFDEF LockLogging}
    If Transaction = Nil Then
      TranID := 0
    Else
      TranID := Transaction.TransactionID;
    {$ENDIF}
    { Did we find an existing lock entry for the database? }
    If ItemIndex <> -1 Then
      Begin
        { Yes. If a lock item already exists then we need to determine if it was
          obtained by a different cursor. }
        LockItem := TfsLockListItem(Items[ItemIndex]);
        assert(LockItem.Status = fflsGranted);
        If ((LockItem.SecondaryID = 0) Or
          (LockItem.SecondaryID = ReqSecondaryID)) Then
          Begin
            { The lock is held by the same cursor or was held by another cursor but
              released after it was finished with the record. Next, determine if the
              existing lock and the requested lock are compatible. }
            If (LockItem.LockType >= LockType) Then
              Begin
                { The lock is compatible, so we increment the lock's RefCount }
                If LockItem.SecondaryID = 0 Then
                  Begin
                    {$IFDEF LockLogging}
                    Log.WriteStringFmt('ReqLock.Compatible, TR %d, DB %d, CU %d, ' +
                      'obtaining exclusive access',
                      [TranID, ReqPrimaryID, ReqSecondaryID]);
                    {$ENDIF}
                    LockItem.F2ndaryID := ReqSecondaryID;
                  End
                Else
                  Begin
                    {$IFDEF LockLogging}
                    Log.WriteStringFmt('ReqLock.Compatible, TR %d, DB %d, CU %d',
                      [TranID, ReqPrimaryID, ReqSecondaryID]);
                    {$ENDIF}
                  End;
                LockItem.RefCount := LockItem.RefCount + 1;
                Result := fflsGranted;
                Exit;
              End
            Else
              Begin
                {$IFDEF LockLogging}
                Log.WriteStringFmt('ReqLock.Incompatible, TR %d, DB %d, CU %d',
                  [TranID, ReqPrimaryID, ReqSecondaryID]);
                {$ENDIF}
                { The LockTypes are not compatible, so we must convert the existing Lock }

                { To facilitate the lock conversion, we add a new lock request into the
                  queue. Once the lock is granted, any existing locks for the same
                  transaction will be removed, and this lock will take it's place. }

                { If there is only 1 item in the queue then we are that item and the
                  conversion can be granted automatically.  Set a flag so we know this
                  later on. }
                CvtOnlyItem := (Count = 1);

                { Create and Initialize the lock item }
                CvtLockItem := TfsLockListItem.Create(ReqPrimaryID);
                CvtLockItem.F2ndaryID := LockItem.F2ndaryID;
                CvtLockItem.LockType := ffca_LockConversion[LockItem.LockType, LockType];
                CvtLockItem.Transaction := Transaction;
                CvtLockItem.RefCount := 1;
                CvtLockItem.Conversion := True;
                CvtLockItem.MaintainLinks := False;
                LockItem.Conversion := True;
                { Mark the granted lock's conversion flag so that if we know to pull
                  a lock item out of the wait conversion queue if the lock request
                  times out. }

              { We are done with the original lock item, so we will set it (the var)
                to the new converted lock item. When this new lock is granted, any
                existing locks for this transaction will be removed. }
                LockItem := CvtLockItem;
              End; { if }
          End
        Else
          Begin
            { The existing lock is being exclusively held by another cursor in the
              same database. This situation represents a coding error. }
            Result := fflsRejected;
            Exit;
          End;
      End
    Else
      Begin
        { Create and Initialize the lock item }
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.CreateLockItem, TR %d, DB %d, CU %d',
          [TranID, ReqPrimaryID, ReqSecondaryID]);
        {$ENDIF}
        LockItem := TfsLockListItem.Create(ReqPrimaryID);
        LockItem.F2ndaryID := ReqSecondaryID;
        LockItem.LockType := LockType;
        LockItem.Transaction := Transaction;
        LockItem.RefCount := 1;
        LockItem.Conversion := False;
        LockItem.MaintainLinks := False;
      End;

    { If there are no items in the queue or we are the only item in the queue
      and we happen to be a conversion request then grant the lock. }
    If (Count = 0) Or CvtOnlyItem Then
      Begin
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.OnlyItem, TR %d, DB %d, CU %d',
          [TranID, ReqPrimaryID, ReqSecondaryID]);
        {$ENDIF}
        Result := AddLock(True, Conditional, LockItem);
        Exit;
      End;

    { If the last lock is waiting, then make the new lock wait in line }
    If (FWaitQueue.Count > 0) Then
      Begin
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.MakeWait, TR %d, DB %d, CU %d',
          [TranID, ReqPrimaryID, ReqSecondaryID]);
        {$ENDIF}
        Result := AddLock(False, Conditional, LockItem)
      End
    Else If ffca_LockCompatibility[LockType, SummaryMode] Then
      Begin
        { No locks are waiting, the summary mode is compatible, so add a granted
          lock. }
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.CompatibleWithSumMode, TR %d, DB %d, CU %d',
          [TranID, ReqPrimaryID, ReqSecondaryID]);
        {$ENDIF}
        Result := AddLock(True, Conditional, LockItem)
      End
    Else
      Begin
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.LastOption, TR %d, DB %d, CU %d',
          [TranID, ReqPrimaryID, ReqSecondaryID]);
        {$ENDIF}
        Result := AddLock(False, Conditional, LockItem);
      End;

    If Result In [fflsWaiting, fflsWaitingConv] Then
      Begin
        { We need to create the waitfor event }
        WaitEvent := TFSNormalEvent.Create;
        LockItem.Event := WaitEvent;
        {$IFDEF LockLogging}
        Log.WriteStringFmt('ReqLock.CreateWaitEvent, TR %d, DB %d, CU %d',
          [TranID, ReqPrimaryID, ReqSecondaryID]);
        {$ENDIF}
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF}
End;
{End !!.10}
{Begin !!.03}
{--------}

Function TfsLockContainer.SimpleDeadlock: Boolean;
Var
  anInx, anInx2: Longint;
  LockItem, LockItem2: TfsLockListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Assumption: The transaction invoking this method has not submitted its
      request for an Exclusive lock. }
    { Scan through the wait queues for a transaction that is requesting an
      Exclusive lock. If found then see if it has been granted a share lock.
      If found then return True else return False. }
    Result := False;
    {Check the wait queue. }
    For anInx := 0 To Pred(FWaitQueue.Count) Do
      Begin
        LockItem := TfsLockListItem(FWaitQueue.Items[anInx]);
        If LockItem.LockType = ffsltExclusive Then
          { Found a waiting request for Exclusive lock. Already granted a
            share lock? }
          For anInx2 := 0 To Pred(Count) Do
            Begin
              LockItem2 := TfsLockListItem(Items[anInx]);
              If (LockItem2 <> Nil) And {!!.06}
              (LockItem2.Transaction = LockItem.Transaction) Then
                Begin {!!.06}
                  Result := True;
                  Exit;
                End;
            End;
      End;

    {Check the wait conversion queue. }
    For anInx := 0 To Pred(FWaitConversionQueue.Count) Do
      Begin
        LockItem := TfsLockListItem(FWaitConversionQueue.Items[anInx]);
        If LockItem.LockType = ffsltExclusive Then
          { Found a waiting request for Exclusive lock. Already granted a
            share lock? }
          For anInx2 := 0 To Pred(Count) Do
            Begin
              LockItem2 := TfsLockListItem(Items[anInx]);
              If LockItem2.Transaction = LockItem.Transaction Then
                Begin
                  Result := True;
                  break;
                End;
            End;
      End; { for }
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{End !!.03}
{--------}

Function TfsLockContainer.SummaryMode: TfsSrcLockType;
Var
  Idx: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := ffsltNone;
    {$IFDEF LockLogging}
    Log.WriteStringFmt('SumMode:Lock container Count: %d', [Count]);
    {$ENDIF}
    For Idx := 0 To Pred(Count) Do
      With TfsLockListItem(Items[Idx]) Do
        Begin
          {$IFDEF LockLogging}
          Log.WriteStringFmt('SumMode:Item %d, lock type %s, status %d (0=rej, 1=grant, 2=wait)',
            [Idx, FFMapLockToName(LockType),
            ord(Status)]);
          {$ENDIF}
          If (LockType > Result) Then
            Result := LockType;
        End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{$IFDEF UseLockContainerPool}
{===TfsLockContainerPool=============================================}

Constructor TfsLockContainerPool.Create(Const InitialCount, RetainCount: Integer);
Var
  aLockContainer: TfsLockContainer;
  Index: Integer;
Begin
  Inherited Create;
  FList := TfsPointerList.Create;
  FRetainCount := RetainCount;
  FPadLock := TfsPadlock.Create;

  { Create the initial set of LockContainers. }
  For Index := 1 To InitialCount Do
    Begin
      aLockContainer := TfsLockContainer.Create;
      FList.Append(aLockContainer);
    End;
End;
{--------}

Destructor TfsLockContainerPool.Destroy;
Var
  Index: Longint;
Begin
  { Explicitly free the lock containers.  They are not freed
    by FList.Free. }
  For Index := pred(FList.Count) Downto 0 Do
    TfsLockContainer(FList[Index]).Free;
  FList.Free;
  FPadLock.Free;
  Inherited Destroy;
End;
{Begin !!.01}
{--------}

Procedure TfsLockContainerPool.Flush;
Var
  aLockContainer: TfsLockContainer;
  anInx: Longint;
Begin
  FPadLock.Lock;
  Try
    If FList.Count > FRetainCount Then
      For anInx := pred(FList.Count) Downto FRetainCount Do
        Begin
          aLockContainer := FList.Pointers[anInx];
          FList.RemoveAt(anInx);
          aLockContainer.Free;
        End;
  Finally
    FPadLock.Unlock;
  End;
End;
{End !!.01}
{--------}

Function TfsLockContainerPool.Get: TfsLockContainer;
Var
  aCount: Longint;
Begin
  FPadLock.Lock;
  Try
    If FList.IsEmpty Then
      Result := TfsLockContainer.Create
    Else
      Begin
        { Get the last item in the list.  This speeds up the RemoveAt
          operation incredibly since it won't have to shift any bytes in the
          list. }
        aCount := Pred(FList.Count);
        Result := FList.Pointers[aCount];
        FList.RemoveAt(aCount);
      End;
  Finally
    FPadLock.Unlock;
  End;
End;
{--------}

Procedure TfsLockContainerPool.Put(Const aLockContainer: TfsLockContainer);
Begin
  FPadLock.Lock;
  Try
    FList.Append(aLockContainer);
  Finally
    FPadLock.Unlock;
  End;
End;
{====================================================================}
{$ENDIF}

{===Utility routines=================================================}

Function FFMapLockToName(aLockType: TfsSrcLockType): String;
Begin
  Case aLockType Of
    ffsltNone: Result := ffcLockNone;
    ffsltIntentS: Result := ffcLockIntentS;
    ffsltIntentX: Result := ffcLockIntentX;
    ffsltShare: Result := ffcLockShare;
    ffsltSIX: Result := ffcLockSIX;
    ffsltUpdate: Result := ffcLockUpdate;
    ffsltExclusive: Result := ffcLockExclusive;
  End; { case }
End;
{$IFDEF LockLogging}
{--------}

Function FFMapRequestStatusToName(aStatus: TffLockRequestStatus): String;
Begin
  Case aStatus Of
    fflrsGranted: Result := ffcRStatusGranted;
    fflrsTimeout: Result := ffcRStatusTimeout;
    fflrsRejected: Result := ffcRStatusRejected;
  End;
End;
{$ENDIF}
{====================================================================}

{===TfsLockListItem==================================================}

Constructor TfsLockListItem.Create(Const aKey: TffBaseID); {!!.10}
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    FPrimaryID := aKey; {!!.10}
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsLockListItem.Compare(aKey: pointer): Integer;
Begin
  Result := FFCmpI32(PffWord32(aKey)^, FPrimaryID); {!!.10}
End;
{--------}

Function TfsLockListItem.Key: pointer;
Begin
  Result := @FPrimaryID; {!!.10}
End;
{====================================================================}

{===TfsLockQueue=====================================================}

Procedure TfsLockQueue.EnqueuePriority(anItem: TFSSpecListItem);
Var
  NewItem: TFSSpecListItem;
  OldItem: TFSSpecListItem;
  Idx: Integer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If GetCount = 0 Then
      Enqueue(anItem)
    Else
      Begin
        { insert the new item at the beginning of the queue, and
          adjust accordingly }
        NewItem := anItem;
        For Idx := 0 To Pred(ffqList.Count) Do
          Begin
            OldItem := ffqList[Idx];
            ffqList[Idx] := NewItem;
            NewItem := OldItem;
          End;
        Enqueue(NewItem);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsLockQueue.Peek: TFSSpecListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := Nil;
    If GetCount > 0 Then
      Result := ffqList[0];
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{===TfsTransContainer================================================}

Constructor TfsTransContainer.Create(Const aKey: TfsSrcTransaction);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Inherited Create;
    Transaction := TfsSrcTransaction(aKey);

    FContentLocks := TFSNormalList.Create;
    FRecordLocks := TFSNormalList.Create;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Destructor TfsTransContainer.Destroy;
Var
  Inx: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}

    FContentLocks.Free;
    { Note: We must *NOT* free the TfsSrcTable referenced by the ExtraData
      property of each item. }
    FContentLocks := Nil;

    For Inx := pred(FRecordLocks.Count) Downto 0 Do
      TFSSpecThreadList(TFSSpecW32ListItem(FRecordLocks[Inx]).ExtraData).Free;
    FRecordLocks.Free;
    FRecordLocks := Nil;

    Transaction.TransLockContainer := Nil;

    Inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsTransContainer.AddContentLock(Container: TfsLockContainer;
  ParentTable: TFSSpecObject;
  LockType: TfsSrcLockType);
Var
  anItem: TFSSpecW32ListItem;
  anIndx: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    anIndx := FContentLocks.Index(TffWord32(Container));
    { Have we referenced this lock container before? }
    If anIndx = -1 Then
      Begin
        { No.  Create a reference to this lock container. }
        anItem := TFSSpecW32ListItem.Create(TffWord32(Container));
        anItem.ExtraData := ParentTable;
        anItem.ExtraData2 := ord(LockType);
        { Note: The table referenced by ExtraData must *NOT* be freed. }
        FContentLocks.Insert(anItem);
      End
    Else
      Begin
        { Yes. Update the lock type. }
        anItem := TFSSpecW32ListItem(FcontentLocks[anIndx]);
        anItem.ExtraDAta2 := ord(LockType);
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsTransContainer.AddRecordLock(Const FI: PffFileInfo;
  Const CursorID: TffCursorID;
  Const ResourceID: TffInt64);
Var
  FileItem: TFSSpecW32ListItem;
  FileIdx: Longint;
  ResList: TfsHash64;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    FileIdx := FRecordLocks.Index(TffWord32(FI));
    If FileIdx = -1 Then
      Begin
        { Create the file item, and resource list }
        FileItem := TFSSpecW32ListItem.Create(TffWord32(FI));
        { Add the file item to the list }
        FRecordLocks.Insert(FileItem);
      End
    Else
      { Retrieve the information from the list }
      FileItem := TFSSpecW32ListItem(FRecordLocks.Items[FileIdx]);

    ResList := TfsHash64(FileItem.ExtraData);
    If Not assigned(ResList) Then
      Begin
        ResList := TfsHash64.Create(fsc_Size521);
        ResList.CanShrink := False;
        FileItem.ExtraData := ResList;
      End;

    ResList.Add(ResourceID, pointer(CursorID));
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsTransContainer.Compare(aKey: pointer): Integer;
Begin
  Result := FFCmpI32(PffLongint(aKey)^, Longint(FTransaction));
End;
{--------}

Function TfsTransContainer.Key: pointer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := @FTransaction;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsTransContainer.RemoveContentLock(Container: TfsLockContainer);
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { If this container is not present then this should fall through without
      doing anything.  No exception should be raised. }
    FContentLocks.Delete(TffWord32(Container));
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Procedure TfsTransContainer.RemoveRecordLock(Const FI: PffFileInfo;
  Const ResourceID: TffInt64);
Var
  FileItem: TFSSpecW32ListItem;
  FileIdx: Longint;
  ResList: TfsHash64;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    FileIdx := FRecordLocks.Index(TffWord32(FI));
    If FileIdx > -1 Then
      Begin
        FileItem := TFSSpecW32ListItem(FRecordLocks.Items[FileIdx]);
        ResList := TfsHash64(FileItem.ExtraData);

        ResList.Remove(ResourceID);

        If ResList.Count = 0 Then
          Begin
            FRecordLocks.Delete(TffWord32(FI));
            ResList.Free;
          End;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsTransContainer.TableContentLockType(Container: TfsLockContainer): TfsSrcLockType;
Var
  aInx: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    { Assumptions: FContentLocks is assigned. }
    With Container.BeginRead Do
      Try
        aInx := FContentLocks.Index(TffWord32(Container));
        { Does the transaction have a content lock on this table? }
        If aInx = -1 Then
          { No. }
          Result := ffsltNone
        Else
          Result := TfsSrcLockType(TFSSpecW32ListItem(FContentLocks[aInx]).ExtraData2);
      Finally
        EndRead;
      End;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsTransContainer.tcGetContentCount: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If assigned(FContentLocks) Then
      Result := FContentLocks.Count
    Else
      Result := 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsTransContainer.tcGetContentContainer(Const aInx: Longint): TfsLockContainer;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    Result := TfsLockContainer(TFSSpecW32ListItem(FContentLocks[aInx]).KeyAsInt);
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsTransContainer.tcGetContentLockType(Const aInx: Longint): TfsSrcLockType;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If assigned(FContentLocks) Then
      Result := TfsSrcLockType(TFSSpecW32ListItem(FContentLocks[aInx]).ExtraData2)
    Else
      Result := ffsltNone;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsTransContainer.tcGetContentTable(Const aInx: Longint): TFSSpecObject;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If assigned(FContentLocks) Then
      Result := TFSSpecObject(TFSSpecW32ListItem(FContentLocks[aInx]).ExtraData)
    Else
      Result := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsTransContainer.tcGetFileCount: Longint;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If assigned(FRecordLocks) Then
      Result := FRecordLocks.Count
    Else
      Result := 0;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{--------}

Function TfsTransContainer.tcGetFiles(Const aInx: Longint): TFSSpecW32ListItem;
Begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter;
  Try
    {$ENDIF} {!!.03}
    If assigned(FRecordlocks) Then
      Result := TFSSpecW32ListItem(FRecordLocks[aInx])
    Else
      Result := Nil;
  {$IFDEF FF_DEBUG_THREADS}Finally ThreadExit;
  End;
  {$ENDIF} {!!.03}
End;
{====================================================================}

{$IFDEF UseLockContainerPool}
{===Initialization/Finalization======================================}

Procedure FinalizeUnit;
Begin
  FSLockContainerPool.Free;
End;
{--------}

Procedure InitializeUnit;
Begin
  FSLockContainerPool := TfsLockContainerPool.Create(250, 1000);
End;

Initialization
  InitializeUnit;

Finalization
  FinalizeUnit;
  {$ENDIF}

End.

