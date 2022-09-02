{*********************************************************}
{* FlashFiler: Database lock manager                     *}
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

{.$DEFINE LockLogging}
{$DEFINE UseLockContainerPool} 

unit ffsrlock;

interface
uses
  {$IFDEF LockLogging}
  fflllog,
  {$ENDIF}
  SysUtils,
  Classes,
  Windows,
  Forms,
  ffllbase,
  ffllthrd,
  ffhash,
  ffsrbase,
  ffsrbde,
  ffllexcp,
  ffconst;

resourcestring
  ffcLockNone      = 'None';
  ffcLockIntentS   = 'Intent Shared';
  ffcLockIntentX   = 'Intent Exclusive';
  ffcLockShare     = 'Share';
  ffcLockSIX       = 'Shared Intent Exclusive';
  ffcLockUpdate    = 'Update';
  ffcLockExclusive = 'Exclusive';
  {$IFDEF LockLogging}
  ffcDurationInstant = 'Instant';
  ffcDurationShort   = 'Short';
  ffcDurationCommit  = 'Commit';
  ffcRStatusGranted  = 'Granted';
  ffcRStatusTimeout  = 'Timeout';
  ffcRStatusDeadlock = 'Deadlock';
  ffcRStatusRejected = 'Rejected';
  {$ENDIF}


type
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
  TffSrLockType = (ffsltNone, ffsltIntentS, ffsltIntentX, ffsltShare,
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

  TffLockListItem  = class(TffListItem)
    private
      FConversion  : Boolean;
{Begin !!.10}
      FPrimaryID   : TffBaseID;
        { The unique ID of the server-side object requesting a lock. The object
          may be a client, session, database, or cursor. }
      F2ndaryID    : TffBaseID;
        { F2ndaryID is used for record locks. If it has the value zero then
          a cursor associated with database FPrimaryID edited and released the
          record. It is okay for a cursor associated with the same database to
          lock the record. If non-zero then a cursor associated with database
          FPrimaryID currently has the record locked. It is not okay to lock
          the record for another cursor associated with the same database. }
{End !!.10}
      FLockType    : TffSrLockType;
      FEvent       : TffEvent;
      FRefCount    : Integer;
      FStatus      : TffLockStatus;
      FTransaction : TffSrTransaction;
    protected
    public
      constructor Create(const aKey : TffBaseID);                      {!!.10}
        {-create the list item; aKey is its access/sort key}
      function Compare(aKey : pointer) : integer; override;
        {-compare Self's key to aKey: return <0 if aKey < Self's, 0 if
          equal, >0 otherwise}
      function Key : pointer; override;
        {-return a pointer to this item's key: it'll be a pointer to a
          cursorID }

      property LockType : TffSrLockType
         read FLockType write FLockType;
{Begin !!.10}
      property PrimaryID : TffBaseID
         read FPrimaryID;
      property SecondaryID : TffBaseID
         read F2ndaryID;
{End !!.10}
      property Status : TffLockStatus
         read FStatus write FStatus;
      property Transaction : TffSrTransaction
         read FTransaction write FTransaction;
      property Event : TffEvent
         read FEvent write FEvent;
      property RefCount : Integer
         read FRefCount write FRefCount;
      property Conversion : Boolean
         read FConversion write FConversion;
  end;

  TffLockQueue = class(TffQueue)
    public
      function Peek : TffListItem;
        { Retrieve a pointer to the first item in the list }
      procedure EnqueuePriority(anItem : TffListItem);
        { Queue and item, but place it first in the queue }
  end;

  { One TffLockContainer is required for each ResourceID, no two resources
    share a LockContainer. TffLockContainer is responsible for maintaining the
    following:

    1. A list of granted lock requests.
    2. A queue of waiting lock requests.
    3. A queue of conversion lock requests. }
  TffLockContainer = class(TffThreadList)
    protected
      FWaitQueue           : TffLockQueue;
      FWaitConversionQueue : TffLockQueue;
      function AddLock(const Granted     : Boolean;
                       const Conditional : Boolean;
                             LockItem    : TffLockListItem) : TffLockStatus;
      function LastLock : TffLockListItem;
      procedure ProcessQueue;
      procedure ProcessLockConversion(const aCursorID : TffCursorID;
                                            aLockListItem : TffLockListItem);
    public
      constructor Create; override;
      destructor Destroy; override;
      function IsEmpty : boolean;
      procedure RelaxRecordLock(const aDatabaseID : TffCursorID);      {!!.10}
      procedure ReleaseCursorLock(const aCursorID : TffCursorID;
                                    var aRefCount : Integer);
        { Release the specified cursor lock.  If the lock's reference count
          is greater than one then the reference count is decremented. }
      procedure ReleaseCursorLockAll(const aCursorID : TffCursorID);
      procedure ReleaseWaitingConversion(const RequestorID : TffBaseID); {!!.10}
      procedure ReleaseWaitingLock(const RequestorID : TffBaseID);     {!!.10}
      function RequestLock(const LockType    : TffSrLockType;
                           const Conditional : Boolean;
                           const Transaction : TffSrTransaction;
                           const RequestorID : TffBaseID;              {!!.10}
                             var WaitEvent   : TffEvent) : TffLockStatus;
{Begin !!.10}
        { Used to request all types of locks excluding record locks. }
      function RequestRecLock(const LockType    : TffSrLockType;
                              const Conditional : Boolean;
                              const Transaction : TffSrTransaction;
                              const ReqPrimaryID,
                                    ReqSecondaryID : TffBaseID;
                                var WaitEvent   : TffEvent) : TffLockStatus;
        { Used to request record locks. }
{End !!.10}
{Begin !!.03}
      function SimpleDeadlock : Boolean;
        { See if a simple deadlock situation may occur. Assumes that this
          method is called only when a) the specified lock container has
          granted locks to more than one transaction, b) the specified
          Transaction has been granted a share lock, & c) the specified
          transaction plans to request an Exclusive lock but has not
          submitted the request at the time of this method call. }
{End !!.03}
      function SummaryMode : TffSrLockType;
  end;

  TffLockManager = class(TffObject)
  protected
    {$IFDEF LockLogging}
    FEventLog : TffEventLog;
    {$ENDIF}
    FTableLocks  : TffThreadHash;
    FTransactions : TffThreadList;
    FStartWithLock : TffPadlock;                                       {!!.10}
    procedure DisposeLockList(Sender : TffBaseHashTable; aData : Pointer);
    procedure DisposeRecordLockList(Sender : TffBaseHashTable; aData : Pointer);
    procedure ReleaseContentLockAll(Container : TffLockContainer;
                                    Transaction : TffSrTransaction);

    procedure ReleaseRecordLockAll(const ResourceID  : TffInt64;
                                   const FI          : PffFileInfo;
                                   const Transaction : TffSrTransaction;
                                   const DatabaseID  : TffDatabaseID); {!!.10}
      { Releases a record lock for a cursor regardless of the record lock's
        reference count.
        Parameters:
          ResourceID - The reference number of the record that was locked.
          FI - The file containing the locked record.
          Transaction - The transaction in which the record was locked.
          CursorID - The cursor locking the record. }

    procedure RelRecLockIterator(aKey : TffInt64; aData : pointer;
                           const cookie1, cookie2, cookie3 : TffWord32);
      { Used to free record locks held by a transaction. }
  public
    constructor Create;
    destructor Destroy; override;

    function AcquireClientLock(const Container : TffLockContainer;
                               const CursorID  : TffCursorID;
                               const Timeout   : TffWord32;
                               const LockType  : TffSrLockType) : TffLockRequestStatus;
      { Use this method to obtain client locks on a server table
        (e.g., TffTable.LockTable).  Container is the table's client lock
        container. Cursor is the cursor requesting the lock. }

    function AcquireContentLock(const Container : TffLockContainer;
                                const ParentTable : TffObject;
                                const Transaction : TffSrTransaction;
                                const Conditional : Boolean;
                                const Timeout : TffWord32;
                                const LockType : TffSrLockType) : TffLockRequestStatus;
      { Use this method to acquire a content lock on a server table for a
        transaction.  Container is the table's content lock container.
        Transaction is the transaction requesting the content lock. }

    function AcquireRecordLock(const ResourceID  : TffInt64;
                               const FI          : PffFileInfo;
                               const LockType    : TffSrLockType;
                               const Conditional : Boolean;
                               const Timeout     : TffWord32;
                               const Transaction : TffSrTransaction;
                               const DatabaseID  : TffDatabaseID;      {!!.10}
                               const CursorID    : TffCursorID) : TffLockRequestStatus;
      { CursorID requests a RecordLock of type LockType on ResourceID/FileID
        for Duration. }

    function AcquireTableLock(const ResourceID  : TffWord32;
                              const LockType    : TffSrLockType;
                              const Conditional : Boolean;
                              const Timeout     : TffWord32;
                              const CursorID    : TffCursorID) : TffLockRequestStatus;
      { CursorID requests a TableLock of type LockType on ResourceID
        for Duration. }

    procedure GetWaitingRecordLocks(const ResourceID   : TffInt64;
                                    const FI           : PffFileInfo;
                                    const Transaction  : TffSrTransaction;
                                      var WaitingLocks : TffPointerList);

    function HasClientLock(const Container : TffLockContainer;
                           const CursorID : TffCursorID) : boolean;
      { Returns True if the client has any kind of client lock on the table. }

    function IsContentLockedBy(const Container : TffLockContainer;
                               const Transaction : TffSrTransaction) : boolean;
      { Does the specified transaction have any kind of content lock on the
        table?  Returns True if the transaction has any kind of content lock. }

    function IsTableLockedBy(const aResourceID : TffWord32;
                             const aCursorID   : TffCursorID;
                             const aLockType   : TffSrLockType) : Boolean;
      { Returns True if a lock of the specified type was granted to the
        specified cursor. }

    function IsRecordLocked(const aResourceID  : TffInt64;
                            const aFI          : PffFileInfo) : Boolean;
      { Returns True if the record is locked.  Assumption: FF only requests
        Exclusive record locks. }

    function TableLockGranted(const ResourceID : Longint) : TffSrLockType;
      { Returns the summary mode for table ResourceID. If a lock is not
        present, this routine returns ffslNone. }

    function RecordLockGranted(const ResourceID : TffInt64;
                               const FI         : PffFileInfo) : TffSrLockType;
      { Returns the summary mode for record ResourceID. If a lock is not
        present, this routine returns ffslNone. }

{Begin !!.10}
    procedure RelaxRecordLock(const ResourceID : TffInt64;
                              const FI : PffFileInfo;
                              const DatabaseID : TffDatabaseID);
      { Called after a successful insert, update, or delete so that another
        cursor within the same transaction may obtain a record lock on the
        same record. }
{End !!.10}

    procedure ReleaseClientLock(const Container : TffLockContainer;
                                const CursorID : TffCursorID);

    procedure ReleaseClientLockAll(const Container : TffLockContainer;
                                   const CursorID : TffCursorID);
    procedure ReleaseClientW(const Container : TffLockContainer;
                             const CursorID : TffCursorID);
    procedure ReleaseClientWC(const Container : TffLockContainer;
                              const CursorID : TffCursorID);

    procedure ReleaseContentLock(const Container : TffLockContainer;
                                 const Transaction : TffSrTransaction);
    procedure ReleaseContentW(const Container : TffLockContainer;
                              const Transaction : TffSrTransaction);
    procedure ReleaseContentWC(const Container : TffLockContainer;
                               const Transaction : TffSrTransaction);

    procedure ReleaseRecordLock(const ResourceID  : TffInt64;
                                const FI          : PffFileInfo;
                                const Transaction : TffSrTransaction;
                                const DatabaseID  : TffDatabaseID);    {!!.10}

    procedure ReleaseRecordW(const ResourceID  : TffInt64;
                             const FI          : PffFileInfo;
                             const DatabaseID  : TffDatabaseID);       {!!.10}

    procedure ReleaseRecordWC(const ResourceID  : TffInt64;
                              const FI          : PffFileInfo;
                              const DatabaseID  : TffDatabaseID);      {!!.10}

    procedure ReleaseTableLock(const ResourceID    : TffWord32;
                               const CursorID      : TffCursorID);

    procedure ReleaseTableLockAll(const aResourceID : Longint;
                                  const aCursorID   : TffCursorID);

    procedure ReleaseTableW(const ResourceID    : TffWord32;
                            const CursorID      : TffCursorID);

    procedure ReleaseTableWC(const ResourceID    : TffWord32;
                             const CursorID      : TffCursorID);

    procedure ReleaseTransactionLocks(const Transaction : TffSrTransaction;
                                      const RecordsOnly : boolean);
      { Call this method when committing a transaction.  Transaction is the
        transaction whose locks are to be freed.  If only record locks are to
        be freed, set RecordsOnly to True.  If both content and record locks
        are to be freed, set RecordsOnly to False. }

{Begin !!.10}
    property StartWithLock : TffPadlock read FStartWithLock;
      { Used by the TffServerEngine.TransactionStartWith method. }
{End !!.10}

  end;

  { This class tracks what record locks and content locks have been acquired
    by a transaction over the course of the transaction's life.  Each
    transaction has its own TffTransContainer.

    Only the thread carrying out an operation in the context of the
    transaction will access the transaction's TffTransContainer therefore
    the TffTransContainer does not need to be threadsafe.

    Each TffTransContainer contains a list of content locks.  Each content
    "lock" is a reference to the TffLockContainer of a TffSrTable.  A lock
    is added to the TffLockContainer when the transaction is granted a content
    lock.  The list of content locks in this class allows us to quickly
    reference the table-specific lock containers when removing the
    content locks on a per-transaction basis.

    Each element in the list of content locks not only points to a table's
    lock container but the element's ExtraData item is also a reference to
    the table itself. }
  TffTransContainer = class(TffListItem)
    protected
      FContentLocks : TffList;
        { List of  }
      FLockManager  : TffLockManager;
      FRecordLocks  : TffList;
      FTransaction  : TffSrTransaction;

      procedure AddContentLock(Container : TffLockContainer;
                               ParentTable : TffObject;
                               LockType : TffSrLockType);
      procedure AddRecordLock(const FI         : PffFileInfo;
                              const CursorID   : TffCursorID;
                              const ResourceID : TffInt64);
      procedure RemoveContentLock(Container : TffLockContainer);
      procedure RemoveRecordLock(const FI         : PffFileInfo;
                                 const ResourceID : TffInt64);
      function tcGetContentCount : Longint;
      function tcGetContentContainer(const aInx : Longint) : TffLockContainer;
      function tcGetContentLockType(const aInx : Longint) : TffSrLockType;
      function tcGetContentTable(const aInx : Longint) : TffObject;
      function tcGetFileCount : Longint;
      function tcGetFiles(const aInx : Longint) : TffWord32ListItem;

    public
      constructor Create(const aKey : TffSrTransaction);
        { Create the list item; aKey is its access/sort key. }

      destructor Destroy; override;

      function Compare(aKey : pointer) : integer; override;
        { Compare Self's key to aKey: return <0 if aKey < Self's, 0 if
          equal, >0 otherwise. }

      function Key : pointer; override;
        { Return a pointer to this item's key: it'll be a pointer to a
          TffInt64. }

      function TableContentLockType(Container : TffLockContainer) : TffSrLockType;
        { Returns the type of lock held by the transaction on a table's content.
          Container is the table's lock container. If the transaction does not
          have a content lock for the table then this function returns
          ffsltNone. }

      property ContentCount : Longint
         read tcGetContentCount;
        { Number of tables for which the transaction has been granted a
          content lock. }

      property ContentContainer[const aInx : Longint] : TffLockContainer
        read tcGetContentContainer;
        { Returns the lock container associated with a particular content
          lock.  aInx is base zero.  The upper bound of aInx is
          pred(ContentCount). }

      property ContentLockType[const aInx : Longint] : TffSrLockType
        read tcGetContentLockType;
        { Returns the type of lock held by a transaction on a table's content.
          This list is base zero. Use pred(ContentCount) to determine the
          upper bound. }

      property ContentTable[const aInx : Longint] : TffObject
        read tcGetContentTable;
        { Returns the TffSrTable (viewed as a TffObject) at the specified
          index.  This list is base zero.  Use pred(ContentCount) to
          determine the upper bound. }

      property FileCount : Longint
        read tcGetFileCount;
        { Returns the number of files for which the transaction has obtained
          record locks. }

      property Files[const aInx : Longint] : TffWord32ListItem
        read tcGetFiles;
        { Returns the data structure holding record locks for a particular file.
          aInx is base zero.  The upper bound is pred(FileCount). }

      property LockManager : TffLockManager
         read FLockManager write FLockManager;

      property Transaction : TffSrTransaction
         read FTransaction write FTransaction;
  end;

  function FFMapLockToName(aLockType : TffSrLockType) : string;
    { Translates a lock type into a lock name. }

  {$IFDEF LockLogging}
  function FFMapRequestStatusToName(aStatus : TffLockRequestStatus) : string;
    { Translates a request status type into a name. }
  {$ENDIF}
const
  ffc_DefaultDeadlockFreq = 5000;
    { The default deadlock detection frequency, in milliseconds.  A value of
      5000 means deadlock detection will occur every 5 seconds. }

{Begin !!.01}
{$IFDEF UseLockContainerPool}
type
  TffLockContainerPool = class(TObject)
  protected
    FList : TffPointerList;
    FRetainCount : Integer;
    FPadLock : TffPadLock;
  public
    constructor Create(const InitialCount, RetainCount : Integer);
    destructor Destroy; override;
    procedure Flush;
    function Get : TffLockContainer;
    procedure Put(const aLockContainer : TffLockContainer);
  end;

var
  FFLockContainerPool : TffLockContainerPool;
{$ENDIF}
{End !!.01}

implementation

{$IFDEF LockLogging}
var
  Log : TffEventLog;
{$ENDIF}

{$IFDEF LockLogging}
const
  { Logging constants }
  csResourceID        = '  ResourceID    : %8d';
  csResourceID64      = '  ResourceID    : %8d:%8d';
  csLockContainer     = '  LockContainer : %8d';
  csLockType          = '  LockType      : %s';
  csDuration          = '  Duration      : %s';
  csTransaction       = '  Transaction   : %8d';
  csConditional       = '  Conditional   : %s';                        {!!.10}
  csCursorID          = '  CursorID      : %8d';
  csDatabaseID        = '  DatabaseID    : %8d';                       {!!.10}
  csTimeout           = '  Timeout       : %d';
  csFI                = '  FI            : %8d';
  csLockRequestStatus = '  LRequestStat  : %s';
{$ENDIF}

const
  { Identifies whether or not a requested lock mode is compatible with an
    existing (i.e., granted) lock mode.  The first dimension of the array
    is the granted mode.  The second dimension of the array is the requested
    mode. }

  ffca_LockCompatibility : array[TffSrLockType, TffSrLockType] of Boolean =
    (                {None   IntS   IntX   Share  SIX    Updt   Excl }
     {ffsltNone}     (true , true , true , true , true , true , true ),
     {ffslIntentS}   (true , true , true , true , true , false, false),
     {ffsltIntentX } (true , true , true , false, false, false, false),
     {ffsltShare}    (true , true , false, true , false, false, false),
     {ffsltSIX}      (true , true , false, false, false, false, false),
     {ffsltUpdate}   (true , false, false, true , false, false, false),
     {ffsltExclusive}(true , false, false, false, false, false, false)
    );

  { This lock conversion matrix is used to determine the new lock
    type when a lock conversion is necessary. }

  ffca_LockConversion : array[TffSrLockType, TffSrLockType] of TffSrLockType =
    (
     {ffsltNone}       (ffsltNone,      ffsltIntentS,   ffsltIntentX,
                        ffsltShare,     ffsltSIX,       ffsltUpdate,
                        ffsltExclusive),

     {ffsltIntentS}    (ffsltIntentS,   ffsltIntentS,   ffsltIntentX,
                        ffsltShare,     ffsltSIX,       ffsltUpdate,
                        ffsltExclusive),

     {ffsltIntentX}    (ffsltIntentX,   ffsltIntentX,   ffsltIntentX,
                        ffsltSIX,       ffsltSIX,       ffsltExclusive,
                        ffsltExclusive),

    {ffsltShare}       (ffsltShare,     ffsltShare,     ffsltSIX,
                        ffsltShare,     ffsltSIX,       ffsltUpdate,
                        ffsltExclusive),

    {ffsltSIX}         (ffsltSIX,       ffsltSIX,       ffsltSIX,
                        ffsltSIX,       ffsltSIX,       ffsltSIX,
                        ffsltExclusive),

    {ffsltUpdate}      (ffsltUpdate,    ffsltUpdate,    ffsltExclusive,
                        ffsltUpdate,    ffsltSIX,       ffsltUpdate,
                        ffsltExclusive),

    {ffsltExclusive}   (ffsltExclusive, ffsltExclusive, ffsltExclusive,
                        ffsltExclusive, ffsltExclusive, ffsltExclusive,
                        ffsltExclusive)
    );

type
  TffWaitForListItem = class(TffIntListItem)
    private
      FWaitingTrans : TffSrTransaction;
    public
      property WaitingTrans : TffSrTransaction
         Read FWaitingTrans write FWaitingTrans;
  end;

{Begin Move !!.01}
{$IFDEF UseLockContainerPool}
//type
//  TffLockContainerPool = class(TObject)
//  protected
//    FList : TffPointerList;
//    FRetainCount : Integer;
//    FPadLock : TffPadLock;
//  public
//    constructor Create(const InitialCount, RetainCount : Integer);
//    destructor Destroy; override;
//    function Get : TffLockContainer;
//    procedure Put(const aLockContainer : TffLockContainer);
//  end;

//var
//  FFLockContainerPool : TffLockContainerPool;
{$ENDIF}
{End Move !!.01}

{===TffLockManager===================================================}
constructor TffLockManager.Create;
begin
  inherited Create;
  FTableLocks := TffThreadHash.Create(ffc_Size59);
  FTableLocks.OnDisposeData := DisposeLockList;

  FTransactions := TffThreadList.Create;
  FTransactions.Sorted := True;

  FStartWithLock := TffPadlock.Create;                                 {!!.10}

  {$IFDEF LockLogging}
  FEventLog := TffEventLog.Create(nil);
  FEventLog.FileName := ExtractFilePath(application.ExeName) + 'FFLOCK.LOG';
  FEventLog.Enabled := True;
  FEventLog.WriteStrings(['******************************',
                          '******************************',
                          Format('Lock Manager Started: %12D', [GetTickCount])]);
  Log := FEventLog;
  {$ENDIF}
end;
{--------}
destructor TffLockManager.Destroy;
begin
  FTableLocks.Clear;
  FTableLocks.Free;
  FTableLocks := nil;

  FTransactions.Free;
  FTransactions := nil;

  FStartWithLock.Free;                                                 {!!.10}

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          Format('Lock Manager Stopped: %12D', [GetTickCount])]);
  FEventLog.Free;
  {$ENDIF}

  inherited Destroy;
end;
{--------}
function TffLockManager.AcquireClientLock(const Container : TffLockContainer;
                                          const CursorID  : TffCursorID;
                                          const Timeout   : TffWord32;
                                          const LockType  : TffSrLockType) : TffLockRequestStatus;
var
  LockStatus : TffLockStatus;
  WaitEvent  : TffEvent;
  {$IFDEF LockLogging}
  StartTime  : DWORD;
  {$ENDIF}
begin
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
  try
    { Add the request to the queue. }
    LockStatus := Container.RequestLock(LockType,
                                        False,
                                        nil,
                                        CursorID,
                                        WaitEvent);
  finally
    Container.EndWrite;
  end;

  if LockStatus = fflsGranted then
    Result := fflrsGranted
  else if LockStatus = fflsRejected then
    Result := fflrsRejected
  else  { waiting }
  { The lock is now in the queue. At this point we must pause the thread
    until the lock is granted. The WaitEvent local var is passed to the
    TffLockContainer.RequestLock method. This keeps us from creating
    an instance of TffEvent unnecessarily. The container is responsible
    for the create operation if it is necessary}
    try
      try
        WaitEvent.WaitFor(Timeout);
        Result := fflrsGranted;
      except
        on E: EffException do begin
          if E.ErrorCode = fferrReplyTimeout then
            Result := fflrsTimeout
          else
            Result := fflrsRejected;
          if LockStatus = fflsWaiting then
            ReleaseClientW(Container, CursorID)
          else
            ReleaseClientWC(Container, CursorID);
        end
        else begin
          if LockStatus = fflsWaiting then
            ReleaseClientW(Container, CursorID)
          else
            ReleaseClientWC(Container, CursorID);
          raise;
        end;
      end
    finally
      WaitEvent.Free;
      WaitEvent := nil;
    end;

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
end;
{--------}
function TffLockManager.AcquireContentLock(const Container : TffLockContainer;
                                           const ParentTable : TffObject;
                                           const Transaction : TffSrTransaction;
                                           const Conditional : Boolean;
                                           const Timeout : TffWord32;
                                           const LockType : TffSrLockType) : TffLockRequestStatus;
var
  aCursorID : TffCursorID;
  LockStatus     : TffLockStatus;
  TransContainer : TffTransContainer;
  WaitEvent      : TffEvent;
  {$IFDEF LockLogging}
  StartTime      : DWORD;
  {$ENDIF}
begin
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
  try
    { Add the request to the queue. }
    LockStatus := Container.RequestLock(LockType, Conditional, Transaction,
                                        aCursorID, WaitEvent);
  finally
    Container.EndWrite;
  end;

  if LockStatus = fflsGranted then
    Result := fflrsGranted
  else if LockStatus = fflsRejected then
    Result := fflrsRejected
  else  { waiting }
  { The lock is now in the queue. At this point we must pause the thread
    until the lock is granted. The WaitEvent local var is passed to the
    TffLockContainer.RequestLock method. This keeps us from creating
    an instance of TffEvent unnecessarily. The container is responsible
    for the create operation if it is necessary}
    try
      try
        {$IFDEF LockLogging}
        FEventLog.WriteStrings(['',
                                '========================================',
                                Format('AcquireContentLock - Waiting: %d', [aCursorID])]);
        {$ENDIF}
        WaitEvent.WaitFor(Timeout);
        Result := fflrsGranted;
      except
        on E: EffException do begin
          if E.ErrorCode = fferrReplyTimeout then
            Result := fflrsTimeout
          else
            Result := fflrsRejected;
          if LockStatus = fflsWaiting then
            ReleaseContentW(Container, Transaction)
          else
            ReleaseContentWC(Container, Transaction);
        end
        else begin
          if LockStatus = fflsWaiting then
            ReleaseContentW(Container, Transaction)
          else
            ReleaseContentWC(Container, Transaction);
          raise;
        end;
      end
    finally
      WaitEvent.Free;
      WaitEvent := nil;
    end;

  if Result = fflrsGranted then begin
    { Add the new lock to the transaction list. }
    FTransactions.BeginWrite;
    try
      TransContainer := TffTransContainer(Transaction.TransLockContainer);
      if not Assigned(TransContainer) then begin
        TransContainer := TffTransContainer.Create(Transaction);
        TransContainer.LockManager := Self;
        Transaction.TransLockContainer := TransContainer;
        FTransactions.Insert(TransContainer);
      end;
      TransContainer.AddContentLock(Container, ParentTable, LockType);
    finally
      FTransactions.EndWrite;
    end;
  end;

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
end;
{--------}
function TffLockManager.AcquireRecordLock(const ResourceID  : TffInt64;
                                          const FI          : PffFileInfo;
                                          const LockType    : TffSrLockType;
                                          const Conditional : Boolean;
                                          const Timeout     : TffWord32;
                                          const Transaction : TffSrTransaction;
                                          const DatabaseID  : TffDatabaseID;  {!!.10}
                                          const CursorID    : TffCursorID
                                         ): TffLockRequestStatus;
var
  TransContainer    : TffTransContainer;
  LockContainerList : TffThreadHash64;
  LockContainer     : TffLockContainer;
  LockStatus        : TffLockStatus;
  WaitEvent         : TffEvent;
  {$IFDEF LockLogging}
  IsCond            : string;
  StartTime         : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  WaitEvent := nil;
  Result := fflrsRejected;

  {$IFDEF LockLogging}
  if Transaction = nil then
    FEventLog.WriteStrings(['',
                            '========================================',
                            Format('AcquireRecordLock BEGIN - Time: %12D', [GetTickCount - StartTime]),
                            Format(csFI, [FI^.fiHandle]),
                            Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
                            Format(csTransaction, [0]),
                            Format(csCursorID, [CursorID])])
  else
    FEventLog.WriteStrings(['',
                            '========================================',
                            Format('AcquireRecordLock BEGIN - Time: %12D', [GetTickCount - StartTime]),
                            Format(csFI, [FI^.fiHandle]),
                            Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
                            Format(csTransaction, [Transaction.TransactionID]),
                            Format(csCursorID, [CursorID])]);
    {End !!.06}
  {$ENDIF}

  { Find the LockContainer List, create one if it does not exist. }
  LockContainerList := FI^.fiRecordLocks;
  if not Assigned(LockContainerList) then begin
    LockContainerList := TffThreadHash64.Create(ffc_Size257);
    LockContainerList.OnDisposeData := DisposeLockList;
    FI^.fiRecordLocks := LockContainerList;
  end;

  { We need write access to the container list before we can find/create
    the LockContainer. }
  LockContainerList.BeginWrite;

  { Has a lock already been created for this table resource? }
  try
    { Find the lock container, create one if it does not exist. }
    LockContainer := LockContainerList.Get(ResourceID);
    if not Assigned(LockContainer) then begin
      {$IFDEF UseLockContainerPool}
      LockContainer := FFLockContainerPool.Get;
      {$ELSE}
      LockContainer := TffLockContainer.Create;
      {$ENDIF}

      { Add the new lock container to the internal list }
      LockContainerList.Add(ResourceID, LockContainer);
    end;

    { We need write access to the container before we can queue the lock }
    LockContainer.BeginWrite;
  finally
    LockContainerList.EndWrite;
  end;

  try
    { We need to add the request to the queue. }
    LockStatus := LockContainer.RequestRecLock(LockType,               {!!.10}
                                               Conditional,
                                               Transaction,
                                               DatabaseID,             {!!.10}
                                               CursorID,
                                               WaitEvent);
  finally
    LockContainer.EndWrite;
  end;

  if LockStatus = fflsGranted then
    Result := fflrsGranted
  else if LockStatus = fflsRejected then
    Result := fflrsRejected
  else if (LockStatus = fflsWaiting) then
  { The lock is now in the queue. At this point we must pause the thread
    until the lock is granted. The WaitEvent local var is passed to the
    TffLockContainer.RequestLock method. This keeps us from creating
    an instance of TffEvent unnecessarily. The container is responsible
    for the create operation if it is necessary}
    try
      try
        {$IFDEF LockLogging}
        FEventLog.WriteStrings(['AcquireRecordLock: WaitEvent.WaitFor',
                                Format(csCursorID, [CursorID])]);
        {$ENDIF}
        WaitEvent.WaitFor(Timeout);
        Result := fflrsGranted;
      except
        on E: EffException do begin
          case E.ErrorCode of
            fferrReplyTimeout : Result := fflrsTimeout;
            fferrWaitFailed   : Result := fflrsRejected;
          end;
          {$IFDEF LockLogging}
          FEventLog.WriteStrings(['AcquireRecordLock: lock request timed out or rejected',
                                  Format(csCursorID, [CursorID])]);
          {$ENDIF}
          if LockStatus = fflsWaiting then
            ReleaseRecordW(ResourceID, FI, DatabaseID)                 {!!.10}
          else
            ReleaseRecordWC(ResourceID, FI, DatabaseID);               {!!.10}
        end
        else begin
          if LockStatus = fflsWaiting then
            ReleaseRecordW(ResourceID, FI, DatabaseID)                 {!!.10}
          else
            ReleaseRecordWC(ResourceID, FI, DatabaseID);               {!!.10}
          raise;
        end;
      end
    finally
      {$IFDEF LockLogging}
      FEventLog.WriteStrings(['AcquireRecordLock: WaitEvent.Free',
                              Format(csCursorID, [CursorID])]);
      {$ENDIF}
      WaitEvent.Free;
      WaitEvent := nil;
    end;

  if Result = fflrsGranted then begin
    { Is a transaction active? }
    if assigned(Transaction) then begin
      { Yes. Add the new lock to the transaction list. }
      FTransactions.BeginWrite;
      try
        TransContainer := TffTransContainer(Transaction.TransLockContainer);
        if not Assigned(TransContainer) then begin
          TransContainer := TffTransContainer.Create(Transaction);
          TransContainer.LockManager := Self;
          Transaction.TransLockContainer := TransContainer;
          FTransactions.Insert(TransContainer);
        end;
        TransContainer.AddRecordLock(FI, DatabaseID, ResourceID);      {!!.10}
      finally
        FTransactions.EndWrite;
      end;
    end;
  end;

  {$IFDEF LockLogging}
  if Conditional then
    IsCond := 'Yes'
  else
    IsCond := 'No';
{Begin !!.06}
  if Transaction = nil then
    FEventLog.WriteStrings(['',
                            '========================================',
                            Format('AcquireRecordLock END - Time: %12D', [GetTickCount - StartTime]),
                            Format(csFI, [FI^.fiHandle]),
                            Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
                            Format(csLockType, [FFMapLockToName(LockType)]),
                            Format(csConditional, [isCond]),           {!!.10}
                            Format(csTimeout, [Timeout]),
                            Format(csTransaction, [0]),
                            Format(csCursorID, [CursorID]),
                            Format(csLockRequestStatus, [FFMapRequestStatusToName(Result)])])
  else
    FEventLog.WriteStrings(['',
                            '========================================',
                            Format('AcquireRecordLock END - Time: %12D', [GetTickCount - StartTime]),
                            Format(csFI, [FI^.fiHandle]),
                            Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
                            Format(csLockType, [FFMapLockToName(LockType)]),
                            Format(csConditional, [isCond]),           {!!.10}
                            Format(csTimeout, [Timeout]),
                            Format(csTransaction, [Transaction.TransactionID]),
                            Format(csCursorID, [CursorID]),
                            Format(csLockRequestStatus, [FFMapRequestStatusToName(Result)])]);
    {End !!.06}
  {$ENDIF}
end;
{--------}
function TffLockManager.AcquireTableLock(const ResourceID    : TffWord32;
                                         const LockType      : TffSrLockType;
                                         const Conditional   : Boolean;
                                         const Timeout       : TffWord32;
                                         const CursorID      : TffCursorID) : TffLockRequestStatus;
var
  LockContainer   : TffLockContainer;
  LockStatus      : TffLockStatus;
  WaitEvent       : TffEvent;
  {$IFDEF LockLogging}
  IsCond          : string;
  StartTime       : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  WaitEvent := nil;
  Result := fflrsRejected;
  { Has a lock already been created for this table resource? }
  FTableLocks.BeginWrite;
  try
    { Find the lock container, create one if it does not exist. }
    LockContainer := FTableLocks.Get(ResourceID);
    if not Assigned(LockContainer) then begin
      {$IFDEF UseLockContainerPool}
      LockContainer := FFLockContainerPool.Get;
      {$ELSE}
      LockContainer := TffLockContainer.Create;
      {$ENDIF}

      { Add the new lock container to the internal list }
      FTableLocks.Add(ResourceID, LockContainer);
    end;

    { We need write access to the container before we can queue the lock }
    LockContainer.BeginWrite;
  finally
    FTableLocks.EndWrite;
  end;

  try
    { We need to add the request to the queue. }
    LockStatus := LockContainer.RequestLock(LockType,
                                            Conditional,
                                            nil,
                                            CursorID,
                                            WaitEvent);
  finally
    LockContainer.EndWrite;
  end;

  if (LockStatus = fflsWaiting) then
  { The lock is now in the queue. At this point we must pause the thread
    until the lock is granted. The WaitEvent local var is passed to the
    TffLockContainer.RequestLock method. This keeps us from creating
    an instance of TffEvent unnecessarily. The container is responsible
    for the create operation if it is necessary}
    try
      try
        WaitEvent.WaitFor(Timeout);
        Result := fflrsGranted;
      except
        on E: EffException do begin
          case E.ErrorCode of
            fferrReplyTimeout : Result := fflrsTimeout;
            fferrWaitFailed   : Result := fflrsRejected;
          end;
          if LockStatus = fflsWaiting then
            ReleaseTableW(ResourceID, CursorID)
          else
            ReleaseTableWC(ResourceID, CursorID);
        end
        else begin
          ReleaseTableLock(ResourceID, CursorID);
          raise;
        end;
      end;
    finally
      WaitEvent.Free;
      WaitEvent := nil;
    end
  else
    if LockStatus = fflsGranted then
      Result := fflrsGranted
    else
      Result := fflrsRejected;
  {$IFDEF LockLogging}
  if Conditional then
    isCond := 'Yes'
  else
    isCond := 'No';
  FEventLog.WriteStrings(['========================================',
                          Format('AcquireTableLock - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID, [ResourceID]),
                          Format(csLockType, [FFMapLockToName(LockType)]),
                          Format(csConditional, [isCond]),             {!!.10}
                          Format(csTimeout, [Timeout]),
                          Format(csCursorID, [CursorID]),
                          Format(csLockRequestStatus, [FFMapRequestStatusToName(Result)])]);
  {$ENDIF}
end;
{--------}
procedure TffLockManager.DisposeLockList(Sender: TffBaseHashTable;
                                         aData: Pointer);
var
  LockList : TffLockContainer;
  Index : Integer;
begin
  if Assigned(aData) then begin
    LockList := TffLockContainer(aData);

    { Free the items in the list. }
    for Index := Pred(LockList.Count) downto 0 do
      LockList.DeleteAt(Index);
    {$IFDEF UseLockContainerPool}
    FFLockContainerPool.Put(LockList);
    {$ELSE}
    LockList.Free;
    {$ENDIF}
  end;
end;
{--------}
procedure TffLockManager.DisposeRecordLockList(Sender : TffBaseHashTable;
                                               aData  : Pointer);
var
  LockList : TffThreadHash;
begin
  if Assigned(aData) then begin
    LockList := TffThreadHash(aData);
    { Free the items in the list. }
    LockList.Clear;
    LockList.Free;
  end;
end;
{--------}
procedure TffLockManager.GetWaitingRecordLocks(const ResourceID   : TffInt64;
                                               const FI           : PffFileInfo;
                                               const Transaction  : TffSrTransaction;
                                                 var WaitingLocks : TffPointerList);
var
  LockContainerList : TffThreadHash64;
  LockContainer : TffLockContainer;
  LockIdx : Integer;
  LockItem : TffLockListItem;
  {$IFDEF LockLogging}
  StartTime       : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  if not Assigned(LockContainerList) then Exit;

  LockContainerList.BeginRead;
  try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    if not Assigned(LockContainer) then Exit;

    LockContainer.BeginRead;
    try
      for LockIdx := 0 to Pred(LockContainer.Count) do begin
        LockItem := TffLockListItem(LockContainer.Items[LockIdx]);
        if LockItem.Status = fflsWaiting then
          WaitingLocks.Append(pointer(LockItem));
      end;
    finally
      LockContainer.EndRead;
    end;
  finally
    LockContainerList.EndRead;
  end;
  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('GetWaitingRecordLocks - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
                          Format(csFI, [FI^.fiHandle]),
                          Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}
end;
{--------}
function TffLockManager.IsRecordLocked(const aResourceID  : TffInt64;
                                       const aFI          : PffFileInfo): Boolean;
var
  LockContainerList : TffThreadHash64;
  LockContainer : TffLockContainer;
  {$IFDEF LockLogging}
  LockStatus      : string;
  StartTime       : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  Result := False;

  { Find the LockContainerList }
  LockContainerList := aFI^.fiRecordLocks;
  if not Assigned(LockContainerList) then Exit;

  LockContainerList.BeginRead;
  try
    { Find the lock container }
    LockContainer := LockContainerList.Get(AResourceID);
    if not Assigned(LockContainer) then Exit;
    Result := (LockContainer.Count > 0);
  finally
    LockContainerList.EndRead;
  end;

  {$IFDEF LockLogging}
  if not Result then
    LockStatus := 'Not ';
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('IsRecordLocked - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID64, [aResourceID.iLow, aResourceID.iHigh]),
                          Format(csFI, [aFI^.fiHandle]),
                          Format('  %sLocked', [LockStatus])]);
  {$ENDIF}
end;
{--------}
function TffLockManager.HasClientLock(const Container : TffLockContainer;
                                      const CursorID : TffCursorID) : boolean;
begin
  Container.BeginRead;
  try
    Result := (Container.fflIndexPrim(CursorID) <> -1);
  finally
    Container.EndRead;
  end;
end;
{--------}
function TffLockManager.IsContentLockedBy(const Container : TffLockContainer;
                                          const Transaction : TffSrTransaction) : boolean;
begin
  Container.BeginRead;
  try
    Result := (Container.fflIndexPrim(TffCursorID(Transaction)) <> -1);
  finally
    Container.EndRead;
  end;
end;
{--------}
function TffLockManager.IsTableLockedBy(const AResourceID : TffWord32;
                                        const aCursorID : TffCursorID;
                                        const ALockType   : TffSrLockType): Boolean;
var
  ItemIndex : Longint;
  LockContainer : TffLockContainer;
  {$IFDEF LockLogging}
  LockStatus      : string;
  StartTime       : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  Result := False;
  FTableLocks.BeginRead;
  try
    { Find the lock container }
    LockContainer := FTableLocks.Get(AResourceID);
    if not Assigned(LockContainer) then Exit;

    LockContainer.BeginRead;
    try
      with LockContainer do begin
        ItemIndex := fflIndexPrim(aCursorID);
        if ItemIndex <> -1 then
          with TffLockListItem(Items[ItemIndex]) do
            Result := ALockType = LockType;
      end;
    finally
      LockContainer.EndRead;
    end;

  finally
    FTableLocks.EndRead;
  end;
  {$IFDEF LockLogging}
  if not Result then
    LockStatus := 'Not ';
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('IsTableLockedBy - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID, [aResourceID]),
                          Format(csCursorID, [aCursorID]),
                          Format(csLockType, [FFMapLockToName(aLockType)]),
                          Format('  %sLocked', [LockStatus])]);
  {$ENDIF}
end;
{--------}
function TffLockManager.RecordLockGranted(const ResourceID : TffInt64;
                                          const FI         : PffFileInfo): TffSrLockType;
var
  LockContainerList : TffThreadHash64;
  LockContainer : TffLockContainer;
  {$IFDEF LockLogging}
  StartTime       : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  Result := ffsltNone;

  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  if not Assigned(LockContainerList) then Exit;

  LockContainerList.BeginRead;
  try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    if not Assigned(LockContainer) then Exit;

    LockContainer.BeginRead;
    try
      Result := LockContainer.SummaryMode;
    finally
      LockContainer.EndRead;
    end;

    { Remove the lock container if it is empty }
    if LockContainer.Count = 0 then
      LockContainerList.Remove(ResourceID);

  finally
    LockContainerList.EndRead;
  end;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('RecordLockGranted - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
                          Format(csFI, [FI^.fiHandle]),
                          Format(csLockType, [FFMapLockToName(Result)])]);
  {$ENDIF}
end;
{Begin !!.10}
{--------}
procedure TffLockManager.RelaxRecordLock(const ResourceID : TffInt64;
                                         const FI : PffFileInfo;
                                         const DatabaseID : TffDatabaseID);
var
  LockContainerList : TffThreadHash64;
  LockContainer : TffLockContainer;
  {$IFDEF LockLogging}
  StartTime : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  if not Assigned(LockContainerList) then Exit;

  LockContainerList.BeginWrite;
  try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    if not Assigned(LockContainer) then Exit;

    LockContainer.BeginWrite;
    try
      LockContainer.RelaxRecordLock(DatabaseID);
    finally
      LockContainer.EndWrite;
    end;

  finally
    LockContainerList.EndWrite;
  end;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('RelaxRecordLock - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
                          Format(csFI, [FI^.fiHandle]),
                          Format(csDatabaseID, [DatabaseID])]);
  {$ENDIF}
end;
{End !!.10}
{--------}
procedure TffLockManager.ReleaseClientLock(const Container : TffLockContainer;
                                           const CursorID : TffCursorID);
var
  RefCount  : Integer;
  {$IFDEF LockLogging}
  StartTime : DWORD;
  {$ENDIF}
begin

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
  try
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
  finally
    Container.EndWrite;
  end;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('ReleaseClientLock - Time: %12D', [GetTickCount - StartTime]),
                          Format(csLockContainer, [Longint(Container)]),
                          Format(csCursorID, [CursorID])]);
  {$ENDIF}
end;
{--------}
procedure TffLockManager.ReleaseClientLockAll(const Container : TffLockContainer;
                                              const CursorID : TffCursorID);
{$IFDEF LockLogging}
var
  StartTime : DWORD;
{$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}

  Container.BeginWrite;
  try
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
                            '========================================',
                            'ReleaseClientLockAll',
                            Format(csCursorID, [CursorID]),
                            Format(csLockContainer, [Longint(Container)]),
                            Format('# container items: %d',[Container.Count])]);
    {$ENDIF}
    Container.ReleaseCursorLockAll(CursorID);
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
                            '========================================',
                            'ReleaseClientLockAll - after purge',
                            Format(csCursorID, [CursorID]),
                            Format('# container items: %d',[Container.Count])]);
    {$ENDIF}
  finally
    Container.EndWrite;
  end;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('ReleaseClientLockAll - Time: %12D', [GetTickCount - StartTime]),
                          Format(csCursorID, [CursorID]),
                          Format(csLockContainer, [Longint(Container)])]);
  {$ENDIF}
end;
{--------}
procedure TffLockManager.ReleaseClientW(const Container : TffLockContainer;
                                        const CursorID : TffCursorID);
begin
  Assert(assigned(Container));
  Container.BeginWrite;
  try
    Container.ReleaseWaitingLock(CursorID);
  finally
    Container.EndWrite;
  end;
end;
{--------}
procedure TffLockManager.ReleaseClientWC(const Container : TffLockContainer;
                                         const CursorID : TffCursorID);
begin
  Assert(assigned(Container));
  Container.BeginWrite;
  try
    Container.ReleaseWaitingConversion(CursorID);
  finally
    Container.EndWrite;
  end;
end;
{--------}
procedure TffLockManager.ReleaseContentLock(const Container : TffLockContainer;
                                            const Transaction : TffSrTransaction);
var
  RefCount : Integer;
  {$IFDEF LockLogging}
  StartTime      : DWORD;
  {$ENDIF}
  TransContainer : TffTransContainer;
begin

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
  try
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
    if RefCount = 0 then
      { Is a transaction active? }
      if assigned(Transaction) then begin
        FTransactions.BeginWrite;
        try
          TransContainer := TffTransContainer(Transaction.TransLockContainer);
          if Assigned(TransContainer) then
            TransContainer.RemoveContentLock(Container);
        finally
          FTransactions.EndWrite;
        end;
      end;
  finally
    Container.EndWrite;
  end;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('ReleaseContentLock - Time: %12D', [GetTickCount - StartTime]),
                          Format(csLockContainer, [Longint(Container)]),
                          Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}
end;
{--------}
procedure TffLockManager.ReleaseContentLockAll(Container : TffLockContainer;
                                               Transaction : TffSrTransaction);
{$IFDEF LockLogging}
var
  StartTime : DWORD;
{$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}

  Container.BeginWrite;
  try
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
                            '========================================',
                            'ReleaseContentLockAll',
                            Format(csTransaction, [Longint(Transaction)]),
                            Format(csLockContainer, [Longint(Container)]),
                            Format('# container items: %d',[Container.Count])]);
    {$ENDIF}
    Container.ReleaseCursorLockAll(TffCursorID(Transaction));
    {$IFDEF LockLogging}
    FEventLog.WriteStrings(['',
                            '========================================',
                            'ReleaseContentLockAll - after purge',
                            Format('# container items: %d',[Container.Count])]);
    {$ENDIF}
  finally
    Container.EndWrite;
  end;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('ReleaseContentLockAll - Time: %12D', [GetTickCount - StartTime]),
                          Format(csTransaction, [Longint(Transaction)]),
                          Format(csLockContainer, [Longint(Container)])]);
  {$ENDIF}
end;
{--------}
procedure TffLockManager.ReleaseContentW(const Container : TffLockContainer;
                                         const Transaction : TffSrTransaction);
begin

  Assert(assigned(Container));
  Assert(assigned(Transaction));

  Container.BeginWrite;
  try
    Container.ReleaseWaitingLock(TffCursorID(Transaction));
  finally
    Container.EndWrite;
  end;
end;
{--------}
procedure TffLockManager.ReleaseContentWC(const Container : TffLockContainer;
                                          const Transaction : TffSrTransaction);
begin

  Assert(assigned(Container));
  Assert(assigned(Transaction));

  Container.BeginWrite;
  try
    Container.ReleaseWaitingConversion(TffCursorID(Transaction));
  finally
    Container.EndWrite;
  end;
end;
{--------}
procedure TffLockManager.ReleaseRecordLock(const ResourceID  : TffInt64;
                                           const FI          : PffFileInfo;
                                           const Transaction : TffSrTransaction;
                                           const DatabaseID  : TffDatabaseID); {!!.10}
var
  TransContainer : TffTransContainer;
  LockContainerList : TffThreadHash64;
  LockContainer : TffLockContainer;
  RefCount : Integer;
  {$IFDEF LockLogging}
  StartTime : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  if not Assigned(LockContainerList) then Exit;

  LockContainerList.BeginWrite;
  try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    if not Assigned(LockContainer) then Exit;

    LockContainer.BeginWrite;
    try
      LockContainer.ReleaseCursorLock(DatabaseID, RefCount);           {!!.10}
    finally
      LockContainer.EndWrite;
    end;

    { Remove the lock from the transaction list }
    if RefCount = 0 then
      { Is a transaction active? }
      if assigned(Transaction) then begin
        FTransactions.BeginWrite;
        try
          TransContainer := TffTransContainer(Transaction.TransLockContainer);
          if Assigned(TransContainer) then
            TransContainer.RemoveRecordLock(FI, ResourceID);
        finally
          FTransactions.EndWrite;
        end;
      end;

    { Remove the lock container if it is empty }
    if LockContainer.IsEmpty then
      LockContainerList.Remove(ResourceID);

  finally
    LockContainerList.EndWrite;
  end;

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('ReleaseRecordLock - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID64, [ResourceID.iLow, ResourceID.iHigh]),
                          Format(csFI, [FI^.fiHandle]),
                          Format(csTransaction, [Transaction.TransactionID]),
                          Format(csDatabaseID, [DatabaseID])]);
  {$ENDIF}
end;
{--------}
procedure TffLockManager.ReleaseRecordLockAll(const ResourceID  : TffInt64;
                                              const FI          : PffFileInfo;
                                              const Transaction : TffSrTransaction;
                                              const DatabaseID  : TffDatabaseID); {!!.10}
var
  LockContainerList : TffThreadHash64;
  LockContainer : TffLockContainer;
  {$IFDEF LockLogging}
  StartTime : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { We might need to release the lock container, and lock container list so
    we get request full access to the list }
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;

  { Find the lock container }
  LockContainer := LockContainerList.Get(ResourceID);
  if not Assigned(LockContainer) then Exit;

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
  try
    LockContainer.ReleaseCursorLockAll(DatabaseID);                    {!!.10}
  finally
    LockContainer.EndWrite;
  end;

  { Remove the lock container if it is empty }
  if LockContainer.Count = 0 then
    LockContainerList.Remove(ResourceID);

end;
{--------}
procedure TffLockManager.ReleaseRecordW(const ResourceID  : TffInt64;
                                        const FI          : PffFileInfo;
                                        const DatabaseID  : TffDatabaseID);  {!!.10}
var
  LockContainerList : TffThreadHash64;
  LockContainer : TffLockContainer;
begin
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  if not Assigned(LockContainerList) then Exit;

  LockContainerList.BeginWrite;
  try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    if not Assigned(LockContainer) then Exit;

    LockContainer.BeginWrite;
    try
      LockContainer.ReleaseWaitingLock(DatabaseID);                    {!!.10}
    finally
      LockContainer.EndWrite;
    end;

  finally
    LockContainerList.EndWrite;
  end;

end;
{--------}
procedure TffLockManager.ReleaseRecordWC(const ResourceID  : TffInt64;
                                         const FI          : PffFileInfo;
                                         const DatabaseID  : TffDatabaseID);  { !!.10}
var
  LockContainerList : TffThreadHash64;
  LockContainer : TffLockContainer;
begin
  { Find the LockContainerList }
  LockContainerList := FI^.fiRecordLocks;
  if not Assigned(LockContainerList) then Exit;

  LockContainerList.BeginWrite;
  try
    { Find the lock container }
    LockContainer := LockContainerList.Get(ResourceID);
    if not Assigned(LockContainer) then Exit;

    LockContainer.BeginWrite;
    try
      LockContainer.ReleaseWaitingConversion(DatabaseID);              {!!.10}
    finally
      LockContainer.EndWrite;
    end;

  finally
    LockContainerList.EndWrite;
  end;

end;
{--------}
procedure TffLockManager.ReleaseTableLock(const ResourceID    : TffWord32;
                                          const CursorID      : TffCursorID);

var
  LockContainer : TffLockContainer;
  RefCount : Integer;
  {$IFDEF LockLogging}
  StartTime : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { We might need to release the lock container, so we request full access
    to the list }
  FTableLocks.BeginWrite;
  try
    { Find the lock container }
    LockContainer := FTableLocks.Get(ResourceID);

    if not Assigned(LockContainer) then
      Exit;

    LockContainer.BeginWrite;
    try
      LockContainer.ReleaseCursorLock(CursorID, RefCount);
    finally
      LockContainer.EndWrite;
    end;

  finally
    FTableLocks.EndWrite;
  end;
  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('ReleaseTableLock - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID, [ResourceID]),
                          Format(csCursorID, [CursorID])]);
  {$ENDIF}
end;
{--------}
procedure TffLockManager.ReleaseTableLockAll(const aResourceID: Integer;
                                             const aCursorID: TffCursorID);
var
  LockContainer : TffLockContainer;
  {$IFDEF LockLogging}
  StartTime : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  { We might need to release the lock container, so we get request full access
    to the list }
  FTableLocks.BeginWrite;
  try
    { Find the lock container }
    LockContainer := FTableLocks.Get(AResourceID);
    if not Assigned(LockContainer) then Exit;

    LockContainer.BeginWrite;
    try
      LockContainer.ReleaseCursorLockAll(aCursorID);
    finally
      LockContainer.EndWrite;
    end;

    { Remove the lock container if it is empty }
    if LockContainer.IsEmpty then
      FTableLocks.Remove(AResourceID);

  finally
    FTableLocks.EndWrite;
  end;
  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('ReleaseTableLockAll - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID, [aResourceID]),
                          Format(csCursorID, [aCursorID])]);
  {$ENDIF}
end;
{--------}
procedure TffLockManager.ReleaseTableW(const ResourceID    : TffWord32;
                                       const CursorID      : TffCursorID);

var
  LockContainer : TffLockContainer;
begin
  { We might need to release the lock container, so we request full access
    to the list }
  FTableLocks.BeginWrite;
  try
    { Find the lock container }
    LockContainer := FTableLocks.Get(ResourceID);

    if not Assigned(LockContainer) then
      Exit;

    LockContainer.BeginWrite;
    try
      LockContainer.ReleaseWaitingLock(CursorID);
    finally
      LockContainer.EndWrite;
    end;

  finally
    FTableLocks.EndWrite;
  end;
end;
{--------}
procedure TffLockManager.ReleaseTableWC(const ResourceID    : TffWord32;
                                        const CursorID      : TffCursorID);

var
  LockContainer : TffLockContainer;
begin
  { We might need to release the lock container, so we request full access
    to the list }
  FTableLocks.BeginWrite;
  try
    { Find the lock container }
    LockContainer := FTableLocks.Get(ResourceID);

    if not Assigned(LockContainer) then
      Exit;

    LockContainer.BeginWrite;
    try
      LockContainer.ReleaseWaitingConversion(CursorID);
    finally
      LockContainer.EndWrite;
    end;

  finally
    FTableLocks.EndWrite;
  end;
end;
{--------}
procedure TffLockManager.ReleaseTransactionLocks(const Transaction : TffSrTransaction;
                                                 const RecordsOnly : boolean);
var
  FI : PffFileInfo;
  FileInx : Longint;
  FileItem : TffWord32ListItem;
  ResList : TffHash64;
  TransContainer : TffTransContainer;
  {$IFDEF LockLogging}
  StartTime : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  TransContainer := TffTransContainer(Transaction.TransLockContainer);

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          'ReleaseTransactionLocks - Start',
                          Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}

  if assigned(TransContainer) then begin
    { Release record locks first. }
    for FileInx := 0 to pred(TransContainer.FileCount) do begin
      FileItem := TransContainer.Files[FileInx];
      FI := PffFileInfo(FileItem.KeyAsInt);
      if assigned(FI^.fiRecordLocks) then begin
        FI^.fiRecordLocks.BeginWrite;
        try
          ResList := TffHash64(FileItem.ExtraData);
          ResList.Iterate(RelRecLockIterator, TffWord32(FileItem),
                          TffWord32(Transaction), 0);
          ResList.Free;
        finally
          FI^.fiRecordLocks.EndWrite;
        end;
      end;  { if have record locks }
      FileItem.ExtraData := nil;
    end;

    { Release content locks next. }
    if not RecordsOnly then
      for FileInx := 0 to pred(TransContainer.ContentCount) do
        ReleaseContentLockAll
          (TransContainer.ContentContainer[FileInx], Transaction);
  end;  { if have transaction container  }

  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('ReleaseTransactionLocks - Time: %12D', [GetTickCount - StartTime]),
                          Format(csTransaction, [Transaction.TransactionID])]);
  {$ENDIF}
end;
{--------}
procedure TffLockManager.RelRecLockIterator(aKey : TffInt64;
                                            aData : pointer;
                                      const cookie1, cookie2, cookie3 : TffWord32);
var
  FileItem : TffWord32ListItem;
  Transaction : TffSrTransaction;
begin
  { Assumptions:
    aKey    = record reference number (i.e., ID of locked resource)
    aData   = database ID
    Cookie1 = fileItem
    Cookie2 = transaction
    Cookie3 = nothing of value }
  FileItem := TffWord32ListItem(cookie1);
  Transaction := TffSrTransaction(cookie2);
  ReleaseRecordLockAll(aKey,
                       PffFileInfo(FileItem.KeyValue),
                       Transaction,
                       TffDatabaseID(aData));                          {!!.10}
end;
{--------}
function TffLockManager.TableLockGranted(const ResourceID: Integer): TffSrLockType;
var
  LockContainer : TffLockContainer;
  {$IFDEF LockLogging}
  StartTime : DWORD;
  {$ENDIF}
begin
  {$IFDEF LockLogging}
  StartTime := GetTickCount;
  {$ENDIF}
  FTableLocks.BeginRead;
  try
    { Find the lock container }
    LockContainer := FTableLocks.Get(ResourceID);
    if not Assigned(LockContainer) then begin
      Result := ffsltNone;
      Exit;
    end;

    Result := LockContainer.SummaryMode;
  finally
    FTableLocks.EndRead;
  end;
  {$IFDEF LockLogging}
  FEventLog.WriteStrings(['',
                          '========================================',
                          Format('TableLockGranted - Time: %12D', [GetTickCount - StartTime]),
                          Format(csResourceID, [ResourceID]),
                          Format(csLockType, [FFMapLockToName(Result)])]);
  {$ENDIF}
end;
{====================================================================}

{===TffLockContainer=================================================}
constructor TffLockContainer.Create;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  Sorted := True;
  FWaitQueue := TffLockQueue.Create;
  FWaitConversionQueue := TffLockQueue.Create;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffLockContainer.Destroy;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  FWaitQueue.Free;
  FWaitQueue := nil;

  FWaitConversionQueue.Free;
  FWaitConversionQueue := nil;

  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffLockContainer.AddLock(const Granted     : Boolean;
                                  const Conditional : Boolean;
                                        LockItem    : TffLockListItem
                                 ): TffLockStatus;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if Granted then
    { Normal Lock is granted, so add it to the list }
    if not LockItem.Conversion then begin
      Insert(LockItem);
      Result := fflsGranted;
    end else begin
     { Grant a conversion lock }
      with TffLockListItem(Items[fflIndexPrim(LockItem.PrimaryID)]) do {!!.10}
        LockType := LockItem.LockType;
      LockItem.Free;
      Result := fflsGranted;
      Exit;
    end
  else if (not Granted) and (not Conditional) then begin
    { A waiting lock is added to the list }
    if LockItem.Conversion then begin
      { Add the lock item to the conversion list }
      FWaitConversionQueue.Enqueue(LockItem);
      Result := fflsWaitingConv;
    end else begin
      { Append lock request to the queue }
      FWaitQueue.Enqueue(LockItem);
      Result := fflsWaiting;
    end;
  end else
    { Since a conditional lock could not be acquired instantly, the request
      is rejected }
    Result := fflsRejected;

  LockItem.Status := Result;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffLockContainer.LastLock: TffLockListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := TffLockListItem(Self.Items[Pred(Count)]);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffLockContainer.ProcessLockConversion(const aCursorID : TffCursorID;
                                                       aLockListItem : TffLockListItem);
var
  anItem : TffLockListItem;
  LockIndex : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  {$IFDEF LockLogging}
  Log.WriteStrings(['',
                    '========================================',
                    Format('LockContainer.ProcessLockConversion: CU %d', [aCursorID])]);
  {$ENDIF}
  { Retrieve the existing lock }
  LockIndex := fflIndexPrim(aCursorID);

  { Convert the granted lock & wake it up. }
  if LockIndex <> -1 then begin
    anItem := TffLockListItem(Items[LockIndex]);
    anItem.LockType := aLockListItem.LockType;
    anItem.Event.SignalEvent;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffLockContainer.ProcessQueue;
var
  anItem : TffLockListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  {$IFDEF LockLogging}
  Log.WriteStrings(['',
                    '========================================',
                    Format('LockContainer.ProcessQueue: %d', [Longint(self)])]);
  {$ENDIF}
  { Process all pending conversion requests first}
  while Assigned(TffLockListItem(FWaitConversionQueue.Peek)) do
    with TffLockListItem(FWaitConversionQueue.Peek) do
      if ffca_LockCompatibility[LockType, SummaryMode] then begin

  {$IFDEF LockLogging}
  Log.WriteStrings(['',
                    Format('LockContainer.SumMode: %s', [FFMapLockToName(SummaryMode)])]);
  {$ENDIF}

        { Compatible waiting lock found, we must now grant it }
        anItem := TffLockListItem(FWaitConversionQueue.Dequeue);
        anItem.Status := fflsGranted;

        { If a lock conversion request has been encountered, we
          must finalize the conversion operation }
        ProcessLockConversion(PrimaryID, anItem);                      {!!.10}

        anItem.Free;
      end else
        Exit;

  {$IFDEF LockLogging}
  Log.WriteStrings(['',
                    Format('Middle LockContainer.SumMode: %s', [FFMapLockToName(SummaryMode)])]);
  {$ENDIF}

  { Check for normal locks }
  while Assigned(TffLockListItem(FWaitQueue.Peek)) do
    with TffLockListItem(FWaitQueue.Peek) do
      if ffca_LockCompatibility[LockType, SummaryMode] then begin
  {$IFDEF LockLogging}
  Log.WriteStrings(['',
                    Format('LockContainer.SumMode: %s', [FFMapLockToName(SummaryMode)])]);
  {$ENDIF}
        { Compatible waiting lock found, we must now move it to the granted
          list & mark it as granted. }
        anItem := TffLockListItem(FWaitQueue.Dequeue);
        anItem.Status := fflsGranted;
        Insert(anItem);
        anItem.Event.SignalEvent;
      end else
        { incompatible waiting lock found }
        Exit;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffLockContainer.IsEmpty : boolean;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := (Count = 0) and
            (FWaitQueue.Count = 0) and
            (FWaitConversionQueue.Count = 0);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.10}
{--------}
procedure TffLockContainer.RelaxRecordLock(const aDatabaseID : TffCursorID);
var
  anItem : TffLockListItem;
  ItemIndex : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  {$IFDEF LockLogging}
  Log.WriteStrings(['',
                    '========================================',
                    Format('LockContainer.RelaxRecordLock: %d, DB %d',
                           [Longint(self), aDatabaseID])]);
  {$ENDIF}
  ItemIndex := fflIndexPrim(aDatabaseID);
  if ItemIndex <> -1 then begin
    anItem := TffLockListItem(Items[ItemIndex]);
    anItem.F2ndaryID := 0;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{End !!.10}
{--------}
procedure TffLockContainer.ReleaseWaitingConversion(const RequestorID : TffBaseID); {!!.10}
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  FWaitConversionQueue.Delete(RequestorID);                            {!!.10}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffLockContainer.ReleaseWaitingLock(const RequestorID : TffBaseID); {!!.10}
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  FWaitQueue.Delete(RequestorID);                                      {!!.10}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffLockContainer.ReleaseCursorLock(const aCursorID : TffCursorID;
                                               var aRefCount    : Integer);
var
  anItem : TffLockListItem;
  ItemIndex : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  {$IFDEF LockLogging}
  Log.WriteStrings(['',
                    '========================================',
                    Format('LockContainer.ReleaseCursorLock: %d, Cursor %d',
                           [Longint(self), aCursorID])]);
  {$ENDIF}
  ItemIndex := fflIndexPrim(aCursorID);
  if ItemIndex <> -1 then begin
    anItem := TffLockListItem(Items[ItemIndex]);
    if anItem.RefCount > 1 then begin
      anItem.RefCount := anItem.RefCount - 1;
      aRefCount := anItem.RefCount;
    end else begin
      aRefCount := 0;
      DeleteAt(ItemIndex);
      ProcessQueue;
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffLockContainer.ReleaseCursorLockAll(const aCursorID : TffCursorID);
var
  ItemIndex : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  {$IFDEF LockLogging}
  Log.WriteStrings(['',
                    '========================================',
                    Format('LockContainer.ReleaseCursorLockAll: %d, cursor %d',
                           [Longint(self), aCursorID])]);
  {$ENDIF}
  ItemIndex := fflIndexPrim(aCursorID);
  if ItemIndex <> -1 then
    with TffLockListItem(Items[ItemIndex]) do begin
      DeleteAt(ItemIndex);
      ProcessQueue;
    end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffLockContainer.RequestLock(const LockType    : TffSrLockType;
                                      const Conditional : Boolean;
                                      const Transaction : TffSrTransaction;
                                      const RequestorID : TffBaseID;   {!!.10}
                                        var WaitEvent   : TffEvent
                                     ) : TffLockStatus;
var
  CvtLockItem : TffLockListItem;
  CvtOnlyItem : boolean;
  ItemIndex   : Longint;
  LockItem    : TffLockListItem;
{$IFDEF LockLogging}
  TranID      : TffTransID;
{$ENDIF}
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  CvtOnlyItem := false;
  ItemIndex := fflIndexPrim(RequestorID);                              {!!.10}
  {$IFDEF LockLogging}
  if Transaction = nil then
    TranID := 0
  else
    TranID := Transaction.TransactionID;
  {$ENDIF}
  if ItemIndex <> -1 then begin
    { If a lock item already exists for this transaction, then we need to
      see if it is compatible }
    LockItem := TffLockListItem(Items[ItemIndex]);
    assert(LockItem.Status = fflsGranted);
    if LockItem.LockType >= LockType then begin
      { The lock is compatible, so we increment the lock's RefCount }
      {$IFDEF LockLogging}
      Log.WriteStringFmt('ReqLock.Compatible, TR %d, CU %d',           {!!.10}
                         [TranID, RequestorID]);                       {!!.10}
      {$ENDIF}
      LockItem.RefCount := LockItem.RefCount + 1;
      Result := fflsGranted;
      Exit;
    end else begin
      {$IFDEF LockLogging}
      Log.WriteStringFmt('ReqLock.Incompatible, TR %d, CU %d',         {!!.10}
                         [TranID, RequestorID]);                       {!!.10}
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
      CvtLockItem := TffLockListItem.Create(RequestorID);              {!!.10}
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
    end;
  end else begin
    { Create and Initialize the lock item }
      {$IFDEF LockLogging}
      Log.WriteStringFmt('ReqLock.CreateLockItem, TR %d, CU %d',
                         [TranID, RequestorID]);                       {!!.10}
      {$ENDIF}
    LockItem := TffLockListItem.Create(RequestorID);                   {!!.10}
    LockItem.LockType := LockType;
    LockItem.Transaction := Transaction;
    LockItem.RefCount := 1;
    LockItem.Conversion := False;
    LockItem.MaintainLinks := False;
  end;

  { If there are no items in the queue or we are the only item in the queue
    and we happen to be a conversion request then grant the lock. }
  if (Count = 0) or CvtOnlyItem then begin
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.OnlyItem, TR %d, CU %d',
                       [TranID, RequestorID]);                         {!!.10}
    {$ENDIF}
    Result := AddLock(True, Conditional, LockItem);
    Exit;
  end;

 { If the last lock is waiting, then make the new lock wait in line }
  if (FWaitQueue.Count > 0) then begin
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.MakeWait, TR %d, CU %d',
                       [TranID, RequestorID]);                         {!!.10}
    {$ENDIF}
    Result := AddLock(False, Conditional, LockItem)
  end
  else if ffca_LockCompatibility[LockType, SummaryMode] then begin
    { No locks are waiting, the summary mode is compatible, so add a granted
      lock. }
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.CompatibleWithSumMode, TR %d, CU %d',
                       [TranID, RequestorID]);                         {!!.10}
    {$ENDIF}
    Result := AddLock(True, Conditional, LockItem)
  end
  else begin
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.LastOption, TR %d, CU %d',
                       [TranID, RequestorID]);                         {!!.10}
    {$ENDIF}
    Result := AddLock(False, Conditional, LockItem);
  end;

  if Result in [fflsWaiting, fflsWaitingConv] then begin
    { We need to create the waitfor event }
    WaitEvent := TffEvent.Create;
    LockItem.Event := WaitEvent;
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.CreateWaitEvent, TR %d, CU %d',
                       [TranID, RequestorID]);                         {!!.10}
    {$ENDIF}
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{Begin !!.10}
{--------}
function TffLockContainer.RequestRecLock(const LockType    : TffSrLockType;
                                         const Conditional : Boolean;
                                         const Transaction : TffSrTransaction;
                                         const ReqPrimaryID,
                                               ReqSecondaryID : TffBaseID;
                                           var WaitEvent   : TffEvent
                                        ) : TffLockStatus;
var
  CvtLockItem : TffLockListItem;
  CvtOnlyItem : boolean;
  ItemIndex   : Longint;
  LockItem    : TffLockListItem;
{$IFDEF LockLogging}
  TranID      : TffTransID;
{$ENDIF}
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}
  CvtOnlyItem := false;
  { Look for an existing lock held by the database. }
  ItemIndex := fflIndexPrim(ReqPrimaryID);
  {$IFDEF LockLogging}
  if Transaction = nil then
    TranID := 0
  else
    TranID := Transaction.TransactionID;
  {$ENDIF}
  { Did we find an existing lock entry for the database? }
  if ItemIndex <> -1 then begin
    { Yes. If a lock item already exists then we need to determine if it was
      obtained by a different cursor. }
    LockItem := TffLockListItem(Items[ItemIndex]);
    assert(LockItem.Status = fflsGranted);
    if ((LockItem.SecondaryID = 0) or
        (LockItem.SecondaryID = ReqSecondaryID)) then begin
      { The lock is held by the same cursor or was held by another cursor but
        released after it was finished with the record. Next, determine if the
        existing lock and the requested lock are compatible. }
      if (LockItem.LockType >= LockType) then begin
        { The lock is compatible, so we increment the lock's RefCount }
        if LockItem.SecondaryID = 0 then begin
          {$IFDEF LockLogging}
          Log.WriteStringFmt('ReqLock.Compatible, TR %d, DB %d, CU %d, ' +
                             'obtaining exclusive access',
                             [TranID, ReqPrimaryID, ReqSecondaryID]);
          {$ENDIF}
          LockItem.F2ndaryID := ReqSecondaryID;
        end else
        begin
          {$IFDEF LockLogging}
          Log.WriteStringFmt('ReqLock.Compatible, TR %d, DB %d, CU %d',
                             [TranID, ReqPrimaryID, ReqSecondaryID]);
          {$ENDIF}
        end;
        LockItem.RefCount := LockItem.RefCount + 1;
        Result := fflsGranted;
        Exit;
      end else begin
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
        CvtLockItem := TffLockListItem.Create(ReqPrimaryID);
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
      end; { if }
    end
    else begin
      { The existing lock is being exclusively held by another cursor in the
        same database. This situation represents a coding error. }
      Result := fflsRejected;
      Exit;
    end;
  end else begin
    { Create and Initialize the lock item }
      {$IFDEF LockLogging}
      Log.WriteStringFmt('ReqLock.CreateLockItem, TR %d, DB %d, CU %d',
                         [TranID, ReqPrimaryID, ReqSecondaryID]);
      {$ENDIF}
    LockItem := TffLockListItem.Create(ReqPrimaryID);
    LockItem.F2ndaryID := ReqSecondaryID;
    LockItem.LockType := LockType;
    LockItem.Transaction := Transaction;
    LockItem.RefCount := 1;
    LockItem.Conversion := False;
    LockItem.MaintainLinks := False;
  end;

  { If there are no items in the queue or we are the only item in the queue
    and we happen to be a conversion request then grant the lock. }
  if (Count = 0) or CvtOnlyItem then begin
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.OnlyItem, TR %d, DB %d, CU %d',
                       [TranID, ReqPrimaryID, ReqSecondaryID]);
    {$ENDIF}
    Result := AddLock(True, Conditional, LockItem);
    Exit;
  end;

 { If the last lock is waiting, then make the new lock wait in line }
  if (FWaitQueue.Count > 0) then begin
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.MakeWait, TR %d, DB %d, CU %d',
                       [TranID, ReqPrimaryID, ReqSecondaryID]);
    {$ENDIF}
    Result := AddLock(False, Conditional, LockItem)
  end
  else if ffca_LockCompatibility[LockType, SummaryMode] then begin
    { No locks are waiting, the summary mode is compatible, so add a granted
      lock. }
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.CompatibleWithSumMode, TR %d, DB %d, CU %d',
                       [TranID, ReqPrimaryID, ReqSecondaryID]);
    {$ENDIF}
    Result := AddLock(True, Conditional, LockItem)
  end
  else begin
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.LastOption, TR %d, DB %d, CU %d',
                       [TranID, ReqPrimaryID, ReqSecondaryID]);
    {$ENDIF}
    Result := AddLock(False, Conditional, LockItem);
  end;

  if Result in [fflsWaiting, fflsWaitingConv] then begin
    { We need to create the waitfor event }
    WaitEvent := TffEvent.Create;
    LockItem.Event := WaitEvent;
    {$IFDEF LockLogging}
    Log.WriteStringFmt('ReqLock.CreateWaitEvent, TR %d, DB %d, CU %d',
                       [TranID, ReqPrimaryID, ReqSecondaryID]);
    {$ENDIF}
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}
end;
{End !!.10}
{Begin !!.03}
{--------}
function TffLockContainer.SimpleDeadlock : Boolean;
var
  anInx, anInx2 : Longint;
  LockItem, LockItem2 : TffLockListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Assumption: The transaction invoking this method has not submitted its
    request for an Exclusive lock. }
  { Scan through the wait queues for a transaction that is requesting an
    Exclusive lock. If found then see if it has been granted a share lock.
    If found then return True else return False. }
  Result := False;
  {Check the wait queue. }
  for anInx := 0 to Pred(FWaitQueue.Count) do begin
    LockItem := TffLockListItem(FWaitQueue.Items[anInx]);
    if LockItem.LockType = ffsltExclusive then
      { Found a waiting request for Exclusive lock. Already granted a
        share lock? }
      for anInx2 := 0 to Pred(Count) do begin
        LockItem2 := TffLockListItem(Items[anInx]);
        if (LockItem2 <> nil) and                                      {!!.06}
           (LockItem2.Transaction = LockItem.Transaction) then begin   {!!.06}
          Result := True;
          Exit;
        end;
      end;
  end;

  {Check the wait conversion queue. }
  for anInx := 0 to Pred(FWaitConversionQueue.Count) do begin
    LockItem := TffLockListItem(FWaitConversionQueue.Items[anInx]);
    if LockItem.LockType = ffsltExclusive then
      { Found a waiting request for Exclusive lock. Already granted a
        share lock? }
      for anInx2 := 0 to Pred(Count) do begin
        LockItem2 := TffLockListItem(Items[anInx]);
        if LockItem2.Transaction = LockItem.Transaction then begin
          Result := True;
          break;
        end;
      end;
  end;  { for }
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{End !!.03}
{--------}
function TffLockContainer.SummaryMode : TffSrLockType;
var
  Idx : Integer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := ffsltNone;
  {$IFDEF LockLogging}
  Log.WriteStringFmt('SumMode:Lock container Count: %d', [Count]);
  {$ENDIF}
  for Idx := 0 to Pred(Count) do
    with TffLockListItem(Items[Idx]) do begin
      {$IFDEF LockLogging}
       Log.WriteStringFmt('SumMode:Item %d, lock type %s, status %d (0=rej, 1=grant, 2=wait)',
                          [Idx, FFMapLockToName(LockType),
                           ord(Status)]);
      {$ENDIF}
      if (LockType > Result) then
        Result := LockType;
    end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}

{$IFDEF UseLockContainerPool}
{===TffLockContainerPool=============================================}
constructor TffLockContainerPool.Create(const InitialCount, RetainCount : Integer);
var
  aLockContainer : TffLockContainer;
  Index : integer;
begin
  inherited Create;
  FList := TffPointerList.Create;
  FRetainCount := RetainCount;
  FPadLock := TffPadlock.Create;

  { Create the initial set of LockContainers. }
  for Index := 1 to InitialCount do begin
    aLockContainer := TffLockContainer.Create;
    FList.Append(aLockContainer);
  end;
end;
{--------}
destructor TffLockContainerPool.Destroy;
var
  Index : Longint;
begin
  { Explicitly free the lock containers.  They are not freed
    by FList.Free. }
  for Index := pred(FList.Count) downto 0 do
    TffLockContainer(FList[Index]).Free;
  FList.Free;
  FPadLock.Free;
  inherited Destroy;
end;
{Begin !!.01}
{--------}
procedure TffLockContainerPool.Flush;
var
  aLockContainer : TffLockContainer;
  anInx : Longint;
begin
  FPadLock.Lock;
  try
    if FList.Count > FRetainCount then
      for anInx := pred(FList.Count) downto FRetainCount do begin
        aLockContainer := FList.Pointers[anInx];
        FList.RemoveAt(anInx);
        aLockContainer.Free;
      end;
  finally
    FPadLock.Unlock;
  end;
end;
{End !!.01}
{--------}
function TffLockContainerPool.Get : TffLockContainer;
var
  aCount : Longint;
begin
  FPadLock.Lock;
  try
    if FList.IsEmpty then
      Result := TffLockContainer.Create
    else begin
      { Get the last item in the list.  This speeds up the RemoveAt
        operation incredibly since it won't have to shift any bytes in the
        list. }
      aCount := Pred(FList.Count);
      Result := FList.Pointers[aCount];
      FList.RemoveAt(aCount);
    end;
  finally
    FPadLock.Unlock;
  end;
end;
{--------}
procedure TffLockContainerPool.Put(const aLockContainer : TffLockContainer);
begin
  FPadLock.Lock;
  try
    FList.Append(aLockContainer);
  finally
    FPadLock.Unlock;
  end;
end;
{====================================================================}
{$ENDIF}

{===Utility routines=================================================}
function FFMapLockToName(aLockType : TffSrLockType) : string;
begin
  case aLockType of
    ffsltNone      : Result := ffcLockNone;
    ffsltIntentS   : Result := ffcLockIntentS;
    ffsltIntentX   : Result := ffcLockIntentX;
    ffsltShare     : Result := ffcLockShare;
    ffsltSIX       : Result := ffcLockSIX;
    ffsltUpdate    : Result := ffcLockUpdate;
    ffsltExclusive : Result := ffcLockExclusive;
  end;  { case }
end;
{$IFDEF LockLogging}
{--------}
function FFMapRequestStatusToName(aStatus : TffLockRequestStatus) : string;
begin
  case aStatus of
    fflrsGranted  : Result := ffcRStatusGranted;
    fflrsTimeout  : Result := ffcRStatusTimeout;
    fflrsRejected : Result := ffcRStatusRejected;
  end;
end;
{$ENDIF}
{====================================================================}

{===TffLockListItem==================================================}
constructor TffLockListItem.Create(const aKey: TffBaseID);             {!!.10}
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  FPrimaryID := aKey;                                                  {!!.10}
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffLockListItem.Compare(aKey: pointer): integer;
begin
  Result := FFCmpI32(PffWord32(aKey)^, FPrimaryID);                    {!!.10}
end;
{--------}
function TffLockListItem.Key : pointer;
begin
  Result := @FPrimaryID;                                               {!!.10}
end;
{====================================================================}

{===TffLockQueue=====================================================}
procedure TffLockQueue.EnqueuePriority(anItem: TffListItem);
var
  NewItem : TffListItem;
  OldItem : TffListItem;
  Idx     : Integer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if GetCount = 0 then
    Enqueue(anItem)
  else begin
    { insert the new item at the beginning of the queue, and
      adjust accordingly }
    NewItem := anItem;
    for Idx := 0 to Pred(ffqList.Count) do begin
      OldItem := ffqList[Idx];
      ffqList[Idx] := NewItem;
      NewItem := OldItem;
    end;
    Enqueue(NewItem);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffLockQueue.Peek: TffListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := nil;
  if GetCount > 0 then
    Result := ffqList[0];
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}

{===TffTransContainer================================================}
constructor TffTransContainer.Create(const aKey: TffSrTransaction);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  inherited Create;
  Transaction := TffSrTransaction(aKey);

  FContentLocks := TffList.Create;
  FRecordLocks := TffList.Create;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
destructor TffTransContainer.Destroy;
var
  Inx : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}

  FContentLocks.Free;
    { Note: We must *NOT* free the TffSrTable referenced by the ExtraData
      property of each item. }
  FContentLocks := nil;

  for Inx := pred(FRecordLocks.Count) downto 0 do
    TffThreadList(TffWord32ListItem(FRecordLocks[Inx]).ExtraData).Free;
  FRecordLocks.Free;
  FRecordLocks := nil;

  Transaction.TransLockContainer := nil;

  inherited Destroy;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffTransContainer.AddContentLock(Container : TffLockContainer;
                                           ParentTable : TffObject;
                                           LockType : TffSrLockType);
var
  anItem : TffWord32ListItem;
  anIndx : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  anIndx := FContentLocks.Index(TffWord32(Container));
  { Have we referenced this lock container before? }
  if anIndx = -1 then begin
    { No.  Create a reference to this lock container. }
    anItem := TffWord32ListItem.Create(TffWord32(Container));
    anItem.ExtraData := ParentTable;
    anItem.ExtraData2 := ord(LockType);
      { Note: The table referenced by ExtraData must *NOT* be freed. }
    FContentLocks.Insert(anItem);
  end
  else begin
    { Yes. Update the lock type. }
    anItem := TffWord32ListItem(FcontentLocks[anIndx]);
    anItem.ExtraDAta2 := ord(LockType);
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffTransContainer.AddRecordLock(const FI         : PffFileInfo;
                                          const CursorID   : TffCursorID;
                                          const ResourceID : TffInt64);
var
  FileItem : TffWord32ListItem;
  FileIdx  : Longint;
  ResList : TffHash64;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  FileIdx := FRecordLocks.Index(TffWord32(FI));
  if FileIdx = -1 then begin
    { Create the file item, and resource list }
    FileItem := TffWord32ListItem.Create(TffWord32(FI));
    { Add the file item to the list }
    FRecordLocks.Insert(FileItem);
  end
  else
    { Retrieve the information from the list }
    FileItem := TffWord32ListItem(FRecordLocks.Items[FileIdx]);

  ResList := TffHash64(FileItem.ExtraData);
  if not assigned(ResList) then begin
    ResList := TffHash64.Create(ffc_Size521);
    ResList.CanShrink := False;
    FileItem.ExtraData := ResList;
  end;

  ResList.Add(ResourceID, pointer(CursorID));
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffTransContainer.Compare(aKey: pointer): integer;
begin
  Result := FFCmpI32(PffLongint(aKey)^, Longint(FTransaction));
end;
{--------}
function TffTransContainer.Key: pointer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := @FTransaction;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffTransContainer.RemoveContentLock(Container : TffLockContainer);
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { If this container is not present then this should fall through without
    doing anything.  No exception should be raised. }
  FContentLocks.Delete(TffWord32(Container));
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
procedure TffTransContainer.RemoveRecordLock(const FI         : PffFileInfo;
                                             const ResourceID : TffInt64);
var
  FileItem : TffWord32ListItem;
  FileIdx  : Longint;
  ResList  : TffHash64;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  FileIdx := FRecordLocks.Index(TffWord32(FI));
  if FileIdx > -1 then begin
    FileItem := TffWord32ListItem(FRecordLocks.Items[FileIdx]);
    ResList := TffHash64(FileItem.ExtraData);

    ResList.Remove(ResourceID);

    if ResList.Count = 0 then begin
      FRecordLocks.Delete(TffWord32(FI));
      ResList.Free;
    end;
  end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffTransContainer.TableContentLockType(Container : TffLockContainer) : TffSrLockType;
var
  aInx : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  { Assumptions: FContentLocks is assigned. }
  with Container.BeginRead do
    try
      aInx := FContentLocks.Index(TffWord32(Container));
      { Does the transaction have a content lock on this table? }
      if aInx = -1 then
        { No. }
        Result := ffsltNone
      else
        Result := TffSrLockType(TffWord32ListItem(FContentLocks[aInx]).ExtraData2);
    finally
      EndRead;
    end;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffTransContainer.tcGetContentCount : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if assigned(FContentLocks) then
    Result := FContentLocks.Count
  else
    Result := 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffTransContainer.tcGetContentContainer(const aInx : Longint) : TffLockContainer;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  Result := TffLockContainer(TffWord32ListItem(FContentLocks[aInx]).KeyAsInt);
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffTransContainer.tcGetContentLockType(const aInx : Longint) : TffSrLockType;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if assigned(FContentLocks) then
    Result := TffSrLockType(TffWord32ListItem(FContentLocks[aInx]).ExtraData2)
  else
    Result := ffsltNone;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffTransContainer.tcGetContentTable(const aInx : Longint) : TffObject;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if assigned(FContentLocks) then
    Result := TffObject(TffWord32ListItem(FContentLocks[aInx]).ExtraData)
  else
    Result := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffTransContainer.tcGetFileCount : Longint;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if assigned(FRecordLocks) then
    Result := FRecordLocks.Count
  else
    Result := 0;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{--------}
function TffTransContainer.tcGetFiles(const aInx : Longint) : TffWord32ListItem;
begin
  {$IFDEF FF_DEBUG_THREADS}ThreadEnter; try{$ENDIF}                    {!!.03}
  if assigned(FRecordlocks) then
    Result := TffWord32ListItem(FRecordLocks[aInx])
  else
    Result := nil;
  {$IFDEF FF_DEBUG_THREADS}finally ThreadExit; end;{$ENDIF}            {!!.03}
end;
{====================================================================}

{$IFDEF UseLockContainerPool}
{===Initialization/Finalization======================================}
procedure FinalizeUnit;
begin
  FFLockContainerPool.Free;
end;
{--------}
procedure InitializeUnit;
begin
  FFLockContainerPool := TFFLockContainerPool.Create(250,1000);
end;

initialization
  InitializeUnit;

finalization
  FinalizeUnit;
{$ENDIF}

end.

