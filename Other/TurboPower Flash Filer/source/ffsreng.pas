{Notes:
   1. The perform dynamic link call has been commented out in the
      server engine create.

   2. Server-side objects are freed when a client requests the object be closed
      (e.g., SessionRemove) & all of its dependent objects report they be
      closed.  For example, a TffSrDatabase can be closed only if no other
      thread is using the TffSrDatabase and its associated cursors report they
      are inactive.

      If a server-side object cannot be freed when the close request is received
      from the client then the server's garbage collection thread will
      eventually free the object.

   3. When adding new TffServerEngine methods, please follow these guidelines:

      a. All steps should be wrapped with a Try..Except block.  At a minimum,
         the Try..Except block must do the following:

           try
             ...
           except
             on E : Exception do begin
               Result := ConvertServerException(E, btEngine.EventLog);
             end;
         end;

         This ensures the client is returned an error code that it
         understands.

      b. If you call any of the CheckxxxIDAndGet methods, the remaining
         code should be wrapped with a try..finally block.  The Finally
         section should call "xxxx.Deactivate".  Why?  Because the
         CheckxxxxIDAndGet methods mark the relevant object as Active to make
         sure it is not freed by another thread.  Once the operation has
         completed, the object must be marked as Inactive so that it may
         be closed and freed at a later time.

}

{*********************************************************}
{* FlashFiler: Server Engine class                       *}
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

{$I FFDEFINE.INC}

{ Enable the following define to debug RAM pages. }
{.$DEFINE RAMPageCheck}

{ Enable the following to debug the deleted record count. }
{.$DEFINE DebugDelCount}

{ Diasable the following to retrieve files using DatabaseTableList that
  are not FlashFiler 2 Tables. }
{$DEFINE OnlyRetrieveTables}                                           {!!.01}

unit ffsreng;

interface

uses
  Windows,
  SysUtils,
  Classes,
  Forms,
  FFStDate,
  FFConst,
  FFLLBase,
  FFLLEng,
  FFLLDict,
  FFLLThrd,
  FFSrMgr,
  FFLLExcp,
  FFLLLog,
  FFLLProt,
  FFLLTemp,
  FFLLUNC,
  FFHash,
  FFNetMsg,
  FFSrBase,
  FFFile,
  FFSqlBas,
  FFSrIntf,
  FFSrBDE,
  FFSrCfg,
  FFSrFMap,
  FFSrIntm,
  FFSrStat,
  FFSrCvEx,
  FFSrFold,
  FFSrIxhl,
  FFSrLock,
  FFSrTran,
  FFSrFltr,
  FFConvFF,
  FFTbBase,
  FFTbData,
  FFTbBLOB,
  FFTbDict,
  FFTbIndx;

{===Read/Write alias data from table=================================}
const
  ffc_SavPrefix             = 'SAV';
  ffc_StdPrefix             = 'FFS';
  ffc_TmpPrefix             = 'XXS';

  ffc_AliasSuffix           = 'ALIAS';
  ffc_IndexSuffix           = 'INDEX';
  ffc_InfoSuffix            = 'INFO';
  ffc_UserSuffix            = 'USER';

  ffc_AliasTableName        = 'FFSALIAS';
  ffc_SavedAliasTableName   = 'SAVALIAS';
  ffc_TempAliasTableName    = 'XXSALIAS';

  ffc_IndexTableName        = 'FFSINDEX';
  ffc_SavedIndexTableName   = 'SAVINDEX';
  ffc_TempIndexTableName    = 'XXSINDEX';

  ffc_GenInfoTableName      = 'FFSINFO';
  ffc_SavedGenInfoTableName = 'SAVINFO';
  ffc_TempGenInfoTableName  = 'XXSINFO';

  ffc_UserTableName         = 'FFSUSER';
  ffc_SavedUserTableName    = 'SAVUSER';
  ffc_TempUserTableName     = 'XXSUSER';

  ffc_AliasScript           = 'FFAlias.sc$';

  ffc_ClientShutdownTime : TffWord32 = 10000;                          {!!.05}

  ffc_StartTranWithDelay : DWORD = 10;                                 {!!.10}
    { Used with TransactionStartWith. If a lock cannot be immediately obtained
      then the operation will be retried every ffc_StartTranWithDelay
      milliseconds. }

type
  TffCursorPosition = (         {Positions of a cursor in an index}
                 cpUnknown,     {..unknown: must be resolved asap}
                 cpBOF,         {..prior to first record}
                 cpEOF,         {..after last record}
                 cpOnCrack,     {..in between two records}
                 cpOnRecord);   {..on a record somewhere}

  TffRecOp = (                  {Record update operations}
              roInsert,         {..insertion}
              roDelete,         {..deletion}
              roModify);        {..modification}

type
  PffSrBookmark = ^TffSrBookmark;
  TffSrBookmark = packed record
    sbHash     : Longint;               {validity check}
    sbIndexID  : Longint;
    sbPos      : TffCursorPosition;
    sbKeyValid : boolean;
    sbFill1    : array [0..1] of byte;  {to DWORD align}
    sbRefNr    : TffInt64;
    sbKeyLen   : Longint;
    sbKey      : array [0..1] of byte;
  end;

type
  TffServerEngine = class;  {forward declaration}
  TffSrTableClass = class of TffSrBaseTable;  {forward declaration}
  TffSrBaseTable = class;   {forward declaration}
  TffSrDatabase = class;    {forward declaration}
  TffSrSession = class;     {forward declaration}
  TffSrClient = class;      {forward declaration}
  TffSrStmtList = class;    {forward declaration}                      {!!.10}

  { This type identifies the state of a TffServerObject.  Given the
    multi-threaded nature of the server engine, it is possible for thread A
    to be using an object while thread B processes a command that would result
    in the closing and freeing of the object.  For example, in thread A a
    cursor is waiting to obtain an exclusive page lock.  While the cursor
    is waiting, the client times out and issues a CloseCursor command to
    the server.  Thread B processes the CloseCursor command.  Thread B
    must see that the cursor is active and thread B must not free the cursor.
    Doing so would cause an access violation as soon as thread A tries to
    use the cursor once more. }
  TffServerObjectState = (ffosInactive, ffosActive, ffosClosePending,
                          ffosClosing);
    { ffosInactive - The object is not being used by a thread.
      ffosActive   - The object is being used by a thread.
      ffosClosePending - Thread A is using the object but thread B wants
        to free the object.  Thread A is responsible for freeing the object
        once it has finished its operation.
      ffosClosing - The object is being freed by a thread. }

  { Contains the essential properties and methods for a server object (e.g.,
    client, session, database, cursor).  Before a thread can use a server object
    it must call the Activate method.  If the object can be used then the
    Activate method returns True.

    When a thread has finished using a server object, it must call the
    Deactivate method.

    When a thread wants to close and free an object, it must call the
    Close method.  If the Close method returns True then the thread must
    call TffServerObject.Free. }
  TffServerObject = class(TffSelfListItem)
  protected
    soClient  : TffSrClient;                                           {!!.10}
      { This is a reference to the server object's parent TffSrClient.
        It is instantiated for TffSrDatabase, TffSrBaseTable,
        and TffSrBaseCursor. }
    soLock    : TffPadlock;
      { Padlock used to prevent re-entrancy on a per-client basis.
        This lock is instantiated only for TffServerObjects of type
        TffSrClient. }
    soState   : TffServerObjectState;
    soTimeout : Longint;
  public
    constructor Create(const aTimeout : Longint);
    destructor Destroy; override;

    function Activate : boolean;
      { This method must be called before a thread can use a server object.
        If State is ffosInactive then sets State to ffosActive and returns
        True.  Otherwise returns False. }

    function CanClose(const Mark : boolean) : boolean; virtual;
      { When a server object is to be freed, call this method.  If the
        object can be freed this method returns True otherwise it returns
        False.  If the Mark parameter is True then the object's state is
        set to ffosClosing. }

    procedure Deactivate;
      { When a thread has finished using a server object, it must call this
        method.
        If State is ffosShutdownPending then the object frees itself.
        If State is ffosActive then switches to ffosInactive.
        If State is ffosShuttingDown then does nothing with the assumption
        that another thread will finish the object's shutdown. }

    procedure ForceClose; virtual;
      { Sets the client's state to ffosClosing so that it will free itself
        when the server next requests the client to be removed. }

    procedure RequestClose; virtual;                                   {!!.03}
      { If an object cannot be closed (i.e., CanClose returns False) then
        call this method to submit a request to close the object. }

    function ShouldClose : boolean; virtual;
      { When a server object is ready to be freed (i.e., State = ffosClosing),
        this method returns True. }

    { Properties }

    property Client : TffSrClient read soClient;                       {!!.10}
      { The object's parent client object. }

    property State : TffServerObjectState read soState write soState;
      { The current state of the object. }

    property Timeout : Longint read soTimeout write soTimeout;
      { The object's timeout value. }
  end;

  { This is the base class for lists of TffServerObjects. }
  TffServerObjectList = class(TffObject)
  protected  {private}
    solList : TffThreadList;
  protected
  public

    constructor Create; virtual;                                      {!!.01}

    destructor Destroy; override;

    procedure BeginRead;
      { A thread must call this method to gain read access to the list. }

    procedure BeginWrite;
      { A thread must call this method to gain write access to the list. }

    function CanClose(const Mark : boolean) : boolean; virtual;
      { Used to determine if all the server objects in the list can be
        closed.  Returns True if all can be closed otherwise returns False. }

    procedure EndRead;
      { A thread must call this method when it no longer needs read access
        to the list.  If it does not call this method, all writers will
        be perpetually blocked. }

    procedure EndWrite;
      { A thread must call this method when it no longer needs write access
        to the list.  If it does not call this method, all readers and writers
        will be perpetualy blocked. }

    procedure ForceClose; virtual;
      { Use this method to force all objects within the list to set themselves
        to a ffosClosing state. }

{Begin !!.06}
    function HasClosableState(const Mark : Boolean) : boolean;
      { Use this method to determine if objects have a closable state. Ignores
        all other facets of the object. If the Mark parameter is True and all
        objects in the list can be closed then sets all objects with state
        ffosInactive to ffosClosing. }
{End !!.06}

    procedure RemoveUnused; virtual;
      { Use this method to free objects that could not be freed at the time
        they were closed. }

{Begin !!.03}
    procedure RequestClose; virtual;
      { Use this method to request a close on all objects contained in the
        list. }
{End !!.03}

    function ShouldClose : boolean; virtual;
      { Use this method to determine if all the objects in the list should
        be closed. }

  end;

  TffSrCursorInfo = packed record
    Deleted : boolean;
      { If true then the record referenced by this information has been
        deleted. }
    KeyPath : TffKeyPath;
      {This is a trail into the current index that leads us to a
       specific record, crack between two records, EOF, or BOF}
    KeyValid : boolean;
      {This variable is set to True when we position to the
       next or previous record, reposition to an existing record,
       retrieve a record for a key, or position to a bookmark that is on a
       valid record.

       When this variable is True, we can rely upon the key stored in
       variable bcCurKey.

       This variable is set to False when we insert a record, modify a
       record, or otherwise need to force a recalculation of the key
       path to a record (e.g., TffSrCursor.SetToBegin,
       TffSrCursor.SwitchToIndex). }
    Pos : TffCursorPosition;
      { This tells us whether the cursor is on a specific record, at BOF,
        at EOF, or on a crack between two records. }
    RefNr : TffInt64;
      { Reference number of the current record.  This is its physical position
        within the file.  For example, if RefNr = 128,556 then the record
        starts at position 128,556 within the data file. }
  end;

  TffContentLockMode = (ffclmCommit, ffclmRead, ffclmWrite);
    { Used by cursor to indicate what type of content lock is needed. }

  TffSrBaseCursor = class;  {forward declaration}
  TffSrCursorClass = class of TffSrBaseCursor;  {forward declaration}  {!!.06}

  TffSrCopyRecordsProc = procedure(aSrcCursor : TffSrBaseCursor;
                                   aSrcRecord : PffByteArray;
                                   aCookie1, aCookie2 : Longint;
                               var include : boolean) of object;
    { Defines the event handler for the CopyRecords method.
      SrcCursor is the cursor from which the record is being copied.
      aSrcRecord is the record to be copied.
      Set include to True if the record is to be copied, otherwise set it to
      False. }

  { Use the following type to describe how columns within a simple table should
    be sorted. }
  TffOrderByDirection = (ffobAscending, ffobDescending);

  PffOrderByArray = ^TffOrderByArray;
  TffOrderByArray = array[0..ffcl_MaxIndexFlds] of TffOrderByDirection;

  { Defines the standard interface for a cursor. Note that once you create a
    cursor, you must call its Open method to open a cursor for an existing
    table.  If the table does not yet exist, use the Build method to create
    the table and open the cursor on the new table. }
  TffSrBaseCursor = class(TffServerObject)
    protected {private}
      bcTableClass : TffSrTableClass;
        { The type of table to be created by the cursor.
          Be sure to initialize it to the appropriate value in the
          inherited constructors before calling one of the TffSrBaseCursor
          constructors. }

      bcBLOBCursors : TffList;       { List of cursors for which we have
                                      dereferenced BLOB links. }
      bcCloseTable : Boolean;        { Set to True if the cursor is to close
                                      its table when the cursor is freed.
                                      Standard cursors leave the table open
                                      because other clients may need to access
                                      the same table. SQL cursors close the
                                      table right away because the result set
                                      is typically for only one client. }
      bcCloseWTrans : Boolean;                                         {!!.05}
      bcDatabase : TffSrDatabase;
      bcEngine   : TffServerEngine;  {the engine with which this cursor is
                                     associated }
      bcExclOwner  : Boolean;           {If True then cursor has exclusively
                                        opened the table. }
      bcExtenders : TffList;
        {-List of engine extenders associated with this cursor. }

      bcIndexID  : Longint;
{Begin !!.03}
      bcLockedRefNum : TffInt64;     { Last record locked via GetRecord
                                      method. The cursor tracks this to
                                      ensure that a record lock obtained
                                      via TffTable.Edit, while an implicit
                                      transaction is in effect, will be
                                      unlocked if the client abruptly
                                      terminates. }
{End !!.03}
      bcNumReadLocks : Integer;       { Number of open read locks.}    {!!.05}
      bcTable       : TffSrBaseTable;
      bcTempStore   : TffBaseTempStorage;

      bcKID        : TffKeyIndexData;  {work field for index access}
      bcCompareData: TffCompareData;   {ditto}
      bcCurKey     : PffByteArray;     {current key}
      bcFilter     : TffSrFilter;      {filter object}
      bcFilterSav  : TffSrFilter;      {overridden filter}
      bcHasRange   : Boolean;          {whether range is active}
      bcInfo       : TffSrCursorInfo;  {the cursor's current position, key path,
                                        reference number, etc. }
{Begin !!.06}
      bcInfoLock   : TffPadlock;       {Used to prevent transaction from
                                        clearing a cursor's key path while the
                                        cursor is navigating to next or prev
                                        record. }
{End !!.06}
      bcOpenMode   : TffOpenMode;
      bcRecordData : PffByteArray;     {work record data area}
      bcRecordLen  : Integer;          {record length}
      bcRng1Valid  : Boolean;          {is low range point valid?}
      bcRng2Valid  : Boolean;          {is high range point valid?}
      bcRng1Key    : PffByteArray;     {range start key}
      bcRng2Key    : PffByteArray;     {range end key}
      bcRng1FldCnt : Integer;          {range start field count}
      bcRng2FldCnt : Integer;          {range end field count}
      bcRng1PtlLen : Integer;          {range start partial length}
      bcRng2PtlLen : Integer;          {range end partial length}
      bcRng1Incl   : Boolean;          {range includes start key}
      bcRng2Incl   : Boolean;          {range includes end key}
      bcSavedInfo  : TffSrCursorInfo;  {temporary work area for bcSaveCurInfo &
                                        bcRestoreCurInfo }

      bcNewRecBuff : PffByteArray;     { exclusively used by extenders }
      bcOldRecBuff : PffByteArray;     { exclusively used by extenders }

      bcNeedNestedTransaction : Boolean; {If set to true all operations on the
                                          cursor use a nested transaction if needed}

      procedure bcAddExtender(anExtender : TffBaseEngineExtender);
        {-Use this method to add an extender to the list of extenders
          interested in a cursor. }

      function bcBLOBCopy(aSrcCursor  : TffSrBaseCursor;
                    const aBLOBNr     : TffInt64;
                      var aDestBLOBNr : TffInt64) : TffResult;
        { Used to copy a BLOB from one cursor to another. }

      function bcBLOBLinkGetLength(const aTableName : TffTableName;
                                   const aBLOBNr    : TffInt64;
                                     var aLength    : Longint) : TffResult; virtual;
        {-Used to obtain the length of a BLOB referenced by a BLOB link within
          a record of this cursor's result set. }

      function bcBLOBLinkRead(const aTableName : TffTableName;
                              const aBLOBNr    : TffInt64;
                              const aOffset    : TffWord32;            {!!.06}
                              const aLen       : TffWord32;            {!!.06}
                                var aBLOB;
                                var aBytesRead : TffWord32)            {!!.06}
                                               : TffResult;
        {-Used to read a BLOB referenced by a BLOB link within a record of this
          cursor's result set. }

      function bcCheckExclusiveReadWrite : TffResult; virtual;
        {-Verifies the cursor has exclusive read-write access to the table. }

      function bcFindBLOBCursor(const aTableName : TffTableName) : TffSrBaseCursor; virtual;
        {-Finds a BLOB cursor based upon a table name. }

      function bcGetAttribs : TffFileAttributes; virtual;

      function bcGetCursorID : TffCursorID; virtual;

      function bcGetPosition : TffCursorPosition;

      function bcGetRefNr : TffInt64;

      procedure bcInit(const aOpenMode  : TffOpenMode;
                       const aShareMode : TffShareMode;
                       const aExclContLock : Boolean); virtual;        {!!.10}
        {-Called from a cursor constructor. Performs misc. initializations. }

      procedure bcInvalidateCurKey;
      function bcIsCurKeyPathValid : boolean;
      function bcIsCurKeyValid: boolean;
      procedure bcRebuildKeyPath;                                      {!!.05 - Moved from TffSrCursor.scRebuildKeyPath}
        { If the cursor has a valid key, this method rebuilds the cursor's key
          path. }

      procedure bcTableOpenPreconditions(aTable     : TffSrBaseTable;
                                   const aIndexName : string;
                                     var aIndexID   : Longint;
                                   const aOpenMode  : TffOpenMode); virtual; abstract;
        { Used by Create method to verify a thread may open a table. }

      procedure bcTableOpenPrim(aDatabase  : TffSrDatabase;
                          const aTableName : TffTableName;
                          const aOpenMode  : TffOpenMode;
                          const aShareMode : TffShareMode;
                          const aForServer : boolean;
                          const aAttribs   : TffFileAttributes); virtual;
        { Primitive engine method for opening a table. }

      procedure bcRecordUpdated(aOp      : TffRecOp;
                                aRefNr   : TffInt64;
                                aIndexID : integer); virtual;
        { Called when another cursor has updated a record in the same
          table.  Gives this cursor a chance to update its internal
          information (e.g., whether or not the current record has been
          deleted, key path status). }

      procedure bcRestoreCurInfo; virtual;
        { Restore the cursor's position, reference number, key, etc.
          as saved via scSaveCurValues. }

      procedure bcSaveCurInfo; virtual;
        { Save the cursor's current position, reference number, key, etc. }

      function bcGetDictionary: TffDataDictionary; virtual;

    public
      constructor Create(anEngine   : TffServerEngine;
                         aDatabase  : TffSrDatabase;
                   const aTimeout   : Longint); virtual;

      destructor Destroy; override;

{Begin !!.10}
      procedure AcqContentLock(const aMode : TffContentLockMode); virtual;
        { Acquire unconditional content lock. }
      function AcqExclContentLock : TffResult; virtual;
        { Acquire conditional content lock (i.e., the lock is obtained only if
          it can be immediately granted). }
{End !!.10}

        { Used by threads to obtain a content lock. }
      function AddIndexToTable(const aIndexDesc : TffIndexDescriptor) : TffResult; virtual; abstract;
      procedure AppendNewRecord(aData : PffByteArray); virtual;

      { BLOB methods }
      function BLOBAdd(var aBLOBNr : TffInt64) : TffResult; virtual;

      function BLOBLinkAdd(const aTableName : TffTableName;
                           const aTableBLOBNr : TffInt64;
                             var aBLOBNr    : TffInt64) : TffResult; virtual;
        { Adds a link to a BLOB in another table to the cursor's table. }

      procedure Build(const aTableName : TffTableName;
                            aDict      : TffDataDictionary;
                      const aOpenMode  : TffOpenMode;
                            aShareMode : TffShareMode;
                            aForServer : boolean;
                            aOverWrite : boolean;
                            aAttribs   : TffFileAttributes;
                            aStoreSize : TffWord32); virtual;
        { Use this method to open a cursor for a table that does not
          yet exist. This method uses aDict to create the table. This method
          then opens the cursor and positions to the Sequential Access Index
          (i.e., index 0). }

      function CanClose(const Mark : boolean) : boolean; override;     {New !!.01}
        { A cursor can close if it is not active & is not involved in a
          transaction. }

      function FileBLOBAdd(const aFileName : TffFullFileName;
                             var aBLOBNr   : TffInt64) : TffResult; virtual;

      function BLOBDelete(const aBLOBNr : TffInt64) : TffResult; virtual;

      function BLOBFree(aBLOBNr : TffInt64) : TffResult; virtual;

      function BLOBGetLength(aBLOBNr : TffInt64;
                         var aFBError: TffResult) : Longint; virtual;

      function BLOBIsLink(aBLOBNr         : TffInt64;                  {!!.11 - New}
                      var aSrcTableName   : TffTableName;
                      var aSrcTableBLOBNr : TffInt64)
                                          : Boolean;

{Begin !!.03}
      function BLOBListSegments(aBLOBNr : TffInt64;
                                aStream : TStream) : TffResult; virtual;
{End !!.03}
      function BLOBRead(aBLOBNr    : TffInt64;
                        aOffset    : TffWord32;                        {!!.06}
                        aLen       : TffWord32;                        {!!.06}
                    var aBLOB;
                    var aBytesRead : TffWord32)                        {!!.06}
                                   : TffResult; virtual;

      function BLOBTruncate(aBLOBNr : TffInt64;
                            aLen    : TffWord32) : TffResult; virtual;

      function BLOBWrite(const aBLOBNr : TffInt64;
                               aOffset : TffWord32;
                               aLen    : TffWord32;
                           var aBLOB) : TffResult; virtual;

      function CheckBookmark(aBookmark : PffByteArray) : TffResult; virtual; abstract;
      procedure ClearIndex; virtual; abstract;
      function CloneCursor(aOpenMode : TffOpenMode) : TffSrBaseCursor; virtual; abstract;
      function CompareBookmarks(aBookmark1, aBookmark2 : PffByteArray;
                            var CmpResult : Longint) : TffResult; virtual; abstract;
      function CopyRecords(aSrcCursor : TffSrBaseCursor; aBLOBCopyMode : TffBLOBCopyMode;
                           aCallback : TffSrCopyRecordsProc;
                           aCookie1, aCookie2 : Longint) : TffResult; virtual;
        { Use this method to copy all records from a source cursor to this
          cursor. Copies only those records matching the range and/or filter
          applied to the source cursor.

          Requirement: The source and destination cursors must have compatible
          dictionaries. The dictionaries must have the same field order, field
          type, length, units, and decimal places.

          If a record contains BLOBs, they are handled based upon the
          aBLOBCopyMode parameter. If mode is ffbcmNoCopy then the BLOB fields
          are set to NULL in the destination record. If mode is ffbcmCopyFull
          then the BLOBs are copied wholesale to the destination cursor.
          If mode is ffbcmCreateLink then the destination cursor is given a
          link to the BLOB in the source cursor.

          Use aCallback to have a validation routine called for each r4ecord
          that is copied. The validation routine has the opportunity to
          inspect the record and tell this routine whether or not to copy the
          record. }

      function CopyRecordParts(aSrcCursor : TffSrBaseCursor;
                               aFields    : PffLongintArray;
                               aNumFields : integer;
                               aBLOBCopyMode : TffBLOBCopyMode;
                               aCallback : TffSrCopyRecordsProc;
                               aCookie1, aCookie2 : Longint) : TffResult; virtual;
        { Similar to the CopyRecords method except this method allows you to
          copy specific fields from the source cursor. aFields identifies the
          fields to be copied. Each element of aFields is a field number in
          the source cursor's dictionary (base zero). The fields are copied in
          the order specified.

          The destination cursor's dictionary must have fields that match the
          specified fields in the source dictionary except that they must be
          in the order specified by aFields.
        }
      function DeleteRecord(aData : PffByteArray) : TffResult; virtual;
{Begin !!.06}
      function DeleteRecords : TffResult; virtual;
        { Delete all records in the cursor's result set, taking into account
          the active filter and/or range. }
{End !!.06}
      function DropIndexFromTable(const aIndexName : TffDictItemName;
                                         aIndexID   : Longint) : TffResult; virtual; abstract;
      function Empty : TffResult; virtual;
      function EnsureWritable(aCheckCurRec, aConditionalLock : boolean) : TffResult; virtual;
        { Ensures the cursor is writable.  If aCheckCurRec is true, this method
         attempts to obtain an Exclusive, Commit duration lock on the
         record. If aConditionalLock is also True then the method succeeds only
         if it is able to immediately obtain the Exclusive lock. }
      function ExtractKey(aData : PffByteArray; aKey : PffByteArray) : TffResult; virtual; abstract;
      function GetBookmark(aBookmark : PffByteArray) : TffResult; virtual; abstract;
      function GetBookmarkSize : integer; virtual; abstract;
      function GetRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; virtual;
      function GetRecordCount(var aRecCount : Longint) : TffResult; virtual; abstract;
      function GetNextRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; virtual; abstract;
      function GetPriorRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; virtual; abstract;
      function GetRecordField(aField : integer;
                              aRecordBuffer  : PffByteArray;
                          var isNull: boolean;
                              aFieldBuffer : pointer) : TffResult; virtual;
        { Obtain the value of a field. }
      function GetRecordForKey(aDirectKey  : boolean;
                               aFieldCount : integer;
                               aPartialLen : integer;
                               aKeyData    : PffByteArray;
                               aData       : PffByteArray;
                               aFirstCall  : Boolean) : TffResult; virtual; abstract;
      function InsertRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; virtual; abstract;
      function InsertRecordNoDefault(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; virtual; abstract;{!!.10}
      function IsInRange(aKey : PffByteArray) : integer; virtual; abstract;
      function IsRecordLocked(aLockType : TffSrLockType) : Boolean; virtual;
{Begin !!.03}
      procedure ListBLOBFreeSpace(aTI : PffTransInfo;
                            const aInMemory : Boolean;
                                  aStream : TStream);
{End !!.03}
      function OverrideFilter(aExpression : pCANExpr;
                               aTimeout    : TffWord32) : TffResult; virtual;
      function ModifyRecord(aData : PffByteArray; aRelLock : Boolean)
                                                           : TffResult; virtual; abstract;
      function NotifyExtenders(const anAction      : TffEngineAction;
                               const aFailAction   : TffEngineAction)
                                                   : TffResult;
        {-Notifies all extenders associated with the cursor about the
          specified action.  If ignoreErrCode is True then error codes
          returned by extenders are ignored.  If failures occur it will
          be taken care of before going back to the calling method.}
      procedure Open(const aTableName : TffTableName;
                     const aIndexName : TffName;
                     const aIndexID   : Longint;
                     const aOpenMode  : TffOpenMode;
                           aShareMode : TffShareMode;
                           aForServer : Boolean;
                     const aExclContLock : Boolean;                    {!!.10}
                           aAttribs   : TffFileAttributes); virtual;
        { Use this method to open a cursor for a table that exists. }

      procedure ReadAutoIncValue(var aValue: TffWord32); virtual;
      procedure RelContentLock(aMode : TffContentLockMode); virtual;
      procedure RelRecordLock(aAllLocks : Boolean); virtual;
      procedure RelTableLock(aAllLocks : Boolean); virtual;
      procedure RemoveIfUnused; virtual;                               {!!.05}
      procedure ResetRange; virtual; abstract;
      function RestoreFilter : TffResult; virtual;
      procedure SetAutoIncValue(aValue: TffWord32); virtual;
      function SetFilter(aExpression : pCANExpr;
                         aTimeout    : TffWord32) : TffResult; virtual;
      function SetRange(aDirectKey   : Boolean;
                        aFieldCount1 : Integer;
                        aPartialLen1 : Integer;
                        aKeyData1    : PffByteArray;
                        aKeyIncl1    : Boolean;
                        aFieldCount2 : Integer;
                        aPartialLen2 : Integer;
                        aKeyData2    : PffByteArray;
                        aKeyIncl2    : Boolean) : TffResult; virtual; abstract;
      procedure SetToBegin; virtual; abstract;
      function SetToBookmark(aBookmark : PffByteArray) : TffResult; virtual; abstract;
      function SetToCursor(aCursor : TffSrBaseCursor) : TffResult; virtual; abstract;
      procedure SetToEnd; virtual; abstract;
      function SetToKey(aSearchAction : TffSearchKeyAction;
                        aDirectKey    : boolean;
                        aFieldCount   : integer;
                        aPartialLen   : integer;
                        aKeyData      : PffByteArray) : TffResult; virtual; abstract;
      function ShouldClose : boolean; override;                        {New !!.01}
        { A cursor can close if it is not involved in a transaction. }
      function SortRecords(aFieldsArray : TffFieldList;
                     const aOrderByArray : TffOrderByArray;
                     const aNumFields : integer) : TffResult; virtual;
        { Use this method to physically sort the records within a table.
          Parameters:
            aFieldsArray - Array of field numbers on which the table is being
                           sorted. Field numbers correspond to the fields in
                           the table's dictionary. Each element in this array
                           must have a corresponding element in aOrderByArray.
            aOrderByArray - Array of order by indicators, one for each field on
                            which the table is being sorted. Each element in
                            this array has a corresponding element in
                            aFieldsArray.
            aNumFields - The number of fields on which the table is being
                         sorted.
        }
      function SwitchToIndex(aIndexID   : integer;
                             aPosnOnRec : boolean) : TffResult; virtual; abstract;

      { Properties }
      property Attribs : TffFileAttributes read bcGetAttribs;
        { Returns the file attributes attached to the table's data file. }
      property CloseTable : boolean read bcCloseTable write bcCloseTable;
        { Set this property to True if the cursor is to close its table when
          the cursor is freed. This is useful for SQL cursors which generate
          temporary tables applicable to only one client. }
      property CursorID : TffCursorID read bcGetCursorID;
      property CursorInfo : TffSrCursorInfo read bcInfo write bcInfo;
      property Database : TffSrDatabase read bcDatabase;
      property Dictionary : TffDataDictionary read bcGetDictionary;
      property Engine : TffServerEngine read bcEngine;
      property ExclOwner : boolean read bcExclOwner write bcExclOwner;
      property Extenders : TffList read bcExtenders;                    {!!.02}
      property Filter: TffSrFilter read bcFilter;
      property IndexID : Longint read bcIndexID;
      property Position : TffCursorPosition read bcGetPosition;
      property RefNr : TffInt64 read bcGetRefNr;
        { Returns the reference number of the current record. }
//    property ServerEngine : TFFServerEngine read bcEngine;           {Deleted !!.03}
      property Table : TffSrBaseTable read bcTable;

      { Used exclusively by extenders, these might not reflect actual values }
      property NewRecordBuffer : PffByteArray read bcNewRecBuff;
      property OldRecordBuffer : PffByteArray read bcOldRecBuff;

      property NeedNestedTransaction : Boolean                         {!!.03}
        read  bcNeedNestedTransaction                                  {!!.03}
        write bcNeedNestedTransaction;                                 {!!.03}
    end;

  TffSrCursor = class(TffSrBaseCursor)
    protected {private}
      scKeyLen     : integer;          {key length for cursor's index}
    protected

      procedure bcInit(const aOpenMode  : TffOpenMode;
                       const aShareMode : TffShareMode;
                       const aExclContLock : Boolean); override;       {!!.10}
      procedure bcTableOpenPreconditions(aTable     : TffSrBaseTable;
                                  const aIndexName : string;
                                    var aIndexID   : Longint;
                                  const aOpenMode  : TffOpenMode); override;
        { Used by Create method to verify a thread may open a table. }

      procedure scRebuildCurKey(aRecData : PffByteArray;
                                aLockObtained : boolean);
        { Rebuilds the cursor's key from the specified record buffer.  If
          aRecData is nil then this method reads the record from the data file
          & rebuilds the key from the retrieved record.
          If you have already obtained a lock on the current record, set
          aLockObtained := True.  Doing so skips an unnecessary lock request. }
//      procedure scRebuildKeyPath;                                    {!!.05 - moved to TffSrBaseCursor.bcRebuildKeyPath}
//        { If the cursor has a valid key, this method rebuilds the cursor's key
//          path. }
    public
      constructor Create(anEngine   : TffServerEngine;
                         aDatabase  : TffSrDatabase;
                   const aTimeout   : Longint); override;

      destructor Destroy; override;
      function AddIndexToTable(const aIndexDesc : TffIndexDescriptor) : TffResult; override;
      function CheckBookmark(aBookmark : PffByteArray) : TffResult; override;
      procedure ClearIndex; override;
      function CloneCursor(aOpenMode : TffOpenMode) : TffSrBaseCursor; override;
      function CompareBookmarks(aBookmark1, aBookmark2 : PffByteArray;
                             var CmpResult : Longint) : TffResult; override;
      function DropIndexFromTable(const aIndexName : TffDictItemName;
                                         aIndexID   : Longint) : TffResult; override;
      function ExtractKey(aData : PffByteArray; aKey : PffByteArray) : TffResult; override;
      function GetBookmark(aBookmark : PffByteArray) : TffResult; override;
      function GetBookmarkSize : integer; override;
      function GetNextRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; override;
      function GetPriorRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; override;
      function GetRecordCount(var aRecCount : Longint) : TffResult; override;
      function GetRecordForKey(aDirectKey  : boolean;
                                aFieldCount : integer;
                                aPartialLen : integer;
                                aKeyData    : PffByteArray;
                                aData       : PffByteArray;
                                aFirstCall  : Boolean) : TffResult; override;
      function InsertRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; override;
      function InsertRecordNoDefault(aData : PffByteArray; aLockType : TffSrLockType) : TffResult; override;{!!.10}
      function IsInRange(aKey : PffByteArray) : integer; override;
      function ModifyRecord(aData : PffByteArray; aRelLock : boolean) : TffResult; override;
      procedure ResetRange; override;
      function SetRange(aDirectKey : boolean;
                         aFieldCount1 : integer;
                         aPartialLen1 : integer;
                         aKeyData1    : PffByteArray;
                         aKeyIncl1    : boolean;
                         aFieldCount2 : integer;
                         aPartialLen2 : integer;
                         aKeyData2    : PffByteArray;
                         aKeyIncl2    : boolean) : TffResult; override;
      procedure SetToBegin;  override;
      function SetToBookmark(aBookmark : PffByteArray) : TffResult; override;
      function SetToCursor(aCursor : TffSrBaseCursor) : TffResult; override;
      procedure SetToEnd; override;
      function SetToKey(aSearchAction : TffSearchKeyAction;
                         aDirectKey    : boolean;
                         aFieldCount   : integer;
                         aPartialLen   : integer;
                         aKeyData      : PffByteArray) : TffResult; override;
      function SwitchToIndex(aIndexID   : integer;
                              aPosnOnRec : boolean) : TffResult; override;
  end;

  TffSrCursorList = class(TffServerObjectList)
    protected {private}
    protected
      function GetCursorItem(Find : TffListFindType; Value : Longint) : TffSrBaseCursor;
    public
      procedure AddCursor(aCursor : TffSrBaseCursor);

      function CursorCount : integer;
        { Returns the number of cursors in the list. }

      procedure DeleteCursor(aCursorID : TffCursorID);
        { Removes a cursor from the list and frees the cursor. }

      procedure RemoveCursor(aCursorID : TffCursorID);
        { Removes a cursor from the list but does not free the cursor. }

      property Cursor [Find : TffListFindType; Value : Longint] : TffSrBaseCursor
         read GetCursorItem; default;

  end;

  { Describes the interface for the representation of a physical table. }
  TffSrBaseTable = class(TffSelfListItem)
    protected
      btBaseName   : PffShStr;
      btBLOBEngine : TffBaseBLOBEngine;                          {!!.11}
      btBufMgr     : TffBufferManager;
      btCursorList : TffSrCursorList;
      btDictionary : TffServerDataDict;
      btEngine     : TffServerEngine;
      btFiles      : TffVCLList;
      btFolder     : TffSrFolder;
      btForServer  : Boolean;
      btContentLocks : TffLockContainer;
      btClientLocks : TffLockContainer;
      btOpenIntents : Longint;
      btPortal      : TffReadWritePortal;
//      btUseInternalRollback : boolean;                                 {!!.03}{Deleted !!.11}

{Begin !!.03}
      procedure btCommitBLOBMgr;
        { Commits the changes made by the BLOB resource manager to its
          in-memory list. }
{End !!.03}
      procedure btCreateFile(aFileInx       : integer;
                             aTI            : PffTransInfo;
                       const aExtension     : TffExtension;
                             aForServer     : boolean;
                             aAttribs       : TffFileAttributes;
                             aStore         : TffBaseTempStorage); virtual;
      procedure btDeleteBLOBsForRecord(aTI : PffTransInfo;
                                       aData : PffByteArray); virtual;
      function btGetBaseName : TffTableName; virtual;
      function btGetCursorList : TffSrCursorList; virtual;
      function btGetDictionary : TffServerDataDict; virtual;
      function btGetFile(Inx : integer) : PffFileInfo; virtual;
      function btGetFileCount  : integer; virtual;
      function btGetFolder : TffSrFolder; virtual;
      procedure btInformCursors(aSrcCursorID : TffCursorID;
                                aOp          : TffRecOp;
                                aRefNr       : TffInt64;
                                aIndexID     : integer); virtual;
      function btGetOpenIntents : Longint; virtual;
{Begin !!.03}
      procedure btRollbackBLOBMgr;
        { Rolls back the changes made by the BLOB resource manager to its
          in-memory list. }
{End !!.03}
      procedure btSetFile(Inx : integer; FI : PffFileInfo); virtual;
      procedure btSetFileCount(FC  : integer); virtual;
      procedure btTableUpdated(aDatabaseID : TffDatabaseID); virtual;
      procedure btUpdateAutoInc(aTI : PffTransInfo; aData : PffByteArray); virtual;
    public
      constructor Create(anEngine    : TffServerEngine;
                   const aBaseName   : TffTableName;
                         aFolder     : TffSrFolder;
                         aBufMgr     : TffBufferManager;
                   const aOpenMode   : TffOpenMode); virtual;
      destructor Destroy; override;

      procedure AcqClientLock(aCursorID    : Longint;
                        const aLockType    : TffSrLockType;
                        const aConditional : Boolean); virtual;

      procedure AcqContentLock(aTrans : TffSrTransaction;
                         const aLockType : TffSrLockType;
                         const aConditional : boolean); virtual;
{Begin !!.10}
      function AcqExclContentLock(aTrans : TffSrTransaction) : TffResult; virtual;
{End !!.10}
      procedure AcqLock(const aCursorID : TffCursorID;
                        const aLockType : TffSrLockType); virtual;
{Begin !!.03}
      procedure AddAttribute(const anAttrib : TffFileAttribute);
        { Add an attribute to the table's FF-specific file attributes. }
{End !!.03}
      procedure AddIndex(const aIndexDesc : TffIndexDescriptor;
                               aTI : PffTransInfo); virtual; abstract;
      procedure BeginCommit; virtual;
        { Before a transaction commits, a thread must call this method.
          This ensures that all readers have finished with the table before
          the table is updated.  When done committing, the thread must call
          TffSrTable.EndCommit. }
      procedure BeginRead; virtual;
        { Threads that are not in a transaction & needing to read data from
          the table must call this method prior to reading.  When done
          reading the thread must call TffSrTable.EndRead. }
      procedure BuildFiles(aTI         : PffTransInfo;
                           aForServer  : boolean;
                           aDictionary : TffDataDictionary;
                           aAttribs    : TffFileAttributes;
                           aStore      : TffBaseTempStorage); virtual; abstract;
      function BuildKeyForRecord(aIndexID    : integer;
                                 aData       : PffByteArray;
                                 aKey        : PffByteArray;
                                 aFieldCount : integer;
                                 aPartialLen : integer) : TffResult; virtual; abstract;
      procedure CloseFiles(commitChanges : boolean; aTI : PffTransInfo); virtual;
      procedure CommitChanges(aTI : PffTransInfo); virtual;
      function CompareKeysForCursor(var aKID  : TffKeyIndexData;
                                        aKey1 : PffByteArray;
                                        aKey2 : PffByteArray)
                                              : Integer; virtual; abstract;
      function DeleteRecord(aTI           : PffTransInfo;
                      const aCursorID     : TffCursorID;
                      const aRefNr        : TffInt64;
                      const aLockObtained : Boolean;
                        var aBTreeChanged : Boolean)                   {!!.05}
                                          : TffResult; virtual; abstract;
      procedure DeregisterOpenIntent; virtual;
        { Use this function to deregister intent to open.  Should only be
          called if RegisterIntentOpen was previously called. }

      procedure DropIndex(aTI : PffTransInfo; aIndexID : Longint); virtual; abstract;
      function EmptyFiles(aTI : PffTransInfo) : TffResult; virtual;
      procedure EndCommit(aDatabaseID : TffDatabaseID); virtual;
        { Call this method after calling BeginCommit and finishing the commit
          operation. }
      procedure EndRead; virtual;
        { Call this method after calling BeginRead and finishing the read
          operation. }
      function FindKey(var aKID        : TffKeyIndexData;
                       var aRefNr      : TffInt64;
                           aTI         : PffTransInfo;
                           aKey        : PffByteArray;
                       var aKeyPath    : TffKeyPath;
                           aAction     : TffSearchKeyAction) : boolean; virtual; abstract;
      function GetNextKey(var aKID       : TffKeyIndexData;
                          var aRefNr     : TffInt64;
                              aTI        : PffTransInfo;
                              aKey       : PffByteArray;
                          var aKeyPath   : TffKeyPath) : TffResult; virtual; abstract;
      function GetNextRecord(aTI        : PffTransInfo;
                       const aDatabaseID : TffDatabaseID;              {!!.10}
                       const aCursorID  : TffCursorID;                 {!!.10}
                         var aKID       : TffKeyIndexData;
                         var aRefNr     : TffInt64;
                             aKey       : PffByteArray;
                         var aKeyPath   : TffKeyPath;
                             aData      : PffByteArray;
                       const aLockType  : TffSrLockType) : TffResult; virtual; abstract;
      procedure GetNextRecordSeq(aTI : PffTransInfo;
                             var aRefNr : TffInt64;
                                 aData : PffByteArray); virtual;
      procedure GetPrevRecordSeq(aTI : PffTransInfo;
                             var aRefNr : TffInt64;
                                 aData : PffByteArray); virtual;
      function GetPriorRecord(aTI        : PffTransInfo;
                        const aDatabaseID : TffDatabaseID;             {!!.10}
                        const aCursorID  : TffCursorID;                {!!.10}
                          var aKID       : TffKeyIndexData;
                          var aRefNr     : TffInt64;
                              aKey       : PffByteArray;
                          var aKeyPath   : TffKeyPath;
                              aData      : PffByteArray;
                        const aLockType  : TffSrLockType) : TffResult; virtual; abstract; {!!.10}
      function GetRecord(aTI        : PffTransInfo;
                   const aDatabaseID : TffDatabaseID;                  {!!.10}
                   const aCursorID  : TffCursorID;                     {!!.10}
                         aRefNr     : TffInt64;
                         aData      : PffByteArray;
                   const aLockType  : TffSrLockType;                   {!!.10}
                   const aLockObtained : boolean;                      {!!.10}
                   const aConditional : boolean) : TffResult; virtual; {!!.10}
        { Use this method to retrieve a record from the data file.
          If a lock has already been obtained via TffSrTable.GetRecordLock
          then set aLockObtained := True.  Doing so skips an unnecessary
          lock request. }
      procedure GetRecordLock(aTI        : PffTransInfo;
                       const aDatabaseID : TffDatabaseID;              {!!.10}
                       const aCursorID  : TffCursorID;                 {!!.10}
                       const aRefNr     : TffInt64;                    {!!.10}
                       const aLockType  : TffSrLockType); virtual;     {!!.10}
{Begin !!.10}
      procedure GetRecordNoLock(aTI : PffTransInfo;
                                aRefNr : TffInt64;
                                aData  : PffByteArray);
        { Retrieve a record without obtaining any type of lock. }
{End !!.10}
      function HasClientLock(const aCursorID : TffCursorID) : boolean; virtual;
        { Returns True if the specified cursor has a client lock (i.e.,
          TffTable.LockTable). }
      function HasLock(const aCursorID : TffCursorID;
                       const aLockType : TffSrLockType) : boolean; virtual;
        { Returns True if the specified cursor has an open lock of the specified
          type on the table. }
{Begin !!.06}
      function HasRecordLocks : Boolean;
        { Returns True if there are any record locks on the table. }
{End !!.06}
      function InsertRecord(aTI        : PffTransInfo;
                            aCursorID  : TffCursorID;
                            aData      : PffByteArray;
                            aLockType  : TffSrLockType;
                        var aNewRefNr  : TffInt64) : TffResult; virtual; abstract;
      function InsertRecordNoDefault(aTI        : PffTransInfo;        {!!.10}
                                     aCursorID  : TffCursorID;
                                     aData      : PffByteArray;
                                     aLockType  : TffSrLockType;
                                 var aNewRefNr  : TffInt64) : TffResult; virtual; abstract;
      function IsContentLockedBy(aTrans : TffSrTransaction) : boolean; virtual;
        { Returns True if the table's contents are locked by the specified
          transaction. This returns True whether the lock is a read lock or
          a write lock. }
      function IsRecordLocked(aTI        : PffTransInfo;
                              aCursorID  : TffCursorID;
                              aRefNr     : TffInt64;
                              aLockType  : TffSrLockType) : Boolean; virtual;
      function IsServerTable : boolean; virtual;
        { Returns True if this table is a server table. }
      procedure MakeKIDForCursor(aIndexID : integer; var aKID : TffKeyIndexData); virtual; abstract;
      procedure OpenFiles(aTI : PffTransInfo; aForServer : boolean;
                          aAttribs : TffFileAttributes); virtual;
      function PutRecord(aTI       : PffTransInfo;
                         aCursorID : TffCursorID;
                         aRefNr    : TffInt64;
                         aData     : PffByteArray;
                         aRelLock  : boolean;                                   {!!.05}
                     var aKeyChanged : Boolean) : TffResult; virtual; abstract; {!!.05}
      procedure RegisterOpenIntent; virtual;
        { Use this method to register intent to open a table. }
{Begin !!.10}
      procedure RelaxRecordLock(aTI : PffTransInfo;
                                aCursorID : TffCursorID;
                                aRefNr : TffInt64); virtual;
{End !!.10}
      procedure RelClientLock(aCursorID : Longint; aRemoveAll : Boolean); virtual;
      procedure RelContentLock(aTrans : TffSrTransaction); virtual;
      procedure RelLock(const aCursorID : TffCursorID;
                        const aAllLocks : boolean); virtual;
      procedure RelRecordLock(aTI : PffTransInfo;
                              aDatabaseID : TffDatabaseID;             {!!.10}
                              aCursorID : TffCursorID;
                              aRefNr : TffInt64); virtual;
      procedure RemoveLocksForCursor(const aDatabaseID : TffDatabaseID; {!!.10}
                                     const aCursorID : TffCursorID;
                                     const aRefNr    : TffInt64;
                                           aTI       : PffTransInfo); virtual;

{Begin !!.03}
      procedure ListBLOBFreeSpace(aTI : PffTransInfo;
                            const aInMemory : Boolean;
                                  aStream : TStream);
{End !!.03}

      procedure SetAttributes(const fileAttribs : TffFileAttributes); virtual;
        { Sets the file attributes on all files of a table instance. This
          should only be called when the table is first opened. }

      procedure SetExclOwner(const aCursorID : TffCursorID); virtual;
        { Marks each file managed by a table as exclusively owned by the
          specified cursor. Only call this method when the table has been
          exclusively opened by the cursor. }

      property BaseName : TffTableName read btGetBaseName;
      property ClientLocks : TffLockContainer read btClientLocks;      {!!.11}
      property CursorList : TffSrCursorList read btGetCursorList;
      property Dictionary : TffServerDataDict read btGetDictionary;
      property FileCount : Integer read btGetFileCount write btSetFileCount;
      property Files [Inx : integer] : PffFileInfo read btGetFile write btSetFile;
      property Folder : TffSrFolder read btGetFolder;
      property OpenIntents : Longint read btGetOpenIntents;
        { The number of threads that have registered their intent to open this
          table. }
      property TableID : Longint read KeyAsInt;
{Begin !!.03}
//      property UseInternalRollback : boolean                         {Deleted !!.11}
//        read btUseInternalRollback                                   {Deleted !!.11}
//        write btUseInternalRollback;                                 {Deleted !!.11}
        { This property is set to True when the server is attempting to
          insert or modify a record.  When set to True and the operation fails,
          the server undoes any modifications made up to the point of failure.
          For example, a record is inserted into a table having four indexes.
          The record is stored in the data file and keys are added to two of
          the indexes.  However, a key violation occurs when adding a key to
          the third index.  The server removes the keys from the first two
          indexes and removes the record from the data file. }
{End !!.03}
  end;

  { Represents a table opened by one or more cursors.  Only one
    instance of this class is created and the instance is freed when all
    cursors have closed the table.

    Table locks are acquired using the parent folder's lock manager.  This
    means that each client opening a table obtains some kind of lock on the
    table.  The following types of locks are used:

      ffsltExclusive - Used to obtain exclusive read-write access to a table.
      ffsltShare - Used to obtain read-only access to a table.
      ffsltIntentS - Used to obtain read-write access to a table.

    Since client A may open a table in read-only mode while clients B, C, & D
    may open a table in non-exclusive read-write mode, we use the ffsltShare
    & ffsltIntentS locks to represent non-exclusive read-write and read-only
    modes.  ffsltShare and ffsltIntentS are compatible locks so any number
    of clients may concurrently access the table.

    If a client wants to open the table exclusively, their request for a
    ffsltExclusive lock will wait until all non-exclusive read-write and
    read-only clients have released their locks.

    Conversely, a client wanting to open the table in read-only or
    non-exclusive read-write mode must wait until a client granted Exclusive
    access to the table has released its lock.

    Notes on LockTable and UnlockTable:
    Just as in the BDE, a client may lock a table for reading or writing.
    Pertinent rules:
    1. Table locking is as described in the previous paragraphs.
    2. If a table is read-locked then no client may edit a record.
    3. If a table is write-locked then only the client obtaining the lock
       may edit records.
  }
  TffSrTable = class(TffSrBaseTable)
    protected
//      stUseInternalRollback : boolean;                               {!!.03}

      stUserBuildKey : TffVCLList;
      stUserCompareKey : TffVCLList;

      function stGetBuiltCompositeKey(aIndexID : integer;
                                      aData    : PffByteArray;
                                      aKeyLen  : Longint;
                                  var aKey     : PffByteArray) : TffResult;
      function stBuildCompositeKey(aIndexID    : integer;
                                   aData       : PffByteArray;
                                   aKey        : PffByteArray;
                                   aFieldCount : integer;
                                   aLastFldLen : integer) : TffResult;
      function stDeleteKeyPrim(aInxFile      : Integer;
                               aTI           : PffTransInfo;
                               aRefNr        : TffInt64;
                               aKey          : PffByteArray;
                               aCompare      : TffKeyCompareFunc;
                               aCmpData      : PffCompareData;
                           var aBTreeChanged : Boolean) : Boolean;     {!!.05}
      function stDeleteKeysForRecord(aTI           : PffTransInfo;
                                     aRefNr        : TffInt64;
                                     aData         : PffByteArray;
                                 var aBTreeChanged : Boolean)          {!!.05}
                                                 : TffResult;
      function stGetUserBuildKey(aIndexID : Integer) : TffKeyBuildFunc;
      function stGetUserCompareKey(aIndexID : Integer) : TffKeyCompareFunc;
      function stInsertKeyPrim(aInxFile: integer;
                               aTI     : PffTransInfo;
                               aRefNr  : TffInt64;
                               aKey    : PffByteArray;
                               aCompare: TffKeyCompareFunc;
                               aCmpData: PffCompareData) : boolean;
      function stInsertKeysForRecord(aTI : PffTransInfo;
                                     aRefNr : TffInt64;
                                     aData : PffByteArray) : TffResult;
      function stUpdateKeysForRecord(aCursorID : TffCursorID;
                                     aTI       : PffTransInfo;
                                     aRefNr    : TffInt64;
                                     aData,
                                     aOldData  : PffByteArray;           {!!.05}
                                 var aKeyChanged : Boolean) : TffResult; {!!.05}
    public
      constructor Create(anEngine    : TffServerEngine;
                   const aBaseName   : TffTableName;
                         aFolder     : TffSrFolder;
                         aBufMgr     : TffBufferManager;
                   const aOpenMode   : TffOpenMode); override;
      destructor Destroy; override;

      procedure AddIndex(const aIndexDesc : TffIndexDescriptor;
                               aTI : PffTransInfo); override;
      procedure BuildFiles(aTI         : PffTransInfo;
                           aForServer  : boolean;
                           aDictionary : TffDataDictionary;
                           aAttribs    : TffFileAttributes;
                           aStore      : TffBaseTempStorage); override;
      function BuildKeyForRecord(aIndexID    : integer;
                                 aData       : PffByteArray;
                                 aKey        : PffByteArray;
                                 aFieldCount : integer;
                                 aPartialLen : integer) : TffResult; override;
      function CompareKeysForCursor(var aKID  : TffKeyIndexData;
                                        aKey1 : PffByteArray;
                                        aKey2 : PffByteArray) : integer; override;
      function DeleteRecord(aTI           : PffTransInfo;
                      const aCursorID     : TffCursorID;
                      const aRefNr        : TffInt64;
                      const aLockObtained : Boolean;
                        var aBTreeChanged : Boolean)                   {!!.05}
                                          : TffResult; override;

      procedure DropIndex(aTI : PffTransInfo; aIndexID : Longint); override;
      function FindKey(var aKID        : TffKeyIndexData;
                       var aRefNr      : TffInt64;
                           aTI         : PffTransInfo;
                           aKey        : PffByteArray;
                       var aKeyPath    : TffKeyPath;
                           aAction     : TffSearchKeyAction) : boolean; override;
      function GetNextKey(var aKID       : TffKeyIndexData;
                          var aRefNr     : TffInt64;
                              aTI        : PffTransInfo;
                              aKey       : PffByteArray;
                          var aKeyPath   : TffKeyPath) : TffResult; override;
      function GetNextRecord(aTI        : PffTransInfo;
                       const aDatabaseID : TffDatabaseID;              {!!.10}
                       const aCursorID  : TffCursorID;                 {!!.10}
                         var aKID       : TffKeyIndexData;
                         var aRefNr     : TffInt64;
                             aKey       : PffByteArray;
                         var aKeyPath   : TffKeyPath;
                             aData      : PffByteArray;
                       const aLockType  : TffSrLockType) : TffResult; override; {!!.10}
      function GetPriorRecord(aTI        : PffTransInfo;
                        const aDatabaseID : TffDatabaseID;             {!!.10}
                        const aCursorID  : TffCursorID;                {!!.10}
                          var aKID       : TffKeyIndexData;
                          var aRefNr     : TffInt64;
                              aKey       : PffByteArray;
                          var aKeyPath   : TffKeyPath;
                              aData      : PffByteArray;
                        const aLockType  : TffSrLockType) : TffResult; override; {!!.10}
      function InsertRecord(aTI        : PffTransInfo;
                            aCursorID  : TffCursorID;
                            aData      : PffByteArray;
                            aLockType  : TffSrLockType;
                        var aNewRefNr  : TffInt64) : TffResult; override;
      function InsertRecordNoDefault(aTI        : PffTransInfo;        {!!.10}
                                     aCursorID  : TffCursorID;
                                     aData      : PffByteArray;
                                     aLockType  : TffSrLockType;
                                 var aNewRefNr  : TffInt64) : TffResult; override;
      procedure MakeKIDForCursor(aIndexID : integer; var aKID : TffKeyIndexData); override;
      function PutRecord(aTI       : PffTransInfo;
                         aCursorID : TffCursorID;
                         aRefNr    : TffInt64;
                         aData     : PffByteArray;
                         aRelLock  : boolean;                          {!!.05}
                     var aKeyChanged : Boolean) : TffResult; override; {!!.05}
      procedure RemoveDynamicLinks;
      procedure ResolveDynamicLinks;

      property BaseName : TffTableName read btGetBaseName;
      property CursorList : TffSrCursorList read btGetCursorList;
      property Dictionary : TffServerDataDict read btGetDictionary;
      property FileCount : integer read btGetFileCount write btSetFileCount;
      property Files [Inx : integer] : PffFileInfo read btGetFile write btSetFile;
      property Folder : TffSrFolder read btGetFolder;
      property OpenIntents : Longint read btOpenIntents;
        { The number of threads that have registered their intent to open this
          table. }
      property TableID : Longint read KeyAsInt;

//      property UseInternalRollback : boolean read stUseInternalRollback write stUseInternalRollback; {!!.03}

  end;

  { The following class may be used to access system tables (e.g., FFSALIAS,
    FFSUSER, etc.). }
  TffSrSystemTable = class(TffSrTable)
  public
    function IsServerTable : boolean; override;
  end;

  TffSrTableList = class(TffObject)
    protected {private}
      tlList : TffThreadList;
      FOwner : TffServerEngine;                                        {!!.06}
    protected
      function GetTableItem(Find : TffListFindType; Value : Longint) : TffSrBaseTable;
    public
      constructor Create;
      destructor Destroy; override;
      procedure AddTable(aTable : TffSrBaseTable);

      function BeginRead : TffSrTableList;
        {-A thread must call this method to gain read access to the list.
          Returns the instance of this object as a convenience. }

      function BeginWrite : TffSrTableList;
        {-A thread must call this method to gain write access to the list.
          Returns the instance of this object as a convenience.}

      procedure DeleteTable(aTableID : Longint);

      procedure EndRead;
        {-A thread must call this method when it no longer needs read access
          to the list.  If it does not call this method, all writers will
          be perpetually blocked. }

      procedure EndWrite;
        {-A thread must call this method when it no longer needs write access
          to the list.  If it does not call this method, all readers and writers
          will be perpetualy blocked. }

      function GetTableFromName(const aTableName : TffTableName) : TffSrBaseTable;
      procedure RemoveIfUnused(aTable : TffSrBaseTable);
      procedure RemoveUnusedTables;
      function TableCount : integer;

      property Owner : TffServerEngine                                 {!!.06}
        read FOwner write FOwner;                                      {!!.06}
      property Table[Find : TffListFindType; Value : Longint] : TffSrBaseTable
        read GetTableItem; default;
  end;

  { An instance of this class mirrors an instance of TffDatabase in the client
    application.  If multiple clients open the same database, there will be
    one instance of TffSrDatabase per client.

    A TffSrDatabase may have one active transaction however there may be
    multiple concurrent transactions on a physical database. }
  TffSrDatabase = class(TffServerObject)
    protected {private}
      dbAlias      : PffShStr;
      dbCheckSpace : Boolean;                                          {!!.11}
      dbCursorList : TffSrCursorList;
      dbEngine     : TffServerEngine;
      dbExtenders  : TffThreadList;
      dbFolder     : TffSrFolder;
      dbOpenMode   : TffOpenMode;
      dbSession    : TffSrSession;
      dbShareMode  : TffShareMode;
      dbStmtList   : TffSrStmtList;                                    {!!.10}
      dbTI         : PffTransInfo;
        {-Transaction-specific information used for locking. }
      dbTrans      : TffSrTransaction;
        {-The active transaction for this database. }
    protected
      procedure dbAddExtender(anExtender : TffBaseEngineExtender);
      function dbGetAlias : TffName;
      function dbGetDatabaseID : TffDatabaseID;
      function dbGetTransID : TffTransID;
        {-Returns the ID of the transaction associated with the cursor. }

      function dbGetTransLSN : TffWord32;
        {-Returns the LSN of the cursor's transaction. }

{Begin !!.11}
      procedure dbSetExistingTableVersion(const Version : Longint);
        { *** WARNING: This procedure is provided for testing & utility
              purposes only. Do not use it unless you really know what you're
              doing. That means you! ***}
      procedure dbSetNewTableVersion(const Version : Longint);
        { *** WARNING: This procedure is provided for testing & utility
              purposes only. Do not use it unless you really know what you're
              doing. That means you! ***}
      procedure dbSetPackSrcTableVersion(const Version : Longint);
        { *** WARNING: This procedure is provided for testing & utility
              purposes only. Do not use it unless you really know what you're
              doing. That means you! ***}
{End !!.11}

      procedure dbSetTrans(aTransaction : TffSrTransaction); virtual;

    public
      constructor Create(anEngine    : TffServerEngine;
                         aSession    : TffSrSession;
                         aFolder     : TffSrFolder;
                         anAlias     : TffName;
                         aOpenMode   : TffOpenMode;
                         aShareMode  : TffShareMode;
                         aTimeout    : Longint;
                         aCheckSpace : Boolean);                       {!!.11}
      destructor Destroy; override;

      function CanClose(const Mark : boolean) : boolean; override;

      procedure ForceClose; override;

      function NotifyExtenders(const anAction      : TffEngineAction;
                               const aFailAction   : TffEngineAction) : TffResult;
        {-Notifies all extenders associated with the cursor about the
          specified action.  If ignoreErrCode is True then error codes
          returned by extenders are ignored.  If failures occur it will
          be taken care of before going back to the calling method.}

      procedure RequestClose; override;                                {!!.03}

      function ShouldClose : boolean; override;

      property Alias : TffName read dbGetAlias;
        {-The alias for which this database was opened. }
      property CheckSpace : Boolean                                    {!!.11}
        read dbCheckSpace;                                             {!!.11}
      property CursorList : TffSrCursorList read dbCursorList;
      property DatabaseID : TffDatabaseID read dbGetDatabaseID;
      property Engine : TffServerEngine read dbEngine;
      property Folder : TffSrFolder read dbFolder;
      property OpenMode : TffOpenMode read dbOpenMode;
      property Session : TffSrSession read dbSession;
      property ShareMode : TffShareMode read dbShareMode;
      property StmtList : TffSrStmtList read dbStmtList;               {!!.10}
      property Transaction : TffSrTransaction read dbTrans write dbSetTrans;
        { The transaction associated with the cursor. }
      property TransactionID : TffTransID read dbGetTransID;
        { The transaction active for this cursor.  If no transaction is
          active then returns zero. }
      property TransactionInfo : PffTransInfo read dbTI;
        { Returns a pointer to the cursor's transaction information. }
      property TransactionLSN : TffWord32 read dbGetTransLSN;
        { Returns the LSN of the transaction associated with the cursor. }
//    property ServerEngine : TffServerEngine read dbEngine;           {Deleted !!.03}
  end;

  TffSrDatabaseList = class(TffServerObjectList)
    protected {private}
    protected
      function GetDatabaseItem(Find : TffListFindType; Value : Longint) : TffSrDatabase;
    public
      procedure AddDatabase(aDatabase : TffSrDatabase);

      function DatabaseCount : integer;
      procedure DeleteDatabase(aDatabaseID : Longint);

      function GetDatabaseForFolder(aFolder : TffSrFolder) : TffSrDatabase;

      property Database [Find : TffListFindType; Value : Longint] : TffSrDatabase read GetDatabaseItem; default;
  end;

  TffSrSession = class(TffServerObject)
    protected {private}
      ssDatabaseList : TffSrDatabaseList;
      ssIsDefault    : boolean;
    protected
      function ssGetSessionID : TffSessionID;
    public
      constructor Create(aClient : TffSrClient; const aIsDef : boolean;
                         const aTimeout : Longint);
      destructor Destroy; override;

      function CanClose(const Mark : boolean) : boolean; override;

      procedure ForceClose; override;

      procedure RequestClose; override;                                {!!.03}

      function ShouldClose : boolean; override;

      property DatabaseList : TffSrDatabaseList read ssDatabaseList;
      property IsDefault : boolean read ssIsDefault;
      property SessionID : TffSessionID read ssGetSessionID;
  end;

  TffSrSessionList = class(TffServerObjectList)
    protected {private}
      slDefSess  : TffSrSession;
      slCurSess  : TffSrSession;
    protected
      function slGetCurSess : TffSrSession;
      function slGetSessionItem(Find : TffListFindType; Value : Longint) : TffSrSession;
      procedure slSetCurSess(CS : TffSrSession);
    public
      procedure AddSession(aSession : TffSrSession);

      procedure DeleteSession(aSessionID : Longint);

      function SessionCount : integer;
      procedure SetDefaultSession(aSession : TffSrSession);
      property CurrentSession : TffSrSession read slGetCurSess write slSetCurSess;
      property Session [Find : TffListFindType; Value : Longint] : TffSrSession read slGetSessionItem;
  end;

{Begin !!.10}
  TffBasePreparedStmt = class(TffServerObject)
  protected
    bpsClientID : TffClientID;
    bpsDatabaseID: TffDatabaseID;
    bpsEngine : TffServerEngine;
  public
    procedure Bind; virtual; abstract;                                 {!!.11}
    function Execute(var aLiveResult: Boolean;
                     var aCursorID: TffCursorID;
                     var aRowsAffected: Integer;
                     var aRecordsRead: Integer): TffResult; virtual; abstract;

    function Parse(aQuery: PChar): Boolean; virtual; abstract;

    property ClientID : TffClientID
      read bpsClientID;
      { ID of owning client. }

    property DatabaseID : TffDatabaseID
      read bpsDatabaseID;
      { ID of owning database. }

    property Engine : TffServerEngine
      read bpsEngine;

    property Handle: LongInt
      read KeyAsInt;
      { Statement handle. }
  end;

  TffSrStmtList = class(TffServerObjectList)
    protected
      function GetStmt(Find : TffListFindType; Value : Longint) : TffBasePreparedStmt;
    public
      procedure AddStmt(aStmt : TffBasePreparedStmt);

      procedure DeleteStmt(aStmtID : TffSQLStmtID);

      procedure RemoveForClient(const aClientID : TffClientID);
        {-Removes all prepared statements associated with a particular client. }

      function StmtCount : integer;

      property Stmt [Find : TffListFindType; Value : Longint] : TffBasePreparedStmt
        read GetStmt; default;
    end;
{End !!.10}

  TffSrClient = class(TffServerObject)
    protected {private}
      clAccepted    : boolean;
      clClientName  : PffShStr;
      clEngine      : TffServerEngine;
      clExtenders   : TffThreadList;
      clSessionList : TffSrSessionList;
      clUserID      : TffName;
      clFirst       : TffName;
      clLast        : TffName;
      clRights      : TffUserRights;
      clFirstSession: TffSrSession;                                    {!!.03}
      clClientVersion : Longint;                                       {!!.11}
    protected
      function clGetClientID : TffClientID;
      function clGetClientName : TffNetName;
    public
      constructor Create(aClientID   : Longint;
                   const aClientName : TffNetName;
                   const aTimeout    : Longint;
                   const aClientVersion : Longint;               {!!.11}
                         aUser       : TffUserItem;
                         anEngine    : TffServerEngine);
      destructor Destroy; override;

      procedure AddClientExtender(anExtender : TffBaseEngineExtender);
        {-Use this method to add an extender to the list of extenders
          interested in clients. }

      function CanClose(const Mark : boolean) : boolean; override;

      procedure ForceClose; override;

      function NotifyExtenders(const anAction    : TffEngineAction;
                               const aFailAction : TffEngineAction) : TffResult;
        {-Use this method to notify client extenders about a client-related
          action. }

      procedure RequestClose; override;                                {!!.03}

      function ShouldClose : boolean; override;

      property Accepted : boolean read clAccepted write clAccepted;
        { Returns True if the client was accepted by the client extender(s). }
      property ClientID : TffClientID read clGetClientID;
      property ClientVersion : Longint read clClientVersion;           {!!.11}
      property ClientName : TffNetName read clGetClientName;
      property Rights : TffUserRights read clRights;
      property SessionList : TffSrSessionList read clSessionList;
  end;

  TffSrClientList = class(TffServerObjectList)
    protected {private}
    protected
      function GetClientItem(Find : TffListFindType; Value : Longint) : TffSrClient;
      procedure SetClientItem(Inx : integer; CI : TffSrClient);
    public
      procedure AddClient(aClient : TffSrClient);

      function ClientCount : integer;
      procedure DeleteClient(aClientID : Longint);

      property Client [Find : TffListFindType; Value : Longint] : TffSrClient read GetClientItem;
  end;

  PffSrRebuildParams = ^TffSrRebuildParams;
  TffSrRebuildParams = record
    rpDB               : TffSrDatabase;
    rpTableName        : TffTableName;
    rpIndexName        : TffName;
    rpIndexID          : Longint;
    rpRebuildStatus    : TffSrRebuildStatus;
    rpCursor           : TffSrCursor;
    rpTargetCursor     : TffSrCursor;
    rpFieldMap         : TffSrFieldMapList;
  end;

  TffServerEngine = class(TffIntermediateServerEngine)
  private
    protected {public}
      seCursorClass   : TffSrCursorClass;                              {!!.06}
      seBufMgr        : TffBufferManager;
      seCanLog        : Boolean;  { If True then can write to event log. }
      seClientHash    : TffHash;                                       {!!.02}
      seConfig        : TffServerConfiguration;
      seConfigLoaded  : Boolean;  { True if config tables have been loaded. }
      seGarbageThread : TffTimerThread;
      seLastFlush     : DWORD;                                         {!!.01}
      seRebuildList   : TffSrRebuildStatusList;
      seStartTime     : DWORD;                                         {!!.10}
      seUniqueID      : TGUID;                                         {!!.10}

      seClientList     : TffSrClientList;
      seConfigDir      : TffPath;
        { The location of the server tables for this server engine.
          IMPORTANT NOTE: When retrieving this value, use the ConfigDir property
          or the seGetConfigDir method directly as this method determines the
          correct config dir for the server if the config dir has not been
          specified (i.e., is set to ''). }
      seCursorList     : TffSrCursorList;
      seDatabaseList   : TffSrDatabaseList;
      seFolderList     : TffSrFolderList;
      seOnRecoveryCheck : TNotifyEvent;
        { Handler called when it is time to check for recovery. }
      seScriptFile     : TffFullFileName;
      seSessionList    : TffSrSessionList;
      seSQLEngine      : TffBaseSQLEngine;
      seTableList      : TffSrTableList;

      seEvtClientDone : TffEvent;
        {This event is used to notify a server when a client is done
         processing during server shutdown. This event is nill except
         when shutting down.}

      function seTransactionStart(const aDB                  : TffSrDatabase;
                                  const aFailSafe, aImplicit : boolean;
                                    var aTransactionID       : TffTransID) : TffResult;
        {-starts a transaction based on aImplicit setting}

      function seTransactionCommitSubset(const aDB : TffSrDatabase) : TffResult;
{Begin !!.11}
      function seClientAddPrim(var aClientID   : TffClientID;
                             const aClientName : TffNetName;
                             const aUserID     : TffName;
                             const aTimeout     : Longint;
                             const aClientVersion : Longint;
                               var aHash       : TffWord32) : TffResult;
{End !!.11}
      procedure seClientRemovePrim(const aClient : TffSrClient);
      function seConvertSingleField(aSourceBuf,
                                     aTargetBuf: PffByteArray;
                                     aSourceCursorID,
                                     aTargetCursorID: Longint;
                                     aSourceFldNr,
                                     aTargetFldNr: Integer;
                                     aBLOBBuffer: Pointer;
                                     aBLOBBufLen: Longint): TffResult;
      function seDatabaseAliasListPrim(aList : TList) : TffResult;
      function seDatabaseDeleteAliasPrim(aAlias : TffName) : TffResult;
      function seDatabaseGetAliasPathPrim(aAlias : TffName;
                                      var aPath  : TffPath) : TffResult;
      function seDeleteTable(const aDB        : TffSrDatabase;
                             const aTableName : TffTableName) : TffResult;
      function seGetConfig : TffServerConfiguration;
      function seGetDictionary(const aDB        : TffSrDatabase;
                               const aTableName : TffTableName;
                               var   aDict      : TffDataDictionary) : TffResult;
      function seIsServerTable(const aTableName : TffTableName) : Boolean;
      function seGetCollectFrequency : Longint;
      function seGetCollectGarbage : Boolean;
      function seGetConfigDir : string;                                {!!.10}
      function seGetMaxRAM : Longint;                                  {!!.01}
      function seGetScriptFile : string;                               {!!.11}
      procedure seSetCollectFrequency(aFreq : Longint);
      procedure seSetCollectGarbage(aValue : Boolean);
      procedure seSetConfigDir(const aPath : string);                  {!!.10}
      procedure seSetMaxRAM(const aValue: Longint);                    {!!.01}
      procedure seSetScriptFile(const aFile : string);                 {!!.11}
      function seTableBuildPrim(aDB        : TffSrDatabase;
                                 aOverwrite : Boolean;
                           const aTableName : TffTableName;
                                 aForServer : Boolean;
                                 aDict      : TffDataDictionary) : TffResult;
      function seTableDeletePrim(DB : TffSrDatabase;
                            const aTableName : TffTableName) : TffResult;
      function seTableExistsPrim(aDB : TffSrDatabase;                  {!!.11}
                           const aTableName: TffTableName) : Boolean;  {!!.11}
      function seTablePackPrim(aRebuildParamsPtr: PffSrRebuildParams): TffResult;
      function seTableRebuildIndexPrim(aRebuildParamsPtr: PffSrRebuildParams): TffResult;
      function seTableGetRecordCountPrim(aRebuildParamsPtr : PffSrRebuildParams) : TffResult;  { !!.10}
      function seTransactionCommit(aDB : TffSrDatabase) : TffResult;
      function seTransactionRollback(aDB : TffSrDatabase) : TffResult;
    protected
      {validation and checking}

      { The Check*IDAndGet routines are responsible for checking the
        engine state to make sure it is ffesStarted.  The
        seCheck*IDAndGet avoid checking the engine state.
        WARNING: Ensure changes are made to Check*IDAndGet and
                 seCheck*IDAndGet }
//    function CheckClientIDAndGet(aClientID : TffClientID;            {!!.01 - Start}
//                             var aClient   : TffSrClient)            {Moved to Public section}
//                                           : TffResult;              {!!.01 - End}
      function seCheckClientIDAndGet(aClientID : TffClientID;
                                 var aClient : TffSrClient) : TffResult;
//    function CheckSessionIDAndGet(aClientID  : TffClientID;          {!!.01 - Start}
//                                  aSessionID : TffSessionID;         {Moved to Public section}
//                              var aClient    : TffSrClient;          {!!.01 - End}
//                              var aSession   : TffSrSession) : TffResult;
      function seCheckSessionIDAndGet(aSessionID : TffSessionID;
                                  var aSession   : TffSrSession) : TffResult;
//    function CheckTransactionIDAndGet(aTransactionID : TffTransID;   {!!.01 - Start}
//                                  var aTrans         : TffSrTransaction) {Moved to Public section}
//                                                     : TffResult;    {!!.01 - End}
      function seCheckCursorIDAndGet(aCursorID : TffCursorID;
                                 var aCursor   : TffSrBaseCursor) : TffResult;
        {-Find the cursor specified by aCursorID. }

      function seCheckDatabaseIDAndGet(aDatabaseID : TffDatabaseID;
                                   var aDatabase   : TffSrDatabase) : TffResult;
      function GetTableInstance(aFolder    : TffSrFolder;
                           const aTableName : TffTableName) : TffSrBaseTable;
      function IsTableNameOpen(aFolder    : TffSrFolder;
                          const aTableName : TffTableName) : boolean;

      {rebuild status related stuff}
      function RebuildRegister(aClientID     : TffClientID;
                                aTotalRecords : Longint) : TffSrRebuildStatus;
      procedure RebuildDeregister(aRebuildID : Longint);

      function seBLOBCopy(aSrc, aTgt                   : TffSrBaseCursor;
                           aSourceBLOBNr, aTargetBLOBNr : TffInt64;
                           aBuffer                      : pointer;
                           aBufLen                      : Longint): TffResult;
      function seDatabaseAddAliasPrim(const aAlias      : TffName;
                                      const aPath       : TffPath;
                                            aCheckSpace : Boolean)      {!!.11}
                                                        : TffResult;
      function seDatabaseOpenPrim(Session      : TffSrSession;
                                   Folder      : TffSrFolder;
                                   anAlias     : TffName;
                                   aOpenMode   : TffOpenMode;
                                   aShareMode  : TffShareMode;
                                   aTimeout    : Longint;
                                   aCheckSpace : Boolean)              {!!.11}
                                               : TffSrDatabase;
        {-Used by the public DatabaseOpenxx methods and used to open system
          tables. }

      function seTableRenamePrim(DB : TffSrDatabase;
                            const aOldName, aNewName : TffName) : TffResult;

      function RecordGetNextSeq(aCursorID : TffCursorID; var aRefNr : TffInt64; aData : PffByteArray) : TffResult;

      {index stuff}
      function IndexClear(aCursorID : TffCursorID) : TffResult;

      {misc stuff}
      procedure CreateAdminUser(SaveToDisk : Boolean);
        {-create the default administrator user}
      procedure ReadAliasData;
        {-read the aliases from the FFSALIAS.FFD table}
      procedure ReadGeneralInfo;
        {-read the general info from the FFSINFO.FFD table}
      procedure ReadKeyProcData;
        {-read the user-defined index data from the FFSINDEX.FFD table}
      procedure ReadUserData;
        {-read the user data from the FFSUSER.FFD table}

    protected

      {State methods}
      procedure scInitialize; override;
      procedure scPrepareForShutdown; override;
      procedure scShutdown; override;
      procedure scStartup; override;

      { Property methods }
      function bseGetAutoSaveCfg : Boolean; override;
      function bseGetReadOnly : Boolean; override;
      procedure bseSetAutoSaveCfg(aValue : Boolean); override;         {!!.01}
      procedure bseSetReadOnly(aValue : Boolean); override;            {!!.01}
      procedure lcSetEventLog(anEventLog : TffBaseLog); override;
      procedure lcSetLogEnabled(const aEnabled : boolean); override;

      { Misc }

      procedure seCleanRebuildList(const aClientID : TffClientID); virtual;
        {-Remove all entries in the rebuild status list for the specified
          client. }

      procedure seCollectGarbage(const aTimerEventCookie : Longint); virtual;
        {-Looks for clients, sessions, databases, cursors, tables, &
          folders that should be closed & freed. }

      procedure seLoadConfig;
        {-Reads in the server configuration tables and processes the
          server script file (if present). }

      procedure seForce(const aMsg     : string;                       {!!.06 - Start}
                              args     : array of const;
                              ReadOnly : Boolean); virtual;            {!!.06 - End}
        {-Use this method to log a formatted string to the event log. Writes to
          the log whether or not logging is enabled. }

      function seGetServerName : TffNetName;
        {-Returns the server's name from its configuration. }

      procedure seSetLoggingState;
        {-Called whenever something is changed that would affect logging.
          Sets a boolean flag that tells the logging routines whether or not
          they can log.  We centralize the logic here so that the logging
          routines don't have to do the checks each time they are called. }

      procedure seSetSQLEngine(anEngine : TffBaseSQLEngine);
        {-Used to set the SQLEngine property of the server engine. }

      {script stuff}
      function CalcPriorityIndex(const PriorityStr : TffShStr) : integer;
      function CalcKeyIndex(const KeyStr : TffShStr) : integer;
      function ValBoolean(const BoolStr : TffShStr;
                                    var   BoolValue : boolean) : boolean;
      procedure ProcessAliasScript;
        {process the FFALIAS.SC$ script file to autocreate aliases}
      procedure ProcessFullScript(const ScriptFileName : TffFullFileName);
        {process a server script file to set general info & aliases}
      procedure ProcessScriptCommand(const KeyStr, ValueStr : TffShStr;
                                       var DeleteScript : Boolean);

    public
      {creation/destruction}
      constructor Create(aOwner : TComponent); override;
      destructor Destroy; override;

      procedure FFNotificationEx(const AOp : Byte; AFrom : TffComponent; {!!.11}
                                 const AData : TffWord32); override;     {!!.11}
        { When the freeing of seSQLEngine is detected, this method
          sets seSQLEngine to nil to avoid using the freed TffBaseSQLEngine. }

      { Event logging }
      procedure Log(const aMsg : string); override;
        {-Use this method to log a string to the event log. }

      procedure LogAll(const Msgs : array of string); override;
        {-Use this method to log multiple strings to the event log. }

      procedure LogFmt(const aMsg : string; args : array of const); override;
        {-Use this method to log a formatted string to the event log. }

      { Object validation }
      function CheckCursorIDAndGet(aCursorID : TffCursorID;
                               var aCursor   : TffSrBaseCursor)
                                             : TffResult;
        {-Find the cursor specified by aCursorID. }
      function CheckDatabaseIDAndGet(aDatabaseID : TffDatabaseID;
                                 var aDatabase   : TffSrDatabase)
                                                 : TffResult;
        {-Find the database specified by aDatabaseID. }
      function CheckClientIDAndGet(aClientID : TffClientID;            {!!.01 - Start}
                               var aClient   : TffSrClient)            {Moved from Public section}
                                             : TffResult;
      function CheckSessionIDAndGet(aClientID  : TffClientID;          {Moved from Public section}
                                    aSessionID : TffSessionID;
                                var aClient    : TffSrClient;
                                var aSession   : TffSrSession)
                                               : TffResult;
      function CheckTransactionIDAndGet(aTransactionID : TffTransID;   {Moved from Public section}
                                    var aTrans         : TffSrTransaction)
                                                       : TffResult;    {!!.01 - End}

      procedure GetServerNames(aList    : TStrings;
                               aTimeout : Longint); override;

      {transaction tracking}
      function TransactionCommit(const aDatabaseID : TffDatabaseID) : TffResult; override;
{Begin !!.01}
      function TransactionCommitSQL(const aDatabaseID : TffDatabaseID;
                                    const notifyExtenders : Boolean) : TffResult;
        { Commit transaction for SQL engine. Does not reset timeout and controls
          extender notification. }
{End !!.01}
      function TransactionCommitSubset(const aDatabaseID : TffDatabaseID) : TffResult;
      function TransactionRollback(const aDatabaseID : TffDatabaseID) : TffResult; override;
{Begin !!.01}
      function TransactionRollbackSQL(const aDatabaseID : TffDatabaseID;
                                      const notifyExtenders : Boolean) : TffResult;
        { Rollback transaction for SQL engine. Does not reset timeout and
          controls extender notification. }
{End !!.01}
      function TransactionStart(const aDatabaseID    : TffDatabaseID;
                                 const aFailSafe      : boolean) : TffResult; override;
        {-starts an explicit transaction}
{Begin !!.01}
      function TransactionStartSQL(const aDatabaseID : TffDatabaseID;
                                   const notifyExtenders : boolean) : TffResult;
        { For use by the SQL engine. Starts a transaction without resetting
          the timeout & controls notification of extenders. }
{End !!.01}

{Begin !!.10}
      function TransactionStartWith(const aDatabaseID : TffDatabaseID;
                                    const aFailSafe : Boolean;
                                    const aCursorIDs : TffPointerList) : TffResult; override;
{End !!.10}

      {client related stuff}
      function ClientAdd(var aClientID   : TffClientID;
                        const aClientName : TffNetName;
                        const aUserID     : TffName;
                        const aTimeout    : Longint;
                          var aHash       : TffWord32) : TffResult; override;
{Begin !!.11}
      function ClientAddEx(var aClientID   : TffClientID;
                         const aClientName : TffNetName;
                         const aUserID     : TffName;
                         const aTimeout     : Longint;
                         const aClientVersion : Longint;
                           var aHash       : TffWord32) : TffResult; override;
        { Same as ClientAdd but client version is supplied via the aClientVersion
          parameter. }
{End !!.11}
      function ClientRemove(aClientID : TffClientID) : TffResult; override;
      function ClientSetTimeout(const aClientID : TffClientID;
                                 const aTimeout : Longint) : TffResult; override;

      {client session related stuff}
      function SessionAdd(const aClientID  : TffClientID; const timeout : Longint;
                             var aSessionID : TffSessionID) : TffResult; override;
      function SessionCloseInactiveTables(aClientID : TffClientID) : TffResult; override;  {!!.06}
      function SessionCount(aClientID : TffClientID; var aCount : integer) : TffResult; override;
      function SessionGetCurrent(aClientID : TffClientID; var aSessionID : TffSessionID) : TffResult; override;
      function SessionRemove(aClientID : TffClientID; aSessionID : TffSessionID) : TffResult; override;
      function SessionSetCurrent(aClientID : TffClientID; aSessionID : TffSessionID) : TffResult; override;
      function SessionSetTimeout(const aClientID  : TffClientID;
                                  const aSessionID : TffSessionID;
                                  const aTimeout   : Longint) : TffResult; override;

      {database related stuff}
      function DatabaseAddAlias(const aAlias      : TffName;
                                const aPath       : TffPath;
                                      aCheckSpace : Boolean;            {!!.11}
                                const aClientID   : TffClientID)
                                                  : TffResult; override;
      function DatabaseAliasList(aList     : TList;
                                 aClientID : TffClientID) : TffResult; override;
      function RecoveryAliasList(aList     : TList;
                                 aClientID : TffClientID) : TffResult; override;
        {-Return a list of database aliases for use by a journal recovery
          engine. The functionality of this method is identical to
          DatabaseAliasList except that it does not require the server engine
          to be started. }
      function DatabaseChgAliasPath(aAlias      : TffName;
                                    aNewPath    : TffPath;
                                    aCheckSpace : Boolean;              {!!.11}
                                    aClientID   : TffClientID)
                                                : TffResult; override;
      function DatabaseClose(aDatabaseID : TffDatabaseID) : TffResult; override;
      function DatabaseDeleteAlias(aAlias    : TffName;
                                   aClientID : TffClientID) : TffResult; override;
      function DatabaseGetAliasPath(aAlias    : TffName;
                                var aPath     : TffPath;
                                    aClientID : TffClientID) : TffResult; override;
      function DatabaseGetFreeSpace(const aDatabaseID : TffDatabaseID;
                                      var aFreeSpace  : Longint) : TffResult; override;
      function DatabaseModifyAlias(const aClientID   : TffClientID;
                                   const aAlias      : TffName;
                                   const aNewName    : TffName;
                                   const aNewPath    : TffPath;
                                         aCheckSpace : Boolean)        {!!.11}
                                                     : TffResult; override;
      function DatabaseOpen(aClientID   : TffClientID;
                      const aAlias      : TffName;
                      const aOpenMode   : TffOpenMode;
                      const aShareMode  : TffShareMode;
                      const aTimeout    : Longint;
                        var aDatabaseID : TffDatabaseID) : TffResult; override;
      function DatabaseOpenNoAlias(aClientID   : TffClientID;
                             const aPath       : TffPath;
                             const aOpenMode   : TffOpenMode;
                             const aShareMode  : TffShareMode;
                             const aTimeout    : Longint;
                               var aDatabaseID : TffDatabaseID) : TffResult; override;
      function DatabaseSetTimeout(const aDatabaseID : TffDatabaseID;
                                  const aTimeout    : Longint) : TffResult; override;
      function DatabaseTableExists(aDatabaseID : TffDatabaseID;
                             const aTableName  : TffTableName;
                               var aExists     : Boolean) : TffResult; override;
      function DatabaseTableList(aDatabaseID : TffDatabaseID;
                           const aMask       : TffFileNameExt;
                                 aList       : TList) : TffResult; override;
      function DatabaseTableLockedExclusive(aDatabaseID : TffDatabaseID;
                                      const aTableName  : TffTableName;
                                        var aLocked     : Boolean) : TffResult; override;
      {rebuild status related stuff}
      function RebuildGetStatus(aRebuildID : Longint;
                          const aClientID  : TffClientID;
                            var aIsPresent : boolean;
                            var aStatus    : TffRebuildStatus) : TffResult; override;

      {table related stuff}
      function TableAddIndex(const aDatabaseID : TffDatabaseID;
                             const aCursorID   : TffCursorID;
                             const aTableName  : TffTableName;
                             const aIndexDesc  : TffIndexDescriptor) : TffResult; override;
      function TableBuild(aDatabaseID : TffDatabaseID;
                          aOverWrite  : boolean;
                    const aTableName  : TffTableName;
                          aForServer  : boolean;
                          aDictionary : TffDataDictionary) : TffResult; override;
      function TableDelete(aDatabaseID : TffDatabaseID;
                     const aTableName  : TffTableName) : TffResult; override;
      function TableDropIndex(aDatabaseID : TffDatabaseID;
                              aCursorID   : TffCursorID;
                        const aTableName  : TffTableName;
                        const aIndexName  : TffDictItemName;
                              aIndexID    : Longint) : TffResult; override;
      function TableEmpty(aDatabaseID : TffDatabaseID;
                          aCursorID   : TffCursorID;
                    const aTableName  : TffTableName) : TffResult; override;
      function TableGetAutoInc(aCursorID   : TffCursorID;
                           var aValue      : TffWord32) : TffResult; override;
      function TableGetDictionary(aDatabaseID : TffDatabaseID;
                            const aTableName  : TffTableName;
                                  aForServer  : boolean;
                                  aStream     : TStream) : TffResult; override;
      function TableGetRecCount(aCursorID : TffCursorID;
                            var aRecCount : Longint) : TffResult; override;
      function TableGetRecCountAsync(aCursorID : TffCursorID;                  {!!.10}
                                 var aTaskID : Longint) : TffResult; override; {!!.10}
      function TableOpen(const aDatabaseID : TffDatabaseID;
                         const aTableName  : TffTableName;
                         const aForServer  : boolean;
                         const aIndexName  : TffName;
                               aIndexID    : Longint;
                         const aOpenMode   : TffOpenMode;
                               aShareMode  : TffShareMode;
                         const aTimeout    : Longint;
                           var aCursorID   : TffCursorID;
                               aStream     : TStream) : TffResult; override;
      function TablePack(aDatabaseID : TffDatabaseID;
                   const aTableName  : TffTableName;
                     var aRebuildID  : Longint): TffResult; override;
      function TableRebuildIndex(aDatabaseID : TffDatabaseID;
                           const aTableName  : TffTableName;
                           const aIndexName  : TffName;
                                 aIndexID    : Longint;
                             var aRebuildID  : Longint): TffResult; override;
      function TableRename(aDatabaseID : TffDatabaseID; const aOldName, aNewName : TffName) : TffResult; override;
      function TableRestructure(aDatabaseID : TffDatabaseID;
                          const aTableName  : TffTableName;
                                aDictionary : TffDataDictionary;
                                aFieldMap   : TffStringList;
                            var aRebuildID  : Longint): TffResult; override;
      function TableSetAutoInc(aCursorID   : TffCursorID;
                               aValue      : TffWord32) : TffResult; override;
{Begin !!.11}
      function TableVersion(aDatabaseID : TffDatabaseID;
                      const aTableName  : TffTableName;
                        var aVersion : Longint) : TffResult; override;
{End !!.11}

      {table locks via cursor}
      function TableIsLocked(aCursorID : TffCursorID; aLockType : TffLockType;
                         var aIsLocked : boolean) : TffResult; override;
      function TableLockAcquire(aCursorID : TffCursorID; aLockType : TffLockType) : TffResult; override;
      function TableLockRelease(aCursorID : TffCursorID; aAllLocks : Boolean) : TffResult; override;

      {cursor stuff}
      function CursorClone(aCursorID : TffCursorID; aOpenMode : TffOpenMode;
                       var aNewCursorID : TffCursorID) : TffResult; override;
      function CursorClose(aCursorID : TffCursorID) : TffResult; override;
      function CursorCompareBookmarks(aCursorID   : TffCursorID;
                                      aBookmark1,
                                      aBookmark2  : PffByteArray;
                                  var aCompResult : Longint) : TffResult; override;
{Begin !!.02}
      function CursorCopyRecords(aSrcCursorID,
                                aDestCursorID : TffCursorID;
                                aCopyBLOBs : Boolean) : TffResult; override;
{End !!.02}
      function CursorDeleteRecords(aCursorID : TffCursorID) : TffResult; override;  {!!.06}
      function CursorGetBookmark(aCursorID : TffCursorID; aBookmark : PffByteArray) : TffResult; override;

      function CursorGetBookmarkSize(aCursorID : TffCursorID;
                                 var aSize     : Integer) : TffResult; override;
      function CursorOverrideFilter(aCursorID   : Longint;
                                    aExpression : pCANExpr;
                                    aTimeout    : TffWord32) : TffResult; override;
      function CursorResetRange(aCursorID : TffCursorID) : TffResult; override;
      function CursorRestoreFilter(aCursorID : Longint) : TffResult; override;
      function CursorSetRange(aCursorID : TffCursorID;
                              aDirectKey : boolean;
                              aFieldCount1 : integer;
                              aPartialLen1 : integer;
                              aKeyData1    : PffByteArray;
                              aKeyIncl1    : boolean;
                              aFieldCount2 : integer;
                              aPartialLen2 : integer;
                              aKeyData2    : PffByteArray;
                              aKeyIncl2    : boolean) : TffResult; override;
      function CursorSetTimeout(const aCursorID : TffCursorID;
                                const aTimeout : Longint) : TffResult; override;
      function CursorSetToBegin(aCursorID : TffCursorID) : TffResult; override;
      function CursorSetToBookmark(aCursorID : TffCursorID; aBookmark : PffByteArray) : TffResult; override;
      function CursorSetToCursor(aDestCursorID : TffCursorID; aSrcCursorID : TffCursorID) : TffResult; override;
      function CursorSetToEnd(aCursorID : TffCursorID) : TffResult; override;
      function CursorSetToKey(aCursorID     : TffCursorID;
                              aSearchAction : TffSearchKeyAction;
                              aDirectKey    : boolean;
                              aFieldCount   : integer;
                              aPartialLen   : integer;
                              aKeyData      : PffByteArray) : TffResult; override;
      function CursorSwitchToIndex(aCursorID  : TffCursorID;
                                   aIndexName : TffDictItemName;
                                   aIndexID   : integer;
                                   aPosnOnRec : boolean) : TffResult; override;
      function CursorSetFilter(aCursorID   : TffCursorID;
                               aExpression : pCANExpr;
                               aTimeout    : TffWord32) : TffResult; override;

{Begin !!.03}
      function CursorListBLOBFreeSpace(aCursorID : TffCursorID;
                                 const aInMemory : Boolean;
                                       aStream : TStream) : TffResult; override;
{End !!.03}

      {record stuff}
      function RecordDelete(aCursorID : TffCursorID; aData : PffByteArray) : TffResult; override;
      function RecordDeleteBatch(aCursorID : TffCursorID;
                                 aBMCount  : Longint;
                                 aBMLen    : Longint;
                                 aData     : PffByteArray;
                                 aErrors   : PffLongintArray) : TffResult; override;
      function RecordExtractKey(aCursorID : TffCursorID; aData : PffByteArray; aKey : PffByteArray) : TffResult; override;
      function RecordGet(aCursorID : TffCursorID; aLockType : TffLockType; aData : PffByteArray) : TffResult; override;
      function RecordGetBatch(aCursorID : TffCursorID;
                              aRecCount : Longint;
                              aRecLen   : Longint;
                          var aRecRead  : Longint;
                              aData     : PffByteArray;
                          var aError    : TffResult) : TffResult; override;
      function RecordGetForKey(aCursorID   : TffCursorID;
                               aDirectKey  : boolean;
                               aFieldCount : integer;
                               aPartialLen : integer;
                               aKeyData    : PffByteArray;
                               aData       : PffByteArray;
                               aFirstCall  : Boolean) : TffResult; override;
      function RecordGetNext(aCursorID : TffCursorID; aLockType : TffLockType; aData : PffByteArray) : TffResult; override;
      function RecordGetPrior(aCursorID : TffCursorID; aLockType : TffLockType; aData : PffByteArray) : TffResult; override;
      function RecordInsert(aCursorID : TffCursorID; aLockType : TffLockType; aData : PffByteArray) : TffResult; override;
      function RecordInsertBatch(aCursorID : TffCursorID;
                                 aRecCount : Longint;
                                 aRecLen   : Longint;
                                 aData     : PffByteArray;
                                 aErrors   : PffLongintArray) : TffResult; override;
      function RecordIsLocked(aCursorID : TffCursorID; aLockType : TffLockType;
                          var aIsLocked : boolean) : TffResult; override;
      function RecordModify(aCursorID : TffCursorID; aData : PffByteArray; aRelLock : Boolean) : TffResult; override;
      function RecordRelLock(aCursorID : TffCursorID; aAllLocks : Boolean) : TffResult; override;

      {BLOB stuff}
      function BLOBCreate(aCursorID : TffCursorID;
                      var aBlobNr   : TffInt64) : TffResult; override;
      function BLOBDelete(aCursorID : TffCursorID; aBLOBNr : TffInt64) : TffResult; override;
{Begin !!.03}
      function BLOBListSegments(aCursorID : TffCursorID;
                                aBLOBNr : TffInt64;
                                aStream : TStream) : TffResult; override;
{End !!.03}
      function BLOBRead(aCursorID  : TffCursorID;
                        aBLOBNr    : TffInt64;
                        aOffset    : TffWord32;                        {!!.06}
                        aLen       : TffWord32;                        {!!.06}
                    var aBLOB;
                    var aBytesRead : TffWord32)                        {!!.06}
                                   : TffResult; override;
      function BLOBFree(aCursorID : TffCursorID; aBLOBNr : TffInt64;
                        readOnly : boolean) : TffResult; override;
      function BLOBGetLength(aCursorID : TffCursorID; aBLOBNr : TffInt64;
                         var aLength   : Longint) : TffResult; override;
      function BLOBTruncate(aCursorID : TffCursorID; aBLOBNr : TffInt64;
                             aBLOBLength : Longint) : TffResult; override;
      function BLOBWrite(aCursorID : TffCursorID; aBLOBNr : TffInt64;
                          aOffset : Longint;
                          aLen    : Longint;
                      var aBLOB    ) : TffResult; override;
      function FileBLOBAdd(aCursorID : TffCursorID;
                     const aFileName : TffFullFileName;
                       var aBLOBNr   : TffInt64) : TffResult; override;

      {query stuff}
      function SQLAlloc(aClientID : TffClientID;
                        aDatabaseID : TffDatabaseID;
                        aTimeout : Longint;
                    var aStmtID : TffSqlStmtID) : TffResult; override;
      function SQLExec(aStmtID : TffSqlStmtID;
                       aOpenMode : TffOpenMode;
                   var aCursorID : TffCursorID;
                       aStream : TStream) : TffResult; override;
      function SQLExecDirect(aClientID : TffClientID;
                             aDatabaseID : TffDatabaseID;
                             aQueryText : PChar;
                             aTimeout  : Longint;
                             aOpenMode : TffOpenMode;
                         var aCursorID : TffCursorID;
                             aStream : TStream) : TffResult; override;
      function SQLFree(aStmtID : TffSqlStmtID) : TffResult; override;
      function SQLPrepare(aStmtID    : TffSqlStmtID;
                          aQueryText : PChar;
                          aStream    : TStream) : TffResult; override;
      function SQLSetParams(aStmtID     : TffSqlStmtID;
                            aNumParams  : word;
                            aParamDescs : Pointer;
                            aDataBuffer : PffByteArray;
                            aDataLen    : integer;
                            aStream     : TStream) : TffResult; override;

      {misc stuff}
      function GetServerDateTime(var aDateTime : TDateTime) : TffResult; override;
                                                               {begin !!.10}
      function GetServerSystemTime(var aSystemTime : TSystemTime)
                                  : TffResult; override;
      function GetServerGUID(var aGUID : TGUID)
                            : TffResult; override;
      function GetServerID(var aUniqueID : TGUID)
                            : TffResult; override;
      function GetServerStatistics(var aStats : TffServerStatistics)
                                  : TffResult; override;
      function GetCommandHandlerStatistics(const aCmdHandlerIdx : Integer;
                                             var aStats : TffCommandHandlerStatistics)
                                          : TffResult; override;
      function GetTransportStatistics(const aCmdHandlerIdx : Integer;
                                      const aTransportIdx  : Integer;
                                        var aStats : TffTransportStatistics)
                                     : TffResult; override;
                                                                 {end !!.10}


      function WriteAliasData : TffResult;
        {-write the aliases to the FFSALIAS.FFD table}
      function WriteGeneralInfo(aOverrideRO : Boolean) : TffResult;
        {-write the general info to the FFSINFO.FFD table}
      function WriteKeyProcData : TffResult;
        {-write the user-defined index data to the FFSINDEX.FFD table}
      function WriteUserData : TffResult;
        {-write the user data to the FFSUSER.FFD table}

      {properties}
      property BufferManager : TffBufferManager read seBufMgr;
      property ClientList : TffSrClientList read seClientList;
      property Configuration : TffServerConfiguration
         read seGetConfig;
      property CursorList : TffSrCursorList read seCursorList;
      property DatabaseList : TffSrDatabaseList read seDatabaseList;
      property FolderList : TffSrFolderList read seFolderList;
      property ServerName : TffNetName read seGetServerName;
      property SessionList : TffSrSessionList read seSessionList;
      property TableList : TffSrTableList read seTableList;
      property CursorClass : TffSrCursorClass                          {!!.06}
        read seCursorClass
        write seCursorClass;

    published

      property CollectGarbage : Boolean
        read seGetCollectGarbage
        write seSetCollectGarbage
        default False;                                                 {!!.01}
        { If True then the server engine is to perform garbage collection. }

      property CollectFrequency : Longint
        read seGetCollectFrequency
        write seSetCollectFrequency
        default ffcl_CollectionFrequency;                              {!!.01}
        { The number of milliseconds between each garbage collection run by the
          server engine. }

      property ConfigDir : string                                      {!!.10}
        read seGetConfigDir
        write seSetConfigDir;

      property MaxRAM : Longint                                        {!!.01}
        read seGetMaxRAM                                               {!!.01}
        write seSetMaxRAM
        default 10;                                                    {!!.01}

      property OnRecoveryCheck : TNotifyEvent
        read seOnRecoveryCheck
        write seOnRecoveryCheck;
        { Called when the server engine is initializing and it is time to
          check for recovery of fail-safe transactions. }

      property ScriptFile : string                                     {!!.11}
        read seGetScriptFile
        write seSetScriptFile;

      property SQLEngine : TffBaseSQLEngine
        read seSQLEngine
        write seSetSQLEngine;

  end;

var
  ffc_AdminUserID : string[5];

implementation

uses
  TypInfo,
  ActiveX,
  ffllcomp,
  ffllcomm,
  ffsrjour,                                                            {!!.06}
//  ffsqleng,                                                          {Deleted !!.03}
  ffsrsort;

const
  ffc_NumBLOBBytesToCopy = ffcl_1MB;
    { When copying BLOBs from one cursor to another, this is the initial number
      of bytes to read from the source BLOB. }
  ffcl_FlushRate = 5 * 60 * 1000;                                      {!!.01}
    { Flush memory pools and other pools every 5 minutes. }            {!!.01}

resourceString
  ffcTable = 'table %s';
  ffcTableContent = 'content of table ''%s''';

{===Utility functions================================================}
function FFMapLock(const aClientLock : TffLockType;
                   const isTableLock : boolean) : TffSrLockType;
  {-Map a client lock type to a server lock type. }
begin
  Result := ffsltNone;
  if isTableLock then
    case aClientLock of
      ffltNoLock    : Result := ffsltShare;
      ffltReadLock  : Result := ffsltIntentS;
      ffltWriteLock : Result := ffsltExclusive;
    end  { case }
  else
    if aClientLock = ffltWriteLock then
      Result := ffsltExclusive
    else
      Result := ffsltNone;
end;

{====================================================================}


type
  { Base thread class for rebuild operations }
  TffSrRebuildBaseThread = class(TffThread)
    protected { private }
    protected
      rthServerEngine: TffServerEngine;
      rthParams: PffSrRebuildParams;                                   {!!.13}
    public
      constructor Create(aServerEngine     : TffServerEngine;
                         aRebuildParamsPtr : PffSrRebuildParams);
      destructor Destroy; override;
    end;

  { Thread class for table reindexing operation }
  TffSrReindexThread = class(TffSrRebuildBaseThread)
  protected
    procedure Execute; override;
  end;

  { Thread class for table packing operation }
  TffSrPackThread = class(TffSrRebuildBaseThread)
  protected
    procedure Execute; override;
  end;

  { Thread class for table restructure operation }
  TffSrRestructureThread = class(TffSrRebuildBaseThread)
  protected
    procedure Execute; override;
  end;

{Begin !!.10}
  { Thread class for asynchronous record count }
  TffSrGetRecordCountThread = class(TffSrRebuildBaseThread)
  protected
    procedure Execute; override;
  end;
{End !!.10}

{===TffSrReindexThread====================================================}
constructor TffSrRebuildBaseThread.Create(
                                     aServerEngine     : TffServerEngine;
                                     aRebuildParamsPtr : PffSrRebuildParams);
begin
  rthServerEngine := aServerEngine;
  rthParams := aRebuildParamsPtr;
//  Dispose(aRebuildParamsPtr);                                        {Deleted !!.13}

  inherited Create(False);
  FreeOnTerminate := True;
end;

destructor TffSrRebuildBaseThread.Destroy;
begin
{Begin !!.13}
  if Assigned(rthParams.rpFieldMap) then
    rthParams.rpFieldMap.Free;
  FFFreeMem(rthParams, SizeOf(rthParams^));
{End !!.13}
  inherited Destroy;
end;
{--------}
procedure TffSrReindexThread.Execute;
begin
  rthServerEngine.seTableRebuildIndexPrim(rthParams);                  {!!.13}
end;
{--------}
procedure TffSrPackThread.Execute;
begin
  rthServerEngine.seTablePackPrim(rthParams);                          {!!.13}
end;
{--------}
procedure TffSrRestructureThread.Execute;
begin
  { Because we are passing a field map within the rebuild parameters,
    TablePackPrim knows that we are doing a restructure. }
  rthServerEngine.seTablePackPrim(rthParams);                          {!!.13}
end;
{Begin !!.10}
{--------}
procedure TffSrGetRecordCountThread.Execute;
begin
  rthServerEngine.seTableGetRecordCountPrim(rthParams);                {!!.13}
end;
{End !!.10}
{====================================================================}

{===TffServerObject==================================================}
constructor TffServerObject.Create(const aTimeout : Longint);
begin
  inherited Create;
  soState := ffosInactive;
  soTimeout := aTimeout;
end;
{--------}
destructor TffServerObject.Destroy;
begin
  inherited Destroy;
end;
{--------}
function TffServerObject.Activate : boolean;
begin
  if soState in [ffosInactive, ffosActive] then begin
    if soClient = nil then
      soLock.Lock
    else
      soClient.soLock.Lock;
    soState := ffosActive;
    Result := True;
  end
  else
    Result := False;
end;
{--------}
function TffServerObject.CanClose(const Mark : boolean) : boolean;
begin
  Result := (soState = ffosInactive) or (soState = ffosClosing);
    { Note: If the state is ffosClosePending then the object is active &
            will be freed once it has completed.  Until then we have to
            leave it alone. }
  if (soState = ffosInactive) and Mark then
    soState := ffosClosing;
end;
{--------}
procedure TffServerObject.Deactivate;
begin
  case soState of
    ffosActive :
      soState := ffosInactive;
    ffosClosePending :
      begin
        soState := ffosClosing;
        if Self.CanClose(True) then
          Self.Free;
      end;
  end;  { case }
  if soClient = nil then
    soLock.Unlock
  else
    soClient.soLock.Unlock;
end;
{--------}
procedure TffServerObject.ForceClose;
begin
  soState := ffosClosing;
end;
{--------}
procedure TffServerObject.RequestClose;
begin
  if soState = ffosActive then
    soState := ffosClosePending
  else if soState = ffosInactive then
    soState := ffosClosing;
end;
{--------}
function TffServerObject.ShouldClose : boolean;
begin
  Result := (soState = ffosClosing);
end;
{====================================================================}

{===TffServerObjectList==============================================}
constructor TffServerObjectList.Create;
begin
  inherited Create;
  solList := TffThreadList.Create;
end;
{--------}
destructor TffServerObjectList.Destroy;
begin
  solList.Free;
  inherited Destroy;
end;
{--------}
procedure TffServerObjectList.BeginRead;
begin
  solList.BeginRead;
end;
{--------}
procedure TffServerObjectList.BeginWrite;
begin
  solList.BeginWrite;
end;
{--------}
function TffServerObjectList.CanClose(const Mark : boolean) : boolean;
var
  Inx : Longint;
begin
  Result := True;
  for Inx := 0 to pred(solList.Count) do begin
    { If any one of the objects cannot be closed then return False.
      Note we have the option to tell each Inactive object to mark itself
      as closed.  This makes sure the object is unavailable until we actually
      free it. Note that we must call CanClose on each object. If we break
      out of this loop early, an object that should be closed may never
      be marked as closable. }
    if (not TffServerObject(solList[Inx]).CanClose(Mark)) then
      Result := False;
  end;
end;
{--------}
procedure TffServerObjectList.EndRead;
begin
  solList.EndRead;
end;
{--------}
procedure TffServerObjectList.EndWrite;
begin
  solList.EndWrite;
end;
{--------}
procedure TffServerObjectList.ForceClose;
var
  Inx : Longint;
begin
  for Inx := 0 to pred(solList.Count) do
    TffServerObject(solList[Inx]).ForceClose;
end;
{Begin !!.06}
{--------}
function TffServerObjectList.HasClosableState(const Mark : Boolean) : boolean;
var
  Inx : Longint;
begin
  Result := True;
  for Inx := 0 to pred(solList.Count) do begin
    { If any one of the objects cannot be closed then return False. }
    if not (TffServerObject(solList[Inx]).State in
            [ffosInactive, ffosClosing]) then begin
      Result := False;
      Break;
    end;
  end;

  { If all objects are in a closable state and we are to mark them as being
    closed then do so. }
  if Result and Mark then
    for Inx := 0 to pred(solList.Count) do
      if TffServerObject(solList[Inx]).State = ffosInactive then
        TffServerObject(solList[Inx]).State := ffosClosing;

end;
{End !!.06}
{--------}
procedure TffServerObjectList.RemoveUnused;
var
  Index : Longint;
begin
  solList.BeginWrite;
  try
    for Index := pred(solList.Count) downto 0 do
{Begin !!.05}
      try
        if TffServerObject(solList[Index]).ShouldClose then
          solList.DeleteAt(Index);
      except
        { If an exception occurred then it is most likely because the object
          has already been deleted. Remove the invalid object from the list. }
        solList.RemoveAt(Index);
      end;
{End !!.05}
  finally
    solList.EndWrite;
  end;
end;
{Begin !!.03}
{--------}
procedure TffServerObjectList.RequestClose;
var
  Inx : Longint;
begin
  for Inx := 0 to pred(solList.Count) do
    TffServerObject(solList[Inx]).RequestClose;
end;
{End !!.03}
{--------}
function TffServerObjectList.ShouldClose : boolean;
var
  Inx : Longint;
begin
  Result := True;
  for Inx := 0 to pred(solList.Count) do begin
    { If any one of the objects cannot be closed then return False. }
    if (not TffServerObject(solList[Inx]).ShouldClose) then begin
      Result := False;
      break;
    end;
  end;
end;
{====================================================================}

{===TffSrBaseCursor==================================================}
constructor TffSrBaseCursor.Create(anEngine   : TffServerEngine;
                                   aDatabase  : TffSrDatabase;
                             const aTimeout   : Longint);
begin
  inherited Create(aTimeout);
  soClient := aDatabase.Client;
  bcCloseTable := False;
  bcCloseWTrans := False;                                              {!!.05}
  bcDatabase := aDatabase;
  bcEngine := anEngine;
  bcExclOwner := False;
  bcExtenders := nil;
  bcInfoLock := TffPadlock.Create;                                     {!!.06}
  bcTable := nil;
  bcTempStore := nil;
  bcNumReadLocks := 0;                                                 {!!.05}
end;
{Begin !!.01}
{--------}
function TffSrBaseCursor.CanClose(const Mark : Boolean) : Boolean;
begin
  { Cursor can be closed if it is not in a transaction or if the table is
    temporary. }
  Result := (bcDatabase.Transaction = nil) or
            (fffaTemporary in bcTable.Files[0].fiAttributes);
  if Result then
    Result := inherited CanClose(Mark)                                 {!!.05 - Start}
  else if (bcDatabase.Transaction <> nil) then
    bcCloseWTrans := True;                                             {!!.05 - End}
end;
{End !!.01}
{--------}
procedure TffSrBaseCursor.Open(const aTableName : TffTableName;
                               const aIndexName : TffName;
                               const aIndexID   : Longint;
                               const aOpenMode  : TffOpenMode;
                                     aShareMode : TffShareMode;
                                     aForServer : boolean;
                               const aExclContLock : Boolean;          {!!.10}
                                     aAttribs   : TffFileAttributes);
var
  aLockType : TffSrLockType;
  NewTable : boolean;
  OpenIntentRegistered : Boolean;
begin
  bcIndexID := aIndexID;

  NewTable := False;
  OpenIntentRegistered := False;

  { The cursor references an instance of TffSrBaseTable. Multiple cursors may
    reference that same instance of TffSrBaseTable since only 1 instance of
    TffSrBaseTable is created per physical table (saves on file handles).

    So we must determine whether the table has already been opened by
    another cursor.  But first, we must obtain write access on the engine's
    table list.  Why?

    1. If the table has not been opened, we don't want two threads trying
       to open it at the same time.  Should that occur, we would wind
       up with duplicate tables in our table list.

    2. If the table is already open, we don't thread A closing the table
       while thread B is trying to "open" the table.  Good recipe for an
       access violation.

    Complication: If the table is open and locked, and this thread wants
      to open the table in an incompatible lock mode, we must make sure
      the table list is available to other threads.  Otherwise, we will
      freeze the server. }
  bcEngine.TableList.BeginWrite;
  try
    { Try & find the open table in the engine's table list. If it exists already
      then reference the existing table. }
    bcTable := bcEngine.GetTableInstance(bcDatabase.Folder, aTableName);

    { Is the table open? }
    if assigned(bcTable) then begin
      { Yes. Register our intent to open the table.  This prevents another
        thread from freeing the table. }
      bcTable.RegisterOpenIntent;
      OpenIntentRegistered := True;

      { Release our lock on the table list.  We must do so because our
        request for a lock on the table may cause this thread to wait.
        Retaining the lock in such a situation would freeze any threads
        wanting access to the table list. }
      bcEngine.TableList.EndWrite;

      { Determine the type of lock for the table, based upon the Open mode and
        Share mode. }
      if (aShareMode = smExclusive) then
        aLockType := ffsltExclusive
      else if (aOpenMode = omReadOnly) then
        { Table is to be opened as Read-only. }
        aLockType := ffsltShare
      else
        { Table is to be opened as Read-Write. }
        aLockType := ffsltIntentS;

      { Acquire the lock.  We will return from this call when the lock
        is granted.  Otherwise an exception will be raised (i.e., another
        thread has the table locked exclusively and isn't giving it
        up). }
      bcTable.AcqLock(CursorID, aLockType);

    end
    else begin
      { No, it is not open. Open it now. }
      try
        bcTableOpenPrim(bcDatabase, aTableName, aOpenMode, aShareMode,
                        aForServer, aAttribs);
      except
        bcEngine.TableList.EndWrite;
        raise;
      end;

      NewTable := true;
      bcTable.RegisterOpenIntent;                                      {!!.01}
      OpenIntentRegistered := True;                                    {!!.01}

    end;

    { Make sure we meet all requirements for opening the table. }
    try
      bcTableOpenPreconditions(bcTable, aIndexName, bcIndexID, aOpenMode);
    except
      { If we created a new table then get rid of it. }
      if NewTable then begin
        if OpenIntentRegistered then                                   {!!.02}
          bcTable.DeregisterOpenIntent;                                {!!.02}
        bcTable.Free;
        bcTable := nil;
      end;
      raise;
    end;

    { Add the newly opened table to the server table list. }
    if NewTable then
      bcEngine.TableList.AddTable(bcTable);

    bcInit(aOpenMode, aShareMode, aExclContLock);                      {Moved !!.01}

  finally
    { If the table was not already opened then we still have the tableList
      locked.  Unlock it. }
    if NewTable then
      bcEngine.TableList.EndWrite;                                     {!!.01}
    if (bcTable <> nil) and OpenIntentRegistered then                  {!!.02}
      { If we registered our intent to open then deregister our intent. }
      bcTable.DeregisterOpenIntent;
  end;

//  bcInit(aOpenMode, aShareMode);                                     {Moved !!.01}

end;
{--------}
procedure TffSrBaseCursor.Build(const aTableName : TffTableName;
                                      aDict      : TffDataDictionary;
                                const aOpenMode  : TffOpenMode;
                                      aShareMode : TffShareMode;
                                      aForServer : boolean;
                                      aOverWrite : boolean;
                                      aAttribs   : TffFileAttributes;
                                      aStoreSize : TffWord32);
var
  aLockType : TffSrLockType;
  aTransID : TffTransID;
  OpenIntentRegistered : Boolean;                                      {!!.10}
  TableDataFile : TffFileNameExt;
  TmpTableName : TffTableName;
begin
  bcIndexID := 0;
  OpenIntentRegistered := False;                                       {!!.10}

  TmpTableName := aTableName;
  if (fffaTemporary in aAttribs) then begin
    { Requirement: If the temporary file attribute is specified, the table must
      have a block size of 64k. This is due to temporary storage (unit FFLLTEMP)
      being restricted to 64k blocks of data. }
    if (aDict.BlockSize < (64 * 1024)) then
      aDict.BlockSize := (64 * 1024);

    { If no tablename specified then generate a unique table name. }
    if TmpTableName = '' then
      TmpTableName := IntToStr(Longint(Self));
  end;

  { Obtain write access to the table list.  Our purpose is to make sure
    the table is not opened.  By obtaining write access, we prevent other
    threads from creating or opening the table. }
  bcEngine.TableList.BeginWrite;
  try

    { Was a tablename specified? }
    if aTableName <> '' then begin
      { Yes. It is possible the table may already exist.
        Try and find the open table in our list. If it exists already
        obviously there's an error (we can't build a new table when it's
        already open). }
      bcTable := bcEngine.GetTableInstance(bcDatabase.Folder, TmpTableName);
      if assigned(bcTable) then
        FFRaiseException(EffException, ffStrResServer, fferrTableOpen,
                         [TmpTableName]);

      { The table name must be a valid file name without extension. }
      if not FFVerifyFileName(TmpTableName) then
        FFRaiseException(EffException, ffStrResServer, fferrInvalidTableName,
                         [TmpTableName]);

      { Is this a temporary table? }
      if not (fffaTemporary in aAttribs) then begin
        { No. The table's data file cannot exist within the database. }
        TableDataFile := FFMakeFileNameExt(TmpTableName, ffc_ExtForData);
        if FFFileExists(FFMakeFullFileName(bcDatabase.Folder.Path,
                                           TableDataFile)) then begin
          if aOverWrite then
            { We want to overwrite this table - we have to delete it first. }
            bcEngine.seDeleteTable(bcDatabase, TmpTableName)
          else
            FFRaiseException(EffException, ffStrResServer, fferrTableExists,
                             [TmpTableName]);
        end;
      end;
    end;

    { Is this cursor to have its own temporary storage? }
    if aStoreSize > 0 then
      bcTempStore := ffcTempStorageClass.Create(bcEngine.ConfigDir,
                                               aStoreSize, 64 * 1024)
    else
      bcTempStore := nil;

    { Create the table. }
    bcTable := bcTableClass.Create(bcEngine, TmpTableName, bcDatabase.Folder,
                                   bcEngine.BufferManager, omReadWrite);

    try
      bcTable.RegisterOpenIntent;                                      {!!.10}
      OpenIntentRegistered := True;                                    {!!.10}
      bcEngine.seTransactionStart(bcDatabase, false, true, aTransID);
      try
        { Create the files comprising the table. }
        bcTable.BuildFiles(bcDatabase.TransactionInfo, aForServer, aDict,
                          aAttribs, bcTempStore);
        bcEngine.seTransactionCommit(bcDatabase);
      except
        bcEngine.seTransactionRollback(bcDatabase);
        raise;
      end;

      { Acquire the right type of lock on the table. }
      if aShareMode = smExclusive then
        aLockType := ffsltExclusive
      else if aOpenMode = omReadOnly then
        aLockType := ffsltShare
      else
        aLockType := ffsltIntentS;

      bcTable.AcqLock(CursorID, aLockType);

      bcTableOpenPreconditions(bcTable, '', bcIndexID, aOpenMode);
    except
      { Destroy the table object. This will close all the files. }
      bcTable.DeregisterOpenIntent;
      bcTable.Free;
      bcTable := nil;
      raise;
    end;{try..finally}

    bcEngine.TableList.AddTable(bcTable);
    bcInit(aOpenMode, aShareMode, False);                              {!!.10}
  finally
    bcEngine.TableList.EndWrite;
    if assigned(bcTable) and OpenIntentRegistered then                 {!!.10}
      bcTable.DeregisterOpenIntent;                                    {!!.10}
  end;
end;
{--------}
destructor TffSrBaseCursor.Destroy;
var
  anExtender : TffBaseEngineExtender;
  anIndex : Longint;
begin
  bcEngine.TableList.BeginWrite;                                       {!!.10}
  try
    { Assumption: If cursor is being closed in the context of a transaction then
      the changes made to the table should be saved. We will retain the cursor's
      locks in the lock manager so that no other cursors can access those
      records. The changes to the table will stay in memory until the transaction
      commits or rolls back. }

{Begin !!.03}
    { If still have a record locked from TffTable.Edit then release the lock. }
    if not FFI64IsZero(bcLockedRefNum) then
      bcTable.RemoveLocksForCursor(bcDatabase.DatabaseID,              {!!.10}
                                   CursorID, bcLockedRefNum,           {!!.10}
                                   bcDatabase.TransactionInfo);
{End !!.03}

    if (bcRecordData <> nil) then                                      {!!.01}
      FFFreeMem(bcRecordData, bcRecordLen);                            {!!.01}

    bcBLOBCursors.Free;

    if bcExclOwner then begin
      bcTable.SetExclOwner(ffc_W32NoValue);
      bcExclOwner := False;
    end;

    if assigned(bcExtenders) then begin
      for anIndex := pred(bcExtenders.Count) downto 0 do begin
        anExtender := TffBaseEngineExtender
                        (TffIntListItem(bcExtenders[anIndex]).KeyAsInt);
        anExtender.Free;
      end;
      bcExtenders.Free;
    end;

//  if bcCloseTable then begin                                         {Deleted !!.02}
    if bcTable <> nil then begin                                       {!!.02}
      bcTable.CursorList.BeginWrite;
      try
        bcTable.CursorList.RemoveCursor(CursorID);
      finally
        bcTable.CursorList.EndWrite;
      end;
      if bcCloseTable then                                             {!!.02}
        bcEngine.TableList.RemoveIfUnused(bcTable);
    end;

    bcTempStore.Free;
    bcInfoLock.Free;                                                   {!!.06}

  finally
    bcEngine.TableList.EndWrite;                                       {!!.10}
    inherited Destroy;
  end;
end;
{--------}
procedure TffSrBaseCursor.AcqContentLock(const aMode : TffContentLockMode); {!!.10}
{ NOTE:: If you change this method then look at AcqContentLockCond for similar
  changes. }
begin
  if (fffaBLOBChainSafe in bcGetAttribs) or                            {!!.05}
     (bcExclOwner and (not bcTable.Dictionary.HasBLOBFields)) then     {!!.03}{!!.05}
    Exit;

  Assert(assigned(bcDatabase.Transaction) or
         (aMode = ffclmRead));

  { Is a transaction active? }
  if assigned(bcDatabase.Transaction) then
    { Yes.  Call the appropriate table method. }
    case aMode of
      ffclmCommit :
        bcTable.BeginCommit;
      ffclmRead   :
        bcTable.AcqContentLock(bcDatabase.Transaction, ffsltShare, False);
      ffclmWrite  :
        bcTable.AcqContentLock(bcDatabase.Transaction, ffsltExclusive, False);
    end { case }
  else begin                                                           {!!.05 - Start}
    { No transaction.  This should be a reader thread that wants read access. }
    if (bcNumReadLocks = 0) then
      bcTable.BeginRead;
    InterlockedIncrement(bcNumReadLocks);
  end;                                                                 {!!.05 - End}
end;
{Begin !!.10}
{--------}
function TffSrBaseCursor.AcqExclContentLock : TffResult;
{ NOTE:: If you change this method then look at AcqContentLock for similar
  changes. }
begin
  if not ((fffaBLOBChainSafe in bcGetAttribs) or
          (bcExclOwner and (not bcTable.Dictionary.HasBLOBFields))) then begin
    Assert(assigned(bcDatabase.Transaction));
    Result := bcTable.AcqExclContentLock(bcDatabase.Transaction);
  end
  else
    Result := DBIERR_NONE;
end;
{End !!.10}
{--------}
procedure TffSrBaseCursor.AppendNewRecord(aData : PffByteArray);
begin
  AcqContentLock(ffclmWrite);
  InsertRecord(aData, ffsltExclusive);
end;
{--------}
procedure TffSrBaseCursor.bcAddExtender(anExtender : TffBaseEngineExtender);
var
  anItem : TffIntListItem;
begin
  if assigned(anExtender) then begin
    if not assigned(bcExtenders) then
      bcExtenders := TffList.Create;
    anItem := TffIntListItem.Create(Longint(anExtender));
    bcExtenders.Insert(anItem);
  end;
end;
{--------}
function TffSrBaseCursor.bcBLOBCopy(aSrcCursor  : TffSrBaseCursor;
                              const aBLOBNr     : TffInt64;
                                var aDestBLOBNr : TffInt64)
                                                : TffResult;
var
  aBLOB      : PffByteArray;
  aBytesRead,
  aLen,
  aOffset    : TffWord32;                                              {!!.06}
  FileName   : TffFullFileName;
begin
  Result := DBIERR_NONE;
  { Assumption: Transaction has already been started by a calling routine. }

  { Is this a file BLOB? }
  if FFTblGetFileNameBLOB
       (aSrcCursor.bcTable.btFiles[aSrcCursor.Dictionary.BLOBFileNumber],
        bcDatabase.TransactionInfo, aBLOBNr, FileName) then begin
    FFTblAddFileBLOB(bcTable.btFiles[Dictionary.BLOBFileNumber],
                     bcDatabase.TransactionInfo,
                     FileName, aDestBLOBNr);
  end
  else begin
    aBytesRead := 0;
    aOffset := 0;
{Begin !!.12}
    aLen := FFMinI(aSrcCursor.BLOBGetLength(aBLOBNr, Result),
                   ffc_NumBLOBBytesToCopy);
    if Result = DBIERR_NONE then begin
{End !!.12}
      FFGetMem(aBLOB, aLen);
      try
        { Create the BLOB in the destination cursor. }
        Result := BLOBAdd(aDestBLOBNr);
        if Result = DBIERR_NONE then
          repeat
            Result := aSrcCursor.BLOBRead(aBLOBNr, aOffset, aLen, aBLOB^,
                                          aBytesRead);
            if aBytesRead > 0 then begin
              Result := BLOBWrite(aDestBLOBNr, aOffset, aBytesRead, aBLOB^);
              inc(aOffset, aBytesRead);
            end;
          until (aBytesRead = 0) or (Result <> DBIERR_NONE);
      finally
        FFFreeMem(aBLOB, aLen);
      end;
    end;  { if }                                                       {!!.12}
  end;  
end;
{--------}
function TffSrBaseCursor.bcBLOBLinkGetLength(const aTableName : TffTableName;
                                             const aBLOBNr    : TffInt64;
                                               var aLength    : Longint) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  Cursor := bcFindBLOBCursor(aTableName);
  Assert(Assigned(Cursor));
  aLength := Cursor.BLOBGetLength(aBLOBNr, Result);
end;
{--------}
function TffSrBaseCursor.bcBLOBLinkRead(const aTableName : TffTableName;
                                        const aBLOBNr    : TffInt64;
                                        const aOffset    : TffWord32;  {!!.06}
                                        const aLen       : TffWord32;  {!!.06}
                                          var aBLOB;
                                          var aBytesRead : TffWord32)  {!!.06}
                                                         : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  Cursor := bcFindBLOBCursor(aTableName);
  Assert(Assigned(Cursor));
  Result := Cursor.BLOBRead(aBLOBNr, aOffset, aLen, aBLOB, aBytesRead);
end;
{--------}
function TffSrBaseCursor.bcCheckExclusiveReadWrite : TffResult;
begin
  Result := DBIERR_NONE;

  { The cursor must have Exclusive access to the table. }
  if (not bcExclOwner) then
    Result := DBIERR_NEEDEXCLACCESS
  else if (bcOpenMode = omReadOnly) and                                {!!.06}
          not (fffaTemporary in bcTable.Files[0].fiAttributes)then     {!!.06}
    { The cursor must be in read-write mode. Temporary files are excluded
      from this rule. }
    Result := DBIERR_TABLEREADONLY;

end;
{--------}
function TffSrBaseCursor.bcFindBLOBCursor(const aTableName : TffTableName)
                                                           : TffSrBaseCursor;
var
  Inx        : Longint;
  UTableName : TffTableName;
begin
  Result := nil;
  UTableName := Uppercase(aTableName);
  { Do we have any BLOB cursors yet? }
  if bcBLOBCursors = nil then
    { No. Instantiate. }
    bcBLOBCursors := TffList.Create;

  { Have we opened a cursor for the referenced table? }
  for Inx := 0 to pred(bcBLOBCursors.Count) do begin
    if UpperCase(TffSrBaseCursor(bcBLOBCursors[Inx]).bcTable.BaseName) =
       UTableName then begin
      Result := TffSrBaseCursor(bcBLOBCursors[Inx]);
      break;
    end;
  end;

  { Did we find a cursor? }
  if Result = nil then begin
    { No. Create one. }
    { Limitation: BLOB links can refer only to standard cursors, not to
      SQL result sets. }
    Result := bcEngine.CursorClass.Create(bcEngine,                    {!!.06}
                                          bcDatabase,
                                          Timeout);
    Result.Open(aTableName, '', 0, omReadOnly, smShared, False, False, []);   {!!.01}
    bcBLOBCursors.Insert(Result);
  end;
end;
{--------}
function TffSrBaseCursor.bcGetAttribs : TffFileAttributes;
begin
  Result := bcTable.Files[0]^.fiAttributes;
end;
{--------}
function TffSrBaseCursor.bcGetCursorID : TffCursorID;
begin
  Result := TffCursorID(Self);
end;
{--------}
function TffSrBaseCursor.bcGetPosition : TffCursorPosition;
begin
  Result := bcInfo.Pos;
end;
{--------}
function TffSrBaseCursor.bcGetRefNr : TffInt64;
begin
  Result := bcInfo.RefNr;
end;
{--------}
procedure TffSrBaseCursor.bcInit(const aOpenMode : TffOpenMode;
                                 const aShareMode : TffShareMode;
                                 const aExclContLock : Boolean);       {!!.10}
var
  anIndex : Longint;
  aMonitor : TffBaseEngineMonitor;
  anExtender : TffBaseEngineExtender;
  MonitorList : TffList;
begin

  { Assumption: This routine only called once a table has been successfully
    opened by the cursor. }
  bcFilter := nil;
  bcFilterSav := nil;
  bcNewRecBuff := nil;
  bcOldRecBuff := nil;

  { Miscellaneous. }
  if bcEngine.Configuration.GeneralInfo^.giReadOnly then
    bcOpenMode := omReadOnly
  else
    bcOpenMode := aOpenMode;
  FreeOnRemove := true;

  { Add ourself to the cursor lists in the table and database. }
  bcTable.CursorList.BeginWrite;
  try
    bcTable.CursorList.AddCursor(Self);
  finally
    bcTable.CursorList.EndWrite;
  end;

  bcDatabase.CursorList.BeginWrite;
  try
    bcDatabase.CursorList.AddCursor(Self);
  finally
    bcDatabase.CursorList.EndWrite;
  end;

  { If there are any monitors interested in cursors then see if they
    are interested in this cursor. }
  MonitorList := bcEngine.GetInterestedMonitors(TffSrBaseCursor);
  if assigned(MonitorList) then begin
    for anIndex := 0 to pred(MonitorList.Count) do begin
      aMonitor := TffBaseEngineMonitor
                    (TffIntListItem(MonitorList[anIndex]).KeyAsInt);
      try
        anExtender := aMonitor.Interested(Self);
        if assigned(anExtender) then
          bcAddExtender(anExtender);
      except
        on E:Exception do
          bcEngine.seForce('Monitor [%s] exception, bcInit: %s',       {!!.06 - Start}
                           [aMonitor.ClassName,E.message],
                           bcEngine.bseGetReadOnly);                   {!!.06 - End}
      end;
    end;
    MonitorList.Free;
  end;

  { Get memory for a record data scratch pad. }
  bcRecordLen := bcTable.Dictionary.RecordLength;
  FFGetMem(bcRecordData, bcRecordLen);

  { If the cursor is the exclusive owner of the table then mark this
    fact. }
  if aShareMode = smExclusive then begin
    bcTable.SetExclOwner(CursorID);
    bcExclOwner := True;
  end;

{Begin !!.10}
  if aExclContLock then
    bcTable.AcqContentLock(bcDatabase.Transaction, ffsltExclusive, False);
{End !!.10}

end;
{--------}
procedure TffSrBaseCursor.bcInvalidateCurKey;
begin
  bcInfo.KeyValid := false;
end;
{--------}
function TffSrBaseCursor.bcIsCurKeyPathValid : boolean;
begin
  Result := (bcInfo.KeyPath.kpPos <> kppUnknown);
end;
{--------}
function TffSrBaseCursor.bcIsCurKeyValid: boolean;
begin
  Result := bcInfo.KeyValid;
end;
{--------}
procedure TffSrBaseCursor.bcRecordUpdated(aOp      : TffRecOp;
                                          aRefNr   : TffInt64;
                                          aIndexID : integer);
begin
  { A cursor is affected by another cursor's operations as follows:

    1. When a cursor inserts a record, it may cause a Structural Modification
       Operation (SMO) in the indices.  Other cursors open on the same
       table may now have invalid key paths.

       In FF 1.x, this routine would reset the key path.  In FF 2.x, we
       leave the key path as is.  The next time the cursor moves to the next
       or previous record, the indexing code will see that the key path has
       been modified and rebuild the key path.

       *** We do not call this routine for a record insertion. ***

    2. When a cursor deletes a record, it may cause an SMO in the indices.  As
       mentioned for inserts, we will rely upon the indexing code to rebuild
       the key path.

       If another cursor is positioned on the deleted record, we must make sure
       the cursor knows the record has been deleted.  This routine sets the
       bcInfo.Deleted flag and positions the cursor to OnCrack.  When this
       notification occurs, any cursors wanting to do something with the record
       will be blocked while waiting for a lock on the record so this should
       be a safe operation.

    3. When a cursor modifies a record, it may cause an SMO in zero or more
       indicies.  As mentioned for inserts, we will rely upon the indexing
       code to rebuild the key path.

       If another cursor is positioned on the modified record, we must make
       it look like the record has been deleted.  This routine sets the
       bcInfo.Deleted flag and positions the cursor to OnCrack.  When this
       notification occurs, any cursors wanting to do something with the record
       will be blocked while waiting for a lock on the record so this should
       be a safe operation.

    In general, this method is thread-safe. It is called only for those cursors
    that belong to the same database as the cursor performing the insert,
    update, or delete. Those cursors should be in the same client thread
    and only one request from that client thread is executed on the server
    at any given time. So operations should not be active for any of the
    other cursors belonging to the same database.
  }

  case aOp of
    roDelete :
      if (FFCmpI64(aRefNr, bcInfo.RefNr) = 0) and
         (bcInfo.Pos = cpOnRecord) then begin
        bcInfo.Deleted := True;
        if bcIsCurKeyPathValid then begin
          Assert(bcInfo.KeyPath.kpCount > 0);
          bcInfo.Pos := cpOnCrack;
          bcInfo.KeyPath.kpPos := kppOnCrackBefore;
        end
        else
          bcInfo.KeyPath.kpPos := kppUnknown;
      end;
    roModify :
      if (aIndexID = IndexID) and
         (FFCmpI64(aRefNr, bcInfo.RefNr) = 0) and
         (bcInfo.Pos = cpOnRecord) then begin
        bcInfo.Deleted := True;
        if bcIsCurKeyPathValid then begin
          Assert(bcInfo.KeyPath.kpCount > 0);
          bcInfo.Pos := cpOnCrack;
          bcInfo.KeyPath.kpPos := kppOnCrackBefore;
        end
        else
          bcInfo.KeyPath.kpPos := kppUnknown;
      end;
  end;{case}
end;
{--------}
procedure TffSrBaseCursor.bcRestoreCurInfo;
begin
  bcInfo := bcSavedInfo;
end;
{--------}
procedure TffSrBaseCursor.bcSaveCurInfo;
begin
  bcSavedInfo := bcInfo;
end;
{--------}
procedure TffSrBaseCursor.bcTableOpenPrim(aDatabase  : TffSrDatabase;
                                    const aTableName : TffTableName;
                                    const aOpenMode  : TffOpenMode;
                                    const aShareMode : TffShareMode;
                                    const aForServer : boolean;
                                    const aAttribs   : TffFileAttributes);
var
  aLockType : TffSrLockType;
  TableDataFile : TffFileNameExt;
begin

  { The table name must be a valid file name without extension. }
  if not FFVerifyFileName(aTableName) then
    FFRaiseException(EffException, ffstrResServer, fferrInvalidTableName,
                     [aTableName]);

  { The table's data file must exist within the database. }
  TableDataFile := FFMakeFileNameExt(aTableName, ffc_ExtForData);
  if not FFFileExists(FFMakeFullFileName(aDatabase.Folder.Path, TableDataFile)) then
    FFRaiseException(EffException, ffstrResServer, fferrUnknownTable,
                     [TableDataFile, aDatabase.Alias]);

  { Create the table instance. }
  bcTable := bcTableClass.Create(aDatabase.Engine, aTableName,         {!!.03}
                               aDatabase.Folder,
                               aDatabase.Engine.BufferManager, aOpenMode);{!!.03}

  try
    { Acquire the right type of lock on the table. }
    if aShareMode = smExclusive then
      aLockType := ffsltExclusive
    else if aOpenMode = omReadOnly then
      aLockType := ffsltShare
    else
      aLockType := ffsltIntentS;

    bcTable.AcqLock(CursorID, aLockType);

    { Open up the files in the table, making sure that all of them
      are in FF format. }
    bcTable.OpenFiles(aDatabase.TransactionInfo, aForServer, aAttribs);
    TffSrTable(bcTable).ResolveDynamicLinks;                           {!!.06}
    bcTable.SetAttributes(aAttribs);
  except
    bcTable.Free;
    bcTable := nil;
    raise;
  end;
end;
{--------}
function TffSrBaseCursor.BLOBAdd(var aBLOBNr : TffInt64) : TffResult;
begin
  Result := NotifyExtenders(ffeaBeforeBLOBCreate, ffeaBLOBCreateFail);

  if Result = DBIERR_NONE then
    try
      AcqContentLock(ffclmWrite);
      FFTblAddBLOB(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                   bcDatabase.TransactionInfo, aBLOBNr);
      NotifyExtenders(ffeaAfterBLOBCreate, ffeaNoAction);
    except
      NotifyExtenders(ffeaBLOBCreateFail, ffeaNoAction);
      raise;
    end;
end;
{--------}
function TffSrBaseCursor.BLOBLinkAdd(const aTableName : TffTableName;
                                     const aTableBLOBNr : TffInt64;
                                       var aBLOBNr    : TffInt64) : TffResult;
begin
  Result := NotifyExtenders(ffeaBeforeBLOBLinkAdd, ffeaBLOBLinkAddFail);

  if Result = DBIERR_NONE then
    try
      AcqContentLock(ffclmWrite);
      FFTblAddBLOBLink(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                       bcDatabase.TransactionInfo, aTableName, aTableBLOBNr,
                       aBLOBNr);
      NotifyExtenders(ffeaAfterBLOBLinkAdd, ffeaNoAction);
    except
      NotifyExtenders(ffeaBLOBLinkAddFail, ffeaNoAction);
      raise;
    end;
end;
{--------}
function TffSrBaseCursor.FileBLOBAdd(const aFileName : TffFullFileName;
                                       var aBLOBNr   : TffInt64) : TffResult;
begin
  Result := NotifyExtenders(ffeaBeforeFileBLOBAdd, ffeaFileBLOBAddFail);

  if Result = DBIERR_NONE then
    try
      AcqContentLock(ffclmWrite);
      FFTblAddFileBLOB(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                       bcDatabase.TransactionInfo, aFileName, aBLOBNr);
      NotifyExtenders(ffeaAfterFileBLOBAdd, ffeaNoAction);
    except
      NotifyExtenders(ffeaFileBLOBAddFail, ffeaNoAction);
      raise;
    end;
end;
{--------}
function TffSrBaseCursor.BLOBDelete(const aBLOBNr : TffInt64) : TffResult;
begin
  Result := NotifyExtenders(ffeaBeforeBLOBDelete, ffeaBLOBDeleteFail);
  if Result = DBIERR_NONE then
    try
      AcqContentLock(ffclmWrite);
      FFTblDeleteBLOB(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                      bcDatabase.TransactionInfo, aBLOBNr);
      NotifyExtenders(ffeaAfterBLOBDelete, ffeaNoAction);
    except
      NotifyExtenders(ffeaBLOBDeleteFail, ffeaNoAction);
      raise;
    end;
end;
{--------}
function TffSrBaseCursor.BLOBFree(aBLOBNr : TffInt64) : TffResult;
begin
  Result := NotifyExtenders(ffeaBeforeBLOBFree, ffeaBLOBFreeFail);

  if Result = DBIERR_NONE then
    try
      AcqContentLock(ffclmWrite);
      if FFTblFreeBLOB(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                       bcDatabase.TransactionInfo, aBLOBNr) then
        Result := DBIERR_BLOBMODIFIED;
      NotifyExtenders(ffeaAfterBLOBFree, ffeaNoAction);
    except
      NotifyExtenders(ffeaBLOBFreeFail, ffeaNoAction);
      raise;
    end;
end;
{--------}
function TffSrBaseCursor.BLOBGetLength(aBLOBNr : TffInt64;
                                   var aFBError: TffResult) : Longint;
begin
  Result := -1;
  aFBError := NotifyExtenders(ffeaBeforeBLOBGetLength, ffeaBLOBGetLengthFail);

  if aFBError = DBIERR_NONE then
    try
      AcqContentLock(ffclmRead);
      try
        Result := FFTblGetBLOBLength(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                                     bcDatabase.TransactionInfo,
                                     aBLOBNr,
                                     bcBLOBLinkGetLength,
                                     aFBError);
        NotifyExtenders(ffeaAfterBLOBGetLength, ffeaNoAction);
      finally
        RelContentLock(ffclmRead);
      end;
    except
      NotifyExtenders(ffeaBLOBGetLengthFail, ffeaNoAction);
      raise;
    end;
end;
{Begin !!.03}
{--------}
function TffSrBaseCursor.BLOBIsLink(aBLOBNr         : TffInt64;        {!!.11 - Start}
                                var aSrcTableName   : TffTableName;
                                var aSrcTableBLOBNr : TffInt64)
                                                    : Boolean;
begin
  Result := FFTblIsBLOBLink(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                            bcDatabase.TransactionInfo,
                            aBLOBNr,
                            aSrcTableName,
                            aSrcTableBLOBNr);
end;
{--------}                                                             {!!.11 - End}
function TffSrBaseCursor.BLOBListSegments(aBLOBNr : TffInt64;
                                          aStream : TStream)
                                                  : TffResult;
begin
  Result := DBIERR_NONE;
  AcqContentLock(ffclmRead);
  try
    FFTblListBLOBSegments(bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                          bcDatabase.TransactionInfo, aBLOBNr,
                          aStream);
  finally
    RelContentLock(ffclmRead);
  end;
end;
{End !!.03}
{--------}
function TffSrBaseCursor.BLOBRead(aBLOBNr    : TffInt64;
                                  aOffset    : TffWord32;              {!!.06}
                                  aLen       : TffWord32;              {!!.06}
                              var aBLOB;
                              var aBytesRead : TffWord32)              {!!.06}
                                             : TffResult;
begin
  Result := NotifyExtenders(ffeaBeforeBLOBRead, ffeaBLOBReadFail);

  if Result = DBIERR_NONE then
    try
      AcqContentLock(ffclmRead);
      try
{Begin !!.11}
        bcTable.btBLOBEngine.Read
                            (bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                             bcDatabase.TransactionInfo,
                             aBLOBNr,
                             aOffset,
                             aLen,
                             bcBLOBLinkRead,
                             aBLOB,
                             aBytesRead,
                             Result);
{End !!.11}
        NotifyExtenders(ffeaAfterBLOBRead, ffeaNoAction);
      finally
        RelContentLock(ffclmRead);
      end;
    except
      NotifyExtenders(ffeaBLOBReadFail, ffeaNoAction);
      raise;
    end;
end;
{--------}
function TffSrBaseCursor.BLOBTruncate(aBLOBNr : TffInt64;
                                      aLen    : TffWord32) : TffResult;
begin
  Result := NotifyExtenders(ffeaBeforeBLOBTruncate, ffeaBLOBTruncateFail);

  if Result = DBIERR_NONE then
    try
      AcqContentLock(ffclmWrite);
{Begin !!.11}
      bcTable.btBLOBEngine.Truncate
                              (bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                               bcDatabase.TransactionInfo, aBLOBNr, aLen);
{End !!.11}
      NotifyExtenders(ffeaAfterBLOBTruncate, ffeaNoAction);
    except
      NotifyExtenders(ffeaBLOBTruncateFail, ffeaNoAction);
      raise;
    end;
end;
{--------}
function TffSrBaseCursor.BLOBWrite(const aBLOBNr : TffInt64;
                                         aOffset : TffWord32;
                                         aLen    : TffWord32;
                                     var aBLOB) : TffResult;
begin
  Result := NotifyExtenders(ffeaBeforeBLOBWrite, ffeaBLOBWriteFail);

  if Result = DBIERR_NONE then
    try
      AcqContentLock(ffclmWrite);
{Begin !!.11}
      bcTable.btBLOBEngine.Write
                           (bcTable.Files[bcTable.Dictionary.BLOBFileNumber],
                            bcDatabase.TransactionInfo, aBLOBNr, aOffset, aLen,
                            aBLOB);
{End !!.11}
      NotifyExtenders(ffeaAfterBLOBWrite, ffeaNoAction);
    except
      on E:Exception do begin
        NotifyExtenders(ffeaBLOBWriteFail, ffeaNoAction);
        raise;
      end;
    end;
end;
{--------}
function TffSrBaseCursor.CopyRecords(aSrcCursor : TffSrBaseCursor;
                                     aBLOBCopyMode : TffBLOBCopyMode;
                                     aCallback : TffSrCopyRecordsProc;
                                     aCookie1, aCookie2 : Longint) : TffResult;
var
  aAutoIncField : Integer;                                             {!!.02}
  aAutoIncHigh : TffWord32;                                            {!!.02}
  aThisAutoInc : TffWord32;                                            {!!.02}
  aBLOBFields : TffPointerList;
  aBLOBNr,
  aSrcBLOBNr : TffInt64;
  aInx,
  aOffset : integer;
  aRecord : PffByteArray;
  aTableName : TffTableName;
  aTransID : TffTransID;
  aVal : PffByteArray;
  Include,
  IsNull : boolean;
begin

  aVal := nil;

{Begin !!.02}
  { Does the target have an autoinc field? }
  if Dictionary.HasAutoIncField(aAutoIncField) then
    { Yes. Get the current seed value. }
    ReadAutoIncValue(aAutoIncHigh)
  else
    { No. Flag it. }
    aAutoIncField := -1;
{End !!.02}

  { Requirement: The cursors must be pointing to different tables. }
  if bcTable = aSrcCursor.Table then
    FFRaiseExceptionNoData(EffException, ffStrResServer, fferrSameTable);

  aTableName := aSrcCursor.Table.BaseName;
  aBLOBFields := TffPointerList.Create;
  try
    { Requirement: The dictionary field types and sizes must match. }
    if not bcTable.Dictionary.HasSameFields(aSrcCursor.Dictionary, aBLOBFields) then
      FFRaiseExceptionNoData(EffException, ffStrResServer, fferrIncompatDict);

    { Save the position of each cursor. }
    bcSaveCurInfo;
    aSrcCursor.bcSaveCurInfo;
    { Create a record buffer. }
    FFGetMem(aRecord, bcTable.Dictionary.RecordLength);
    try
      { Position the source cursor to BOF. }
      aSrcCursor.SetToBegin;

      { Start a transaction. It will be nested if a transaction is already
        in progress. }
      Result := bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
      try
        while (Result = DBIERR_NONE) do begin

          { Grab a record from the source cursor. }
          Result := aSrcCursor.GetNextRecord(aRecord, ffsltNone);
          if Result = DBIERR_NONE then begin

            { Was a callback function specified? }
            Include := True;
            if assigned(aCallback) then
              aCallback(aSrcCursor, aRecord, aCookie1, aCookie2, Include);

            if Include then begin
              { Any BLOB fields? }
              if aBLOBFields.Count > 0 then begin
                { Yes. Copy or link as necessary. }
                for aInx := 0 to pred(aBLOBFields.Count) do begin
                  aOffset := bcTable.Dictionary.FieldOffset[Integer(aBLOBFields[aInx])];
                  { Is the BLOB field null? }
                  aSrcCursor.Dictionary.GetRecordField(Integer(aBLOBFields[aInx]),
                                                       aRecord, IsNull, aVal);
                  if not IsNull then begin
                    case aBLOBCopyMode of
                      ffbcmNoCopy :
                        bcTable.Dictionary.SetRecordField
                          (Integer(aBLOBFields[aInx]), aRecord, nil);
                      ffbcmCopyFull :
                        begin
                          aSrcBLOBNr := PffInt64(@aRecord^[aOffset])^;
                          Result := bcBLOBCopy(aSrcCursor, aSrcBLOBNr, aBLOBNr);
                          if Result = DBIERR_NONE then
                            PffInt64(@aRecord^[aOffset])^ := aBLOBNr
                          else
                            break;
                        end;
                    else  { link the BLOBs }
                      { Get the BLOB reference out of the record. }
                      aSrcBLOBNr := PffInt64(@aRecord^[aOffset])^;
                      { Add a BLOB link. }
                      BLOBLinkAdd(aTableName, aSrcBLOBNr, aBLOBNr);
                      { Update the BLOB reference in the record. }
                      PffInt64(@aRecord^[aOffset])^ := aBLOBNr;
                    end;  { case }
                  end; { if BLOB field not null }
                end;  { for }
              end;
              Result := InsertRecord(aRecord, ffsltNone);
{Begin !!.02}
              { If the target has an autoinc field then keep track of the
                highest value. }
              if (Result = DBIERR_NONE) and(aAutoIncField > -1) then begin
                Dictionary.GetRecordField(aAutoIncField,
                                          aRecord, IsNull, @aThisAutoInc);
                if not IsNull and (aThisAutoInc > aAutoIncHigh) then
                  aAutoIncHigh := aThisAutoInc;
              end;
{End !!.02}
            end;
          end;  { if }
        end;  { while }
{Begin !!.02}
        if Result = DBIERR_EOF then begin
          { If the destination has an autoinc field then update the seed
            value. }
          if aAutoIncField <> -1 then
            FFTblSetAutoIncValue(Table.Files[0],
                                 Database.TransactionInfo,
                                 aAutoIncHigh);
          Result := bcEngine.seTransactionCommit(bcDatabase);
        end
{End !!.02}
        else
          bcEngine.seTransactionRollback(bcDatabase);
      except
        bcEngine.seTransactionRollback(bcDatabase);
        raise;
      end;
    finally
      { Free the record buffer. }
      FFFreeMem(aRecord, bcTable.Dictionary.RecordLength);
      { Restore the position of each cursor. }
      bcRestoreCurInfo;
      aSrcCursor.bcRestoreCurInfo;
    end;
  finally
    aBLOBFields.Free;
  end;
end;
{--------}
function TffSrBaseCursor.CopyRecordParts(aSrcCursor : TffSrBaseCursor;
                                         aFields    : PffLongintArray;
                                         aNumFields : integer;
                                         aBLOBCopyMode : TffBLOBCopyMode;
                                         aCallback : TffSrCopyRecordsProc;
                                         aCookie1, aCookie2 : Longint) : TffResult;
var
  aBLOBFields : TffPointerList;
  aInx : integer;
  aDestRec, aSrcRec : PffByteArray;
  aSrcBLOBNr, aBLOBNr : TffInt64;
  aOffset : integer;
  aTableName : TffTableName;
  aTransID : TffTransID;
  aVal : PffByteArray;
  DestLen : integer;
  Include : boolean;
  IsNull : boolean;
begin

  { Requirement: The cursors must be pointing to different tables. }
  if bcTable = aSrcCursor.Table then
    FFRaiseExceptionNoData(EffException, ffStrResServer, fferrSameTable);

  aTableName := aSrcCursor.Table.BaseName;
  aBLOBFields := TffPointerList.Create;
  try
    { Requirement: The dictionary field types and sizes must match. }
    if not bcTable.Dictionary.HasSameFieldsEx(aSrcCursor.Dictionary, aFields,
                                             aNumFields, aBLOBFields) then
      FFRaiseExceptionNoData(EffException, ffStrResServer, fferrIncompatDict);

    { Save the position of each cursor. }
    bcSaveCurInfo;
    aSrcCursor.bcSaveCurInfo;

    { Create record buffers. }
    DestLen := bcTable.Dictionary.RecordLength;
    FFGetMem(aDestRec, DestLen);
    FFGetMem(aSrcRec, aSrcCursor.Dictionary.RecordLength);
    FFGetMem(aVal, aSrcCursor.Dictionary.BlockSize);
    try
      { Position the source cursor to BOF. }
      aSrcCursor.SetToBegin;

      { Start a transaction. It will be nested if a transaction is already
        in progress. }
      Result := bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
      try
        while (Result = DBIERR_NONE) do begin

          { Grab a record from the source cursor. }
          Result := aSrcCursor.GetNextRecord(aSrcRec, ffsltNone);
          if Result = DBIERR_NONE then begin

            { Was a callback function specified? }
            Include := True;
            if assigned(aCallback) then
              aCallback(aSrcCursor, aSrcRec, aCookie1, aCookie2, Include);

            if Include then begin
              { Build the destination record. }
              FillChar(aDestRec^, destLen, 0);
              for aInx := 0 to pred(aNumFields) do begin
                aSrcCursor.Dictionary.GetRecordField(aFields^[aInx],
                                                     aSrcRec, IsNull, aVal);
                if IsNull then
                  bcTable.Dictionary.SetRecordField(aInx, aDestRec, nil)
                else begin
                  { Is this a BLOB field? }
                  if bcTable.Dictionary.FieldType[aInx] in
                    [fftBLOB..fftBLOBFile] then begin
                    aOffset := aSrcCursor.Dictionary.FieldOffset[aFields^[aInx]];
                    { Yes. How is it to be handled? }
                    case aBLOBCopyMode of
                      ffbcmNoCopy :
                        bcTable.Dictionary.SetRecordField(aInx, aDestRec, nil);
                      ffbcmCopyFull :
                        begin
                          aSrcBLOBNr := PffInt64(@aSrcRec^[aOffset])^;
                          Result := bcBLOBCopy(aSrcCursor, aSrcBLOBNr, aBLOBNr);
                          if Result = DBIERR_NONE then
                            PffInt64(@aDestRec^[aOffset])^ := aBLOBNr
                          else
                            break;
                        end;
                    else  { link the BLOBs }
                      { Get the BLOB reference out of the record. }
                      aSrcBLOBNr := PffInt64(@aSrcRec^[aOffset])^;
                      { Add a BLOB link. }
                      BLOBLinkAdd(aTableName, aSrcBLOBNr, aBLOBNr);
                      { Update the BLOB reference in the record. }
                      PffInt64(@aDestRec^[aOffset])^ := aBLOBNr;
                    end;  { case }
                  end
                  else
                    bcTable.Dictionary.SetRecordField(aInx, aDestRec, aVal);
                end;
              end;
              { Insert the record. }
              Result := InsertRecord(aDestRec, ffsltNone);
            end;
          end;  { if }
        end;  { while }
        if Result = DBIERR_EOF then
          Result := bcEngine.seTransactionCommit(bcDatabase)
        else
          bcEngine.seTransactionRollback(bcDatabase);
      except
        bcEngine.seTransactionRollback(bcDatabase);
        raise;
      end;
    finally
      { Free the record buffers. }
      FFFreeMem(aSrcRec, aSrcCursor.Dictionary.RecordLength);
      FFFreeMem(aDestRec, DestLen);
      FFFreeMem(aVal, aSrcCursor.Dictionary.BlockSize);
      { Restore the position of each cursor. }
      bcRestoreCurInfo;
      aSrcCursor.bcRestoreCurInfo;
    end;
  finally
    aBLOBFields.Free;
  end;
end;
{--------}
function TffSrBaseCursor.DeleteRecord(aData : PffByteArray) : TffResult;
var
  BTreeChanged : Boolean;                                              {!!.05}
  LockedRefNr : TffInt64;                                              {!!.05}
begin
  Result := DBIERR_NONE;                                               {!!.01}
  { Are we on a record? }
  if (bcInfo.Pos <> cpOnRecord) then begin
    { No. }
    Result := DBIERR_NOCURRREC;
    Exit;
  end;

  { Note: By this time, any other cursor deleting the record ahead of us has
    completed and has set bcInfo.Deleted.  We can be assured of this because
    TffServerEngine.RecordDelete calls Cursor.EnsureWritable(true) which
    obtains a lock on the record to be deleted.  We won't get that lock until
    the other cursor has finished. }

  try                                                                  {!!.01}
    { Has this record already been deleted? }
    if bcInfo.Deleted then begin
      { Yes. }
      Result := DBIERR_KEYORRECDELETED;
      Exit;
    end;

    AcqContentLock(ffclmWrite);
    if (aData = nil) and                                               {!!.02}
       ((bcFilter <> nil) or (bcExtenders <> nil)) then                {!!.02}
      aData := bcRecordData;
    if (aData <> nil) then begin
      Table.GetRecord(bcDatabase.TransactionInfo,                      {!!.10}
                      bcDatabase.DatabaseID,                           {!!.10}
                      CursorID, bcInfo.refNr, aData,                   {!!.10}
                      ffsltExclusive, True, False);  { lock obtained in EnsureWritable } {!!.02}
      if Assigned(bcFilter) then
        if not bcFilter.MatchesRecord(aData) then begin
          { Release the record lock. }
//          Table.RelRecordLock(bcDatabase.TransactionInfo, CursorID, bcInfo.refNr); {Deleted !!.01}
          Result := DBIERR_NOCURRREC;
          Exit;
        end;
    end;

    { Notify extenders. }
    bcOldRecBuff := aData;
    try
      Result := NotifyExtenders(ffeaBeforeRecDelete, ffeaDeleteRecFail);
      { If the extenders object, we can't continue. }
      if Result = DBIERR_NONE then begin
        BTreeChanged := False;                                         {!!.05 - Start}
        LockedRefNr := bcInfo.refNr;                                   {!!.05}
        Result := Table.DeleteRecord(Database.TransactionInfo, CursorID,
                                     bcInfo.refNr, True, BTreeChanged);
        if (Result = DBIERR_NONE) then begin
          bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID,{!!.10}
                                  bcInfo.RefNr);                       {!!.10}
          if bcInfo.KeyPath.kpPos = kppUnknown then
            bcInfo.Pos := cpUnknown
          else if (BTreeChanged) then begin
            bcRebuildKeyPath;
          end else if (bcInfo.KeyPath.kpPos = kppOnKey) then begin
            bcInfo.KeyPath.kpPos := kppOnCrackBefore;
            bcInfo.Deleted := True;
            bcInfo.Pos := cpOnCrack;
          end;                                                         {!!.05 - End}

          { Notify extenders of successful delete. }
          NotifyExtenders(ffeaAfterRecDelete, ffeaNoAction);
        end else
          { Notify extenders of failed delete. }
          NotifyExtenders(ffeaDeleteRecFail, ffeaNoAction);
      end;
    finally
      bcOldRecBuff := nil;
    end;
{Begin !!.01}
  finally
    { Release the record lock if an error occurred or we are in an implicit
      transaction. }
{Begin !!.03}
    if (Result <> DBIERR_NONE) or
       bcDatabase.Transaction.IsImplicit then begin
      Table.RelRecordLock(bcDatabase.TransactionInfo,                  {!!.10}
                          bcDatabase.DatabaseID,                       {!!.10}
                          CursorID, LockedRefNr);                      {!!.05}{!!.10}
      { Did an edit occur just prior to the delete? }
      if not FFI64IsZero(bcLockedRefNum) then begin
        Table.RelRecordLock(bcDatabase.TransactionInfo,                {!!.10}
                            bcDatabase.DatabaseID,                     {!!.10}
                            CursorID, bcLockedRefNum);                 {!!.10}
        FFInitI64(bcLockedRefNum);
      end;
    end;
{End !!.03}
  end;
{End !!.01}
end;
{Begin !!.06}
{--------}
function TffSrBaseCursor.DeleteRecords : TffResult;
var
  aRecord : PffByteArray;
  aTransID : TffTransID;
begin

  { Create a record buffer. }
  FFGetMem(aRecord, bcTable.Dictionary.RecordLength);
  try
    { Position to BOF. }
    SetToBegin;

    { Start a transaction. It will be nested if a transaction is already
      in progress. }
    Result := bcEngine.seTransactionStart(bcDatabase, False, True, aTransID);
    try
      while (Result = DBIERR_NONE) do begin

        { If on a record then get it otherwise move to the next
          record. }
        if bcInfo.Pos = cpOnRecord then begin
          Result := GetRecord(aRecord, ffsltExclusive);
          { Is a filter active? }
          if Result = DBIERR_NOCURRREC then
            { Yes. The current record didn't match the filter. Find the next
              record that matches the filter. }
            Result := GetNextRecord(aRecord, ffsltExclusive);
        end
        else
          Result := GetNextRecord(aRecord, ffsltExclusive);
        if Result = DBIERR_NONE then
          Result := DeleteRecord(aRecord);
      end;  { while }
      if Result = DBIERR_EOF then
        Result := bcEngine.seTransactionCommit(bcDatabase)
      else
        bcEngine.seTransactionRollback(bcDatabase);
    except
      bcEngine.seTransactionRollback(bcDatabase);
      raise;
    end;
  finally
    FFFreeMem(aRecord, bcTable.Dictionary.RecordLength);
  end;
end;
{End !!.06}
{--------}
function TffSrBaseCursor.Empty : TffResult;
begin
  { Requirement: Transaction must be started. }
  Assert(Assigned(bcDatabase.Transaction));

  { The cursor must have Exclusive Read-Write access. }
  Result := bcCheckExclusiveReadWrite;
  if Result <> DBIERR_NONE then
    exit;

  { Get the table to empty itself. }
  AcqContentLock(ffclmWrite);
  Result := bcTable.EmptyFiles(Database.TransactionInfo);
end;
{--------}
function TffSrBaseCursor.EnsureWritable(aCheckCurRec, aConditionalLock : Boolean) : TffResult;
begin
  { The cursor must have been opened in read-write mode. }
  if (bcOpenMode = omReadOnly) then begin
    Result := DBIERR_TABLEREADONLY;
    Exit;
  end;

  { There cannot be any type of lock on the table (unless its ours and
    is a write lock). }
  if Table.btClientLocks.Count > 0 then
    if Table.btClientLocks.SummaryMode = ffsltExclusive then begin
      if not Table.HasClientLock(CursorID) then begin
        Result := DBIERR_FILELOCKED;
        Exit;
      end;
    end
    else begin
      Result := DBIERR_FILELOCKED;
      Exit;
    end;


  { Make sure the cursor is positioned on a record. }
  if aCheckCurRec then begin
    if (bcInfo.pos <> cpOnRecord) then begin
      Result := DBIERR_NOCURRREC;
      Exit;
    end;
//    if Assigned(bcFilter) then begin                                 {Deleted !!.02}
    AcqContentLock(ffclmRead);
    try
      Table.GetRecord(Database.TransactionInfo,                        {!!.10}
                      bcDatabase.DatabaseID,                           {!!.10}
                      CursorID, bcInfo.refNr,                          {!!.10}
                      bcRecordData, ffsltExclusive, false, aConditionalLock);  {!!.02}
        { Note: We assume we can ask for an Exclusive lock because this
          method is passed True only from the Engine.RecordDelete and
          Engine.RecordModify methods. }
      if assigned(bcFilter) and                                        {!!.02}
         (not bcFilter.MatchesRecord(bcRecordData)) then begin         {!!.02}
        Result := DBIERR_NOCURRREC;
        Exit;
      end;
    finally
      RelContentLock(ffclmRead);
    end;
//    end;                                                             {Deleted !!.02}
  end;

  { There must have been a transaction started for our owning database. }
  if not assigned(Database.Transaction) then begin
    Result := DBIERR_NOACTIVETRAN;
    Exit;
  end;

  Result := DBIERR_NONE;

end;
{--------}
procedure TffSrBaseCursor.ReadAutoIncValue(var aValue: TffWord32);
begin
  AcqContentLock(ffclmRead);
  try
    aValue := FFTblReadAutoIncValue(bcTable.Files[0], bcDatabase.TransactionInfo);
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrBaseCursor.bcGetDictionary : TffDataDictionary;
begin
  Result := Table.Dictionary;
end;
{--------}
function TffSrBaseCursor.GetRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult;
begin
  { Request a lock on the record prior to our testing any logic.  We must
    make sure that a delete in progress has finished before we make
    any decisions. }
{Begin !!.03}{Begin !!.06}
  if (bcInfo.pos = cpOnRecord) and (aLockType <> ffsltNone) then begin
    { If there is a write lock on the table then return an error. }
    if (bcTable.btClientLocks.Count > 0) then
      { If table is write locked but not by this client then cannot obtain
        a lock on the record. If table is read locked by any client then cannot
        obtain a lock on the record. }
      if Table.btClientLocks.SummaryMode = ffsltExclusive then begin
        if (not bcTable.HasClientLock(CursorID)) then begin
          Result := DBIERR_FILELOCKED;
          Exit;
        end;
      end
      else begin
        Result := DBIERR_FILELOCKED;
        Exit;
      end;

    { Made it this far. Obtain the record lock. }
    Table.GetRecordLock(bcDatabase.TransactionInfo,                    {!!.10}
                        bcDatabase.DatabaseID,                         {!!.10}
                        CursorID, bcInfo.refNr, aLockType);            {!!.10}
    bcLockedRefNum := bcInfo.refNr;
  end;  { if }
{End !!.03}{End !!.06}

  if (bcInfo.pos = cpOnRecord) then begin
    AcqContentLock(ffclmRead);
    bcInfoLock.Lock;                                                   {!!.06}
    try
      Result := Table.GetRecord(bcDatabase.TransactionInfo,            {!!.10}
                                bcDatabase.DatabaseID,                 {!!.10}
                                CursorID,                              {!!.10}
                                bcInfo.refNr, aData, aLockType, true, false);  {!!.02}
      if Assigned(bcFilter) then begin
        if not bcFilter.MatchesRecord(aData) then begin
          { Release the record lock. }
          Table.RelRecordLock(bcDatabase.TransactionInfo,              {!!.10}
                              bcDatabase.DatabaseID,                   {!!.10}
                              CursorID, bcInfo.RefNr);                 {!!.10}
          Result := DBIERR_NOCURRREC;
          Exit;
        end;
      end;
{Begin !!.02}
      if (Result = DBIERR_NONE) and (aData <> nil) then
        Move(aData^, bcRecordData^, bcRecordLen);
{End !!.02}
    finally
      bcInfoLock.Unlock;                                               {!!.06}
      RelContentLock(ffclmRead);
    end;
  end
  else if bcInfo.pos = cpEOF then
    Result := DBIERR_EOF
  else if bcInfo.Deleted then
    Result := DBIERR_KEYORRECDELETED
  else
    Result := DBIERR_NOCURRREC;
end;
{--------}
function TffSrBaseCursor.GetRecordField(aField : integer;
                                        aRecordBuffer  : PffByteArray;
                                    var isNull: boolean;
                                        aFieldBuffer : pointer) : TffResult;
begin
  Result := DBIERR_NONE;
  bcTable.Dictionary.GetRecordField(aField, aRecordBuffer, isNull, aFieldBuffer);
end;
{--------}
function TffSrBaseCursor.IsRecordLocked(aLockType : TffSrLockType) : Boolean;
begin
  Result := bcTable.IsRecordLocked(Database.TransactionInfo, CursorID,
                                  bcInfo.refNr, aLockType);
end;
{Begin !!.03}
{--------}
procedure TffSrBaseCursor.ListBLOBFreeSpace(aTI : PffTransInfo;
                                      const aInMemory : Boolean;
                                            aStream : TStream);
begin
  Assert(bcTable <> nil);
  bcTable.ListBLOBFreeSpace(aTI, aInMemory, aStream);
end;
{End !!.03}
{--------}
function TffSrBaseCursor.NotifyExtenders(const anAction      : TffEngineAction;
                                         const aFailAction   : TffEngineAction) : TffResult;
var
  anExtender : TffBaseEngineExtender;
  anIndex    : Longint;
  anIndex2   : Longint;
begin
  Result := DBIERR_NONE;
  if assigned(bcExtenders) then
    for anIndex := 0 to pred(bcExtenders.Count) do begin
      anExtender := TffBaseEngineExtender
                      (TffIntListItem(bcExtenders[anIndex]).KeyAsInt);
      if (anAction in anExtender.InterestedActions) or
         (anExtender.InterestedActions = []) then begin
        Result := anExtender.Notify(Self, anAction);                  {!!.06}
          {since we aren't ignoring Notify's error code, we must
           capture it.  If an extender reports an error we will not
           process the rest of the extenders and we will notify the
           previous extenders that we are "undoing" the previous action}
        if Result <> DBIERR_NONE then begin
          for anIndex2 := 0 to pred(anIndex) do begin
            anExtender := TffBaseEngineExtender
                          (TffIntListItem(bcExtenders[anIndex2]).KeyAsInt);
            anExtender.Notify(self, aFailAction);
          end;
          break;
        end;
      end;
    end;
end;
{--------}
function TffSrBaseCursor.OverrideFilter(aExpression : pCANExpr;
                                        aTimeout    : TffWord32)
                                                    : TffResult;
begin
  Result := DBIERR_NONE;
  try
    bcFilterSav := bcFilter;
    bcFilter := nil;
    if Assigned(aExpression) then
      bcFilter := TffSrFilter.Create(self, bcTable.Dictionary,         {!!.11}
                                     aExpression,
                                     aTimeout);
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E,
                                         bcEngine.FEventLog,
                                         bcEngine.bseGetReadOnly);
    end;
  end;
end;
{--------}
procedure TffSrBaseCursor.RelContentLock(aMode : TffContentLockMode);
begin
  if (fffaBLOBChainSafe in bcGetAttribs) or                            {!!.05}
     (bcExclOwner and (not bcTable.Dictionary.HasBLOBFields)) then     {!!.03}{!!.05}
    Exit;

  Assert(assigned(bcDatabase.Transaction) or (aMode = ffclmRead));

  if assigned(bcDatabase.Transaction) then begin
    { Content locks obtained by a transaction via AcqContentLock are freed when
      the transaction's locks are released. }
    if aMode = ffclmCommit then
      bcTable.EndCommit(bcDatabase.DatabaseID);
  end else begin                                                       {!!.05 - Start}
    InterlockedDecrement(bcNumReadLocks);
    { If the number of read locks ever goes below 0, it's outta whack.}
    Assert(bcNumReadLocks >= 0);
    if (bcNumReadLocks = 0) then
      bcTable.EndRead;
  end;                                                                 {!!.05 - End}
end;
{--------}
procedure TffSrBaseCursor.RelRecordLock(aAllLocks : boolean);
begin
  Assert((not aAllLocks), 'Unsupported: Release all record locks for a cursor');
{Begin !!.03}
  if not FFI64IsZero(bcInfo.refNr) then begin
    bcTable.RemoveLocksForCursor(bcDatabase.DatabaseID,                {!!.10}
                                 CursorID, bcInfo.refNr,               {!!.10}
                                 bcDatabase.TransactionInfo);          {!!.10}
    if FFCmpI64(bcInfo.refNr, bcLockedRefNum) = 0 then
      FFInitI64(bcLockedRefNum);
  end;
{End !!.03}
end;
{--------}
procedure TffSrBaseCursor.RelTableLock(aAllLocks : Boolean);
begin
  bcTable.RelLock(CursorID, aAllLocks);
end;
{--------}
procedure TffSrBaseCursor.RemoveIfUnused;                              {!!.05 - Added}
begin
  if (State = ffosClosing) then
    Free;
end;                                                                   {!!.05 - End added}
{--------}
function TffSrBaseCursor.RestoreFilter : TffResult;
begin
  Result := DBIERR_NONE;
  try
    bcFilter.Free;
    bcFilter := bcFilterSav;
    bcFilterSav := nil;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E,
                                         bcEngine.FEventLog,
                                         bcEngine.bseGetReadOnly);
    end;
  end;
end;
{--------}
procedure TffSrBaseCursor.SetAutoIncValue(aValue: TffWord32);
begin
  AcqContentLock(ffclmWrite);
  FFTblSetAutoIncValue(bcTable.Files[0], bcDatabase.TransactionInfo, aValue);
end;
{--------}
function TffSrBaseCursor.SetFilter(aExpression : pCANExpr;
                                   aTimeout    : TffWord32) : TffResult;

begin
  Result := DBIERR_NONE;
  try
    bcFilter.Free;
    bcFilter := nil;
    if Assigned(aExpression) then
      bcFilter := TffSrFilter.Create(self, bcTable.Dictionary,         {!!.11}
                                     aExpression, aTimeout);           {!!.11}
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, bcEngine.EventLog, bcEngine.bseGetReadOnly);
    end;
  end;
end;
{Begin !!.01}
{--------}
function TffSrBaseCursor.ShouldClose : boolean;
begin
  Result := (bcDatabase.Transaction = nil) and (soState = ffosClosing);
end;
{End !!.01}
{--------}
function TffSrBaseCursor.SortRecords(aFieldsArray : TffFieldList;
                               const aOrderByArray : TffOrderByArray;
                               const aNumFields : integer) : TffResult;
var
  aRecord : PffByteArray;
  aTransID : TffTransID;
  RecLen : Longint;
  SortEngine : TffSrBaseSortEngine;
begin

  { The cursor must have Exclusive Read-Write access. }
  Result := bcCheckExclusiveReadWrite;
  if Result <> DBIERR_NONE then
    exit;

  { Create the sort engine. }
  SortEngine := ffcSortEngineClass.Create(bcEngine, bcDatabase, aFieldsArray,
                                          aOrderByArray, aNumFields,
                                          bcTable.Dictionary, bcIndexID);
  RecLen := bcTable.Dictionary.RecordLength;
  FFGetMem(aRecord, RecLen);
  try
    { Start a transaction. }
    bcEngine.seTransactionStart(bcDatabase, false, true, aTransID);
    try
      { Position to the beginning of the table. }
      Result := DBIERR_NONE;
      SetToBegin;

      { Walk through the records, posting them to the sort engine. }
      while (Result = DBIERR_NONE) do begin
        Result := GetNextRecord(aRecord, ffsltNone);
        if Result = DBIERR_NONE then begin
          SortEngine.Put(aRecord);
        end;
      end;
      bcEngine.seTransactionCommit(bcDatabase);
    except
      bcEngine.seTransactionRollback(bcDatabase);
      raise;
    end;

    bcEngine.seTransactionStart(bcDatabase, false, true, aTransID);
    try
{Begin !!.01}
      { Empty the table. }
//      Result := Empty;
//      if Result = DBIERR_NONE then begin
      { Position to start of table. We will overwrite existing records
        in order to preserve BLOB data. }
      Result := DBIERR_NONE;
      SetToBegin;
        { Read the records back from the sort engine. }
      while (Result = DBIERR_NONE) do begin
        if SortEngine.Get(aRecord) then begin
          GetNextRecord(nil, ffsltNone);
          Result := ModifyRecord(aRecord, true);
        end
        else
          break;
      end;
//      end;
{End !!.01}
      bcEngine.seTransactionCommit(bcDatabase);
    except
      { Rollback if an exception occurs. }
      bcEngine.seTransactionRollback(bcDatabase);
      raise;
    end;
  finally
    FFFreeMem(aRecord, RecLen);
    SortEngine.Free;
  end;

  SetToBegin;

end;
{====================================================================}

{===TffSrCursor======================================================}
constructor TffSrCursor.Create(anEngine   : TffServerEngine;
                               aDatabase  : TffSrDatabase;
                         const aTimeout   : Longint);
begin
  bcTableClass := TffSrTable;
  inherited Create(anEngine, aDatabase, aTimeout);
end;
{--------}
destructor TffSrCursor.Destroy;
begin

  { Notify extenders. }
  NotifyExtenders(ffeaBeforeCursorClose, ffeaNoAction);

{Begin !!.02}
  bcEngine.TableList.BeginRead;
  try
    { If we exclusively opened the table then remove the mark from the
      table. }
    if bcExclOwner then begin
      bcTable.SetExclOwner(ffc_W32NoValue);
      bcExclOwner := False;
    end;

    { Free the table locks held by the cursor. }
    if assigned(bcTable) then
      bcTable.RelLock(CursorID, True);
  finally
    bcEngine.TableList.EndRead;
  end;
{End !!.02}

//  if (bcRecordData <> nil) then                                      {!!.01}
//    FFFreeMem(bcRecordData, bcRecordLen);                            {!!.01}
  if (bcRng1Key <> nil) then begin
    FFFreeMem(bcRng1Key, scKeyLen);
    bcRng1Key := nil;
  end;
  if (bcRng2Key <> nil) then begin
    FFFreeMem(bcRng2Key, scKeyLen);
    bcRng2Key := nil;
  end;
  if (bcCurKey <> nil) then begin
    FFFreeMem(bcCurKey, scKeyLen);
    bcCurKey := nil;
  end;
  bcFilter.Free;
  bcFilter := nil;
  bcFilterSav.Free;
  bcFilterSav := nil;
  inherited Destroy;
end;
{--------}
function TffSrCursor.AddIndexToTable(const aIndexDesc : TffIndexDescriptor) : TffResult;
begin

  { The cursor must have Exclusive Read-Write access. }
  Result := bcCheckExclusiveReadWrite;
  if Result <> DBIERR_NONE then
    exit;

  { The index descriptor cannot be a user-defined index. }
  if (aIndexDesc.idCount = -1) then begin
    Result := DBIERR_INVALIDINDEXTYPE;
    Exit;
  end;

  { The index descriptor must be valid. }
  if not Table.Dictionary.IsIndexDescValid(aIndexDesc) then begin
    Result := DBIERR_INVALIDIDXDESC;
    Exit;
  end;

  { The index name cannot already exist. }
  if (Table.Dictionary.GetIndexFromName(aIndexDesc.idName) <> -1) then begin
    Result := DBIERR_INDEXEXISTS;
    Exit;
  end;

  { There must be room for the new index. }
  if (Table.Dictionary.IndexCount = ffcl_MaxIndexes) then begin
    Result := DBIERR_INDEXLIMIT;
    Exit;
  end;

  { Let the table do its stuff. }
  Result := DBIERR_NONE;
  AcqContentLock(ffclmWrite);
  Table.AddIndex(aIndexDesc, Database.TransactionInfo)
end;
{--------}
procedure TffSrCursor.bcInit(const aOpenMode  : TffOpenMode;
                             const aShareMode : TffShareMode;
                             const aExclContLock : Boolean);           {!!.10}
begin
  inherited bcInit(aOpenMode, aShareMode, aExclContLock);              {!!.10}

  { Resolve any special build key and compare key routine links
    (i.e., user-defined indexes) for the new table. }
  {TffSrTable(bcTable).ResolveDynamicLinks;}                           {!!.06}

  { Get our work areas for the key. }
  bcKID.kidCompareData := @bcCompareData;
  scKeyLen := bcTable.Dictionary.IndexKeyLength[bcIndexID];
  FFGetMem(bcCurKey, scKeyLen);
  FFGetMem(bcRng1Key, scKeyLen);
  FFGetMem(bcRng2Key, scKeyLen);

  { Initialise our key index data record. }
  bcTable.MakeKIDForCursor(bcIndexID, bcKID);

  { Set up the position of the cursor to BOF. }
  SetToBegin;
end;
{--------}
procedure TffSrCursor.bcTableOpenPreconditions(aTable     : TffSrBaseTable;
                                         const aIndexName : string;
                                           var aIndexID   : Longint;
                                         const aOpenMode  : TffOpenMode);
begin

  { Validate the index information; if the index name is non-blank
    it must exist and will supercede the index number; if the index
    name is blank the index number must exist}
  if (aIndexName <> '') then begin
    aIndexID := aTable.Dictionary.GetIndexFromName(aIndexName);
    if (aIndexID = -1) then
      FFRaiseException(EffException, ffStrResServer, fferrUnknownIndex,
                       [aTable.BaseName, aIndexName, aIndexID]);
  end
  else if (0 > aIndexID) or (aIndexID >= aTable.Dictionary.IndexCount) then
    FFRaiseException(EffException, ffStrResServer, fferrUnknownIndex,
                     [aTable.BaseName, aIndexName, aIndexID]);

  { If the table's data file is open in read-only mode it means the
    physical file is read-only: hence this call's openmode must be
    read-only as well. }
  if (aTable.Files[0]^.fiOpenMode = omReadOnly) and
     (aOpenMode <> omReadOnly) then
    FFRaiseException(EffException, ffStrResServer, fferrCursorReadOnly,
                     [aTable.BaseName]);

end;
{--------}
function TffSrCursor.CheckBookmark(aBookmark : PffByteArray) : TffResult;
var
  CheckHash : Longint;
begin
  Result := DBIERR_INVALIDBOOKMARK;
  if (aBookmark = nil) then
    Exit;
  with PffSrBookmark(aBookmark)^ do begin
    if (sbIndexID <> IndexID) then
      Exit;
    if (sbKeyLen <> scKeyLen) then
      Exit;
    CheckHash := FFCalcElfHash(sbIndexID,
                               ffcl_FixedBookmarkSize - sizeof(sbHash) + sbKeyLen);
    if (sbHash <> CheckHash) then
      Exit;
  end;
  Result := DBIERR_NONE;
end;
{--------}
procedure TffSrCursor.ClearIndex;
begin
  with bcCompareData do begin
    cdFldCnt := 0;
    cdPartLen := 0;
  end;
  AcqContentLock(ffclmWrite);
  FFTblDeleteAllKeys(Database.TransactionInfo, bcKID);
end;
{--------}
function TffSrCursor.CloneCursor(aOpenMode : TffOpenMode) : TffSrBaseCursor;
begin
  {NOTE: we are not checking rights for this action because the client
         had the rights to open the cursor}

  { Resolve the open mode. }
  if (bcOpenMode = omReadOnly) then
    aOpenMode := omReadOnly;

  { Create the cursor. }
  Result := bcEngine.CursorClass.Create(bcEngine,                      {!!.06}
                                        bcDatabase,
                                        soTimeout);
  Result.Open(bcTable.BaseName , '', bcIndexID, aOpenMode, smShared,
              bcTable.IsServerTable, False, []);

  AcqContentLock(ffclmRead);
  try
    { Set up all of the position, range, etc, fields. }
    Result.bcKID := bcKID;
    Result.bcKID.kidCompareData := @Result.bcCompareData;
    Result.bcCompareData := bcCompareData;
    Result.bcInfo := bcInfo;
    if bcInfo.KeyValid then
      Move(bcCurKey^, Result.bcCurKey^, scKeyLen);
    Result.bcHasRange := bcHasRange;
    if bcHasRange then begin
      Result.bcRng1Valid := bcRng1Valid;
      if bcRng1Valid then begin
        Move(bcRng1Key^, Result.bcRng1Key^, scKeyLen);
        Result.bcRng1FldCnt := bcRng1FldCnt;
        Result.bcRng1PtlLen := bcRng1PtlLen;
        Result.bcRng1Incl := bcRng1Incl;
      end;
      Result.bcRng2Valid := bcRng2Valid;
      if bcRng2Valid then begin
        Move(bcRng2Key^, Result.bcRng2Key^, scKeyLen);
        Result.bcRng2FldCnt := bcRng2FldCnt;
        Result.bcRng2PtlLen := bcRng2PtlLen;
        Result.bcRng2Incl := bcRng2Incl;
      end;
    end;
    if Assigned(bcFilter) then
      Result.SetFilter(bcFilter.Expression,bcFilter.Timeout);
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrCursor.CompareBookmarks(aBookmark1, aBookmark2 : PffByteArray;
                                  var CmpResult : Longint) : TffResult;
var
  BM1 : PffSrBookmark absolute aBookmark1;
  BM2 : PffSrBookmark absolute aBookmark2;
begin
  Result := CheckBookmark(aBookmark1);
  if (Result = DBIERR_NONE) then
    Result := CheckBookmark(aBookmark2);
  if (Result <> DBIERR_NONE) then
    Exit;
  case BM1^.sbPos of
    cpUnknown : CmpResult := -1;
    cpBOF     : if (BM2^.sbPos = cpBOF) then
                  CmpResult := 0
                else
                  CmpResult := -1;
    cpEOF     : if (BM2^.sbPos = cpEOF) then
                  CmpResult := 0
                else
                  CmpResult := 1;
  else
    {bookmark 1 is on a crack or on a record}
    case BM2^.sbPos of
      cpUnknown : CmpResult := 1;
      cpBOF     : CmpResult := 1;
      cpEOF     : CmpResult := -1;
    else
      {bookmark 2 is also on a crack or on a record}
      {check the reference numbers, if equal the key ought to be}
      if (ffCmpI64(BM1^.sbRefNr, BM2^.sbRefNr) = 0) then
        CmpResult := 0
      else begin
        {compare the keys}
        with bcCompareData do begin
          cdFldCnt := 0;
          cdPartLen := 0;
        end;
        CmpResult := Table.CompareKeysForCursor(bcKID,
                                                PffByteArray(@BM1^.sbKey),
                                                PffByteArray(@BM2^.sbKey));
        if (CmpResult = 0) then
          if (ffCmpI64(BM1^.sbRefNr, BM2^.sbRefNr) = -1) then
            CmpResult := -1
          else
            CmpResult := 1;
      end;
    end;{case}
  end;{case}
end;
{--------}
function TffSrCursor.DropIndexFromTable(const aIndexName : TffDictItemName;
                                              aIndexID   : Longint) : TffResult;
var
  CompareData : TffCompareData;
  KID         : TffKeyIndexData;
begin

  {if the index name is set, convert to an index ID}
  if (aIndexName <> '') then
    aIndexID := Table.Dictionary.GetIndexFromName(aIndexName);

  {check the index number (count index 0 as invalid as well)}
  if (aIndexID <= 0) or (aIndexID >= Table.Dictionary.IndexCount) then begin
    Result := DBIERR_NOSUCHINDEX;
    Exit;
  end;

  {the index number cannot be our index number}
  if (aIndexID = IndexID) then begin
    Result := DBIERR_ACTIVEINDEX;
    Exit;
  end;

  { The cursor must have Exclusive Read-Write access. }
  Result := bcCheckExclusiveReadWrite;
  if Result <> DBIERR_NONE then
    exit;

  { Delete all the keys and then drop the index. }
  Result := DBIERR_NONE;
  KID.kidCompareData := @CompareData;
  Table.MakeKIDForCursor(aIndexID, KID);
  AcqContentLock(ffclmWrite);
  FFTblDeleteAllKeys(Database.TransactionInfo, KID);
  Table.DropIndex(Database.TransactionInfo, aIndexID);

end;
{--------}
function TffSrCursor.ExtractKey(aData : PffByteArray; aKey : PffByteArray) : TffResult;
begin
  Result := DBIERR_NOCURRREC;
  if (aData = nil) and (bcInfo.pos = cpOnRecord) then begin
    aData := bcRecordData;
    AcqContentLock(ffclmRead);
    try
      Table.GetRecord(bcDatabase.TransactionInfo,                      {!!.10}
                      bcDatabase.DatabaseID,                           {!!.10}
                      CursorID, bcInfo.refNr, aData,                   {!!.10}
                      ffsltNone, false, false);                        {!!.02}
    finally
      RelContentLock(ffclmRead);
    end;
    if Assigned(bcFilter) then
      if not bcFilter.MatchesRecord(aData) then
        aData := nil;
  end;
  if (aData <> nil) then begin
    Result := Table.BuildKeyForRecord(IndexID, aData, aKey, 0, 0);
  end;
end;
{--------}
function TffSrCursor.GetBookmark(aBookmark : PffByteArray) : TffResult;
begin
  Result := DBIERR_NONE;
  AcqContentLock(ffclmRead);
  try
    FillChar(PffSrBookmark(aBookmark)^, ffcl_FixedBookmarkSize, 0);
    with PffSrBookmark(aBookmark)^ do begin
      sbIndexID := IndexID;
      sbPos := bcInfo.pos;
      sbKeyValid := bcInfo.KeyValid;
      sbRefNr := bcInfo.refNr;
      sbKeyLen := scKeyLen;
      if bcInfo.KeyValid then
        Move(bcCurKey^, sbKey, scKeyLen)
      else
        FillChar(sbKey, scKeyLen, 0);
      sbHash := FFCalcElfHash(sbIndexID,
                              ffcl_FixedBookmarkSize - sizeof(sbHash) + sbKeyLen);
    end;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrCursor.GetBookmarkSize : integer;
begin
  Result := ffcl_FixedBookmarkSize + scKeyLen;
end;
{--------}
function TffSrCursor.GetNextRecord(aData     : PffByteArray;
                                   aLockType : TffSrLockType) : TffResult;
var
  KeyCompareResult : integer;
  Action           : TffSearchKeyAction;
begin
  { If we are at EOF, then obviously there's no next record. }
  if (bcInfo.pos = cpEOF) then begin
    Result := DBIERR_EOF;
    Exit;
  end;

  AcqContentLock(ffclmRead);
  bcInfoLock.Lock;                                                     {!!.06}
  try
    { If our position is at BOF and we have a range active, position the
      index at the start of the range}
    if (bcInfo.pos = cpBOF) and bcHasRange and bcRng1Valid then begin
      { Position at start of range. }
      if bcRng1Incl then
        Action := skaGreaterEqual
      else
        Action := skaGreater;
      { Note: the following call will always return true in this case. }
      Move(bcRng1Key^, bcCurKey^, scKeyLen);
      with bcCompareData do begin
        cdFldCnt := bcRng1FldCnt;
        cdPartLen := bcRng1PtlLen;
      end;
      Table.FindKey(bcKID, bcInfo.refNr, Database.TransactionInfo,
                    bcCurKey, bcInfo.KeyPath, Action);

      { Is the keypath positioned at EOF? }
      if (bcInfo.KeyPath.kpPos = kppEOF) then begin
        {Yes.  The start of the range is at EOF, so it's not likely we'll find a
         'next record <g>. }
        Result := DBIERR_EOF;
        SetToEnd;
        Exit;
      end;

      { Make sure the keypath is on the crack before the key so that the next
        key call returns the right record. }
      if (bcInfo.KeyPath.kpPos = kppOnKey) then begin
        Assert(bcInfo.keyPath.kpCount > 0);
        bcInfo.KeyPath.kpPos := kppOnCrackBefore;
      end;

    end
    { Otherwise do we need to rebuild the key path? }
    else if (not bcIsCurKeyPathValid) then
      bcRebuildKeyPath;                                                {!!.05}

    { Make sure that we have somewhere to read the record into. }
    if (aData = nil) then
      aData := bcRecordData;

    { Get the next record. }
    with bcCompareData do begin
      cdFldCnt := 0;
      cdPartLen := 0;
    end;

    if Assigned(bcFilter) then
      bcFilter.BeginTimeout;
    repeat
      Result := bcTable.GetNextRecord(bcDatabase.TransactionInfo,
                                      bcDatabase.DatabaseID,           {!!.10}
                                      CursorID, bcKID, bcInfo.refNr, bcCurKey,
                                      bcInfo.KeyPath, aData, aLockType);
      if (Result <> DBIERR_NONE) then begin
        if (Result = DBIERR_EOF) then
          SetToEnd;
        Exit;
      end;
      {in theory we're on a record}
      bcInfo.Deleted := False;
      bcInfo.KeyValid := True;
      bcInfo.pos := cpOnRecord;
      {check that we're in range if required}
      if bcHasRange and bcRng2Valid then begin
        {check whether beyond end of range}
        with bcCompareData do begin
          cdFldCnt := bcRng2FldCnt;
          cdPartLen := bcRng2PtlLen;
        end;
        KeyCompareResult := bcTable.CompareKeysForCursor(bcKID, bcCurKey, bcRng2Key);
        if (KeyCompareResult > 0) or
           ((KeyCompareResult = 0) and (not bcRng2Incl)) then begin
          Result := DBIERR_EOF;
          SetToEnd;
        end;
      end;
{Begin !!.03}
//    until (Result <> DBIERR_NONE) or not Assigned(bcFilter) or
//      bcFilter.MatchesRecord(aData) or bcFilter.CheckTimeout(Result);
    until (Result <> DBIERR_NONE) or ((not Assigned(bcFilter) or
      bcFilter.MatchesRecord(aData) or bcFilter.CheckTimeout(Result)) and
     (not Assigned(bcFilterSav) or bcFilterSav.MatchesRecord(aData)));
{End !!.03}

    { Place the lock if needed... record will not be read again}
{Begin !!.02}
    if (Result = DBIERR_NONE) then begin
      if aData <> bcRecordData then
        Move(aData^, bcRecordData^, bcRecordLen);
      if (aLockType <> ffsltNone) then
        Result := bcTable.GetRecord(bcDatabase.TransactionInfo,        {!!.10}
                                    bcDatabase.DatabaseID,             {!!.10}
                                    CursorID,                          {!!.10}
                                    bcInfo.refNr, nil, aLockType, false, false);  {!!.02}
    end;
{End !!.02}
  finally
    bcInfoLock.Unlock;                                                 {!!.06}
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrCursor.GetPriorRecord(aData : PffByteArray; aLockType : TffSrLockType) : TffResult;
var
  KeyCompareResult : integer;
  Action           : TffSearchKeyAction;
begin

  { If we are at BOF, then obviously there's no prior record. }
  if (bcInfo.pos = cpBOF) then begin
    Result := DBIERR_BOF;
    Exit;
  end;

  AcqContentLock(ffclmRead);
  bcInfoLock.Lock;                                                     {!!.06}
  try
    { If our position is at EOF and we have a range active, position the
      index at the end of the range. }
    if (bcInfo.pos = cpEOF) and bcHasRange and bcRng2Valid then begin
      { Position at end of range. }
      if bcRng2Incl then
        Action := skaGreater
      else
        Action := skaGreaterEqual;

      { Note: the following call will always return true in this case. }
      Move(bcRng2Key^, bcCurKey^, scKeyLen);
      with bcCompareData do begin
        cdFldCnt := bcRng2FldCnt;
        cdPartLen := bcRng2PtlLen;
      end;
      bcTable.FindKey(bcKID, bcInfo.refNr, bcDatabase.TransactionInfo,
                     bcCurKey, bcInfo.KeyPath, Action);
    end
    { Otherwise, do we need to rebuild the key path? }
    else if (not bcIsCurKeyPathValid) then
      bcRebuildKeyPath;                                                {!!.05}

    { Make sure that we have somewhere to read the record into. }
    if (aData = nil) then
      aData := bcRecordData;

    { Get the previous record. }
    with bcCompareData do begin
      cdFldCnt := 0;
      cdPartLen := 0;
    end;

    if Assigned(bcFilter) then
      bcFilter.BeginTimeout;
    repeat
      Result := bcTable.GetPriorRecord(bcDatabase.TransactionInfo,
                                       bcDatabase.DatabaseID,          {!!.10}
                                       CursorID, bcKID,
                                       bcInfo.refNr, bcCurKey,
                                       bcInfo.KeyPath, aData, ffsltNone);
      if (Result <> DBIERR_NONE) then begin
        if (Result = DBIERR_BOF) then
          SetToBegin;
        Exit;
      end;

      { In theory we're on a record. }
      bcInfo.Deleted := false;
      bcInfo.KeyValid := true;
      bcInfo.pos := cpOnRecord;

      { Check that we're in range if required. }
      if bcHasRange and bcRng1Valid then begin
        {check whether beyond start of range}
        with bcCompareData do begin
          cdFldCnt := bcRng1FldCnt;
          cdPartLen := bcRng1PtlLen;
        end;
        KeyCompareResult := bcTable.CompareKeysForCursor(bcKID, bcCurKey, bcRng1Key);
        if (KeyCompareResult < 0) or
           ((KeyCompareResult = 0) and (not bcRng1Incl)) then begin
          Result := DBIERR_BOF;
          SetToBegin;
        end;
      end;
    until (Result <> DBIERR_NONE) or not Assigned(bcFilter) or
      bcFilter.MatchesRecord(aData) or bcFilter.CheckTimeout(Result);

    { Place the lock if needed... record will not be read again. }
{Begin !!.02}
    if (Result = DBIERR_NONE) then begin
      if aData <> bcRecordData then
        Move(aData^, bcRecordData^, bcRecordLen);
      if (aLockType <> ffsltNone) then
        Result := bcTable.GetRecord(bcDatabase.TransactionInfo,        {!!.10}
                                    bcDatabase.DatabaseID,             {!!.10}
                                    CursorID,                          {!!.10}
                                    bcInfo.refNr, nil, aLockType, false, false);  {!!.02}
    end;
{End !!.02}
  finally
    bcInfoLock.Unlock;                                                 {!!.06}
    RelContentLock(ffclmRead);
  end;

end;
{--------}
function TffSrCursor.GetRecordCount(var aRecCount : Longint) : TffResult;
var
  Action           : TffSearchKeyAction;
  KeyCompareResult : integer;
  SavedKey         : PffByteArray;
  Info             : TffRecordInfo;
begin
  Result := DBIERR_NONE;
  AcqContentLock(ffclmRead);
  try
    if bcHasRange or Assigned(bcFilter) then begin
      {set count to zero}
      aRecCount := 0;
      {save the current position}
      bcSaveCurInfo;
      FFGetMem(SavedKey, bcKID.kidCompareData^.cdKeyLen);              {!!.06}
      try
        Move(bcCurKey^, SavedKey^, bcKID.kidCompareData^.cdKeyLen);
        {BOF}
        SetToBegin;
        if bcHasRange and bcRng1Valid then begin
          {position at start of range}
          if bcRng1Incl then
            Action := skaGreaterEqual
          else
            Action := skaGreater;
          {note: the following FindKey call will always return true in
           this case}
          Move(bcRng1Key^, bcCurKey^, scKeyLen);
          with bcCompareData do begin
            cdFldCnt := bcRng1FldCnt;
            cdPartLen := bcRng1PtlLen;
          end;
          Table.FindKey(bcKID, bcInfo.refNr, Database.TransactionInfo, bcCurKey,
                        bcInfo.KeyPath, Action);
          {check whether the keypath was positioned at EOF, if so the
           start of the range is at EOF, so it's not likely we'll find a
           'next' key or any keys at all <g>}
          if (bcInfo.KeyPath.kpPos = kppEOF) then begin
            {note the reset of the cursor position still occurs}
            Exit;
          end;
          {make sure that the keypath is on the crack before the key so that
           the next key call in a minute returns the right record}
          if (bcInfo.KeyPath.kpPos = kppOnKey) then begin
            Assert(bcInfo.KeyPath.kpCount > 0);
            bcInfo.KeyPath.kpPos := kppOnCrackBefore;
          end;
        end;
        {while not EOF or other error do}
        while (Result = DBIERR_NONE) do begin
{Begin !!.05}
          { Check for timeout. }
          if FFGetRetry < GetTickCount then
            FFRaiseExceptionNoData(EffServerException, ffStrResServer,
                                   fferrGeneralTimeout);
{End !!.05}
          {readnext key}
          Result := Table.GetNextKey(bcKID, bcInfo.refNr, Database.TransactionInfo,
                                     bcCurKey, bcInfo.KeyPath);
          if (Result = DBIERR_NONE) then begin
            {check that we're in range if required}
            if bcHasRange and bcRng2Valid then begin
              {check whether beyond end of range}
              with bcCompareData do begin
                cdFldCnt := bcRng2FldCnt;
                cdPartLen := bcRng2PtlLen;
              end;
              KeyCompareResult :=
                 Table.CompareKeysForCursor(bcKID, bcCurKey, bcRng2Key);
              if (KeyCompareResult > 0) or
                ((KeyCompareResult = 0) and (not bcRng2Incl)) then begin
                Result := DBIERR_EOF;
              end
              else {key is in range} begin
                if Assigned(bcFilter) then begin
                  Table.GetRecord(bcDatabase.TransactionInfo,          {!!.10}
                                  bcDatabase.DatabaseID,               {!!.10}
                                  CursorID, bcInfo.refNr,              {!!.10}
                                  bcRecordData, ffsltNone, false, false);  {!!.02}
                  if bcFilter.MatchesRecord(bcRecordData) then
                    inc(aRecCount);
                end else
                inc(aRecCount);
              end;
            end
            else {end of range = end of index path} begin
              if Assigned(bcFilter) then begin
                Table.GetRecord(bcDatabase.TransactionInfo,            {!!.10}
                                bcDatabase.DatabaseID,                 {!!.10}
                                CursorID, bcInfo.refNr,                {!!.10}
                                bcRecordData, ffsltNone, false, false);  {!!.02}
                if bcFilter.MatchesRecord(bcRecordData) then
                  inc(aRecCount);
              end else
              inc(aRecCount);
            end;
          end;
        end;
        Result := DBIERR_NONE;
      {endwhile}
      finally
        {reset current position}
        bcRestoreCurInfo;
        Move(SavedKey^, bcCurKey^, bcKID.kidCompareData^.cdKeyLen);
        FFFreeMem(SavedKey, bcKID.kidCompareData^.cdKeyLen);           {!!.06}
      end;
    end
    else begin
      FFTblGetRecordInfo(Table.Files[0], Database.TransactionInfo, Info);
      aRecCount := Info.riRecCount;
    end;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrCursor.GetRecordForKey(aDirectKey  : boolean;
                                     aFieldCount : integer;
                                     aPartialLen : integer;
                                     aKeyData    : PffByteArray;
                                     aData       : PffByteArray;
                                     aFirstCall  : Boolean) : TffResult;
var
  Action        : TffSearchKeyAction;
  aTI           : PffTransInfo;
  RecFound      : boolean;
  KeyToFind     : PffByteArray;
  TmpCompareData : TffCompareData;                                     {!!.11}
begin
  {calculate the key}
  if aDirectKey then
    Move(aKeyData^, bcCurKey^, scKeyLen)
  else if (IndexID = 0) then begin
    Result := DBIERR_INVALIDINDEXTYPE;
    Exit;
  end
  else begin
    Result := Table.BuildKeyForRecord(IndexID, aKeyData, bcCurKey, aFieldCount,
                                      aPartialLen);
    if (Result <> DBIERR_NONE) then
      Exit;
  end;

  AcqContentLock(ffclmRead);
  bcInfoLock.Lock;                                                     {!!.06}
  try
    {now position the index on that exact key or the one that partially
     matches it}
    if aFirstCall then begin
      FFInitKeyPath(bcInfo.KeyPath);
      bcInfo.refNr.iLow := 0;
      bcInfo.refNr.iHigh := 0;
      bcInfo.Deleted := false;
    end;
    Action := skaEqual;
    {try to find the exact or partial key}
    with bcCompareData do begin
      cdFldCnt := aFieldCount;
      cdPartLen := aPartialLen;
    end;

    Result := DBIERR_NONE;
    aTI := Database.TransactionInfo;
    KeyToFind := nil;
    try
      // we need a copy of the key
      if Assigned(bcFilter) or (not aFirstCall) then begin
        FFGetMem(KeyToFind, scKeyLen);
        Move(bcCurKey^, KeyToFind^, scKeyLen)
      end;

      if Assigned(bcFilter) then begin
        if aData = nil then
          aData := bcRecordData;
        bcFilter.BeginTimeout;
      end;
      repeat
        if aFirstCall then begin
          RecFound := Table.FindKey(bcKID, bcInfo.refNr, aTI,
                                    bcCurKey, bcInfo.KeyPath, Action);
          aFirstCall := False;
        end else begin
          RecFound := (Table.GetNextKey(bcKID, bcInfo.refNr, aTI,
                                        bcCurKey, bcInfo.KeyPath) = DBIERR_NONE) and
            (Table.CompareKeysForCursor(bcKID, bcCurKey, KeyToFind) = 0);
        end;

        if RecFound then begin
          TmpCompareData := bcCompareData;                             {!!.11}
          if IsInRange(bcCurKey) <> 0 then begin                       {!!.11}
            bcCompareData := TmpCompareData;                           {!!.11}
            Result := DBIERR_RECNOTFOUND;                              {!!.11}
          end else begin                                               {!!.11}
            bcCompareData := TmpCompareData;                           {!!.11}
            if Assigned(aData) then
              Table.GetRecord(aTI, bcDatabase.DatabaseID,              {!!.10}
                              CursorID, bcInfo.refNr, aData, ffsltNone,{!!.10}
                              false, false);                           {!!.02}
            bcInfo.KeyValid := true;
            bcInfo.pos := cpOnRecord;
          end
        end else
          Result := DBIERR_RECNOTFOUND;
      until (Result <> DBIERR_NONE) or                                 {!!.11}
            ((not Assigned(bcFilter)) or                               {!!.11}
             bcFilter.MatchesRecord(aData) or                          {!!.11}
             bcFilter.CheckTimeout(Result));                           {!!.11}

      { If we didn't find the key then set to the end of the dataset. }
      if Result = DBIERR_RECNOTFOUND then
        SetToEnd;
    finally
      if Assigned(KeyToFind) then
        FFFreeMem(KeyToFind, scKeyLen);
    end;
  finally
    bcInfoLock.Unlock;                                                 {!!.06}
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrCursor.InsertRecord(aData     : PffByteArray;
                                  aLockType : TffSrLockType) : TffResult;
var
  NewRefNr : TffInt64;
  SavInfo  : TffSrCursorInfo;                                          {!!.12}
  SavKey   : PffByteArray;                                             {!!.12}
begin
  { Notify extenders. }
  bcNewRecBuff := aData;
  SavKey := nil;                                                       {!!.12}
  try
    Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);

    if Result = DBIERR_NONE then begin
      AcqContentLock(ffclmWrite);
      Result := bcTable.InsertRecord(bcDatabase.TransactionInfo,
                                    CursorID, aData, aLockType, NewRefNr);
      if (Result = DBIERR_NONE) then begin
{Begin !!.12}
        { If a range is active then save the current key & cursor information.
          We may need to reposition the cursor to its original position if
          the inserted record does not fit into the range. }
        if bcHasRange then begin
          FFGetMem(SavKey, scKeyLen);
          Move(bcCurKey^, SavKey^, scKeyLen);
          SavInfo := bcInfo;
        end;

        FFInitKeyPath(bcInfo.KeyPath);
        bcInfo.pos := cpOnRecord;
        bcInfo.refNr := NewRefNr;
        bcInfo.Deleted := false;
        scRebuildCurKey(aData, true);
        if bcHasRange and (IsInRange(bcCurKey) <> 0) then begin
          bcInfo := SavInfo;
          Move(SavKey^, bcCurKey^, scKeyLen);
        end;
{End !!.12}
        bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID,  {!!.10}
                                bcInfo.RefNr);                         {!!.10}

        { Notify extenders of successful insert. }
        NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
      end else
        { Notify extenders of failed insert. }
        NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
    end;
  finally
    if SavKey <> nil then                                              {!!.12}
      FFFreeMem(SavKey, scKeyLen);                                     {!!.12}
    bcNewRecBuff := nil;
  end;
end;
{--------}
function TffSrCursor.InsertRecordNoDefault(aData     : PffByteArray;   {!!.10}
                                           aLockType : TffSrLockType) : TffResult;
var
  NewRefNr : TffInt64;
begin
  { Notify extenders. }
  bcNewRecBuff := aData;
  try
    Result := NotifyExtenders(ffeaBeforeRecInsert, ffeaInsertRecFail);

    if Result = DBIERR_NONE then begin
      AcqContentLock(ffclmWrite);
      Result := bcTable.InsertRecordNoDefault(bcDatabase.TransactionInfo,{!!.10}
                                              CursorID, aData, aLockType,
                                              NewRefNr);
      if (Result = DBIERR_NONE) then begin
        FFInitKeyPath(bcInfo.KeyPath);
        bcInfo.pos := cpOnRecord;
        bcInfo.refNr := NewRefNr;
        bcInfo.Deleted := false;
        scRebuildCurKey(aData, true);
        bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID,  {!!.10}
                                bcInfo.RefNr);                         {!!.10}

        { Notify extenders of successful insert. }
        NotifyExtenders(ffeaAfterRecInsert, ffeaNoAction);
      end else
        { Notify extenders of failed insert. }
        NotifyExtenders(ffeaInsertRecFail, ffeaNoAction);
    end;
  finally
    bcNewRecBuff := nil;
  end;
end;
{--------}
function TffSrCursor.IsInRange(aKey : PffByteArray) : integer;
var
  KeyCompareResult : integer;
begin
  Result := 0;
  if not bcHasRange then
    Exit;
  if bcRng1Valid then begin
    with bcCompareData do begin
      cdFldCnt := bcRng1FldCnt;
      cdPartLen := bcRng1PtlLen;
    end;
    KeyCompareResult := Table.CompareKeysForCursor(bcKID, aKey, bcRng1Key);
    if (KeyCompareResult < 0) then begin
      Result := -1;
      Exit;
    end;
    if (KeyCompareResult = 0) then begin
      if not bcRng1Incl then
        Result := -1;
      Exit;
    end;
  end;
  if bcRng2Valid then begin
    with bcCompareData do begin
      cdFldCnt := bcRng2FldCnt;
      cdPartLen := bcRng2PtlLen;
    end;
    KeyCompareResult := Table.CompareKeysForCursor(bcKID, aKey, bcRng2Key);
    if (KeyCompareResult > 0) then begin
      Result := 1;
      Exit;
    end;
    if (KeyCompareResult = 0) then begin
      if not bcRng2Incl then
        Result := 1;
      Exit;
    end;
  end;
end;
{--------}
function TffSrCursor.ModifyRecord(aData : PffByteArray; aRelLock : boolean) : TffResult;
var                                                                    {!!.05}
  aKeyChanged : Boolean;                                               {!!.05}
  SavKey      : PffByteArray;                                          {!!.05}
begin

  { Note: By this time, any other cursor deleting or modifying the record ahead
    of us has completed and has set bcInfo.Deleted.  We can be assured of this
    because TffServerEngine.RecordDelete calls Cursor.EnsureWritable(true) which
    obtains a lock on the record to be deleted.  We won't get that lock until
    the other cursor has finished. }

  { Has this record already been deleted? }
  if bcInfo.Deleted then begin
    { Yes. }
    Result := DBIERR_KEYORRECDELETED;
    Exit;
  end;

  { Are we on a record? }
  if (bcInfo.Pos <> cpOnRecord) then begin
    { No. }
    case bcInfo.Pos of
      cpBOF :  Result := DBIERR_BOF;
      cpEOF :  Result := DBIERR_EOF;
    else
      Result := DBIERR_NOCURRREC;
    end;
    Exit;
  end;

  { Notify extenders. }
  FFGetMem(bcOldRecBuff, bcRecordLen);                                 {!!.02}
  bcNewRecBuff := aData;
  try
    Move(bcRecordData^, bcOldRecBuff^, bcRecordLen);                   {!!.02}
    Result := NotifyExtenders(ffeaBeforeRecUpdate, ffeaUpdateRecFail);
    if Result = DBIERR_NONE then begin
      AcqContentLock(ffclmWrite);
{Begin !!.05}
      Result := bcTable.PutRecord(bcDatabase.TransactionInfo, CursorID,
                                  bcInfo.refNr, aData, aRelLock, aKeyChanged);
      if (Result = DBIERR_NONE) then begin
        bcTable.RelaxRecordLock(bcDatabase.TransactionInfo, CursorID,  {!!.10}
                                bcInfo.RefNr);                         {!!.10}
        { Was the key for the current index changed? }
        SavKey := nil;
        if aKeyChanged then begin
          { Yes. Save the current key & rebuild it so that we may reposition to
            the record. }
          FFGetMem(SavKey, scKeyLen);
          try
            Move(bcCurKey^, SavKey^, scKeyLen);
            scRebuildCurKey(aData, true);
            { Does the new key fall outside of the current range? }
            if IsInRange(bcCurKey) <> 0 then
              { Yes. Restore the old key. The cursor will be repositioned to the
                next record. }
              Move(SavKey^, bcCurKey^, scKeyLen);
          finally
            FFFreeMem(SavKey, scKeyLen);
          end;
        end;

        FFInitKeyPath(bcInfo.KeyPath);
        bcInfo.pos := cpOnRecord;
        bcRebuildKeyPath;
{End !!.05}
        { Notify extenders of successful update. }
        NotifyExtenders(ffeaAfterRecUpdate, ffeaNoAction);
      end else
        { Notify extenders of failed update. }
        NotifyExtenders(ffeaUpdateRecFail, ffeaNoAction);
    end;
  finally
    FFFreeMem(bcOldRecBuff, bcRecordLen);                              {!!.02}
    bcOldRecBuff := nil;
    bcNewRecBuff := nil;
  end;
end;
{--------}
procedure TffSrCursor.ResetRange;
begin
  bcHasRange := false;
end;
{--------}
procedure TffSrCursor.scRebuildCurKey(aRecData : PffByteArray;
                                      aLockObtained : boolean);
begin
  bcInvalidateCurKey;
  if (IndexID = 0) then begin
    Move(bcInfo.refNr, bcCurKey^, scKeyLen);
    bcInfo.KeyValid := true;
  end
  else begin
    AcqContentLock(ffclmRead);
    try
      { If we have been passed the record buffer then use it otherwise
        read the record from the file. }
      if assigned(aRecData) then
        Move(aRecData^, bcRecordData^, Table.Files[0]^.fiRecordLength)
      else
        Table.GetRecord(bcDatabase.TransactionInfo,                    {!!.10}
                        bcDatabase.DatabaseID,                         {!!.10}
                        CursorID, bcInfo.refNr,                        {!!.10}
                        bcRecordData, ffsltNone, aLockObtained, false);  {!!.02}

      {calculate the key for this record}
      bcInfo.KeyValid :=
        (Table.BuildKeyForRecord(IndexID, bcRecordData, bcCurKey, 0, 0) = DBIERR_NONE);
    finally
      RelContentLock(ffclmRead);
    end;
  end;
end;
{--------}
procedure TffSrBaseCursor.bcRebuildKeyPath;                            {!!.05 - Moved from TffSrCursor.scRebuildKeyPath}
var
  InRangeResult : Integer;
begin

  { Assumption: Content read lock already obtained. }

  { If we have a valid key, calculate the actual key path by finding that key
    within the current index. }
  if bcIsCurKeyValid then begin
    FFInitKeyPath(bcInfo.KeyPath);
    with bcCompareData do begin
      cdFldCnt := 0;
      cdPartLen := 0;
    end;
    if Table.FindKey(bcKID, bcInfo.refNr, bcDatabase.TransactionInfo, bcCurKey,
                     bcInfo.KeyPath, skaGreaterEqual) then begin
      { Does the key fit within the current range? }
      InRangeResult := IsInRange(bcCurKey);
      if InRangeResult <> 0 then
        bcInfo.pos := cpOnCrack
      else
        { Make sure that the current position is set to reflect the
          keypath's position. }
        case bcInfo.KeyPath.kpPos of
          kppBOF           : SetToBegin;
          kppOnCrackBefore,
          kppOnCrackAfter  : bcInfo.pos := cpOnCrack;
          kppEOF           : SetToEnd;
        end;{case}
    end;  { if }
  end;
end;
{--------}
function TffSrCursor.SetRange(aDirectKey : boolean;
                              aFieldCount1 : integer;
                              aPartialLen1 : integer;
                              aKeyData1    : PffByteArray;
                              aKeyIncl1    : boolean;
                              aFieldCount2 : integer;
                              aPartialLen2 : integer;
                              aKeyData2    : PffByteArray;
                              aKeyIncl2    : boolean) : TffResult;
begin
  Result := DBIERR_NONE;
  {we now have a range}
  bcRng1Valid := (aKeyData1 <> nil);
  bcRng2Valid := (aKeyData2 <> nil);
  {calculate the keys}
  if aDirectKey then begin
    if bcRng1Valid then
      Move(aKeyData1^, bcRng1Key^, scKeyLen);
    if bcRng2Valid then
      Move(aKeyData2^, bcRng2Key^, scKeyLen);
  end
  else begin
    if bcRng1Valid then
      Result := Table.BuildKeyForRecord(IndexID, aKeyData1, bcRng1Key,
                                        aFieldCount1, aPartialLen1);
    if (Result = DBIERR_NONE) and bcRng2Valid then
      Result := Table.BuildKeyForRecord(IndexID, aKeyData2, bcRng2Key,
                                        aFieldCount2, aPartialLen2);
    if (Result <> DBIERR_NONE) then
      Exit;
  end;
  {store the other fields}
  if bcRng1Valid then begin
    bcRng1FldCnt := aFieldCount1;
    bcRng1PtlLen := aPartialLen1;
    bcRng1Incl := aKeyIncl1;
  end;
  if bcRng2Valid then begin
    bcRng2FldCnt := aFieldCount2;
    bcRng2PtlLen := aPartialLen2;
    bcRng2Incl := aKeyIncl2;
  end;
  {position ourselves at BOF}
  SetToBegin;
  bcHasRange := true;
end;
{--------}
procedure TffSrCursor.SetToBegin;
begin
  AcqContentLock(ffclmRead);
  try
    bcInfo.pos := cpBOF;
    FFSetKeyPathToBOF(bcInfo.KeyPath);
    bcInvalidateCurKey;
    ffInitI64(bcInfo.refNr);
    bcInfo.Deleted := false;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrCursor.SetToBookmark(aBookmark : PffByteArray) : TffResult;
begin
  Result := CheckBookmark(aBookmark);
  if (Result = DBIERR_NONE) then begin

    { Requirement: The cursor must be on the same index as the bookmark. }
    if IndexID <> PffSrBookmark(aBookmark)^.sbIndexID then begin
      Result := DBIERR_INVALIDBOOKMARK;
      exit;
    end;

    AcqContentLock(ffclmRead);
    bcInfoLock.Lock;                                                   {!!.06}
    try
      { Initialize the key path. }
      FFInitKeyPath(bcInfo.KeyPath);
      with PffSrBookmark(aBookmark)^ do begin
        bcInfo.pos := sbPos;
        bcInfo.refNr := sbRefNr;
        bcInfo.KeyValid := sbKeyValid;
        bcInfo.Deleted := false;
        if sbKeyValid then
          Move(sbKey, bcCurKey^, sbKeyLen);
        try
          { See if the record still exists by rebuilding the key path. }
          bcRebuildKeyPath;                                            {!!.05}

          { Does the record still exist? }
          if (ffCmpI64(bcInfo.refNr, sbRefNr) <> 0) then begin
            { No.  Position the cursor to the crack before the record. }
            bcInfo.pos := cpOnCrack;
            bcInfo.refNr := sbRefNr;
            if (bcInfo.KeyPath.kpPos = kppOnKey) then begin
              Assert(bcInfo.KeyPath.kpCount > 0);
              bcInfo.KeyPath.kpPos := kppOnCrackBefore;
            end;
            bcInfo.Deleted := true;
          end;
        except
          SetToBegin;
          Result := DBIERR_INVALIDBOOKMARK;
        end;
      end;
    finally
      bcInfoLock.Unlock;                                               {!!.06}
      RelContentLock(ffclmRead);
    end;
  end;
end;
{--------}
function TffSrCursor.SetToCursor(aCursor : TffSrBaseCursor) : TffResult;
var
  InRangeResult : integer;
begin
  Result := DBIERR_NONE;
  if (aCursor.Table <> Table) then begin
    Result := DBIERR_DIFFERENTTABLES;
    Exit;
  end;

  AcqContentLock(ffclmRead);
  try
    { If the cursors are using the same index, copy over the source cursor's
      information as is.}
    if (aCursor.IndexID = IndexID) then begin
      bcInfo := aCursor.bcInfo;
      if bcInfo.KeyValid then
        Move(aCursor.bcCurKey^, bcCurKey^, scKeyLen);
      { If this cursor has a range applied and the record to which it is
        positioning does not fit within the range, position the cursor on crack. }
      if (bcInfo.pos in [cpOnRecord, cpOnCrack]) and bcInfo.KeyValid then begin
        InRangeResult := IsInRange(bcCurKey);
        if InRangeResult <> 0 then
          aCursor.bcInfo.Pos := cpOnCrack;
      end;
    end
    else begin
      { Otherwise, the cursor's are on different indices. }

      {  If the source cursor is not on a record then return an error.  This
        could happen if the source cursor was not originally on a record or
        the record has been deleted by the time we were granted a lock on the
        record. }
      if (aCursor.bcInfo.pos <> cpOnRecord) then begin
        Result := DBIERR_NOCURRREC;
        Exit;
      end;

      { Otherwise, position this cursor to the same record as the source cursor.
        We can use the source cursor's refNr as is.  We don't need to figure out
        the key path.  However, we do need to rebuild this cursor's key based
        upon the current index. }
      bcInfo.pos := cpOnRecord;
      bcInfo.refNr := aCursor.bcInfo.refNr;
      FFInitKeyPath(bcInfo.KeyPath);
      scRebuildCurKey(nil, true);
      bcRebuildKeyPath;                                                {!!.05}
    end;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
procedure TffSrCursor.SetToEnd;
begin
  AcqContentLock(ffclmRead);
  try
    bcInfo.pos := cpEOF;
    FFSetKeyPathToEOF(bcInfo.KeyPath);
    bcInvalidateCurKey;
    ffInitI64(bcInfo.refNr);
    bcInfo.Deleted := false;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrCursor.SetToKey(aSearchAction : TffSearchKeyAction;
                              aDirectKey  : boolean;
                              aFieldCount : integer;
                              aPartialLen : integer;
                              aKeyData    : PffByteArray) : TffResult;
var
  aTI : PffTransInfo;
  InRangeResult : integer;
begin
  {calculate the key}
  if aDirectKey then
    Move(aKeyData^, bcCurKey^, scKeyLen)
  else begin
    Result := Table.BuildKeyForRecord(IndexID, aKeyData, bcCurKey,
                                      aFieldCount, aPartialLen);
    if (Result <> DBIERR_NONE) then
      Exit;
  end;

  AcqContentLock(ffclmRead);
  bcInfoLock.Lock;                                                     {!!.06}
  try
    {now position the index on that key or the one that partially
     matches it}
    FFInitKeyPath(bcInfo.KeyPath);
    ffInitI64(bcInfo.refNr);
    bcInfo.Deleted := false;
    aTI := Database.TransactionInfo;
    {try to find the key according to the search action}
    with bcCompareData do begin
      cdFldCnt := aFieldCount;
      cdPartLen := aPartialLen;
    end;
    if Table.FindKey(bcKID, bcInfo.refNr, aTI, bcCurKey, bcInfo.KeyPath,
                     aSearchAction) then begin
      {we found it}
      Result := DBIERR_NONE;
      {if we're at EOF, set all current key variables and exit}
      if (bcInfo.KeyPath.kpPos = kppEOF) then begin
        SetToEnd;
        Exit;
      end;
      {but did we? better see whether we're in the current range}
      InRangeResult := IsInRange(bcCurKey);
      {the key we found is before the start of the range: position
       ourselves at BOF, and only signal an error if the search action
       was "search for equal"}
      if (InRangeResult < 0) then begin
        if aSearchAction = skaEqual then
          Result := DBIERR_RECNOTFOUND;
        SetToBegin;
        Exit;
      end;
      {the key we found is after the end of the range: position
       ourselves at EOF, and only signal an error if the search action
       was "search for equal"}
      if (InRangeResult > 0) then begin
        if aSearchAction = skaEqual then
          Result := DBIERR_RECNOTFOUND;
        SetToEnd;
        Exit;
      end;
      if Assigned(bcFilter) then begin
        Table.GetRecord(aTI, bcDatabase.DatabaseID,                    {!!.10}
                        CursorID, bcInfo.refNr, bcRecordData, ffsltNone, {!!.10}
                        false, false);                                 {!!.02}
        if not bcFilter.MatchesRecord(bcRecordData) then begin
          if aSearchAction = skaEqual then
            Result := DBIERR_RECNOTFOUND
          else begin                                                     {begin !!.11}
            repeat
              Result := bcTable.GetNextRecord(aTI,
                                             bcDatabase.DatabaseID,
                                             CursorID, bcKID, bcInfo.refNr,
                                             bcCurKey, bcInfo.KeyPath, bcRecordData,
                                             ffsltNone);
              if (Result <> DBIERR_NONE) then begin
                if (Result = DBIERR_EOF) then
                  SetToEnd;
                Exit;
              end;
              {in theory we're on a record}
              bcInfo.Deleted := False;
              bcInfo.KeyValid := True;
              bcInfo.pos := cpOnRecord;
            until (Result <> DBIERR_NONE) or
                  (not Assigned(bcFilter) or bcFilter.MatchesRecord(bcRecordData)
                   or bcFilter.CheckTimeout(Result));
          end;                                                             {end !!.11}
          if Result = DBIERR_FF_FilterTimeout then
            Exit;
          if Result <> DBIERR_NONE then begin
            SetToEnd;
            Exit;
          end;
        end;
      end;
      {SetToKey is supposed to leave the position on the crack before
       the record, so make sure}
      bcInfo.KeyValid := true;
      bcInfo.pos := cpOnCrack;
      if (bcInfo.KeyPath.kpPos = kppOnKey) then begin
        Assert(bcInfo.KeyPath.kpCount > 0);
        bcInfo.KeyPath.kpPos := kppOnCrackBefore;
      end;
    end
    else {we didn't find the key} begin
      {if the search action was "search for equal", signal an error and
       position ourselves at BOF}
      if aSearchAction = skaEqual then begin
        Result := DBIERR_RECNOTFOUND;
        SetToBegin;
        Exit;
      end;
      {otherwise we're fine}
      Result := DBIERR_NONE;
      {if we're at EOF, set all current key variables and exit}
      if (bcInfo.KeyPath.kpPos = kppEOF) then begin
        SetToEnd;
        Exit;
      end;
      {check whether we're in the current range or not}
      InRangeResult := IsInRange(bcCurKey);
      if InRangeResult <> 0 then begin
        bcInfo.Pos := cpOnCrack;
        Exit;
      end;

      if Assigned(bcFilter) then begin
        Table.GetRecord(aTI, bcDatabase.DatabaseID,                    {!!.10}
                        CursorID, bcInfo.refNr, bcRecordData, ffsltNone, {!!.10}
                        false, false);                                 {!!.02}
        if not bcFilter.MatchesRecord(bcRecordData) then begin
          Result := GetNextRecord(bcRecordData, ffsltNone);
          if Result = DBIERR_FF_FilterTimeout then
            Exit;
          if Result <> DBIERR_NONE then begin
            SetToEnd;
            Exit;
          end;
        end;
      end;
      {otherwise set all current key variables}
      bcInfo.KeyValid := true;
      bcInfo.pos := cpOnCrack;
    end;
  finally
    bcInfoLock.Unlock;                                                 {!!.06}
    RelContentLock(ffclmRead);
  end;
end;
{--------}
function TffSrCursor.SwitchToIndex(aIndexID   : integer;
                                   aPosnOnRec : boolean) : TffResult;
begin
  {Assumption: aIndexID has been validated}
  Result := DBIERR_NONE;

  if aPosnOnRec and (bcInfo.pos <> cpOnRecord) then begin
    Result := DBIERR_NOCURRREC;
    Exit;
  end;

  AcqContentLock(ffclmRead);
  try
    {set the index}
    bcIndexID := aIndexID;
    {free the key buffers}
    FFFreeMem(bcCurKey, scKeyLen);
    FFFreeMem(bcRng1Key, scKeyLen);
    FFFreeMem(bcRng2Key, scKeyLen);
    {we lose our range}
    bcHasRange := false;
    {get our work areas for the key}
    scKeyLen := bcTable.Dictionary.IndexKeyLength[aIndexID];
    FFGetMem(bcCurKey, scKeyLen);
    FFGetMem(bcRng1Key, scKeyLen);
    FFGetMem(bcRng2Key, scKeyLen);
    {initialise our key index data record}
    bcTable.MakeKIDForCursor(aIndexID, bcKID);
    { Set up the position of the cursor to the current record or BOF. }
    if aPosnOnRec then begin
      { Note that we've already checked that bcInfo.pos is cpOnRecord. }
      scRebuildCurKey(nil, false);
      bcRebuildKeyPath;                                                {!!.05}
    end else
      SetToBegin;
  finally
    RelContentLock(ffclmRead);
  end;
end;
{====================================================================}

{===TffSrCursorList==================================================}
procedure TffSrCursorList.AddCursor(aCursor : TffSrBaseCursor);
begin
  solList.Insert(aCursor);
end;
{--------}
function TffSrCursorList.CursorCount : integer;
begin
  Assert(Assigned(solList));
  Result := solList.Count;
end;
{--------}
procedure TffSrCursorList.DeleteCursor(aCursorID : TffCursorID);
begin
  solList.Delete(aCursorID);
end;
{--------}
function TffSrCursorList.GetCursorItem(Find : TffListFindType; Value : Longint) : TffSrBaseCursor;
var
  Inx : integer;
begin
  Result := nil;
  if (Find = ftFromID) then begin
    Inx := solList.Index(Value);
    if (Inx <> -1) then
      Result := TffSrBaseCursor(solList[Inx]);
  end
  else {Find = ftFromIndex}
    Result := TffSrBaseCursor(solList[Value]);
end;
{--------}
procedure TffSrCursorList.RemoveCursor(aCursorID : TffCursorID);
begin
  solList.Remove(aCursorID);
end;
{====================================================================}

{===TffSrBaseTable===================================================}
constructor TffSrBaseTable.Create(anEngine    : TffServerEngine;
                            const aBaseName   : TffTableName;
                                  aFolder     : TffSrFolder;
                                  aBufMgr     : TffBufferManager;
                            const aOpenMode   : TffOpenMode);
begin
  inherited Create;
  btBaseName := FFShStrAlloc(aBaseName);
  btBufMgr := aBufMgr;
  btEngine := anEngine;
  btFolder := aFolder;
  {create the data dictionary, it'll be empty for now}
  btDictionary := TffServerDataDict.Create(4096);
  btDictionary.SetBaseName(aBaseName);
  {create the list of file info records, set the capacity to 8,
   generally tables will have less than this number of files}
  btFiles := TffVCLList.Create;
  btFiles.Capacity := 8;
  {create the cursor list}
  btCursorList := TffSrCursorList.Create;
  btContentLocks := TffLockContainer.Create;
  btClientLocks := TffLockContainer.Create;
  btPortal := TffReadWritePortal.Create;
//  btUseInternalRollback := False;                                      {!!.03}{Deleted !!.11}
end;
{--------}
destructor TffSrBaseTable.Destroy;
begin
  try                                                                  {!!.06}
    CloseFiles(false, nil);
  finally                                                              {!!.06}
    btCursorList.Free;
    btFiles.Free;
    btDictionary.Free;
    btContentLocks.Free;
    btClientLocks.Free;
    btPortal.Free;
    FFShStrFree(btBaseName);
    inherited Destroy;
  end;                                                                 {!!.06}
end;
{--------}
procedure TffSrBaseTable.AcqClientLock(aCursorID    : Longint;
                                 const aLockType    : TffSrLockType;
                                 const aConditional : Boolean);
var
  LockStatus : TffLockRequestStatus;
  RetryUntil : DWORD;
  TblStr     : string;
  TickCount  : DWORD;
begin

  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left?  Note that we are doing this check to avoid
    the situation where we ask for a lock and pass a negative timeout. }
  if (RetryUntil > TickCount) and
     ((RetryUntil - TickCount) >= 5) then begin

{Begin !!.06}
    { If there are record locks already on the table then raise an
      exception. }
    if HasRecordLocks then
      FFRaiseException(EffServerException, ffStrResServer, fferrLockRejected,
                       [FFMapLockToName(ffsltExclusive), '',
                       PffFileInfo(btFiles[0])^.fiName^]);
{End !!.06}

    { Obtain an exclusive lock on the table content. }
    LockStatus := Folder.LockMgr.AcquireClientLock(btClientLocks,
                                                   aCursorID,
                                                   (RetryUntil - TickCount),
                                                   aLockType);

    { Raise an exception if something went awry. }
    if LockStatus <> fflrsGranted then
      TblStr := format(ffcTableContent,[btBaseName^]);
    case LockStatus of
      fflrsTimeout :
        FFRaiseException(EffServerException, ffStrResServer, fferrTableLockTimeout,
                         [FFMapLockToName(aLockType), TblStr]);
      fflrsRejected :
        FFRaiseException(EffServerException, ffStrResServer, fferrLockRejected,
                         [FFMapLockToName(aLockType), TblStr, '']);
    end;  { case }
  end else
    { No.  Assume we will time out waiting for the resource. }
    FFRaiseExceptionNoData(EffServerException, ffStrResServer,
                           fferrGeneralTimeout);
end;
{--------}
procedure TffSrBaseTable.AcqContentLock(aTrans       : TffSrTransaction;
                                  const aLockType    : TffSrLockType;
                                  const aConditional : Boolean);
var
  LockStatus : TffLockRequestStatus;
  RetryUntil : DWORD;
  TblStr     : string;
  TickCount  : DWORD;
  TranLockType : TffSrLockType;                                        {!!.03}
begin

{Begin !!.03}
  { Does the transaction have a lock container? }
  if assigned(aTrans.TransLockContainer) then begin
    { Yes. Does it already have a sufficient lock on this table? }
    TranLockType := TffTransContainer(aTrans.TransLockContainer).TableContentLockType(btContentLocks);
    if TranLockType >= aLockType then
      { Yes. Exit. We don't need to request another lock since we have one
        already. }
      Exit;

    { Does this transaction already have a share lock on this table & is now
      requesting an exclusive lock? }
    if (TranLockType = ffsltShare) and
       (aLockType = ffsltExclusive) and
       (btContentLocks.Count > 1) then begin
      { Yes. Does another transaction currently have a share lock on this
        table and is already waiting for an exclusive lock? }
      btContentLocks.BeginRead;
      try
        if btContentLocks.SimpleDeadlock then
          { Yes. We have a simple deadlock situation. Raise a deadlock exception
            so that this transaction is rolled back. This will free up its
            share lock which may allow the other transaction to continue
            processing. }
          FFRaiseException(EffServerException, ffStrResServer,
                           fferrDeadlock,
                           [FFMapLockToName(aLockType),
                            Format(ffcTableContent,[btBaseName^]),
                            aTrans.TransactionID]);
      finally
        btContentLocks.EndRead;
      end;
    end;
  end;
{End !!.03}

  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left?  Note that we are doing this check to avoid
    the situation where we ask for a lock and pass a negative timeout. }
  if (RetryUntil > TickCount) and
     ((RetryUntil - TickCount) >= 5) then begin

    { Obtain an exclusive lock on the table content. }
    LockStatus := Folder.LockMgr.AcquireContentLock(btContentLocks,
                                                    Self,
                                                    aTrans,
                                                    aConditional,
                                                    (RetryUntil - TickCount),
                                                    aLockType);

    { Raise an exception if something went awry. }
    if LockStatus <> fflrsGranted then
      TblStr := format(ffcTableContent,[btBaseName^]);
    case LockStatus of
      fflrsTimeout :
        FFRaiseException(EffServerException, ffStrResServer, fferrTableLockTimeout,
                         [FFMapLockToName(aLockType), TblStr]);
      fflrsRejected :
        FFRaiseException(EffServerException, ffStrResServer, fferrLockRejected,
                         [FFMapLockToName(aLockType), TblStr, '']);
    end;  { case }
  end
  else
    { No.  Assume we will time out waiting for the resource. }
    FFRaiseExceptionNoData(EffServerException, ffStrResServer,
                           fferrGeneralTimeout);
end;
{Begin !!.10}
{--------}
function TffSrBaseTable.AcqExclContentLock(aTrans : TffSrTransaction) : TffResult;
var
  LockStatus : TffLockRequestStatus;
begin
  { Obtain an exclusive lock on the table content. }
  LockStatus := Folder.LockMgr.AcquireContentLock(btContentLocks,
                                                  Self,
                                                  aTrans,
                                                  True,
                                                  0,
                                                  ffsltExclusive);

  { Set the result. }
  case LockStatus of
    fflrsGranted  : Result := DBIERR_NONE;
    fflrsTimeout  : Result := fferrLockTimeout;
    fflrsRejected : Result := fferrLockRejected;
  else
    Result := DBIERR_FF_Unknown;
  end;  { case }
end;
{End !!.10}
{--------}
procedure TffSrBaseTable.AcqLock(const aCursorID : TffCursorID;
                                 const aLockType : TffSrLockType);
var
  LockStatus : TffLockRequestStatus;
  RetryUntil : DWORD;
  TblStr     : string;
  TickCount  : DWORD;
begin

  RetryUntil := FFGetRetry;
  TickCount := GetTickCount;

  { Do we have any time left?  Note that we are doing this check to avoid
    the situation where we ask for a lock and pass a negative timeout. }
  if (RetryUntil > TickCount) and
     ((RetryUntil - TickCount) >= 5) then begin

    { Obtain an exclusive lock on the file header. }
    LockStatus := Folder.LockMgr.AcquireTableLock(TableID, aLockType,
                                                  False,
                                                  (RetryUntil - TickCount),
                                                  aCursorID);
    { Raise an exception if something went awry. }
    if LockStatus <> fflrsGranted then
      TblStr := format(ffcTable,[btBaseName^]);
    case LockStatus of
      fflrsTimeout :
        FFRaiseException(EffServerException, ffStrResServer, fferrTableLockTimeout,
                         [FFMapLockToName(aLockType), TblStr]);
      fflrsRejected :
        FFRaiseException(EffServerException, ffStrResServer, fferrLockRejected,
                         [FFMapLockToName(aLockType), TblStr, '']);
    end;  { case }
  end else
    { No.  Assume we will time out waiting for the resource. }
    FFRaiseExceptionNoData(EffServerException, ffStrResServer,
                           fferrGeneralTimeout);
end;
{Begin !!.03}
{--------}
procedure TffSrBaseTable.AddAttribute(const anAttrib : TffFileAttribute);
var
  Index : Longint;
begin
  for Index := 0 to pred(FileCount) do
    include(Files[Index].fiAttributes, anAttrib);
end;
{End !!.03}
{--------}
procedure TffSrBaseTable.BeginCommit;
begin
  btPortal.BeginWrite;
end;
{--------}
procedure TffSrBaseTable.BeginRead;
begin
  btPortal.BeginRead;
end;
{Begin !!.03}
{--------}
procedure TffSrBaseTable.btCommitBLOBMgr;
var
  anInx : LongInt;
begin
  for anInx := 0 to pred(FileCount) do
    if Files[anInx].fiBLOBrscMgr <> nil then
      Files[anInx].fiBLOBrscMgr.Commit;
end;
{End !!.03}
{--------}
procedure TffSrBaseTable.btCreateFile(aFileInx   : Integer;
                                      aTI        : PffTransInfo;
                                const aExtension : TffExtension;
                                      aForServer : Boolean;
                                      aAttribs   : TffFileAttributes;
                                      aStore     : TffBaseTempStorage);
var
  RecLen       : Integer;
  BlockSize    : Longint;
  FI           : PffFileInfo;
  FileHeader   : PffBlockHeaderFile;
  aRelMethod   : TffReleaseMethod;
begin
  {ASSUMPTION: btFiles.Count has already been set to the correct number
               of files so that the aFileInx'th element of the btFiles
               array can be set
               btFiles[aFileInx] is nil, except for aFileInx=0}

  {create the file inforec (note that the descriptor for file 0, the
   data file, has already been created)}
  if (aFileInx <> 0) then begin
    Files[aFileInx] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
                          aExtension, btBufMgr);
    with Files[aFileInx]^ do begin
      fiAttributes := aAttribs;
      fiForServer := aForServer;
      fiEncrypted := btEngine.Configuration.GeneralInfo^.giAllowEncrypt and
                     btDictionary.IsEncrypted;
      fiTempStore := aStore;
    end;
  end;

  FI := Files[aFileInx];

  { Create the file on disk. }
  RecLen := Dictionary.RecordLength;
  BlockSize := Dictionary.FileBlockSize[aFileInx];
  FFOpenFile(FI, omReadWrite, smExclusive, true, true);
  try
    {patch up the file's block size for the buffer manager}
    FI^.fiBlockSize := BlockSize;
    FI^.fiBlockSizeK := BlockSize div 1024;                            {!!.11}
    FI^.fiLog2BlockSize := FFCalcLog2BlockSize(BlockSize);
    {add a new block for the new header}
    FileHeader := PffBlockHeaderFile(btBufMgr.AddBlock(FI, aTI, 0, aRelMethod));
    {set up the file header information}
    with FileHeader^ do begin
      bhfSignature := ffc_SigHeaderBlock;
      bhfNextBlock := $FFFFFFFF;
      bhfThisBlock := 0;
      bhfLSN := 0;
      bhfBlockSize := BlockSize;
      bhfEncrypted := ord(
         btEngine.Configuration.GeneralInfo^.giAllowEncrypt and
           Dictionary.IsEncrypted);
      bhfLog2BlockSize := FFCalcLog2BlockSize(BlockSize);
      bhfUsedBlocks := 1; {ie this header block}
      bhfAvailBlocks := 0;
      bhf1stFreeBlock := $FFFFFFFF;
      bhfRecordCount := 0;
      bhfDelRecCount := 0;
      bhf1stDelRec.iLow := $FFFFFFFF;
      bhfRecordLength := RecLen;
      bhfRecLenPlusTrailer := RecLen + Sizeof(Byte);
      bhfRecsPerBlock := (BlockSize - ffc_BlockHeaderSizeData) div bhfRecLenPlusTrailer;
      bhf1stDataBlock := $FFFFFFFF;
      bhfLastDataBlock := $FFFFFFFF;
      bhfBLOBCount := 0;
      bhfDelBLOBHead.iLow := $FFFFFFFF;
      bhfDelBLOBTail.iLow := $FFFFFFFF;
      bhfAutoIncValue := 0;
      bhfIndexCount := Dictionary.IndexCount;
      bhfHasSeqIndex := 1;
      bhfIndexHeader := ffc_W32NoValue;
      bhfDataDict := 0;
      bhfFieldCount := Dictionary.FieldCount;
      bhfFFVersion := btFolder.NewTableVersion;                        {!!.11}
    end;
    aRelMethod(PffBlock(FileHeader));
  except
    aRelMethod(PffBlock(FileHeader));
    btBufMgr.RemoveFile(FI);
    FFCloseFile(FI);
    if (aFileInx <> 0) then begin
      FFFreeFileInfo(FI);
      btFiles[aFileInx] := nil;
    end;
    raise;
  end;{try..except}
end;
{--------}
procedure TffSrBaseTable.btDeleteBLOBsForRecord(aTI : PffTransInfo;
                                                aData : PffByteArray);
var
  FldInx  : integer;
  FldDesc : PffFieldDescriptor;
  BLOBNr  : TffInt64;
  IsNull  : boolean;
begin
  with Dictionary do begin
    for FldInx := 0 to pred(FieldCount) do begin
      FldDesc := FieldDescriptor[FldInx];
      if (FldDesc^.fdType >= fftBLOB) and
         (FldDesc^.fdType <= ffcLastBLOBType) then begin
        GetRecordField(FldInx, aData, IsNull, @BLOBNr);
        if (not IsNull) and (BLOBNr.iLow <> ffc_W32NoValue) then       {!!.03}
          FFTblDeleteBLOB(Files[BLOBFileNumber], aTI, BLOBNr);
      end;
    end;
  end;
end;
{--------}
function TffSrBaseTable.btGetBaseName : TffTableName;
begin
  Result := btBaseName^;
end;
{--------}
function TffSrBaseTable.btGetCursorList : TffSrCursorList;
begin
  Result := btCursorList;
end;
{--------}
function TffSrBaseTable.btGetDictionary : TffServerDataDict;
begin
  Result := btDictionary;
end;
{--------}
function TffSrBaseTable.btGetFile(Inx : integer) : PffFileInfo;
begin
  if (0 <= Inx) and (Inx < btFiles.Count) then
    Result := PffFileInfo(btFiles[Inx])
  else
    Result := nil;
end;
{--------}
function TffSrBaseTable.btGetFileCount  : integer;
begin
  Result := btFiles.Count;
end;
{--------}
function TffSrBaseTable.btGetFolder : TffSrFolder;
begin
  Result := btFolder;
end;
{--------}
procedure TffSrBaseTable.btInformCursors(aSrcCursorID : TffCursorID;
                                         aOp          : TffRecOp;
                                         aRefNr       : TffInt64;
                                         aIndexID     : integer);
var
  Inx    : integer;
  Cursor, SrcCursor : TffSrBaseCursor;
begin
  SrcCursor := TffSrBaseCursor(aSrcCursorID);
  CursorList.BeginRead;
  try
    for Inx := 0 to pred(CursorList.CursorCount) do begin
      Cursor := CursorList[ftFromIndex, Inx];
      { Is the cursor within the context of our transaction? }
      if (Cursor.Database = SrcCursor.Database) and
         (Cursor.CursorID <> aSrcCursorID) then
        Cursor.bcRecordUpdated(aOp, aRefNr, aIndexID);
    end;
  finally
    CursorList.EndRead;
  end;
end;
{--------}
function TffSrBaseTable.btGetOpenIntents : Longint;
begin
  Result := btOpenIntents;
end;
{Begin !!.03}
{--------}
procedure TffSrBaseTable.btRollbackBLOBMgr;                            {!!.05 - Rewritten}
begin
  Files[btDictionary.BLOBFileNumber].fiBLOBrscMgr.RollBack;
end;                                                                   {!!.05 - End rewritten}
{End !!.03}
{--------}
procedure TffSrBaseTable.btSetFile(Inx : integer; FI : PffFileInfo);
begin
  btFiles[Inx] := FI;
end;
{--------}
procedure TffSrBaseTable.btSetFileCount(FC  : integer);
begin
  if (FC <> btFiles.Count) then
    btFiles.Count := FC;
end;
{--------}
procedure TffSrBaseTable.btTableUpdated(aDatabaseID : TffDatabaseID);
var
  Inx    : integer;
  Cursor : TffSrBaseCursor;
  Database : TffSrDatabase;
begin
  { The purpose of this routine is to invalidate the key path of any
    other cursors attached to this table. We do this because an operation
    may have caused a Structural Modification Operation (SMO) in the index
    used by the cursor and the key path is no longer valid.

    This method is thread-safe for the following reasons:

    1. A server thread committing a transaction must gain write access to the
       table being modified. No other threads will be performing any read or
       write operations on that table until the transaction has committed.

    2. This routine attempts to activate a cursor if the cursor belongs to
       another client. If a thread is in the middle of an operation pertaining
       to the cursor's client (e.g., RecordGetNext) then this routine will not
       be able to update the cursor until the other thread has finished, and
       vice versa.

    Future: We could get rid of this if the index structure was such that all
    keys were in leaf pages. Then the cursor could just check the LSN of its
    current leaf page to see if it should reset its key path. }
  Database := TffSrDatabase(aDatabaseID);
  CursorList.BeginRead;
  try
    for Inx := 0 to pred(CursorList.CursorCount) do begin
      Cursor := CursorList[ftFromIndex, Inx];
      { Is this cursor attached to another database? }
      if (Cursor.Database <> Database) then begin
{Begin !!.06}
        Cursor.bcInfoLock.Lock;
        try
          FFInitKeyPath(Cursor.bcInfo.KeyPath);
        finally
          Cursor.bcInfoLock.Unlock;
        end;
{End !!.06}
      end;
    end;
  finally
    CursorList.EndRead;
  end;
end;
{--------}
procedure TffSrBaseTable.btUpdateAutoInc(aTI : PffTransInfo; aData : PffByteArray);
var
  FldInx       : integer;
  AutoIncValue : Longint;
  IsNull       : boolean;
begin
  with Dictionary do begin
    if HasAutoIncField(FldInx) then begin
      GetRecordField(FldInx, aData, IsNull, @AutoIncValue);
      if IsNull or (AutoIncValue = 0) then begin
        AutoIncValue := FFTblNextAutoIncValue(Files[0], aTI);
        SetRecordField(FldInx, aData, @AutoIncValue);
      end;
    end;
  end;
end;
{--------}
procedure TffSrBaseTable.CloseFiles(commitChanges : boolean; aTI : PffTransInfo);
var
  FileInx  : integer;
  TempFile : PffFileInfo;
begin
  for FileInx := 0 to pred(FileCount) do begin
    TempFile := Files[FileInx];
    if (TempFile <> nil) then begin
      if FFFileIsOpen(TempFile) then begin
        if commitChanges then
          TempFile^.fiBufMgr.CommitFileChanges(TempFile, aTI^.tirTrans);
        FFCloseFile(TempFile);
      end;
      TempFile^.fiBufMgr.RemoveFile(TempFile);
      FFFreeFileInfo(TempFile);
    end;
  end;
end;
{--------}
procedure TffSrBaseTable.CommitChanges(aTI : PffTransInfo);
var
  FileInx  : integer;
  TempFile : PffFileInfo;
begin
  for FileInx := 0 to pred(FileCount) do begin
    TempFile := Files[FileInx];
    if (TempFile <> nil) and
       FFFileIsOpen(TempFile) then
        TempFile^.fiBufMgr.CommitFileChanges(TempFile, aTI^.tirTrans);
  end;
end;
{--------}
procedure TffSrBaseTable.DeregisterOpenIntent;
begin
  if btOpenIntents > 0 then
    dec(btOpenIntents);
end;
{--------}
function TffSrBaseTable.EmptyFiles(aTI : PffTransInfo) : TffResult;
var
  aAttribs : TffFileAttributes;
  aStore   : TffBaseTempStorage;
  TempDict : TffServerDataDict;
begin
  Result := DBIERR_NONE;
  { Preserve the existing attributes. Assume that each file managed by the
    table has the same set of attributes. }
  aAttribs := Files[0]^.fiAttributes;
  aStore := TffBaseTempStorage(Files[0]^.fiTempStore);

  TempDict := TffServerDataDict.Create(Dictionary.BlockSize);
  try
    TempDict.Assign(Dictionary);
    { Flush out any changes related to this file.  They will be eliminated
      when we rebuild the file but we want to make sure they are no longer
      part of an explicit transaction. }
    CloseFiles(true, aTI);
    BuildFiles(aTI, false, TempDict, aAttribs, aStore);

    { Is this a temporary file? }
    if not (fffaTemporary in aAttribs) then begin
      { No. Commit the changes to the file. }
      CloseFiles(true, aTI);
      OpenFiles(aTI, false, aAttribs);
    end;
  finally
    TempDict.Free;
  end;
end;
{--------}
procedure TffSrBaseTable.EndCommit(aDatabaseID : TffDatabaseID);
begin
  btTableUpdated(aDatabaseID);
  btPortal.EndWrite;
end;
{--------}
procedure TffSrBaseTable.EndRead;
begin
  btPortal.EndRead;
end;
{--------}
procedure TffSrBaseTable.GetNextRecordSeq(aTI : PffTransInfo;
                                      var aRefNr : TffInt64;
                                          aData : PffByteArray);
begin
  FFTblReadNextRecord(Files[0], aTI, aRefNr, aRefNr, aData);
end;
{--------}
procedure TffSrBaseTable.GetPrevRecordSeq(aTI : PffTransInfo;
                                       var aRefNr : TffInt64;
                                           aData : PffByteArray);
begin
  FFTblReadPrevRecord(Files[0], aTI, aRefNr, aRefNr, aData);
end;
{--------}
function TffSrBaseTable.GetRecord(aTI        : PffTransInfo;
                            const aDatabaseID : TffDatabaseID;         {!!.10}
                            const aCursorID  : TffCursorID;            {!!.10}
                                  aRefNr     : TffInt64;
                                  aData      : PffByteArray;
                            const aLockType  : TffSrLockType;          {!!.10}
                            const aLockObtained : boolean;             {!!.02}{!!.10}
                            const aConditional : Boolean) : TffResult; {!!.02}{!!.10}
begin

  Result := DBIERR_NONE;

  { Acquire a lock on the record. }
  if (not aLockObtained) then
    FFAcqRecordLock(Files[0], aTI, aRefNr, aLockType, aDatabaseID,     {!!.10}
                    aCursorID, aConditional);                          {!!.02}{!!.10}

  try
    if Assigned(aData) then
      FFTblReadRecord(Files[0], aTI, aRefNr, aData);
  except
    if aLockType <> ffsltNone then
      FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID);
    raise;
  end;
end;
{--------}
procedure TffSrBaseTable.GetRecordLock(aTI        : PffTransInfo;
                                 const aDatabaseID : TffDatabaseID;    {!!.10}
                                 const aCursorID  : TffCursorID;       {!!.10}
                                 const aRefNr     : TffInt64;          {!!.10}
                                 const aLockType  : TffSrLockType);    {!!.10}
begin
  { Acquire a lock on the record. }
  FFAcqRecordLock(Files[0], aTI, aRefNr, aLockType, aDatabaseID,       {!!.10}
                  aCursorID, false);                                   {!!.02}{!!.10}
end;
{Begin !!.10}
{--------}
procedure TffSrBaseTable.GetRecordNoLock(aTI : PffTransInfo;
                                         aRefNr : TffInt64;
                                         aData  : PffByteArray);
begin
  if Assigned(aData) then
    FFTblReadRecord(Files[0], aTI, aRefNr, aData);
end;
{End !!.10}
{--------}
function TffSrBaseTable.HasClientLock(const aCursorID : TffCursorID) : boolean;
begin
  Result := Folder.LockMgr.HasClientLock(btClientLocks, aCursorID);
end;
{--------}
function TffSrBaseTable.HasLock(const aCursorID : TffCursorID;
                                const aLockType : TffSrLockType) : boolean;
begin
  if (aLockType = ffsltNone) then
    Result := true
  else
    Result := Folder.LockMgr.IsTableLockedBy(TableID, aCursorID, aLockType);
end;
{Begin !!.06}
{--------}
function TffSrBaseTable.HasRecordLocks : Boolean;
var
  RecordLocks : TffThreadHash64;
begin
  RecordLocks := PffFileInfo(btFiles[0])^.fiRecordLocks;
  Result := (RecordLocks <> nil) and (RecordLocks.Count > 0);
end;
{End !!.06}
{--------}
function TffSrBaseTable.IsContentLockedBy(aTrans : TffSrTransaction) : boolean;
begin
  Result := Folder.LockMgr.IsContentLockedBy(btContentLocks, aTrans);
end;
{--------}
function TffSrBaseTable.IsRecordLocked(aTI        : PffTransInfo;
                                       aCursorID  : TffCursorID;
                                       aRefNr     : TffInt64;
                                       aLockType  : TffSrLockType) : Boolean;
begin
  Result := Folder.LockMgr.IsRecordLocked(aRefNr, Files[0]);
end;
{--------}
function TffSrBaseTable.IsServerTable : boolean;
begin
  Result := btForServer;
end;
{Begin !!.03}
{--------}
procedure TffSrBaseTable.ListBLOBFreeSpace(aTI       : PffTransInfo;
                                     const aInMemory : Boolean;
                                           aStream   : TStream);
var
  anInx : LongInt;
  aStr  : string;
begin
  for anInx := 0 to pred(FileCount) do
    if Files[anInx].fiBLOBrscMgr <> nil then begin
      aStr := Files[anInx].fiName^ + #13#10;
      if anInx > 0 then
        aStr := #13#10 + aStr;
      aStream.Write(aStr[1], Length(aStr));
      Files[anInx].fiBLOBrscMgr.ListFreeSpace(Files[anInx], aTI, aInMemory,
                                              aStream);
    end;
end;
{End !!.03}
{--------}
procedure TffSrBaseTable.OpenFiles(aTI : PffTransInfo; aForServer : boolean;
                                   aAttribs : TffFileAttributes);
var
  FileInx  : integer;
  FileCnt  : integer;
  DataFile : PffFileInfo;
  Page     : PffBlock;
  TempFile : PffFileInfo;
  State    : integer;
  aRelMethod : TffReleaseMethod;
begin
  State := 0;
  FileCnt := 0;
  TempFile := nil;
  try
    { Allocate the first file inforec: it'll be for the data file. }
    btFiles.Count := 1;
    btFiles[0] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
                                  ffc_ExtForData,
                                  btBufMgr);
    State := 25;
    PffFileInfo(btFiles[0])^.fiAttributes := aAttribs;
    PffFileInfo(btFiles[0])^.fiForServer := aForServer;

    { Open up the data file. }
    DataFile := Files[0];
    FFOpenFile(DataFile, omReadWrite, smExclusive, aForServer, false);
    State := 50;

    { Make sure it's a proper FF file: try to load the header record,
      make it fixed (this'll also check the encryption level). }
    Page := btBufMgr.AddFile(DataFile, aTI, false, aRelMethod);
{Begin !!.11}
    { Adjust in-memory version if overridden via folder. }
    if btFolder.ExistingTableVersion <> 0 then
      Files[0].fiFFVersion := btFolder.ExistingTableVersion;
{End !!.11}
    aRelMethod(Page);

    { Read the data dictionary. }
    Dictionary.ReadFromFile(DataFile, aTI);
    Dictionary.BindIndexHelpers;

    { Set up the count of files in the Files array. }
    FileCnt := Dictionary.FileCount;
    FileCount := FileCnt;
    for FileInx := 1 to pred(FileCnt) do begin
      Files[FileInx] := nil;
    end;
    { Now read through the Dictionary's file list and allocate the
      file inforecs, obviously don't do file 0 since it's been done
      already. }
    State := 100;
    for FileInx := 1 to pred(FileCnt) do begin
      Files[FileInx] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
                                        Dictionary.FileExt[FileInx],
                                        btBufMgr);
      PffFileInfo(btFiles[FileInx])^.fiAttributes := aAttribs;
      PffFileInfo(btFiles[FileInx])^.fiForServer := aForServer;
    end;

    { Now open up all the new files, ie excepting file 0 which is
      already open (it was opened to read the data dictionary); read
      the header record from each file as well, as a security check
      to see whether the file is in FF format. }
    State := 200;
    for FileInx := 1 to pred(FileCnt) do begin
      TempFile := Files[FileInx];
      FFOpenFile(TempFile,
                 DataFile^.fiOpenMode, DataFile^.fiShareMode,
                 DataFile^.fiWriteThru, false);
      Page := btBufMgr.AddFile(TempFile, aTI, false, aRelMethod);
      aRelMethod(Page);
    end;
{Begin !!.11}
    Files[Dictionary.BLOBFileNumber].fiBLOBrscMgr :=
      TffBaseBLOBResourceMgr.GetMgr(Files[Dictionary.BLOBFileNumber]);
    btBLOBEngine := TffBaseBLOBEngine.GetEngine(Files[Dictionary.BLOBFileNumber]);
{End !!.11}
    State := 300;
    btForServer := aForServer;
  except
    if (State = 300) then
      {BLOB Resource Mgr created}
      Files[Dictionary.BLOBFileNumber].fiBLOBrscMgr.Free;
    if (State >= 200) then begin
      {some files are open, all file inforecs are created}
      for FileInx := 1 to pred(FileCnt) do begin
        TempFile := Files[FileInx];
        if FFFileIsOpen(TempFile) then
          FFCloseFile(TempFile);
        TempFile^.fiBufMgr.RemoveFile(TempFile);
      end;
    end;
    if (State >= 100) then begin
      {at least some of the inforecs have been created}
      for FileInx := 1 to pred(FileCnt) do begin
        TempFile := Files[FileInx];
        FFFreeFileInfo(TempFile);
      end;
    end;
    if (State >= 50) then begin
      {at least the data file is open}
      TempFile := Files[0];
      if FFFileIsOpen(TempFile) then
        FFCloseFile(TempFile);
      TempFile^.fiBufMgr.RemoveFile(TempFile);
    end;
    if (State >= 25) then begin
      {at least the data file inforec has been allocated}
      TempFile := Files[0];
      FFFreeFileInfo(TempFile);
    end;
    if (State >= 0) then begin
      {empty the files list}
      FileCount := 0;
    end;
    raise;
  end;{try..except}
end;
{--------}
procedure TffSrBaseTable.RegisterOpenIntent;
begin
  inc(btOpenIntents);
end;
{--------}
procedure TffSrBaseTable.RelClientLock(aCursorID : Longint; aRemoveAll : Boolean);
begin
  if (not aRemoveAll) then                                            {!!.03}
    Folder.LockMgr.ReleaseClientLock(btClientLocks, aCursorID)
  else
    Folder.LockMgr.ReleaseClientLockAll(btClientLocks, aCursorID);
end;
{--------}
procedure TffSrBaseTable.RelContentLock(aTrans : TffSrTransaction);
begin
  Folder.LockMgr.ReleaseContentLock(btContentLocks, aTrans);
end;
{--------}
procedure TffSrBaseTable.RelLock(const aCursorID : TffCursorID;
                                 const aAllLocks : Boolean);
begin
  if aAllLocks then
    Folder.LockMgr.ReleaseTableLockAll(TableID, aCursorID)
  else
    Folder.LockMgr.ReleaseTableLock(TableID, aCursorID);
end;
{Begin !!.10}
{--------}
procedure TffSrBaseTable.RelaxRecordLock(aTI : PffTransInfo;
                                         aCursorID : TffCursorID;
                                         aRefNr : TffInt64);
begin
  FFRelaxRecordLock(Files[0], aTI, aCursorID, aRefNr);
end;
{End !!.10}
{--------}
procedure TffSrBaseTable.RelRecordLock(aTI       : PffTransInfo;
                                       aDatabaseID : TffDatabaseID;    {!!.10}
                                       aCursorID : TffCursorID;
                                       aRefNr    : TffInt64);
begin
  FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID);      {!!.10}
end;
{--------}
procedure TffSrBaseTable.RemoveLocksForCursor(const aDatabaseID : TffDatabaseID; {!!.10}
                                              const aCursorID : TffCursorID;
                                              const aRefNr    : TffInt64;
                                                    aTI       : PffTransInfo);
begin
  { In FF 1, if aRefNr = 0 then all of a cursor's locks were
    released. We do not have such a need for FF2 since the only time a cursor
    has record locks is if it is in a transaction and has acquired exclusive
    locks on one or more records.  When the transaction is committed or rolled
    back, the record locks are released. }
  FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID);      {!!.10}
end;
{--------}
procedure TffSrBaseTable.SetAttributes(const fileAttribs : TffFileAttributes);
var
  Index : Longint;
begin
  for Index := 0 to pred(FileCount) do
    Files[Index].fiAttributes := fileAttribs;
end;
{--------}
procedure TffSrBaseTable.SetExclOwner(const aCursorID : TffCursorID);
var
  Index : Longint;
begin
  for Index := 0 to pred(FileCount) do
    Files[Index].fiExclOwner := aCursorId;
end;
{====================================================================}

{===TffSrTable=======================================================}
constructor TffSrTable.Create(anEngine    : TffServerEngine;
                        const aBaseName   : TffTableName;
                              aFolder     : TffSrFolder;
                              aBufMgr     : TffBufferManager;
                        const aOpenMode   : TffOpenMode);
begin
  inherited Create(anEngine, aBaseName, aFolder, aBufMgr, aOpenMode);
  {create the user routine arrays}
  stUserBuildKey := TffVCLList.Create;
  stUserCompareKey := TffVCLList.Create;
  {miscellaneous}
  FreeOnRemove := true;
//  stUseInternalRollback := False;                                    {!!.03}
end;
{--------}
destructor TffSrTable.Destroy;
begin
  stUserCompareKey.Free;
  stUserBuildKey.Free;
  RemoveDynamicLinks;
  inherited Destroy;
end;
{--------}
procedure TffSrTable.AddIndex(const aIndexDesc : TffIndexDescriptor;
                              aTI : PffTransInfo);
var
  IndexInx : integer;
begin
  {assumption: aIndexDesc has been validated}
  IndexInx := Dictionary.IndexCount;
  with aIndexDesc do
  Dictionary.AddIndex(idName, idDesc, idFile, idCount, idFields,
                      idFieldIHlprs, idDups, idAscend, idNoCase);
  Dictionary.BindIndexHelpers;
  Dictionary.WriteToFile(Files[0], aTI);
  FFTblAddIndex(Files[aIndexDesc.idFile],
                aTI,
                IndexInx,
                Dictionary.IndexKeyLength[IndexInx],
                aIndexDesc.idDups,
                IndexInx = 0);
end;
{--------}
procedure TffSrTable.BuildFiles(aTI         : PffTransInfo;
                                aForServer  : boolean;
                                aDictionary : TffDataDictionary;
                                aAttribs    : TffFileAttributes;
                                aStore      : TffBaseTempStorage);
var
  FileInx  : integer;
  IndexInx : integer;
  DataFile : PffFileInfo;
  FileCnt  : integer; {dup for speed}
begin
  {allocate the first file inforec now: it'll be for the data file}
  btFiles.Count := 1;
  btFiles[0] := FFAllocFileInfo(FFMakeFullFileName(Folder.Path, BaseName),
                               ffc_ExtForData, btBufMgr);
  with PffFileInfo(btFiles[0])^ do begin
    fiAttributes := aAttribs;
    fiForServer := aForServer;
    fiEncrypted := btEngine.Configuration.GeneralInfo^.giAllowEncrypt and
                   aDictionary.IsEncrypted;
    fiRecLenPlusTrailer := aDictionary.RecordLength + SizeOf(Byte);
    fiRecordLength := aDictionary.RecordLength;
    fiTempStore := aStore;
  end;

  { Validate the dictionary. }
  aDictionary.CheckValid;

  { Assimilate the dictionary. }
  btDictionary.ForceOffReadOnly;
  btDictionary.Assign(aDictionary);
  btDictionary.BindIndexHelpers;

  { Get the file count for this table (for speed reasons, etc). }
  FileCnt := Dictionary.FileCount;
  FileCount := FileCnt;

  { Get the data file for speed reasons. }
  DataFile := Files[0];

  { Build all the files and assume that all will contain indexes. }
  for FileInx := 0 to pred(FileCnt) do begin
    btCreateFile(FileInx, aTI, btDictionary.FileExt[FileInx], aForServer,
                 aAttribs, aStore);
    FFTblPrepareIndexes(btFiles[FileInx], aTI);
  end;

  { Write the dictionary. }
  Dictionary.WriteToFile(DataFile, aTI);

  { Add the indexes to their associated index files. }
  with btDictionary do begin
    for IndexInx := 0 to pred(IndexCount) do begin
      FFTblAddIndex(Files[IndexFileNumber[IndexInx]],
                    aTI,
                    IndexInx,
                    IndexKeyLength[IndexInx],
                    IndexAllowDups[IndexInx],
                    IndexInx = 0);
    end;
  end;

{Begin !!.11}
  Files[btDictionary.BLOBFileNumber].fiBLOBrscMgr :=
    TffBaseBLOBResourceMgr.GetMgr(Files[Dictionary.BLOBFileNumber]);
  btBLOBEngine := TffBaseBLOBEngine.GetEngine(Files[btDictionary.BLOBFileNumber]);
{End !!.11}
  Files[btDictionary.BLOBFileNumber].fiMaxSegSize :=
    FFCalcMaxBLOBSegSize(Files[btDictionary.BLOBFileNumber]);

end;
{--------}
function TffSrTable.BuildKeyForRecord(aIndexID    : integer;
                                      aData       : PffByteArray;
                                      aKey        : PffByteArray;
                                      aFieldCount : integer;
                                      aPartialLen : integer) : TffResult;
var
  BuildKey    : TffKeyBuildFunc;
  LenKeyToGen : integer;
begin
  if (Dictionary.IndexType[aIndexID] = itComposite) then begin
    Result := stBuildCompositeKey(aIndexID, aData, aKey, aFieldCount, aPartialLen);
  end
  else {user-defined index} begin
    BuildKey := stGetUserBuildKey(aIndexID);
    if (aFieldCount = 0) and (aPartialLen = 0) then
      LenKeyToGen := Dictionary.IndexKeyLength[aIndexID]
    else
      LenKeyToGen := aPartialLen;
    if not BuildKey(aIndexID, aData, aKey^, LenKeyToGen) then
      Result := DBIERR_KEYVIOL
    else
      Result := DBIERR_NONE;
  end;
end;
{--------}
function TffSrTable.CompareKeysForCursor(var aKID  : TffKeyIndexData;
                                             aKey1 : PffByteArray;
                                             aKey2 : PffByteArray) : integer;
var
  CompareKey : TffKeyCompareFunc;
begin
  with aKID, kidCompareData^ do begin
    if (kidIndexType = itComposite) then begin
      Result := FFKeyCompareComposite(aKey1^, aKey2^, kidCompareData);
    end
    else {user-defined index} if (kidIndex = 0) then begin
      Result := FFCmpDW(PffWord32(aKey1)^, PffWord32(aKey2)^);
    end
    else {not index 0} begin
      CompareKey := stGetUserCompareKey(kidIndex);
      Result := CompareKey(aKey1^, aKey2^, kidCompareData);
    end;
  end;
end;
{--------}
function TffSrTable.DeleteRecord(aTI           : PffTransInfo;
                           const aCursorID     : TffCursorID;
                           const aRefNr        : TffInt64;
                           const aLockObtained : Boolean;
                             var aBTreeChanged : Boolean) : TffResult; {!!.05}
var
  OldData : PffByteArray;
  RecLen  : integer;
begin
  RecLen := Dictionary.RecordLength;
  FFGetMem(OldData, RecLen);

  { If we have yet to lock the record then do so. }
  if (not aLockObtained) then
    FFAcqRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,
                    aTI^.tirTrans.DatabaseID,                          {!!.10}
                    aCursorID, false);                                 {!!.02}{!!.10}
    { Note: We leave all such locks active until the transaction is committed. }

  try
    FFTblReadRecord(Files[0], aTI, aRefNr, OldData);
    Result := stDeleteKeysForRecord(aTI, aRefNr,
                                    OldData, aBTreeChanged);           {!!.05}
    if (Result <> DBIERR_NONE) then
      Exit;
    btDeleteBLOBsForRecord(aTI, OldData);
    FFTblDeleteRecord(Files[0], aTI, aRefNr);
  finally
    btInformCursors(aCursorID, roDelete, aRefNr, 0);
    FFFreeMem(OldData, RecLen);
  end;{try..finally}
end;
{--------}
procedure TffSrTable.DropIndex(aTI : PffTransInfo; aIndexID : Longint);
var
  i : integer;
begin
  Dictionary.RemoveIndex(aIndexID);
  Dictionary.WriteToFile(Files[0], aTI);
  for i := 0 to pred(Dictionary.FileCount) do
    FFTblDeleteIndex(Files[i], aTI, aIndexID);
end;
{--------}
function TffSrTable.FindKey(var aKID        : TffKeyIndexData;
                            var aRefNr      : TffInt64;
                                aTI         : PffTransInfo;
                                aKey        : PffByteArray;
                            var aKeyPath    : TffKeyPath;
                                aAction     : TffSearchKeyAction) : boolean;
begin
  Result := FFTblFindKey(aKID, aRefNr, aTI, aKey, aKeyPath, aAction);
end;
{--------}
function TffSrTable.GetNextKey(var aKID       : TffKeyIndexData;
                               var aRefNr     : TffInt64;
                                   aTI        : PffTransInfo;
                                   aKey       : PffByteArray;
                               var aKeyPath   : TffKeyPath) : TffResult;
begin
  if FFTblNextKey(aKID, aRefNr, aTI, aKey, aKeyPath) then
    Result := DBIERR_NONE
  else
    Result := DBIERR_EOF;
end;
{--------}
function TffSrTable.GetNextRecord(aTI        : PffTransInfo;
                            const aDatabaseID : TffDatabaseID;         {!!.10}
                            const aCursorID  : TffCursorID;            {!!.10}
                              var aKID       : TffKeyIndexData;
                              var aRefNr     : TffInt64;
                                  aKey       : PffByteArray;
                              var aKeyPath   : TffKeyPath;
                                  aData      : PffByteArray;
                            const aLockType  : TffSrLockType) : TffResult; {!!.10}
begin
  Result := DBIERR_NONE;

  try
    if FFTblNextKey(aKID, aRefNr, aTI, aKey, aKeyPath) then begin
      FFAcqRecordLock(Files[0], aTI, aRefNr, aLockType,                {!!.10}
                      aDatabaseID, aCursorID, false);                  {!!.10}
      FFTblReadRecord(Files[0], aTI, aRefNr, aData);
    end
    else
      Result := DBIERR_EOF;
  except
    if aLockType <> ffsltNone then
      FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID);  {!!.10}
    raise;
  end;
end;
{--------}
function TffSrTable.GetPriorRecord(aTI       : PffTransInfo;
                             const aDatabaseID : TffDatabaseID;        {!!.10}
                             const aCursorID : TffCursorID;            {!!.10}
                               var aKID      : TffKeyIndexData;
                               var aRefNr    : TffInt64;
                                   aKey      : PffByteArray;
                               var aKeyPath  : TffKeyPath;
                                   aData     : PffByteArray;
                             const aLockType : TffSrLockType) : TffResult; {!!.10}
begin
  Result := DBIERR_NONE;

  try
    if FFTblPrevKey(aKID, aRefNr, aTI, aKey, aKeyPath) then begin
      FFAcqRecordLock(Files[0], aTI, aRefNr, aLockType,                {!!.10}
                      aDatabaseID, aCursorID, false);                  {!!.10}
      FFTblReadRecord(Files[0], aTI, aRefNr, aData);
    end
    else
      Result := DBIERR_BOF;
  except
    if aLockType <> ffsltNone then
      FFRelRecordLock(Files[0], aTI, aRefNr, aDatabaseID, aCursorID);  {!!.10}
    raise;
  end;
end;
{--------}
function TffSrTable.InsertRecord(aTI        : PffTransInfo;
                                 aCursorID  : TffCursorID;
                                 aData      : PffByteArray;
                                 aLockType  : TffSrLockType;
                             var aNewRefNr  : TffInt64) : TffResult;
var
  RefNr : TffInt64;
begin
  RefNr.iLow := 0;
  RefNr.iHigh := 0;
  if not Dictionary.CheckRequiredRecordFields(aData) then
    Result := DBIERR_REQDERR
  else begin
    {we need to add the default field values}
    if Dictionary.DefaultFieldCount > 0 then
      Dictionary.SetDefaultFieldValues(aData);

    { Updating the autoinc value obtains an exclusive lock on block 0 which
      prevents other cursors from inserting the same or additional records
      until we are done. }
    btUpdateAutoInc(aTI, aData);
    FFTblAddRecord(Files[0], aTI, RefNr, aData);
    {initialize result to an invalid value}
    Result := -1;
    try
      aNewRefNr := RefNr;
      Result := stInsertKeysForRecord(aTI, RefNr, aData);
      if (Result = DBIERR_NONE) then
        FFAcqRecordLock(Files[0], aTI, aNewRefNr, aLockType,           {!!.10}
                        aTI^.tirTrans.DatabaseID, aCursorID, false);   {!!.10}
    finally
       { If the insertion of the keys was not successful and we
         are to cleanup after ourselves then remove the inserted record. }
       if (Result <> DBIERR_NONE) then begin                           {!!.11}
         FFTblDeleteRecord(Files[0], aTI, RefNr);
         RefNr.iLow := 0;
         RefNr.iHigh := 0;
       end;
    end;
  end;
end;
{--------}
function TffSrTable.InsertRecordNoDefault(aTI        : PffTransInfo;   {!!.10}
                                          aCursorID  : TffCursorID;
                                          aData      : PffByteArray;
                                          aLockType  : TffSrLockType;
                                      var aNewRefNr  : TffInt64) : TffResult;
var
  RefNr : TffInt64;
begin
  RefNr.iLow := 0;
  RefNr.iHigh := 0;
  if not Dictionary.CheckRequiredRecordFields(aData) then
    Result := DBIERR_REQDERR
  else begin
    { Updating the autoinc value obtains an exclusive lock on block 0 which
      prevents other cursors from inserting the same or additional records
      until we are done. }
    btUpdateAutoInc(aTI, aData);
    FFTblAddRecord(Files[0], aTI, RefNr, aData);
    {initialize result to an invalid value}
    Result := -1;
    try
      aNewRefNr := RefNr;
      Result := stInsertKeysForRecord(aTI, RefNr, aData);
      if (Result = DBIERR_NONE) then
        FFAcqRecordLock(Files[0], aTI, aNewRefNr, aLockType,           {!!.10}
                        aTI^.tirTrans.DatabaseID, aCursorID, false);   {!!.10}
    finally
       { If the insertion of the keys was not successful and we
         are to cleanup after ourselves then remove the inserted record. }
       if (Result <> DBIERR_NONE) then begin                           {!!.11}
         FFTblDeleteRecord(Files[0], aTI, RefNr);
         RefNr.iLow := 0;
         RefNr.iHigh := 0;
       end;
    end;
  end;
end;
{--------}
procedure TffSrTable.MakeKIDForCursor(aIndexID : integer; var aKID : TffKeyIndexData);
begin
  with Dictionary, aKID, kidCompareData^ do begin
    kidFI := Files[IndexFileNumber[aIndexID]];
    kidIndex := aIndexID;
    if (aIndexID = 0) then begin
      kidCompare := FFKeyCompareI64;
      kidIndexType := itUserDefined;
    end
    else if (IndexType[aIndexID] = itComposite) then begin
      kidCompare := FFKeyCompareComposite;
      kidIndexType := itComposite;
    end
    else begin
      kidCompare := stGetUserCompareKey(aIndexID);
      kidIndexType := itUserDefined;
    end;
    cdKeyLen := IndexKeyLength[aIndexID];
    cdDict := pointer(Dictionary);
    cdIndex := aIndexID;
    cdFldCnt := 0;   {for completeness}
    cdPartLen := 0;  {for completeness}
    cdAscend := IndexIsAscending[aIndexID];
    cdNoCase := IndexIsCaseInsensitive[aIndexID];
  end;
end;
{--------}
function TffSrTable.PutRecord(aTI       : PffTransInfo;
                              aCursorID : TffCursorID;
                              aRefNr    : TffInt64;
                              aData     : PffByteArray;
                              aRelLock  : boolean;                     {!!.05}
                          var aKeyChanged : Boolean) : TffResult;      {!!.05}
var
  OldData: PffByteArray;
  RecLen : integer;
begin

  { Assumption: By the time we have reached this point, the transaction has
    acquired a content lock on the table and we are the only ones who can
    modify the record. }

  RecLen := 0;
  if not Dictionary.CheckRequiredRecordFields(aData) then begin
    Result := DBIERR_REQDERR;
    Exit;
  end;

  Result := DBIERR_NONE;
  try
//  try                                                                {!!.11}
    RecLen := Dictionary.RecordLength;
    FFGetMem(OldData, RecLen);

    FFTblReadRecord(Files[0], aTI, aRefNr, OldData);

    { Acquire an exclusive lock. }
    FFAcqRecordLock(Files[0], aTI, aRefNr, ffsltExclusive,             {!!.10}
                    aTI^.tirTrans.DatabaseID, aCursorID, false);       {!!.10}
    try                                                                {!!.11}
      { There's no need to update index 0, the refnr has not changed. }
      Result := stUpdateKeysForRecord(aCursorID, aTI, aRefNr, aData, OldData, aKeyChanged); {!!.05}
      if (Result <> DBIERR_NONE) then
        Exit;
      FFTblUpdateRecord(Files[0], aTI, aRefNr, aData);
    except
      FFRelRecordLock(Files[0], aTI, aRefNr, aTI^.tirTrans.DatabaseID, {!!.10}
                      aCursorID);                                      {!!.10}
      raise;                                                           {!!.01}
    end;
  finally
    FFFreeMem(OldData, RecLen);
  end;{try..finally}
end;
{--------}
procedure TffSrTable.RemoveDynamicLinks;
var
  i           : Integer;
  KeyProcItem : TffKeyProcItem;
  Inx         : Integer;
begin
  {unlink user-defined indexes}
  with btEngine.Configuration do begin
    for i := 1 to pred(Dictionary.IndexCount) do begin
      if (Dictionary.IndexType[i] <> itComposite) then begin
        Inx := KeyProcList.KeyProcIndex(Folder.Path, BaseName, i);
        if (Inx <> -1) then begin
          KeyProcItem := KeyProcList[Inx];
          KeyProcItem.Unlink;
        end;
      end;
    end;
  end;
end;
{--------}
procedure TffSrTable.ResolveDynamicLinks;
var
  i           : integer;
  KeyProcItem : TffKeyProcItem;
  Inx         : integer;
begin
  stUserBuildKey.Clear;
  stUserCompareKey.Clear;
  {add nil pointers for index 0 as this can never be a user-defined
   index}
  stUserBuildKey.Add(nil);
  stUserCompareKey.Add(nil);
  {fill the arrays with data for each index}
  for i := 1 to pred(Dictionary.IndexCount) do begin
    if (Dictionary.IndexType[i] = itComposite) then begin
      stUserBuildKey.Add(nil);
      stUserCompareKey.Add(nil);
    end
    else {it's a user-defined index} begin
      with btEngine.Configuration do begin
        Inx := KeyProcList.KeyProcIndex(Folder.Path, BaseName, i);
        if (Inx <> -1) then begin
          KeyProcItem := KeyProcList[Inx];
          if KeyProcItem.Link then begin
            stUserBuildKey.Add(pointer(@KeyProcItem.BuildKey));
            stUserCompareKey.Add(pointer(@KeyProcItem.CompareKey));
          end
          else
            FFRaiseExceptionNoData(EffServerException,
                                   ffStrResServer,
                                   fferrResolveTableLinks);
        end else
          FFRaiseExceptionNoData(EffServerException, ffStrResServer, fferrResolveTableLinks);
      end;
    end;
  end;
end;
{--------}
function TffSrTable.stGetBuiltCompositeKey(aIndexID : integer;
                                           aData    : PffByteArray;
                                           aKeyLen  : Longint;
                                       var aKey     : PffByteArray) : TffResult;
var
  WorkKey : PffByteArray;
begin
  FFGetMem(WorkKey, aKeyLen);
  try
    Result := stBuildCompositeKey(aIndexID, aData, WorkKey, 0, 0);
    if (Result <> DBIERR_NONE) then
      FFFreeMem(WorkKey, aKeyLen)                                      {!!.06}
    else
      aKey := WorkKey;
  except
    FFFreeMem(WorkKey, aKeyLen);
    raise;
  end;{try..except}
end;
{--------}
function TffSrTable.stBuildCompositeKey(aIndexID    : integer;
                                        aData       : PffByteArray;
                                        aKey        : PffByteArray;
                                        aFieldCount : integer;
                                        aLastFldLen : integer) : TffResult;
var
  KeyOffset   : integer;
  IndexDscrptr: PffIndexDescriptor;
  FieldDesc   : PffFieldDescriptor;
  FieldNumber : integer;
  LenToUse    : integer;
  FldCnt      : integer;
begin
  Result := DBIERR_NONE;
  KeyOffset := 0;
  IndexDscrptr := Dictionary.IndexDescriptor[aIndexID];
  with IndexDscrptr^ do begin
    {clear the entire key}
    FFInitKey(aKey, idKeyLen, idCount);
    {calculate the number of complete fields we can use}
    if (aFieldCount = 0) then
      if (aLastFldLen = 0) then
        FldCnt := idCount
      else {partial key}
        FldCnt := 0
    else
      if (aLastFldLen = 0) then
        FldCnt := FFMinI(aFieldCount, idCount)
      else {partial key}
        FldCnt := FFMinI(aFieldCount, pred(idCount));

    {build using complete fields}
    if (FldCnt > 0) then
      for FieldNumber := 0 to pred(FldCnt) do begin
        FieldDesc := Dictionary.FieldDescriptor[idFields[FieldNumber]];
        with FieldDesc^ do begin
          if not Dictionary.IsRecordFieldNull(idFields[FieldNumber], aData) then begin
            Move(aData^[fdOffset], aKey^[KeyOffset], fdLength);
            FFSetKeyFieldNonNull(aKey, idKeyLen, idCount, FieldNumber);
          end;
          inc(KeyOffset, fdLength);
        end;
      end;

    {add the last partial field if required - must be string}
    if (aLastFldLen <> 0) then begin
      FieldNumber := idFields[FldCnt];
      if not Dictionary.IsRecordFieldNull(FieldNumber, aData) then begin
        FieldDesc := Dictionary.FieldDescriptor[FieldNumber];
        with FieldDesc^ do
          if (fdType >= fftShortString) then begin
            if (fdType = fftWideString) then
              LenToUse := sizeof(WideChar) * aLastFldLen
            else
              LenToUse := aLastFldLen;
            if (fdType = fftShortString) or
               (fdType = fftShortAnsiStr) then begin
              Move(aData^[fdOffset], aKey^[KeyOffset], LenToUse+1);
              aKey^[KeyOffset] := LenToUse;
            end
            else
              Move(aData^[fdOffset], aKey^[KeyOffset], LenToUse);
            FFSetKeyFieldNonNull(aKey, idKeyLen, idCount, FldCnt);
          end
          else
            Result := DBIERR_INVALIDFLDTYPE;
      end;
    end;
  end;
end;
{--------}
function TffSrTable.stDeleteKeyPrim(aInxFile      : Integer;
                                    aTI           : PffTransInfo;
                                    aRefNr        : TffInt64;
                                    aKey          : PffByteArray;
                                    aCompare      : TffKeyCompareFunc;
                                    aCmpData      : PffCompareData;
                                var aBTreeChanged : Boolean)           {!!.05}
                                                  : Boolean;
var
  KID : TffKeyIndexData;
begin
  with KID do begin
    kidFI := Files[aInxFile];
    kidIndex := aCmpData^.cdIndex;
    kidCompare := aCompare;
    kidCompareData := aCmpData;
  end;
  Result := FFTblDeleteKey(aTI, aKey, aRefNr, KID, aBTreeChanged);     {!!.05}
end;
{--------}
function TffSrTable.stDeleteKeysForRecord(aTI           : PffTransInfo;
                                          aRefNr        : TffInt64;
                                          aData         : PffByteArray;
                                      var aBTreeChanged : Boolean)     {!!.05}
                                                        : TffResult;
var
  IndexNumber : integer;
  IndexDscrptr: PffIndexDescriptor;
  Key         : PffByteArray;
  BuildKey    : TffKeyBuildFunc;
  Compare     : TffKeyCompareFunc;
  CmpData     : TffCompareData;
  tmpBtreeChanged : Boolean;                                           {!!.05}
begin
  Result := DBIERR_NONE;
  with CmpData do begin
    cdDict := pointer(Dictionary);
    cdIndex := 0;
    cdFldCnt := 0;
    cdPartLen := 0;
    cdAscend := true;   {for index 0}
    cdNoCase := true;   {for index 0}
  end;
  aBTreeChanged := True;                                               {!!.05}
  with Dictionary do begin
    if not stDeleteKeyPrim(0, aTI, aRefNr, PffByteArray(@aRefNr),
                           FFKeyCompareI64, @CmpData,
                           tmpBTreeChanged) then begin                 {!!!.05}
      Result := DBIERR_KEYVIOL;
      Exit;
    end;
    aBTreeChanged := tmpBtreeChanged;                                  {!!.05}
    for IndexNumber := 1 to pred(IndexCount) do begin
      IndexDscrptr := IndexDescriptor[IndexNumber];
      with IndexDscrptr^ do begin
        if (idCount <> -1) then begin {a composite index}
          CmpData.cdIndex := IndexNumber;
          CmpData.cdAscend := idAscend;
          CmpData.cdNoCase := idNoCase;
          CmpData.cdKeyLen := idKeyLen;
          Result := stGetBuiltCompositeKey(IndexNumber, aData, idKeyLen, Key);
          if (Result <> DBIERR_NONE) then
            Exit;
          try
            if not stDeleteKeyPrim(idFile, aTI, aRefNr,
                                   Key, FFKeyCompareComposite,
                                   @CmpData, tmpBTreeChanged) then begin {!!.05}
              Result := DBIERR_KEYVIOL;
              Exit;
            end;
            if tmpBtreeChanged then                                    {!!.05}
              aBTreeChanged := true;                                   {!!.05}
          finally
            FFFreeMem(Key, CmpData.cdKeyLen);
          end;{try..finally}
        end
        else {a user-defined index} begin
          CmpData.cdIndex := IndexNumber;
          CmpData.cdAscend := idAscend;
          CmpData.cdNoCase := idNoCase;
          CmpData.cdKeyLen := idKeyLen;
          FFGetMem(Key, CmpData.cdKeyLen);
          try
            BuildKey := stGetUserBuildKey(IndexNumber);
            Compare := stGetUserCompareKey(IndexNumber);
            if BuildKey(IndexNumber, aData, Key^, CmpData.cdKeyLen) then
              if not stDeleteKeyPrim(idFile, aTI, aRefNr,
                                     Key, Compare, @CmpData,
                                     tmpBTreeChanged) then begin       {!!.05}
                Result := DBIERR_KEYVIOL;
                Exit;
              end;
            if tmpBtreeChanged then                                    {!!.05}
              aBTreeChanged := true;                                   {!!.05}
          finally
            FFFreeMem(Key, CmpData.cdKeyLen);
          end;{try..finally}
        end;
      end;
    end;
  end;
end;
{--------}
function TffSrTable.stGetUserBuildKey(aIndexID : integer) : TffKeyBuildFunc;
begin
  if (0 <= aIndexID) and (aIndexID < stUserBuildKey.Count) then
    @Result := stUserBuildKey[aIndexID]
  else
    Result := nil;
end;
{--------}
function TffSrTable.stGetUserCompareKey(aIndexID : integer) : TffKeyCompareFunc;
begin
  if (0 <= aIndexID) and (aIndexID < stUserCompareKey.Count) then
    @Result := stUserCompareKey[aIndexID]
  else
    Result := nil;
end;
{--------}
function TffSrTable.stInsertKeyPrim(aInxFile: integer;
                                    aTI     : PffTransInfo;
                                    aRefNr  : TffInt64;
                                    aKey    : PffByteArray;
                                    aCompare: TffKeyCompareFunc;
                                    aCmpData: PffCompareData) : boolean;
var
  KID : TffKeyIndexData;
begin
  with KID do begin
    kidFI := Files[aInxFile];
    kidIndex := aCmpData^.cdIndex;
    kidCompare := aCompare;
    kidCompareData := aCmpData;
  end;
  Result := FFTblInsertKey(KID, aRefNr, aTI, aKey);
end;
{--------}
function TffSrTable.stInsertKeysForRecord(aTI    : PffTransInfo;
                                          aRefNr : TffInt64;
                                          aData  : PffByteArray) : TffResult;
var
  IndexNumber  : integer;
  IndexDscrptr : PffIndexDescriptor;
  Key          : PffByteArray;
  BuildKey     : TffKeyBuildFunc;
  Compare      : TffKeyCompareFunc;
  CmpData      : TffCompareData;
  BTreeChanged : Boolean;                                              {!!.05}

Procedure RollBackInsertKeys(LastIndexAdded : integer);
var
  IndexNumber : integer;
  Key2 : PffByteArray;                                                 {!!.03}
begin
  { Remove any keys that were successfully added before the error occurred. }
  with Dictionary do begin
    for IndexNumber := LastIndexAdded downto 1 do begin
      IndexDscrptr := IndexDescriptor[IndexNumber];
      with IndexDscrptr^ do begin
        if (idCount <> -1) then begin {a composite index}
          CmpData.cdIndex := IndexNumber;
          CmpData.cdAscend := idAscend;
          CmpData.cdNoCase := idNoCase;
          CmpData.cdKeyLen := idKeyLen;
          Result := stGetBuiltCompositeKey(IndexNumber, aData,
                                           idKeyLen, Key2);            {!!.03}
          if (Result = DBIERR_NONE) then try
            stDeleteKeyPrim(idFile, aTI, aRefNr, Key2,                 {!!.03}
                            FFKeyCompareComposite, @CmpData,
                            BTreeChanged);                             {!!.05}
          finally
            FFFreeMem(Key2, CmpData.cdKeyLen);                         {!!.03}
          end;{try..finally}
        end
        else {a user-defined index} begin
          CmpData.cdIndex := IndexNumber;
          CmpData.cdAscend := idAscend;
          CmpData.cdNoCase := idNoCase;
          CmpData.cdKeyLen := idKeyLen;
          FFGetMem(Key, CmpData.cdKeyLen);
          try
            BuildKey := stGetUserBuildKey(IndexNumber);
            Compare := stGetUserCompareKey(IndexNumber);
            if BuildKey(IndexNumber, aData, Key2^, CmpData.cdKeyLen) then {!!.03}
              stInsertKeyPrim(idFile, aTI, aRefNr,
                              Key2, Compare, @CmpData);                {!!.03}
          finally
            FFFreeMem(Key2, CmpData.cdKeyLen);                         {!!.03}
          end;{try..finally}
        end;
      end;
    end;
  {delete the internal RefNr key}
  with CmpData do begin
    cdDict := pointer(Dictionary);
    cdIndex := 0;
    cdFldCnt := 0;
    cdPartLen := 0;
    cdAscend := true; {for index 0}
    cdNoCase := true; {for index 0}
  end;
  stDeleteKeyPrim(0, aTI, aRefNr, PffByteArray(@aRefNr),
                  FFKeyCompareI64, @CmpData, BTreeChanged);            {!!.05}
  end;
end;

begin
  Result := DBIERR_NONE;
  with CmpData do begin
    cdDict := pointer(Dictionary);
    cdIndex := 0;
    cdFldCnt := 0;
    cdPartLen := 0;
    cdAscend := true; {for index 0}
    cdNoCase := true; {for index 0}
  end;
  with Dictionary do begin
    if not stInsertKeyPrim(0, aTI, aRefNr, PffByteArray(@aRefNr),
                           FFKeyCompareI64, @CmpData) then begin
      Result := DBIERR_KEYVIOL;
      Exit;
    end;
    for IndexNumber := 1 to pred(IndexCount) do begin
      IndexDscrptr := IndexDescriptor[IndexNumber];
      with IndexDscrptr^ do begin
        if (idCount <> -1) then begin {a composite index}
          CmpData.cdIndex := IndexNumber;
          CmpData.cdAscend := idAscend;
          CmpData.cdNoCase := idNoCase;
          CmpData.cdKeyLen := idKeyLen;
          Result := stGetBuiltCompositeKey(IndexNumber, aData, idKeyLen, Key);
          if (Result <> DBIERR_NONE) then
            Exit;
          try
            if not stInsertKeyPrim(idFile, aTI, aRefNr, Key,
                                   FFKeyCompareComposite, @CmpData) then begin
//              if UseInternalRollBack then                            {Deleted !!.11}
              RollBackInsertKeys(Pred(IndexNumber));
              Result := DBIERR_KEYVIOL;
              Exit;
            end;
          finally
            FFFreeMem(Key, idKeyLen);                                  {!!.06}
          end;{try..finally}
        end
        else {a user-defined index} begin
          CmpData.cdIndex := IndexNumber;
          CmpData.cdAscend := idAscend;
          CmpData.cdNoCase := idNoCase;
          CmpData.cdKeyLen := idKeyLen;
          FFGetMem(Key, CmpData.cdKeyLen);
          try
            BuildKey := stGetUserBuildKey(IndexNumber);
            Compare := stGetUserCompareKey(IndexNumber);
            if BuildKey(IndexNumber, aData, Key^, CmpData.cdKeyLen) then
              if not stInsertKeyPrim(idFile, aTI, aRefNr,
                                     Key, Compare, @CmpData) then begin
//                if UseInternalRollBack then                          {Deleted !!.11}
                RollBackInsertKeys(Pred(IndexNumber));
                Result := DBIERR_KEYVIOL;
                Exit;
              end;
          finally
            FFFreeMem(Key, CmpData.cdKeyLen);
          end;{try..finally}
        end;
      end;
    end;
  end;
end;
{--------}
function TffSrTable.stUpdateKeysForRecord(aCursorID   : TffCursorID;
                                          aTI         : PffTransInfo;
                                          aRefNr      : TffInt64;
                                          aData,
                                          aOldData    : PffByteArray;  {!!.05}
                                      var aKeyChanged : Boolean) : TffResult; {!!.05}
{Reorganized !!.10}
var
  IndexNumber     : Integer;
  CurrentIndexNum : Integer;                                           {!!.05}
  IndexDscrptr    : PffIndexDescriptor;
  OldKey          : PffByteArray;
  NewKey          : PffByteArray;
  CompResult      : Integer;
  BuildKey        : TffKeyBuildFunc;
  Compare         : TffKeyCompareFunc;
  CmpData         : TffCompareData;
  OldKeyBuilt     : Boolean;
  NewKeyBuilt     : Boolean;
  IndexChanged    : array [1..255] of Boolean;

Procedure RollbackUpdateKeys( LastIndexUpdated : Integer;
                              DoLastInsertOnly : Boolean);
var
  OldKey2     : PffByteArray;
  NewKey2     : PffByteArray;
  IndexNumber2 : Integer;
begin
  for IndexNumber2 := LastIndexUpdated downto 1 do begin
    IndexDscrptr := Dictionary.IndexDescriptor[IndexNumber2];
    OldKey2 := nil;
    NewKey2 := nil;
    CmpData.cdIndex := IndexNumber2;
    CmpData.cdAscend := IndexDscrptr^.idAscend;
    CmpData.cdNoCase := IndexDscrptr^.idNoCase;
    CmpData.cdKeyLen := IndexDscrptr^.idKeyLen;
    with IndexDscrptr^ do
      try
        if (idCount <> -1) then begin {a composite index}
          Result := stGetBuiltCompositeKey(IndexNumber2, aOldData,
                                           idKeyLen, OldKey2);
          if (Result = DBIERR_NONE) then
            Result := stGetBuiltCompositeKey(IndexNumber2, aData,
                                             idKeyLen, NewKey2);
          if (Result <> DBIERR_NONE) then
            Continue; {carry on with the next index in case of error}
          CompResult := FFKeyCompareComposite(OldKey2^, NewKey2^, @CmpData);
          if (CompResult <> 0) then begin
            if (not DoLastInsertOnly) then
              {Remove the NewKey on this index}
              stDeleteKeyPrim(idFile, aTI, aRefNr, NewKey2,
                              FFKeyCompareComposite, @CmpData,
                              IndexChanged[IndexNumber2]);             {!!.05}
            {Restore the OldKey value on this index}
            stInsertKeyPrim(idFile, aTI, aRefNr, OldKey2,
                            FFKeyCompareComposite, @CmpData);
          end;
        end
        else {a user-defined index} begin
          BuildKey := stGetUserBuildKey(IndexNumber2);
          Compare := stGetUserCompareKey(IndexNumber2);
          FFGetMem(OldKey2, CmpData.cdKeyLen);
          FFGetMem(NewKey2, CmpData.cdKeyLen);
          OldKeyBuilt := BuildKey(IndexNumber2, aOldData,
                                  OldKey2^, CmpData.cdKeyLen);
          NewKeyBuilt := BuildKey(IndexNumber2, aData,
                                  NewKey2^, CmpData.cdKeyLen);
          if OldKeyBuilt and NewKeyBuilt then
            CompResult := Compare(OldKey2^, NewKey2^, @CmpData)
          else if (OldKeyBuilt or NewKeyBuilt) then
            CompResult := 1 {value doesn't matter so long as it's <> 0}
          else
            CompResult := 0;
          if (CompResult <> 0) then begin
            if NewKeyBuilt and (not DoLastInsertOnly) then
              {Remove the NewKey on this index}
              stDeleteKeyPrim(idFile, aTI, aRefNr,
                              NewKey2, Compare, @CmpData,
                              IndexChanged[IndexNumber2]);             {!!.05}
            if OldKeyBuilt then
              {Restore the OldKey value on this index}
              stInsertKeyPrim(idFile, aTI, aRefNr,
                              OldKey2, Compare, @CmpData);
          end;
        end;  { if }
      finally
        if Assigned(NewKey2) then
          FFFreeMem(NewKey2, CmpData.cdKeyLen);
        if Assigned(OldKey2) then
          FFFreeMem(OldKey2, CmpData.cdKeyLen);
      end;{try..finally}
  end;  { for }
end;
begin
  Result := DBIERR_NONE;
  CurrentIndexNum := TffSrBaseCursor(aCursorID).IndexID;               {!!.05}
  aKeyChanged := False;                                                {!!.05}
  with CmpData do begin
    cdDict := pointer(Dictionary);
    cdFldCnt := 0;
    cdPartLen := 0;
  end;
  with Dictionary do try
    for IndexNumber := 1 to pred(IndexCount) do begin
      IndexChanged[IndexNumber] := False;
      IndexDscrptr := IndexDescriptor[IndexNumber];
      OldKey := nil;
      NewKey := nil;
      CmpData.cdIndex := IndexNumber;
      CmpData.cdAscend := IndexDscrptr^.idAscend;
      CmpData.cdNoCase := IndexDscrptr^.idNoCase;
      CmpData.cdKeyLen := IndexDscrptr^.idKeyLen;
      with IndexDscrptr^ do
        try
          if (idCount <> -1) then begin {a composite index}
            Result := stGetBuiltCompositeKey(IndexNumber, aOldData, idKeyLen, OldKey);
            if (Result = DBIERR_NONE) then
              Result := stGetBuiltCompositeKey(IndexNumber, aData, idKeyLen, NewKey);
            if (Result <> DBIERR_NONE) then
              Exit;
            CompResult := FFKeyCompareComposite(OldKey^, NewKey^, @CmpData);
            if (CompResult <> 0) then begin
              if (IndexNumber = CurrentIndexNum) then                  {!!.05}
                aKeyChanged := True;                                   {!!.05}
              if not stDeleteKeyPrim(idFile, aTI, aRefNr, OldKey,
                                     FFKeyCompareComposite, @CmpData,
                                     IndexChanged[IndexNumber]) then begin {!!.05}
                Result := DBIERR_KEYVIOL;
                Exit;
              end;
              if not stInsertKeyPrim(idFile, aTI, aRefNr, NewKey,
                                     FFKeyCompareComposite, @CmpData) then begin
//                if UseInternalRollBack then                          {Deleted !!.11}
                RollbackUpdateKeys(IndexNumber,True);
                Result := DBIERR_KEYVIOL;
                Exit;
              end;
              IndexChanged[IndexNumber] := True;                       {!!.06}
            end;
          end
          else {a user-defined index} begin
            BuildKey := stGetUserBuildKey(IndexNumber);
            Compare := stGetUserCompareKey(IndexNumber);
            FFGetMem(OldKey, CmpData.cdKeyLen);
            FFGetMem(NewKey, CmpData.cdKeyLen);
            OldKeyBuilt := BuildKey(IndexNumber, aOldData, OldKey^, CmpData.cdKeyLen);
            NewKeyBuilt := BuildKey(IndexNumber, aData, NewKey^, CmpData.cdKeyLen);
            if OldKeyBuilt and NewKeyBuilt then
              CompResult := Compare(OldKey^, NewKey^, @CmpData)
            else if (OldKeyBuilt or NewKeyBuilt) then
              CompResult := 1 {value doesn't matter so long as it's <> 0}
            else
              CompResult := 0;
            if (CompResult <> 0) then begin
              if (IndexNumber = CurrentIndexNum) then                  {!!.05}
                aKeyChanged := True;                                   {!!.05}
              if OldKeyBuilt then
                if not stDeleteKeyPrim(idFile, aTI, aRefNr,
                                       OldKey, Compare, @CmpData,
                                       IndexChanged[IndexNumber]) then begin {!!.05}
//                  if UseInternalRollBack then                        {Deleted !!.11}
                  RollbackUpdateKeys(Pred(IndexNumber),False);
                  Result := DBIERR_KEYVIOL;
                  Exit;
                end;
              if NewKeyBuilt then
                if not stInsertKeyPrim(idFile, aTI, aRefNr,
                                       NewKey, Compare, @CmpData) then begin
//                  if UseInternalRollBack then                        {Deleted !!.11}
                  RollbackUpdateKeys(IndexNumber,True);
                  Result := DBIERR_KEYVIOL;
                  Exit;
                end;
              IndexChanged[IndexNumber] := True;
            end;
          end;  { if }
        finally
          if Assigned(NewKey) then
            FFFreeMem(NewKey, idKeyLen);
          if Assigned(OldKey) then
            FFFreeMem(OldKey, idKeyLen);
        end;{try..finally}
    end;  { for }
  finally {with dictionary do try...}
    {Inform other cursors at end when we are sure everything worked}
    if Result = DBIERR_NONE then begin
       for IndexNumber := 1 to pred(IndexCount) do
          if IndexChanged[IndexNumber] then
             btInformCursors(aCursorID, roModify, aRefNr, IndexNumber);
    end;
  end;  { with dictionary do }
end;
{====================================================================}

{===TffSrSystemTable=================================================}
function TffSrSystemTable.IsServerTable : boolean;
begin
  Result := True;
end;
{====================================================================}

{===TffSrTableList===================================================}
constructor TffSrTableList.Create;
begin
  inherited Create;
  tlList := TffThreadList.Create;
end;
{--------}
destructor TffSrTableList.Destroy;
begin
  tlList.Free;
  inherited Destroy;
end;
{--------}
procedure TffSrTableList.AddTable(aTable : TffSrBaseTable);
begin
  tlList.Insert(aTable);
end;
{--------}
function TffSrTableList.BeginRead : TffSrTableList;
begin
  tlList.BeginRead;
  Result := Self;
end;
{--------}
function TffSrTableList.BeginWrite : TffSrTableList;
begin
  tlList.BeginWrite;
  Result := Self;
end;
{--------}
procedure TffSrTableList.DeleteTable(aTableID : Longint);
begin
  tlList.Delete(aTableID);
end;
{--------}
procedure TffSrTableList.EndRead;
begin
  tlList.EndRead;
end;
{--------}
procedure TffSrTableList.EndWrite;
begin
  tlList.EndWrite;
end;
{--------}
function TffSrTableList.GetTableFromName(const aTableName : TffTableName) : TffSrBaseTable;
var
  Inx : integer;
begin
  for Inx := 0 to pred(tlList.Count) do begin
    Result := TffSrTable(tlList[Inx]);
    if (FFCmpShStrUC(Result.BaseName, aTableName, 255) = 0) then
      Exit;
  end;
  Result := nil;
end;
{--------}
function TffSrTableList.GetTableItem(Find : TffListFindType; Value : Longint) : TffSrBaseTable;
var
  Inx : integer;
begin
  Result := nil;
  if (Find = ftFromID) then begin
    Inx := tlList.Index(Value);
    if (Inx <> -1) then
      Result := TffSrTable(tlList[Inx]);
  end
  else {Find = ftFromIndex} begin
    if (0 <= Value) and (Value < tlList.Count) then
      Result := TffSrTable(tlList[Value]);
  end;
end;
{--------}
procedure TffSrTableList.RemoveIfUnused(aTable : TffSrBaseTable);
begin
  { Assumption: TableList has not been write locked by the calling routine. }
  tlList.BeginWrite;
  try
    if (aTable.CursorList.CursorCount = 0) and
       (aTable.OpenIntents = 0) then begin
      aTable.Free;
    end;
  finally
    tlList.EndWrite;
  end;
end;
{--------}
procedure TffSrTableList.RemoveUnusedTables;
var
  Inx   : Integer;
  Table : TffSrTable;
begin
  { Assumption: TableList has not been write locked by the calling routine. }
  tlList.BeginWrite;
  try
    for Inx := pred(TableCount) downto 0 do begin
      Table := TffSrTable(tlList[Inx]);
      if (Table.CursorList.CursorCount = 0) and
         (Table.OpenIntents = 0) then
{Begin !!.06}
        try
          Table.Free;
        except
          on E:Exception do
            if FOwner <> nil then
              FOwner.seForce('Exception removing unused table: %s',
                              [E.Message],
                              FOwner.bseGetReadOnly);
        end;
{End !!.06}
    end;
  finally
    tlList.EndWrite;
  end;
end;
{--------}
function TffSrTableList.TableCount : integer;
begin
  Result := tlList.Count;
end;
{=====================================================================}

{== TffSrDatabase ====================================================}
constructor TffSrDatabase.Create(anEngine    : TffServerEngine;
                                 aSession    : TffSrSession;
                                 aFolder     : TffSrFolder;
                                 anAlias     : TffName;
                                 aOpenMode   : TffOpenMode;
                                 aShareMode  : TffShareMode;
                                 aTimeout    : Longint;
                                 aCheckSpace : Boolean);               {!!.11}
var                                                                    {!!.11}
  OSVerInfo : TOSVersionInfo;                                           {!!.11}
begin
  inherited Create(aTimeout);
  dbAlias := FFShStrAlloc(anAlias);
  dbEngine := anEngine;
  dbExtenders := nil;
  soClient := aSession.Client;
  dbCursorList := TffSrCursorList.Create;
  dbFolder := aFolder;
//  FDeadlocked := False;
  dbOpenMode := aOpenMode;
  dbSession := aSession;
  dbShareMode := aShareMode;
  dbStmtList := TffSrStmtList.Create;                                  {!!.10}
  { Initialize the transaction information. }
  FFGetZeroMem(dbTI, SizeOf(TffTransInfo));
  with dbTI^ do begin
    tirTrans := nil;
    tirLockMgr := dbFolder.LockMgr;
  end;
  dbTrans := nil;
  FreeOnRemove := True;
  Session.DatabaseList.BeginWrite;
  try
    Session.DatabaseList.AddDatabase(Self);
  finally
    Session.DatabaseList.EndWrite;
  end;

  OSVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);             {!!.11 - Start}
  if ((aCheckSpace) and
      (GetVersionEx(OSVerInfo))) then
    dbCheckSpace := ((OSVerInfo.dwPlatformId = VER_PLATFORM_WIN32_NT) or
                     ((OSVerInfo.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS) and
                      (OSVerInfo.dwBuildNumber > 1000)))
  else
    dbCheckSpace := False;                                             {!!.11 - End}
end;
{--------}
destructor TffSrDatabase.Destroy;
var
  anIndex    : Longint;
  anExtender : TffBaseEngineExtender;
begin

  { If a transaction is active then the transaction must be rolled back. }
  if assigned(dbTrans) then
    dbEngine.seTransactionRollback(Self);

  { Free all registered extenders. }
  if assigned(dbExtenders) then begin
    for anIndex := pred(dbExtenders.Count) downto 0 do begin
      anExtender := TffBaseEngineExtender
                      (TffIntListItem(dbExtenders[anIndex]).KeyAsInt);
      anExtender.Free;
    end;
    dbExtenders.Free;
  end;

  FFShStrFree(dbAlias);
{Begin !!.10}
  for anIndex := pred(dbStmtList.StmtCount) downto 0 do
    dbEngine.SQLEngine.FreeStmt(dbStmtList.Stmt[ftFromIndex, anIndex].Handle);
  dbStmtList.Free;
{End !!.10}
  dbCursorList.Free;
  Folder.DecRefCount;
  dbFolder := nil;
  FFFreeMem(dbTI, SizeOf(TffTransInfo));
  inherited Destroy;
end;
{--------}
function TffSrDatabase.CanClose(const Mark : boolean) : boolean;
begin
  CursorList.BeginRead;
  dbStmtList.BeginRead;                                                {!!.10}
  try
    Result := (inherited CanClose(Mark)) and                           {!!.06}
              CursorList.HasClosableState(Mark) and                    {!!.06}{!!.10}
              dbStmtList.CanClose(Mark);                               {!!.10}
  finally
    dbStmtList.EndRead;                                                {!!.10}
    CursorList.EndRead;
  end;
end;
{--------}
procedure TffSrDatabase.ForceClose;
begin
  inherited ForceClose;

{Begin !!.01}
  { If a transaction is active then the transaction must be rolled back. }
  if assigned(dbTrans) then
    dbEngine.seTransactionRollback(Self);
{End !!.01}

  CursorList.BeginRead;
  dbStmtList.BeginRead;                                                {!!.10}
  try
    CursorList.ForceClose;
    dbStmtList.ForceClose;                                             {!!.10}
  finally
    dbStmtList.EndRead;                                                {!!.10}
    CursorList.EndRead;
  end;
end;
{--------}
function TffSrDatabase.NotifyExtenders(const anAction      : TffEngineAction;
                                       const aFailAction   : TffEngineAction) : TffResult;
var
  anExtender : TffBaseEngineExtender;
  anIndex    : Longint;
  anIndex2   : Longint;
begin
  Result := DBIERR_NONE;
  if assigned(dbExtenders) then
    for anIndex := 0 to pred(dbExtenders.Count) do begin
      anExtender := TffBaseEngineExtender
                      (TffIntListItem(dbExtenders[anIndex]).KeyAsInt);
      if (anAction in anExtender.InterestedActions) or
         (anExtender.InterestedActions = []) then begin
        Result := anExtender.Notify(Self, anAction);                  {!!.06}
          {since we aren't ignoring Notify's error code, we must
           capture it.  If an extender reports an error we will not
           process the rest of the extenders and we will notify the
           previous extenders that we are "undoing" the previous action}
        if Result <> DBIERR_NONE then begin
          for anIndex2 := 0 to pred(anIndex) do begin
            anExtender := TffBaseEngineExtender
                          (TffIntListItem(dbExtenders[anIndex2]).KeyAsInt);
            anExtender.Notify(self, aFailAction);
          end;
          break;
        end;
      end;
    end;
end;
{--------}
procedure TffSrDatabase.dbAddExtender(anExtender : TffBaseEngineExtender);
var
  anItem : TffIntListItem;
begin
  if assigned(anExtender) then begin
    if not assigned(dbExtenders) then
      dbExtenders := TffThreadList.Create;
    anItem := TffIntListItem.Create(Longint(anExtender));
    dbExtenders.Insert(anItem);
  end;
end;
{--------}
function TffSrDatabase.dbGetAlias : TffName;
begin
  Result := dbAlias^;
end;
{--------}
function TffSrDatabase.dbGetTransID : TffTransID;
begin
  if assigned(dbTrans) then
    Result := dbTrans.TransactionID
  else
    Result := 0;
end;
{--------}
function TffSrDatabase.dbGetTransLSN : TffWord32;
begin
  if assigned(dbTrans) then
    Result := dbTrans.LSN
  else
    Result := 0;
end;
{Begin !!.11}
{--------}
procedure TffSrDatabase.dbSetExistingTableVersion(const Version : Longint);
begin
  dbFolder.ExistingTableVersion := Version;
end;
{--------}
procedure TffSrDatabase.dbSetNewTableVersion(const Version : Longint);
begin
  dbFolder.NewTableVersion := Version;
end;
{--------}
procedure TffSrDatabase.dbSetPackSrcTableVersion(const Version : Longint);
begin
  dbFolder.PackSrcTableVersion := Version;
end;
{End !!.11}
{--------}
procedure TffSrDatabase.dbSetTrans(aTransaction : TffSrTransaction);
begin
  dbTrans := aTransaction;
  dbTI^.tirTrans := aTransaction;
end;
{--------}
function TffSrDatabase.dbGetDatabaseID : TffDatabaseID;
begin
  Result := TffDatabaseID(Self);
end;
{Begin !!.03}
{--------}
procedure TffSrDatabase.RequestClose;
begin
  CursorList.BeginRead;
  dbStmtList.BeginRead;                                                {!!.10}
  try
    inherited RequestClose;
    CursorList.RequestClose;
    dbStmtList.RequestClose;                                           {!!.10}
  finally
    dbStmtList.EndRead;                                                {!!.10}
    CursorList.EndRead;
  end;
end;
{End !!.03}
{--------}
function TffSrDatabase.ShouldClose : boolean;
{Begin !!.01}
var
  aCursor : TffSrBaseCursor;
  aStmt : TffBasePreparedStmt;                                         {!!.10}
  anInx : Longint;
begin
  Result := inherited ShouldClose;
  { Database can close?  }
  if Result then begin
    { Yes. Lock the cursor list for read-only access. }
    CursorList.BeginRead;
    dbStmtList.BeginRead;                                              {!!.10}
    try
      { Is a transaction active? }
      if assigned(dbTrans) then begin
        { Yes. See if state of all cursors will allow us to rollback the
          transaction. }
{Begin !!.10}
        for anInx := 0 to pred(dbStmtList.StmtCount) do begin
          aStmt := dbStmtList.Stmt[ftFromIndex, anInx];
          if aStmt.State <> ffosClosing then begin
            Result := False;
            Break;
          end;
        end;
        if Result then
{End !!.10}
          for anInx := 0 to pred(CursorList.CursorCount) do begin
            aCursor := CursorList.Cursor[ftFromIndex, anInx];
            if aCursor.State <> ffosClosing then begin
              Result := False;
              Break;
            end;
          end;
        if Result then
          dbEngine.seTransactionRollback(Self);
      end
      else
        { No transaction is active. See if cursors may be closed. }
        Result := Result and CursorList.ShouldClose and                {!!.10}
                  dbStmtList.ShouldClose;                              {!!.10}
    finally
      dbStmtList.EndRead;                                              {!!.10}
      CursorList.EndRead;
    end;
  end;
{End !!.01}
end;
{====================================================================}

{===TffSrDatabaseList================================================}
procedure TffSrDatabaseList.AddDatabase(aDatabase : TffSrDatabase);
begin
  solList.Insert(aDatabase);
end;
{--------}
function TffSrDatabaseList.DatabaseCount : integer;
begin
  Result := solList.Count;
end;
{--------}
procedure TffSrDatabaseList.DeleteDatabase(aDatabaseID : Longint);
begin
  solList.Delete(aDatabaseID);
end;
{--------}
function TffSrDatabaseList.GetDatabaseForFolder(aFolder : TffSrFolder) : TffSrDatabase;
var
  Inx : integer;
begin
  for Inx := 0 to pred(solList.Count) do begin
    Result := TffSrDatabase(solList[Inx]);
    if (Result.Folder = aFolder) then
      Exit;
  end;
  Result := nil;
end;
{--------}
function TffSrDatabaseList.GetDatabaseItem(Find : TffListFindType; Value : Longint) : TffSrDatabase;
var
  Inx : integer;
begin
  Result := nil;
  if (Find = ftFromID) then begin
    Inx := solList.Index(Value);
    if (Inx <> -1) then
      Result := TffSrDatabase(solList[Inx]);
  end
  else {Find = ftFromIndex} begin
    if (0 <= Value) and (Value < solList.Count) then
      Result := TffSrDatabase(solList[Value]);
  end;
end;
{====================================================================}


{===TffSrSession===============================================}
constructor TffSrSession.Create(aClient : TffSrClient;
                                const aIsDef : boolean;
                                const aTimeout : Longint);
begin
  inherited Create(aTimeout);
  soClient := aClient;
  ssDatabaseList := TffSrDatabaseList.Create;
  ssIsDefault := aIsDef;
  FreeOnRemove := true;
  aClient.SessionList.BeginWrite;
  try
    aClient.SessionList.AddSession(Self);
  finally
    aClient.SessionList.EndWrite;
  end;
end;
{--------}
destructor TffSrSession.Destroy;
begin
  ssDatabaseList.Free;
  inherited Destroy;
end;
{--------}
function TffSrSession.CanClose(const Mark : boolean) : boolean;
begin
  DatabaseList.BeginRead;
  try
    Result := (inherited CanClose(Mark)) and DatabaseList.CanClose(Mark);
  finally
    DatabaseList.EndRead;
  end;
end;
{--------}
procedure TffSrSession.ForceClose;
begin
  inherited ForceClose;
  DatabaseList.BeginRead;
  try
    DatabaseList.ForceClose;
  finally
    DatabaseList.EndRead;
  end;
end;
{--------}
function TffSrSession.ssGetSessionID : TffSessionID;
begin
  Result := TffSessionID(Self);
end;
{Begin !!.03}
{--------}
procedure TffSrSession.RequestClose;
begin
  DatabaseList.BeginRead;
  try
    inherited RequestClose;
    DatabaseList.RequestClose;
  finally
    DatabaseList.EndRead;
  end;
end;
{End !!.03}
{--------}
function TffSrSession.ShouldClose : boolean;
begin
  DatabaseList.BeginRead;
  try
    Result := (inherited ShouldClose) and DatabaseList.ShouldClose;
  finally
    DatabaseList.EndRead;
  end;
end;
{====================================================================}

{Begin !!.10}
{===TffSrStmtList====================================================}
procedure TffSrStmtList.AddStmt(aStmt : TffBasePreparedStmt);
begin
  solList.Insert(aStmt);
end;
{--------}
function TffSrStmtList.StmtCount : integer;
begin
  Result := solList.Count;
end;
{--------}
procedure TffSrStmtList.DeleteStmt(aStmtID : TffSQLStmtID);
begin
  solList.Delete(aStmtID);
end;
{--------}
function TffSrStmtList.GetStmt(Find : TffListFindType; Value : Longint) : TffBasePreparedStmt;
var
  Inx : integer;
begin
  Result := nil;
  if (Find = ftFromID) then begin
    Inx := solList.Index(Value);
    if (Inx <> -1) then
      Result := TffBasePreparedStmt(solList[Inx]);
  end
  else {Find = ftFromIndex} begin
    if (0 <= Value) and (Value < solList.Count) then
      Result := TffBasePreparedStmt(solList[Value]);
  end;
end;
{--------}
procedure TffSrStmtList.RemoveForClient(const aClientID : TffClientID);
var
  anInx : Longint;
begin
  with solList.BeginWrite do
    try
      for anInx := Pred(solList.Count) downto 0 do begin
        if TffBasePreparedStmt(solList[anInx]).ClientID = aClientID then
          solList.DeleteAt(anInx);
      end;
    finally
      solList.EndWrite;
    end;
end;
{====================================================================}
{End !!.10}

{===TffSrSessionList====================================================}
procedure TffSrSessionList.AddSession(aSession : TffSrSession);
begin
  solList.Insert(aSession);
end;
{--------}
procedure TffSrSessionList.DeleteSession(aSessionID : Longint);
begin
  solList.Delete(aSessionID);
end;
{--------}
function TffSrSessionList.slGetCurSess : TffSrSession;
begin
  Result := slCurSess;
end;
{--------}
function TffSrSessionList.slGetSessionItem(Find : TffListFindType; Value : Longint) : TffSrSession;
var
  Inx : Longint;
begin
  Result := nil;
  if (Find = ftFromID) then begin
    Inx := solList.Index(Value);
    if (Inx <> -1) then
      Result := TffSrSession(solList[Inx]);
  end
  else {Find = ftFromIndex}
    if (0 <= Value) and (Value < solList.Count) then
      Result := TffSrSession(solList[Value]);
end;
{--------}
function TffSrSessionList.SessionCount : integer;
begin
  Result := solList.Count;
end;
{--------}
procedure TffSrSessionList.slSetCurSess(CS : TffSrSession);
begin
  if (slCurSess = nil) then
    slCurSess := slDefSess;
  if (slCurSess <> CS) then
    if (CS = nil) then {CS=nil means the default session}
      slCurSess := slDefSess
    else
      slCurSess := CS;
end;
{--------}
procedure TffSrSessionList.SetDefaultSession(aSession : TffSrSession);
begin
  slDefSess := aSession;
  CurrentSession := nil;
end;
{====================================================================}


{===TffSrClient=====================================================}
constructor TffSrClient.Create(aClientID   : Longint;
                         const aClientName : TffNetName;
                         const aTimeout    : Longint;
                         const aClientVersion : Longint;               {!!.11}
                               aUser       : TffUserItem;
                               anEngine    : TffServerEngine);
//var                                                                  {Deleted !!.03}
//  DefSess : TffSrSession;                                            {Deleted !!.03}
begin
  inherited Create(aTimeout);
  clAccepted := False;
  clClientName := FFShStrAlloc(aClientName);
  clClientVersion := aClientVersion;                                   {!!.11}
  clEngine := anEngine;
  clExtenders := nil;
  soLock := TffPadLock.Create;
  clSessionList := TffSrSessionList.Create;
  clFirstSession := TffSrSession.Create(Self, true, timeout);          {!!.03}
  SessionList.BeginWrite;
  try
    SessionList.SetDefaultSession(clFirstSession);                     {!!.03}
  finally
    SessionList.EndWrite;
  end;
  FreeOnRemove := true;
  {Note: we do NOT save the reference to the user object, these get
         destroyed and rebuilt ad hoc}
  if (aUser <> nil) then
    with aUser do begin
      clUserID := UserID;
      clFirst  := FirstName;
      clLast   := LastName;
      clRights := Rights;
    end;
end;
{--------}
destructor TffSrClient.Destroy;
var
  anExtender : TffBaseEngineExtender;
  anIndex    : Longint;
begin

  try                                                                  {!!.03}
    { Notify the extenders. }
    if clAccepted then
      NotifyExtenders(ffeaBeforeRemoveClient, ffeaNoAction);

    { Get rid of the rebuild status info associated with this client. }
    clEngine.seCleanRebuildList(ClientID);

    { Free all registered extenders. }
    if assigned(clExtenders) then begin
      for anIndex := pred(clExtenders.Count) downto 0 do begin
        anExtender := TffBaseEngineExtender
                        (TffIntListItem(clExtenders[anIndex]).KeyAsInt);
        anExtender.Free;
      end;
      clExtenders.Free;
    end;

  {Begin !!.03}
    { Remove any SQL prepared statements associated with this client. }
//    if Assigned(clEngine.seSQLEngine) then                           {Deleted !!.10}
//      clEngine.seSQLEngine.RemoveForClient(ClientID);                {Deleted !!.10}
  {End !!.03}

    clSessionList.Free;
    FFShStrFree(clClientName);
    soLock.Free;
  finally                                                              {!!.03}
    inherited Destroy;
  end;                                                                 {!!.03}
end;
{--------}
procedure TffSrClient.AddClientExtender(anExtender : TffBaseEngineExtender);
var
  anItem : TffIntListItem;
begin
  if assigned(anExtender) then begin
    if not assigned(clExtenders) then
      clExtenders := TffThreadList.Create;
    anItem := TffIntListItem.Create(Longint(anExtender));
    clExtenders.Insert(anItem);
  end;
end;
{--------}
function TffSrClient.CanClose(const Mark : boolean) : boolean;
begin
  SessionList.BeginRead;
  try
    Result := (inherited CanClose(Mark)) and SessionList.CanClose(Mark);
  finally
    SessionList.EndRead;
  end;
end;
{--------}
function TffSrClient.clGetClientID : TffClientID;
begin
  result := TffClientID(Self);
end;
{--------}
procedure TffSrClient.ForceClose;
begin
  inherited ForceClose;
  SessionList.BeginRead;
  try
    SessionList.ForceClose;
  finally
    SessionList.EndRead;
  end;
end;
{--------}
function TffSrClient.clGetClientName : TffNetName;
begin
  Result := clClientName^;
end;
{--------}
function TffSrClient.NotifyExtenders(const anAction    : TffEngineAction;
                                     const aFailAction : TffEngineAction) : TffResult;
var
  anExtender : TffBaseEngineExtender;
  anIndex    : Longint;
  anIndex2   : Longint;
begin
  Result := DBIERR_NONE;
  if assigned(clExtenders) then
    for anIndex := 0 to pred(clExtenders.Count) do begin
      anExtender := TffBaseEngineExtender
                      (TffIntListItem(clExtenders[anIndex]).KeyAsInt);
      if (anAction in anExtender.InterestedActions) or
         (anExtender.InterestedActions = []) then begin
        Result := anExtender.Notify(Self, anAction);
        { If an extender reports a failure, subsequent extenders will not be
          notified of the action. }
        if Result <> DBIERR_NONE then begin
          for anIndex2 := 0 to pred(anIndex) do begin
            anExtender := TffBaseEngineExtender(TffIntListItem(clExtenders[anIndex2]).KeyAsInt);
            anExtender.Notify(self, aFailAction);
          end;
          break;
        end;
      end;
    end;
end;
{Begin !!.03}
{--------}
procedure TffSrClient.RequestClose;
begin
  SessionList.BeginRead;
  try
    inherited RequestClose;
    SessionList.RequestClose;
  finally
    SessionList.EndRead;
  end;
end;
{End !!.03}
{--------}
function TffSrClient.ShouldClose : boolean;
begin
  SessionList.BeginRead;
  try
    Result := (inherited ShouldClose) and SessionList.ShouldClose;
  finally
    SessionList.EndRead;
  end;
end;
{====================================================================}


{===TffSrClientList====================================================}
procedure TffSrClientList.AddClient(aClient : TffSrClient);
begin
  solList.Insert(aClient)
end;
{--------}
function TffSrClientList.ClientCount : integer;
begin
  Result := solList.Count;
end;
{--------}
procedure TffSrClientList.DeleteClient(aClientID : Longint);
begin
  solList.Delete(aClientID);
end;
{--------}
function TffSrClientList.GetClientItem(Find : TffListFindType; Value : Longint) : TffSrClient;
var
  Inx : integer;
begin
  Result := nil;
  if (Find = ftFromID) then begin
    Inx := solList.Index(Value);
    if (Inx <> -1) then
      Result := TffSrClient(solList[Inx]);
  end
  else {Find = ftFromIndex}
    if (0 <= Value) and (Value < solList.Count) then
      Result := TffSrClient(solList[Value]);
end;
{--------}
procedure TffSrClientList.SetClientItem(Inx : integer; CI : TffSrClient);
begin
  solList[Inx] := CI;
end;
{=====================================================================}


{===TffServerEngine===================================================}
constructor TffServerEngine.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  CursorClass := TffSrCursor;                                          {!!.06}

  FileProcsInitialize;

  seCanLog := False;

  seClientHash := TffHash.Create(ffc_Size127);                         {!!.02}

  {create the configuration object}
  seConfig := TffServerConfiguration.Create;
  seConfigLoaded := False;

  {create the client list, the open database list, the open table
   list, the transaction list}
  seClientList := TffSrClientList.Create;
  seSessionList := TffSrSessionList.Create;
  seDatabaseList := TffSrDatabaseList.Create;
  seTableList := TffSrTableList.Create;
  seTableList.Owner := Self;                                           {!!.06}
  seCursorList := TffSrCursorList.Create;

  seConfigDir := '';
  seFolderList := TffSrFolderList.Create;
  seRebuildList := TffSrRebuildStatusList.Create;

  { Create the buffer manager. Temporary storage size will be updated after
    reading FFSINFO. }
  seBufMgr := TffBufferManager.Create(ConfigDir, ffcl_TempStorageSize);

  { Ensure the seEvtClientDone is set to nil. }
  seEvtClientDone := nil;
  seOnRecoveryCheck := nil;
  seScriptFile := '';

end;
{--------}
destructor TffServerEngine.Destroy;
begin
  { Tell garbage collector to end. }
  if assigned(seGarbageThread) then begin
    seGarbageThread.DieDieDie;
    seGarbageThread.WaitFor;
    seGarbageThread.Free;
  end;

  { Make sure we are shutdown. }
  State := ffesInactive;

  FFNotifyDependents(ffn_Destroy);                                     {!!.01}{!!.11 moved}
  if Assigned(seSQLEngine) then                                        {!!.11}
    seSQLEngine.FFRemoveDependent(Self);                               {!!.11}

  seCursorList.Free;
  seTableList.Free;
  seDatabaseList.Free;
  seSessionList.Free;
  seClientList.Free;
  seFolderList.Free;
  seConfig.Free;
  seBufMgr.Free;
  seRebuildList.Free;
  seClientHash.Free;                                                   {!!.02}

  inherited Destroy;
end;
{--------}
{Rewritten !!.11}
procedure TffServerEngine.FFNotificationEx(const AOp : Byte; AFrom : TffComponent;
                                           const AData : TffWord32);
var
  RecalcLogFlag : boolean;
begin
  RecalcLogFlag := (AFrom = FEventLog);
  inherited;
  if (AFrom = seSQLEngine) and (AOp in [ffn_Destroy, ffn_Remove]) then begin
    seSQLEngine.FFRemoveDependent(Self);
    seSQLEngine := nil;
  end;

  if RecalcLogFlag then
    seSetLoggingState;
end;
{--------}
procedure TffServerEngine.scInitialize;
begin
  LogAll(['FF Server initializing...',
            format('  Version: %5.4f %s',
                   [ffVersionNumber / 10000,
                    ffSpecialString])]);
  seLoadConfig;

{Begin !!.06}
  Log('Performing recovery check...');
  if assigned(seOnRecoveryCheck) then
    seOnRecoveryCheck(Self)
  else
    with FFRecoveryClass.Create do
      try
        Check(Self);
      finally
        Free;
      end;
  Log('Finished recovery check...');
{End !!.06}

  { Perform garbage collection? }
  if Configuration.GeneralInfo^.giCollectEnabled then
    { Yes. Start the garbage collector thread. }
    seGarbageThread := TffTimerThread.Create
                         (Configuration.GeneralInfo^.giCollectFreq,
                          seCollectGarbage, 0, false);

  seLastFlush := GetTickCount;                                         {!!.01}

  {$IFDEF DebugDelCount}
  FFTBDATA.aLog := FEventLog;
  {$ENDIF}
  {$IFDEF RAMPageCheck}
  FFSRBASE.aLog := FEventLog;
  {$ENDIF}
end;
{--------}
procedure TffServerEngine.scPrepareForShutdown;
var
  aClient         : TffSrClient;
  ClientDoneEvent : TffEvent;
  i               : Integer;
begin
  Log('FF Server preparing for shutdown.');

  { Kill the garbage collection thread. }                              {!!.01}
  if assigned(seGarbageThread) then                                    {!!.01}
    seGarbageThread.DieDieDie;                                         {!!.01}

  {Begin !!.03}
    { Ask the SQL engine to get rid of any remaining prepared statements. }
    if Assigned(seSQLEngine) then
      seSQLEngine.RequestClose;
  {End !!.03}

  if ClientList.ClientCount > 0 then
    { Attempt to clear out those clients in a "closing" state. }
    seCollectGarbage(0);

  FFNotifyDependents(ffn_Deactivate);                                 {!!.03}

  if ClientList.ClientCount > 0 then begin
    {Create an event to wait on the clients to finish what they're
     doing. We will give them a chance to signal us that they're done
     and then we'll just cut them off.}
    ClientDoneEvent := TffEvent.Create;
    try
      seEvtClientDone := ClientDoneEvent;
      try
        ClientDoneEvent.WaitFor(ffc_ClientShutdownTime);
      except
        for i := Pred(ClientList.ClientCount) downto 0 do begin
          aClient := ClientList.Client[ftFromIndex, i];
          aClient.ForceClose;
          seClientRemovePrim(aClient);
        end;
      end;
    finally
      seEvtClientDone := nil;
      ClientDoneEvent.Free;
    end;
  end;
end;
{--------}
procedure TffServerEngine.scStartup;
begin
  Log('FF Server started.');

  seStartTime := GetTickCount;                                        {!!.10}
  CoCreateGUID(seUniqueID);                                           {!!.10}
end;
{--------}
procedure TffServerEngine.scShutDown;
begin
  Log('FF Server shutting down.');
end;
{--------}
procedure TffServerEngine.seCleanRebuildList(const aClientID : TffClientID);
begin
  if assigned(seRebuildList) then
    seRebuildList.DeleteAllForClient(aClientID);
end;
{--------}
procedure TffServerEngine.seCollectGarbage(const aTimerEventCookie : Longint);
begin
  try                                                                  {!!.01}
    if assigned(seSQLEngine) then                                      {!!.01}
      seSQLEngine.CollectGarbage;                                      {!!.01}
    ClientList.RemoveUnused;
//    SessionList.RemoveUnused;                                        {Deleted !!.10}
//    DatabaseList.RemoveUnused;                                       {Deleted !!.10}
//    CursorList.RemoveUnused;                                         {Deleted !!.10}
    TableList.RemoveUnusedTables;
    FolderList.RemoveUnusedFolders;
    { Time to flush pools? }                                           {!!.01}
    if (GetTickCount - seLastFlush) >= ffcl_FlushRate then begin       {!!.01}
      FFLockContainerPool.Flush;                                       {!!.01}
      FFSemPool.Flush;                                                 {!!.01}
{Begin !!.05}
      seBufMgr.Lock;
      try
        seBufMgr.FlushPools([]);                                       {!!.01}
      finally
        seBufMgr.Unlock;
      end;
{End !!.05}
      seLastFlush := GetTickCount;                                     {!!.01}
    end;
  except                                                               {!!.01}
    on E:EffException do                                               {!!.01}
      seForce('Error in garbage collection: %s',                       {!!.01}{!!.06 - Start}
              [E.Message],                                             {!!.01}
              bseGetReadOnly);                                         {!!.01}{!!.06 - End}
  end;                                                                 {!!.01}
end;
{--------}
function TffServerEngine.seDatabaseAddAliasPrim(const aAlias      : TffName;
                                                const aPath       : TffPath;
                                                      aCheckSpace : Boolean) {!!.11}
                                                                  : TffResult;
begin
  { Assumption: Thread-safeness enforced at a higher level. }

  { Does the alias already exist? }
  if seConfig.AliasList.AliasExists(aAlias) then
    { No.  Return error code. }
    Result := DBIERR_NAMENOTUNIQUE
  else begin
    { Yes. Add the new Alias and its path. }
    seConfig.AddAlias(aAlias, aPath, aCheckSpace);                     {!!.11}
    Result := DBIERR_NONE;
  end;
end;
{--------}
function TffServerEngine.seDeleteTable(const aDB        : TffSrDatabase;
                                       const aTableName : TffTableName)
                                                        : TffResult;
var
  Dict : TffDataDictionary;
begin
  Dict := TffDataDictionary.Create(4096);;
  try
    Result := seGetDictionary(aDB, aTableName, Dict);
    { Retrieved the dictionary? }
    if Result = DBIERR_NONE then begin
      { Yes. Delete the files specified by the dictionary. }
      FFTblHlpDelete(aDB.Folder.Path, aTableName, Dict);
      Result := DBIERR_NONE;
    end
    else if (Result <> DBIERR_INVALIDTABLENAME) and
            (Result <> DBIERR_NOSUCHTABLE) then
      { No. Assuming the result code is not one of the above errors then the
        file exists but has no dictionary. Delete the data file. }
      FFDeleteFile(FFMakeFullFileName(aDB.Folder.Path,
                                      FFMakeFileNameExt(aTableName,
                                                        ffc_ExtForData)));
  finally
    Dict.Free;
  end;
end;
{--------}
function TffServerEngine.seGetCollectFrequency : Longint;
begin
  Result := Configuration.GeneralInfo^.giCollectFreq;
end;
{--------}
function TffServerEngine.seGetCollectGarbage : Boolean;
begin
  Result := Configuration.GeneralInfo^.giCollectEnabled;
end;
{--------}
function TffServerEngine.seGetConfig : TffServerConfiguration;
begin
  if (not seConfigLoaded) then
    seLoadConfig;
  Result := seConfig;
end;
{Begin !!.01}
{--------}
function TffServerEngine.seGetMaxRAM : Longint;
begin
  Result := Configuration.GeneralInfo^.giMaxRAM;
end;
{End !!.01}
{--------}
function TffServerEngine.seGetScriptFile : string;                     {!!.11}
begin
  Result := seScriptFile;
end;
{--------}
function TffServerEngine.seIsServerTable(const aTableName : TffTableName) : boolean;
var
  aPrefix, aSuffix : TffTableName;
begin
  Result := False;
  aPrefix := Uppercase(Copy(aTableName, 1, 3));
  { Is this prefixed with characters normally used for server tables? }
  if (aPrefix = ffc_SavPrefix) or
     (aPrefix = ffc_StdPrefix) or
     (aPrefix = ffc_TmpPrefix) then begin
    aSuffix := Uppercase(Copy(aTableName, 4, 5));
    Result := (aSuffix = ffc_AliasSuffix) or
              (aSuffix = ffc_IndexSuffix) or
              (aSuffix = ffc_InfoSuffix) or
              (aSuffix = ffc_UserSuffix);
  end;
end;
{--------}
function TffServerEngine.seGetDictionary(const aDB        : TffSrDatabase;
                                         const aTableName : TffTableName;
                                         var   aDict      : TffDataDictionary) : TffResult;
var
  Table         : TffSrTable;
  TableDataFile : TffFileNameExt;
begin
  Result := DBIERR_NONE;
  Assert(assigned(aDB));
  try
    Table := TffSrTable(GetTableInstance(aDB.Folder, aTableName));
    if Table = nil then begin
      if not FFVerifyFileName(aTableName) then begin
        Result := DBIERR_INVALIDTABLENAME;
        Exit;
      end;
      TableDataFile := FFMakeFileNameExt(aTableName, ffc_ExtForData);
      if not FFFileExists(FFMakeFullFileName(aDB.Folder.Path, TableDataFile)) then begin
        Result := DBIERR_NOSUCHTABLE;
        Exit;
      end;
      Table := TffSrTable.Create(self, aTableName, aDB.Folder, seBufMgr, omReadOnly);
      try
        Table.OpenFiles(aDB.dbTI, seIsServerTable(aTableName), []);
        aDict.Assign(Table.Dictionary);
      finally
        Table.Free;
      end;
    end else
      aDict.Assign(Table.Dictionary);
  except
    on E: Exception do
      Result := ConvertServerException(E, EventLog);
  end;
end;
{--------}
function TffServerEngine.seGetServerName : TffNetName;
begin
  Result := seConfig.GeneralInfo^.giServerName;
end;
{--------}
procedure TffServerEngine.seLoadConfig;
var                                                                    {!!.01}
  aRemainingTime : Longint;                                            {!!.01}
begin

  if (not seConfigLoaded) and                                          {!!.03}
     (not (csLoading in ComponentState)) and                           {!!.03}
     (not (csDestroying in ComponentState)) then                       {!!.03}
    try

      aRemainingTime := FFGetRemainingTime;                            {!!.01}

      { Mark config as loaded.  We must do this in order to avoid recursive
        calls by CreateAdminUser. }
      seConfigLoaded := True;

      { Read the general info. }
      ReadGeneralInfo;

      { Update the buffer manager's Max RAM. }                         {!!.01}
      seBufMgr.MaxRAM := Configuration.GeneralInfo^.giMaxRAM;          {!!.01}

      { Do we need to update the temporary storage size? }
      if Configuration.GeneralInfo^.giTempStoreSize <>
         seBufMgr.TempStoreSize then
         seBufMgr.TempStoreSize := Configuration.GeneralInfo^.giTempStoreSize;

      { Read the aliases. }
      ReadAliasData;

      { Read the users. }
      ReadUserData;
      if (seConfig.UserList.Count = 0) then
        CreateAdminUser(IsReadOnly);

      { Read the keyprocs. }
      ReadKeyProcData;

      { Process alias script and full script (if present). }
      ProcessAliasScript;
      if seScriptFile <> '' then
        ProcessFullScript(seScriptFile);

      { Save out the changes that may have been made via scripts. }
      WriteGeneralInfo(false);
      WriteAliasData;

      FFSetRetry(aRemainingTime);                                      {!!.01}

    except
      seConfigLoaded := False;
      raise;
    end;

end;
{--------}
procedure TffServerEngine.seSetLoggingState;
begin
  seCanLog := FLogEnabled and assigned(FEventLog) and (not IsReadOnly);
end;
{--------}
procedure TffServerEngine.seSetCollectFrequency(aFreq : Longint);
begin
  Configuration.GeneralInfo^.giCollectFreq := aFreq;
  if not ((csLoading in ComponentState) or                             {!!.01}
          (csDesigning in ComponentState)) then                        {!!.01}
    WriteGeneralInfo(False);
end;
{--------}
procedure TffServerEngine.seSetCollectGarbage(aValue : Boolean);
begin
  Configuration.GeneralInfo^.giCollectEnabled := aValue;
  if not ((csLoading in ComponentState) or                             {!!.01}
          (csDesigning in ComponentState)) then                        {!!.01}
    WriteGeneralInfo(False);
end;
{--------}
procedure TffServerEngine.seSetConfigDir(const aPath : string);        {!!.10}
begin
//  scCheckInactive;                                                   {Deleted !!.01}
  seConfigDir := aPath;
end;
{Begin !!.01}
{--------}
procedure TffServerEngine.seSetMaxRAM(const aValue : Longint);
begin
  Configuration.GeneralInfo^.giMaxRAM := aValue;
  seBufMgr.MaxRAM := aValue;
  if not ((csLoading in ComponentState) or
          (csDesigning in ComponentState)) then
    WriteGeneralInfo(False);
end;
{--------}
procedure TffServerEngine.seSetScriptFile(const aFile: string);        {!!.11}
begin
  seScriptFile := aFile;
end;
{--------}
function TffServerEngine.seGetConfigDir : string;                      {!!.10}
begin
  if (csDesigning in ComponentState) then
    Result := seConfigDir
  else
    { If we are not in design mode, then we want to make sure the Default
      config dir setting is the application's path. }
    if (seConfigDir = '') then begin                                   {!!.06 - Start}
      Result := FFExtractPath(Application.ExeName);
      if (Result[Length(Result)] <> '\') then
        Result := Result + '\';
    end else                                                           {!!.06 - End}
      Result := seConfigDir;
end;
{--------}
procedure TffServerEngine.seSetSQLEngine(anEngine : TffBaseSQLEngine);
begin
  if seSQLEngine = anEngine then Exit;

  if assigned(seSQLEngine) then
    seSQLEngine.FFRemoveDependent(Self);                               {!!.11}

  if assigned(anEngine) then
    anEngine.FFAddDependent(Self);                                     {!!.11}

  seSQLEngine := anEngine;

end;
{--------}
procedure TffServerEngine.Log(const aMsg : string);
begin
  if seCanLog then
    FEventLog.WriteString(aMsg);
end;
{--------}
procedure TffServerEngine.LogAll(const Msgs : array of string);
begin
  if seCanLog then
    FEventLog.WriteStrings(Msgs);
end;
{--------}
procedure TffServerEngine.LogFmt(const aMsg : string; args : array of const);
begin
  if seCanLog then
    FEventLog.WriteString(format(aMsg, args));
end;
{--------}
procedure TffServerEngine.seForce(const aMsg     : string;             {!!.06 - Start}
                                        args     : array of const;
                                        ReadOnly : Boolean);
begin
  if ((FEventLog <> nil) and
      (not ReadOnly)) then                                             {!!.06 - End}
    FEventLog.WriteString(Format(aMsg, args));
end;
{--------}
function TffServerEngine.seTransactionCommit(aDB : TffSrDatabase)
                                                 : TffResult;
var
  aContainer : TffTransContainer;
  aInx       : Longint;
  aTable     : TffSrTable;
  aTableList : TffPointerList;
  Nested     : Boolean;
  Committed  : Boolean;                                                {!!.05}
begin
  Committed := False;                                                  {!!.05}
  { Obtain a commit lock on all tables this transaction has modified.
    We must do this to make sure the readers have finished. }
  aTableList := TffPointerList.Create;
  aContainer := TffTransContainer(aDB.Transaction.TransLockContainer);
  Nested := aDB.Transaction.Nested;
  try
    if assigned(aContainer) and (not Nested) then
      for aInx := 0 to pred(aContainer.ContentCount) do begin
        if aContainer.ContentLockType[aInx] = ffsltExclusive then begin
          aTable := TffSrTable(aContainer.ContentTable[aInx]);
          aTable.BeginCommit;
          aTableList.Append(Pointer(aTable));
        end;
      end;

    Result := aDB.Folder.TransactionMgr.Commit(aDB.TransactionID, Nested);
    Committed := (Result = DBIERR_NONE);                               {!!.05}
    if (not Nested) then
      aDB.Transaction := nil;
  finally
    if (not Nested) then
      for aInx := 0 to pred(aTableList.Count) do begin
        aTable := TffSrTable(aTableList.List[aInx]);
        if (Committed) then                                            {!!.05}
          aTable.btCommitBLOBMgr;                                      {!!.03}
        aTable.EndCommit(aDB.DatabaseID);
      end;
    aTableList.Free;
  end;

  if (not Nested) and Committed then begin                             {!!.05 - Start}{!!.10}
    for aInx := Pred(aDB.dbCursorList.CursorCount) downto 0 do         {!!.13}
      if ((TffSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]) <> nil) and
          (TffSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]).bcCloseWTrans)) then
        TffSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]).RemoveIfUnused;
  end;                                                                 {!!.05 - End}
end;
{--------}
function TffServerEngine.seTransactionRollback(aDB : TffSrDatabase)
                                                   : TffResult;
{Rewritten !!.03}
var
  aContainer : TffTransContainer;
  aInx       : Longint;
  aTable     : TffSrTable;
  aTableList : TffPointerList;
  Nested     : Boolean;
begin
  Result := DBIERR_NONE;
//  Assert(assigned(aDB.Transaction));
  if aDB.Transaction <> nil then begin                                 {!!.05}
    aTableList := TffPointerList.Create;
    aContainer := TffTransContainer(aDB.Transaction.TransLockContainer);
    Nested := aDB.Transaction.Nested;
    try
      { Determine which tables were affected by the transaction. We will rollback
        the changes to their BLOB mgr's in-memory deleted chain. }
      if assigned(aContainer) and (not Nested) then
        for aInx := 0 to pred(aContainer.ContentCount) do
          if aContainer.ContentLockType[aInx] = ffsltExclusive then begin
            aTable := TffSrTable(aContainer.ContentTable[aInx]);
            aTableList.Append(Pointer(aTable));
          end;

      { Tell the transaction manager to rollback. }
      aDB.Folder.TransactionMgr.Rollback(aDB.TransactionID, Nested);

      { Nested transaction? }
      if (not Nested) then begin
        { No. For each table involved, rollback the changes to the BLOB resource
          manager's in-memory deleted chain. }
        for aInx := 0 to pred(aTableList.Count) do begin
          aTable := TffSrTable(aTableList.List[aInx]);
          aTable.btRollbackBLOBMgr;
        end;
        aDB.Transaction := nil;
        for aInx := Pred(aDB.dbCursorList.CursorCount) downto 0 do     {!!.13}
          if ((TffSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]) <> nil) and
              (TffSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]).bcCloseWTrans)) then
            TffSrBaseCursor(aDB.dbCursorList.solList.Items[aInx]).RemoveIfUnused;
                                                                       {!!.05 - End}
      end;
    finally
      aTableList.Free;
    end;
  end;                                                                 {!!.05}
end;
{--------}
function TffServerEngine.BLOBCreate(aCursorID : TffCursorID;
                                var aBLOBNr   : TffInt64) : TffResult;
{Restructured !!.10}
var
  Cursor   : TffSrBaseCursor;
  StartedTrans : boolean;
  TransID  : TffTransID;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if Result = DBIERR_NONE then
    try
      StartedTrans := False;
      try
        FFSetRetry(Cursor.Timeout);
        if Result = DBIERR_NONE then begin
          Result := Cursor.EnsureWritable(False, False);               {!!.02}
          if (Result = DBIERR_NOACTIVETRAN) or
             Cursor.NeedNestedTransaction then begin                   {!!.03}
            Result := seTransactionStart(Cursor.Database, False,
                                         ffcl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          end;

          if (Result = DBIERR_NONE) then begin
            Result := Cursor.BLOBAdd(aBLOBNr);
            if StartedTrans then
              if Result = DBIERR_NONE then
                seTransactionCommit(Cursor.Database)
              else
                seTransactionRollback(Cursor.Database);
          end;
        end;
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.BLOBDelete(aCursorID : TffCursorID; aBLOBNr : TffInt64) : TffResult;
var
  Cursor  : TffSrBaseCursor;
  StartedTrans : boolean;
  TransID : TffTransID;
{Restructured !!.10}
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if (Result = DBIERR_NONE) then
    try
      StartedTrans := False;
      try
        FFSetRetry(Cursor.Timeout);
        if Result = DBIERR_NONE then begin
          Result := Cursor.EnsureWritable(False, False);               {!!.02}

          if (Result = DBIERR_NOACTIVETRAN) or
             Cursor.NeedNestedTransaction then begin                   {!!.03}
            Result := seTransactionStart(Cursor.Database, false,
                                         ffcl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          end;
          if (Result = DBIERR_NONE) then begin
            Result := Cursor.BLOBDelete(aBLOBNr);
            if StartedTrans then
              if Result = DBIERR_NONE then
                seTransactionCommit(Cursor.Database)
              else
                seTransactionRollback(Cursor.Database);
          end;
        end;
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.BLOBFree(aCursorID : TffCursorID;
                                  aBLOBNr   : TffInt64;
                                  ReadOnly  : boolean) : TffResult;
{Restructured !!.10}
var
  Cursor  : TffSrBaseCursor;
  StartedTrans : boolean;
  TransID : TffTransID;
begin
  { If the BLOB was opened in read-only mode then nothing to do. }
  if readOnly then begin
    Result := DBIERR_NONE;
    Exit;
  end;

  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if (Result = DBIERR_NONE) then
    try
      StartedTrans := False;
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False);                {!!.02}
        if (Result = DBIERR_NOACTIVETRAN) or
           Cursor.NeedNestedTransaction then begin                    {!!.03}
          Result := seTransactionStart(Cursor.Database, false,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := (Result = DBIERR_NONE);
        end;

        if (Result = DBIERR_NONE) then begin
          Result := Cursor.BLOBFree(aBLOBNr);
          if StartedTrans then
            if (Result = DBIERR_NONE) or                               {!!.01}
               (Result = DBIERR_BLOBMODIFIED) then                     {!!.01}
              seTransactionCommit(Cursor.Database)
            else
              seTransactionRollback(Cursor.Database);
        end; { if }
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.BLOBGetLength(aCursorID : TffCursorID; aBLOBNr : TffInt64;
                                   var aLength   : Longint) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if Result = DBIERR_NONE then
      try
        FFSetRetry(Cursor.Timeout);
        if (Result = DBIERR_NONE) then
          aLength := Cursor.BLOBGetLength(aBLOBNr, Result);
      finally
        Cursor.Deactivate;
      end;  { try..finally }
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{Begin !!.03}
{--------}
function TffServerEngine.BLOBListSegments(aCursorID : TffCursorID;
                                          aBLOBNr : TffInt64;
                                          aStream : TStream) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        if Result = DBIERR_NONE then
          Result := Cursor.BLOBListSegments(aBLOBNr, aStream);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{End !!.03}
{--------}
function TffServerEngine.BLOBRead(aCursorID  : TffCursorID;
                                  aBLOBNr    : TffInt64;
                                  aOffset    : TffWord32;              {!!.06}
                                  aLen       : TffWord32;              {!!.06}
                              var aBLOB;
                              var aBytesRead : TffWord32)              {!!.06}
                                             : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        if Result = DBIERR_NONE then
          Result := Cursor.BLOBRead(aBLOBNr, aOffset, aLen, aBLOB, aBytesRead);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.BLOBTruncate(aCursorID   : TffCursorID;
                                      aBLOBNr     : TffInt64;
                                      aBLOBLength : Longint) : TffResult;
{Restructured !!.10}
var
  Cursor  : TffSrBaseCursor;
  StartedTrans : boolean;
  TransID : TffTransID;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if Result = DBIERR_NONE then
    try
      StartedTrans := False;
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False);                 {!!.02}
        if (Result = DBIERR_NOACTIVETRAN) or
           Cursor.NeedNestedTransaction then begin                     {!!.03}
          Result := seTransactionStart(Cursor.Database, false,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := (Result = DBIERR_NONE);
        end;
        if (Result = DBIERR_NONE) then begin
          Result := Cursor.BLOBTruncate(aBLOBNr, aBLOBLength);
          if StartedTrans then
            if Result = DBIERR_NONE then
              seTransactionCommit(Cursor.Database)
            else
              seTransactionRollback(Cursor.Database);
        end;  { if }
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.BLOBWrite(aCursorID : TffCursorID;
                                   aBLOBNr   : TffInt64;
                                   aOffset   : Longint;
                                   aLen      : Longint;
                               var aBLOB)    : TffResult;
{Restructured !!.10}
var
  Cursor  : TffSrBaseCursor;
  StartedTrans : boolean;
  TransID : TffTransID;
begin
  Result := DBIERR_NONE;                                               {!!.01 - Start}
  if aLen = 0 then
    Exit;                                                              {!!.01 - End}

  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if Result = DBIERR_NONE then
    try
      StartedTrans := False;
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False);                 {!!.02}
        if (Result = DBIERR_NOACTIVETRAN) or
           Cursor.NeedNestedTransaction then begin                     {!!.03}
          Result := seTransactionStart(Cursor.Database, false,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := (Result = DBIERR_NONE);
        end;
        if (Result = DBIERR_NONE) then begin
          Result := Cursor.BLOBWrite(aBLOBNr, aOffset, aLen, aBLOB);
          if StartedTrans then
            if Result = DBIERR_NONE then
              seTransactionCommit(Cursor.Database)
            else
              seTransactionRollback(Cursor.Database);
        end;
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.FileBLOBAdd(aCursorID : TffCursorID;
                               const aFileName : TffFullFileName;
                                 var aBLOBNr   : TffInt64) : TffResult;
{Restructured !!.10}
var
  Cursor  : TffSrBaseCursor;
  StartedTrans : boolean;
  TransID : TffTransID;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if Result = DBIERR_NONE then
    try
      StartedTrans := False;
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False);                 {!!.02}
        if (Result = DBIERR_NOACTIVETRAN) or
           Cursor.NeedNestedTransaction then begin                     {!!.03}
          Result := seTransactionStart(Cursor.Database, false,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := (Result = DBIERR_NONE);
        end;
        if (Result = DBIERR_NONE) then begin
          Result := Cursor.FileBLOBAdd(aFileName, aBLOBNr);
          if StartedTrans then
            if Result = DBIERR_NONE then
              seTransactionCommit(Cursor.Database)
            else
              seTransactionRollback(Cursor.Database);
        end;
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.CheckClientIDAndGet(aClientID : TffClientID;
                                         var aClient   : TffSrClient) : TffResult;
begin
  if State <> ffesStarted then begin
    Result := DBIERR_FF_ServerUnavail;
    Exit;
  end;

  Result := seCheckClientIDAndGet(aClientID, aClient);
  if Result = DBIERR_NONE then begin
    Result := DBIERR_FF_UnknownClient;
    if aClient.Activate then
      Result := DBIERR_NONE;
  end;
end;
{--------}
function TffServerEngine.seCheckClientIDAndGet(aClientID : TffClientID;
                                           var aClient   : TffSrClient) : TffResult;
begin
  Result := DBIERR_FF_UnknownClient;
  try
    if TObject(aClientID) is TffSrClient then begin
      aClient := TffSrClient(aClientID);
      Result := DBIERR_NONE;
    end;
  except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  end;
end;
{--------}
function TffServerEngine.CheckCursorIDAndGet(aCursorID : TffCursorID;
                                         var aCursor   : TffSrBaseCursor) : TffResult;
begin
  if State <> ffesStarted then begin
    Result := DBIERR_FF_ServerUnavail;
    Exit;
  end;

  Result := seCheckCursorIDAndGet(aCursorID, aCursor);
  if Result = DBIERR_NONE then begin
    Result := DBIERR_FF_UnknownCursor;
    if aCursor.Activate then
      Result := DBIERR_NONE;
  end;
end;
{--------}
function TffServerEngine.seCheckCursorIDAndGet(aCursorID : TffCursorID;
                                           var aCursor   : TffSrBaseCursor) : TffResult;
begin
  Result := DBIERR_FF_UnknownCursor;
  try
    if TObject(aCursorID) is TffSrBaseCursor then begin
      aCursor := TffSrBaseCursor(aCursorID);
      Result := DBIERR_NONE;
    end;
  except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  end;
end;
{--------}
function TffServerEngine.CheckDatabaseIDAndGet(aDatabaseID : TffDatabaseID;
                                           var aDatabase   : TffSrDatabase) : TffResult;
begin
  if State <> ffesStarted then begin
    Result := DBIERR_FF_ServerUnavail;
    Exit;
  end;

  Result := seCheckDatabaseIDAndGet(aDatabaseID, aDatabase);
  if Result = DBIERR_NONE then begin
    Result := DBIERR_FF_UnknownDB;
    if aDatabase.Activate then
      Result := DBIERR_NONE;
  end;
end;
{--------}
function TffServerEngine.seCheckDatabaseIDAndGet(aDatabaseID : TffDatabaseID;
                                             var aDatabase   : TffSrDatabase) : TffResult;
begin
  Result := DBIERR_FF_UnknownDB;
  try
    if TObject(aDatabaseID) is TffSrDatabase then begin
      aDatabase := TffSrDatabase(aDatabaseID);
      Result := DBIERR_NONE;
    end;
  except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  end;
end;
{--------}
function TffServerEngine.CheckTransactionIDAndGet(aTransactionID : TffTransID;
                                              var aTrans : TffSrTransaction) : TffResult;
begin
  if State <> ffesStarted then begin
    Result := DBIERR_FF_ServerUnavail;
    Exit;
  end;

  Result := DBIERR_INVALIDHNDL;
  try
    if TObject(aTransactionID) is TffSrTransaction then begin
      aTrans := TffSrTransaction(aTransactionID);
      Result := DBIERR_NONE;
    end;
  except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  end;
end;
{Begin !!.11}
{--------}
function TffServerEngine.ClientAdd(  var aClientID   : TffClientID;
                                   const aClientName : TffNetName;
                                   const aUserID     : TffName;
                                   const aTimeout    : Longint;
                                     var aHash       : TffWord32) : TffResult;
begin
  Result := seClientAddPrim(aClientID, aClientName, aUserID, aTimeout,
                            FFVersionNumber, aHash);
end;
{--------}
function TffServerEngine.ClientAddEx(var aClientID   : TffClientID;
                                   const aClientName : TffNetName;
                                   const aUserID     : TffName;
                                   const aTimeout     : Longint;
                                   const aClientVersion : Longint;
                                     var aHash       : TffWord32) : TffResult;
begin
  Result := seClientAddPrim(aClientID, aClientName, aUserID, aTimeout,
                            aClientVersion, aHash);
end;
{--------}
function TffServerEngine.seClientAddPrim(  var aClientID   : TffClientID;
                                         const aClientName : TffNetName;
                                         const aUserID     : TffName;
                                         const aTimeout    : Longint;
                                         const aClientVersion : Longint;
                                           var aHash       : TffWord32) : TffResult;
var
  aMonitor    : TffBaseEngineMonitor;
  anExtender  : TffBaseEngineExtender;
  anIndex     : Longint;
  MonitorList : TffList;
  NewClient   : TffSrClient;
  User        : TffUserItem;
begin
  FFSetRetry(aTimeout);  { Probably not needed but let's do it just in case. }
  aClientID := ffc_NoClientID;
  try
    if seConfig.GeneralInfo^.giIsSecure and
       (seConfig.UserList.Count <> 0) then begin
      if not seConfig.UserList.UserExists(aUserID) then begin
        Result := DBIERR_INVALIDUSRPASS;
        Exit;
      end;
      with seConfig.UserList do begin
        User := UserItem[UserIndex(aUserID)];
        aHash := PasswordHash[aUserID];
      end;
    end
    else begin
      User := nil;
      aHash := 0;
    end;
    NewClient := TffSrClient.Create(aClientID, aClientName, aTimeout,
                                    aClientVersion, User, Self);

    { If there are any monitors interested in client then see if they
      are interested in this client. }
    MonitorList := GetInterestedMonitors(TffSrClient);
    if assigned(MonitorList) then begin
      for anIndex := 0 to pred(MonitorList.Count) do begin
        aMonitor := TffBaseEngineMonitor
                      (TffIntListItem(MonitorList[anIndex]).KeyAsInt);
        try
          anExtender := aMonitor.Interested(NewClient);
          if assigned(anExtender) then
            NewClient.AddClientExtender(anExtender);
        except
          on E:Exception do
            seForce('Monitor [%s] exception, ClientAdd: %s',           {!!.06 - Start}
                    [aMonitor.ClassName, E.message],
                    bseGetReadOnly);                                   {!!.06 - End}
        end;
      end;
      MonitorList.Free;
    end;

    { Now notify the extenders about the client.  If somebody complains
      then disallow the client. }
    Result := NewClient.NotifyExtenders(ffeaAfterCreateClient, ffeaNoAction);
    if Result <> DBIERR_NONE then begin
      NewClient.Free;
      exit;
    end else begin
      NewClient.Accepted := True;
      try
        ClientList.BeginWrite;
        try
          ClientList.AddClient(NewClient);
          seClientHash.Add(NewClient.ClientID, nil);                   {!!.02}
        finally
          ClientList.EndWrite;
        end;
        {add the default session to our session list}
        SessionList.BeginWrite;
        try
          { Assumption: No need to lock NewClient.SessionList since
            we have not confirmed creation of client to the client. }
          SessionList.AddSession(NewClient.SessionList.Session[ftFromIndex, 0]);
        finally
          SessionList.EndWrite;
        end;
      except
        NewClient.Free;
        raise;
      end;{try..except}
      aClientID := NewClient.ClientID;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
procedure TffServerEngine.seClientRemovePrim(const aClient : TffSrClient);
begin
  if aClient.CanClose(True) then begin
    ClientList.DeleteClient(aClient.ClientID);
    TableList.RemoveUnusedTables;
    FolderList.RemoveUnusedFolders;
    {If the server is waiting on us to finish, let it know we're
     done so it can move on.}
    if ((Assigned(seEvtClientDone)) and
        (ClientList.ClientCount = 0)) then
      seEvtClientDone.SignalEvent;
  end else
    aClient.RequestClose;
end;
{--------}
function TffServerEngine.ClientRemove(aClientID : TffClientID) : TffResult;
var
  Client : TffSrClient;
begin
  try
    { Note: We lock the client list because we may have 2 threads trying to
      do a remove for the same client.  Thread A could be processing the
      RemoveClient request from the remote client while thread B could be
      processing a remote client hangup (i.e., initiated from transport level).}
    ClientList.BeginWrite;
    try
{Begin !!.02}
        { Is the client is listed in the hash table? }
        if not seClientHash.Remove(aClientID) then begin
          { No. The client has already been removed. }
          Result := DBIERR_FF_UnknownClient;
          Exit;
        end;
{End !!.02}

      { Find the client object.  Note that we will always get an exception on
        the 2nd removal request for each client. The exception is swallowed
        in seCheckClientIDAndGet. We get the exception because the client is
        already freed. We live with the exception because we don't want to
        pay the cost of doing a sequential scan through the list of clients.
        This could be onerous when hundreds of clients are connected to the
        server. }
      Result := seCheckClientIDAndGet(aClientID, Client);
      if Result = DBIERR_NONE then begin
        FFSetRetry(Client.Timeout);
        seClientRemovePrim(Client);
      end;
    finally
      ClientList.EndWrite;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.ClientSetTimeout(const aClientID : TffClientID;
                                          const aTimeout  : Longint) : TffResult;
var
  Client : TffSrClient;
begin
  try
    Result := CheckClientIDAndGet(aClientID, Client);
    if Result = DBIERR_NONE then
      try
        Client.Timeout := aTimeout;
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.seBLOBCopy(aSrc,
                                    aTgt          : TffSrBaseCursor;
                                    aSourceBLOBNr,
                                    aTargetBLOBNr : TffInt64;
                                    aBuffer       : Pointer;
                                    aBufLen       : Longint)
                                                  : TffResult;
var
  SourceLen  : Longint;
  SegmentLen : Longint;
  BytesRead  : TffWord32;                                              {!!.06}
  Offset     : Longint;
  FileName   : TffFullFileName;
begin

  with aSrc.Table do begin

    { Assumption: Transaction has already been started by a calling routine. }

    { See if we have a file BLOB }
    if FFTblGetFileNameBLOB(Files[Dictionary.BLOBFileNumber],
                            aSrc.Database.TransactionInfo,
                            aSourceBLOBNr, FileName) then begin
      FFTblAddFileBLOB(Files[Dictionary.BLOBFileNumber],
                       aSrc.Database.TransactionInfo,
                       FileName, aTargetBLOBNr);
      Result := DBIERR_NONE;
    end
    else begin

      { Otherwise copy the BLOB in segments based on the size of the
        given transfer buffer }
      SourceLen := aSrc.BLOBGetLength(aSourceBLOBNr, Result);
      if Result <> DBIERR_NONE then Exit;

      Offset := 0;
      SegmentLen := FFMinI(aBufLen, SourceLen);
      while Offset < SourceLen do begin
        Result := aSrc.BLOBRead(aSourceBLOBNr, Offset, SegmentLen, aBuffer^,
                                BytesRead);
        if Result <> DBIERR_NONE then Exit;

        Result := aTgt.BLOBWrite(aTargetBLOBNr, Offset, BytesRead, aBuffer^);
        if Result <> DBIERR_NONE then Exit;

        Inc(Offset, BytesRead);
      end;  { while }
    end;
  end;  { with }
end;
{--------}
function TffServerEngine.SessionAdd(const aClientID : TffClientID;
                                    const timeout : Longint;
                                    var   aSessionID : TffSessionID) : TffResult;
var
  Client : TffSrClient;
  Session : TffSrSession;
begin
  try
    Result := CheckClientIDAndGet(aClientID, Client);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Client.Timeout);  { Just in case }
//        Session := TffSrSession.Create(Client, false, timeout);      {Deleted !!.03}
        SessionList.BeginWrite;
        try
{Begin !!.03}
          if Assigned(Client.clFirstSession) then begin
            Session := Client.clFirstSession;
            Client.clFirstSession := nil;
          end else begin
            Session := TffSrSession.Create(Client, false, timeout);
            SessionList.AddSession(Session);
          end;
{End !!.03}
        finally
          SessionList.EndWrite;
        end;
        aSessionID := Session.SessionID;
      finally
        Client.Deactivate;
      end
    else
      aSessionID := 0;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{Begin !!.06}
{--------}
function TffServerEngine.SessionCloseInactiveTables(aClientID : TffClientID) : TffResult;
var
  Client : TffSrClient;
begin
  try
    Result := CheckClientIDAndGet(aClientID, Client);
    if (Result = DBIERR_NONE) then
      try
        TableList.RemoveUnusedTAbles;
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SessionCount(aClientID : TffClientID;
                                      var aCount    : integer) : TffResult;
var
  Client : TffSrClient;
begin
  try
    Result := CheckClientIDAndGet(aClientID, Client);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Client.Timeout);  { Just in case }
        Client.SessionList.BeginRead;
        try
          aCount := Client.SessionList.SessionCount
        finally
          Client.SessionList.EndRead;
        end
      finally
        Client.Deactivate;
      end
    else
      aCount := 0;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SessionGetCurrent(aClientID  : TffClientID;
                                       var aSessionID : TffSessionID) : TffResult;
var
  Client  : TffSrClient;
  aSession : TffSrSession;
begin
  try
    aSessionID := 0;
    Result := CheckClientIDAndGet(aClientID, Client);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Client.Timeout);  { just in case }
        Client.SessionList.BeginRead;
        try
          aSession := Client.SessionList.CurrentSession;
        finally
          Client.SessionList.EndRead;
        end;
        if assigned(aSession) then
          aSessionID := aSession.SessionID;
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SessionRemove(aClientID  : TffClientID;
                                       aSessionID : TffSessionID) : TffResult;
var
  Session : TffSrSession;
begin
  try
    Result := seCheckSessionIDAndGet(aSessionID, Session);
    if (Result = DBIERR_NONE) then begin
      FFSetRetry(Session.Timeout);  { just in case }
      if Session.CanClose(True) then begin
        Session.Free;
        TableList.RemoveUnusedTables;
      end else
        Session.RequestClose;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SessionSetCurrent(aClientID  : TffClientID;
                                           aSessionID : TffSessionID) : TffResult;
var
  Client  : TffSrClient;
  aSession : TffSrSession;
begin
  try
    Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, aSession);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Client.Timeout);  { just in case }
        Client.SessionList.BeginWrite;
        try
          Client.SessionList.CurrentSession := aSession;
        finally
          Client.SessionList.EndWrite;
        end;
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SessionSetTimeout(const aClientID : TffClientID;
                                           const aSessionID : TffSessionID;
                                           const aTimeout : Longint) : TffResult;
var
  Client : TffSrClient;
  Session : TffSrSession;
begin
  try
    Result := CheckSessionIDAndGet(aClientID, aSessionID, Client, Session);
    if Result = DBIERR_NONE then
      try
        Session.Timeout := aTimeout;
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorClone(aCursorID    : TffCursorID;
                                     aOpenMode    : TffOpenMode;
                                 var aNewCursorID : TffCursorID)
                                                  : TffResult;

var
  aCursor,                                                             {!!.03}
  aNewCursor : TffSrBaseCursor;                                        {!!.03}
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, aCursor);
    if (Result = DBIERR_NONE) then begin                               {!!.06 - Start}
      FFSetRetry(aCursor.Timeout);
      aNewCursor := aCursor.CloneCursor(aOpenMode);                    {!!.03}
      CursorList.BeginWrite;
      try
        CursorList.AddCursor(aNewCursor);                              {!!.03}
        aNewCursorID := aNewCursor.CursorID;                           {!!.03}
      finally
        CursorList.EndWrite;
        aCursor.Deactivate;
      end; { try..finally }
    end; { if }                                                        {!!.06 - End}
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorClose(aCursorID : TffCursorID) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := seCheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then begin
      FFSetRetry(Cursor.Timeout);
      if Cursor.CanClose(True) then
        Cursor.Free
      else
        Cursor.RequestClose;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorCompareBookmarks(aCursorID   : TffCursorID;
                                                aBookmark1,
                                                aBookmark2  : PffByteArray;
                                            var aCompResult : Longint) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        if Result = DBIERR_NONE then
          Result := Cursor.CompareBookmarks(aBookmark1, aBookmark2, aCompResult);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{Begin !!.02}
{--------}
function TffServerEngine.CursorCopyRecords(aSrcCursorID,
                                           aDestCursorID : TffCursorID;
                                           aCopyBLOBs : Boolean) : TffResult;
var
  aBLOBCopyMode : TffBLOBCopyMode;
  SrcCursor,
  DestCursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aSrcCursorID, SrcCursor);
    if (Result = DBIERR_NONE) then
      try
        Result := CheckCursorIDAndGet(aDestCursorID, DestCursor);
        if (Result = DBIERR_NONE) then
          try
            FFSetRetry(DestCursor.Timeout);                            {!!.10}
            if aCopyBLOBs then
              aBLOBCopyMode := ffbcmCopyFull
            else
              aBLOBCopyMode := ffbcmNoCopy;
            Result := DestCursor.CopyRecords(SrcCursor, aBLOBCopyMode, nil,
                                             0, 0);
          finally
            DestCursor.Deactivate;
          end;
      finally
        SrcCursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{End !!.02}
{Begin !!.06}
{--------}
function TffServerEngine.CursorDeleteRecords(aCursorID : TffCursorID) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);                                    {!!.10}
        Result := Cursor.DeleteRecords;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{End !!.02}
{--------}
function TffServerEngine.CursorGetBookmark(aCursorID : TffCursorID;
                                           aBookmark : PffByteArray) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);  { just in case }
        Result := Cursor.GetBookmark(aBookmark);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorGetBookmarkSize(aCursorID : TffCursorID;
                                           var aSize : integer) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);  { just in case }
        aSize := Cursor.GetBookmarkSize;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{Begin !!.03}
{--------}
function TffServerEngine.CursorListBLOBFreeSpace(aCursorID : TffCursorID;
                                           const aInMemory : Boolean;
                                                 aStream : TStream) : TffResult;
var
  Cursor : TffSrBaseCursor;
  StartedTrans : Boolean;
  TransID : TffTransID;
begin
  StartedTrans := False;
  try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    if (Result = DBIERR_NONE) then
      try
        if Cursor.Database.Transaction = nil then begin
          Result := seTransactionStart(Cursor.Database, False,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := (Result = DBIERR_NONE);
        end;
        FFSetRetry(Cursor.Timeout);
        Cursor.ListBLOBFreeSpace(Cursor.Database.TransactionInfo, aInMemory,
                                 aStream);
      finally
        if StartedTrans then
          seTransactionRollback(Cursor.Database);
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{End !!.03}
{--------}
function TffServerEngine.CursorOverrideFilter(aCursorID   : Longint;
                                              aExpression : pCANExpr;
                                              aTimeout    : TffWord32) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.OverrideFilter(aExpression, aTimeout);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorResetRange(aCursorID : TffCursorID) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        if (Cursor.IndexID = 0) then
          Result := DBIERR_NOASSOCINDEX
        else begin
          FFSetRetry(Cursor.Timeout);  { just in case }
          Cursor.ResetRange;
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorRestoreFilter(aCursorID   : Longint) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.RestoreFilter;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorSetFilter(aCursorID   : TffCursorID;
                                         aExpression : pCANExpr;
                                         aTimeout    : TffWord32) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        if aExpression^.iTotalSize <= SizeOf(CANExpr) then             {!!.01}
          aExpression:= nil;                                           {!!.01}
        Result := Cursor.SetFilter(aExpression, aTimeout);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorSetRange(aCursorID : TffCursorID;
                                        aDirectKey : boolean;
                                        aFieldCount1 : integer;
                                        aPartialLen1 : integer;
                                        aKeyData1    : PffByteArray;
                                        aKeyIncl1    : boolean;
                                        aFieldCount2 : integer;
                                        aPartialLen2 : integer;
                                        aKeyData2    : PffByteArray;
                                        aKeyIncl2    : boolean) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        if (Cursor.IndexID = 0) then
          Result := DBIERR_NOASSOCINDEX
        else begin
          FFSetRetry(Cursor.Timeout);  { just in case }
          Result := Cursor.SetRange(aDirectKey,
                                    aFieldCount1, aPartialLen1, aKeyData1, aKeyIncl1,
                                    aFieldCount2, aPartialLen2, aKeyData2, aKeyIncl2);
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorSetTimeout(const aCursorID : TffCursorID;
                                          const aTimeout : Longint) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if Result = DBIERR_NONE then
      try
        Cursor.Timeout := aTimeout;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorSetToBegin(aCursorID : TffCursorID) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Cursor.SetToBegin;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorSetToBookmark(aCursorID : TffCursorID;
                                             aBookmark : PffByteArray) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.SetToBookmark(aBookmark);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorSetToCursor(aDestCursorID : TffCursorID; aSrcCursorID : TffCursorID) : TffResult;
var
  DestCursor : TffSrBaseCursor;
  SrcCursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aDestCursorID, DestCursor);
    if (Result = DBIERR_NONE) then
      try
        Result := seCheckCursorIDAndGet(aSrcCursorID, SrcCursor);
          { We call the primitive seCheckCursorIDAndGet here because
            the client was just locked by the call to get the destination
            cursor. }
        if (Result = DBIERR_NONE) then begin
          FFSetRetry(DestCursor.Timeout);
          Result := DestCursor.SetToCursor(SrcCursor);
        end;
      finally
        DestCursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorSetToEnd(aCursorID : TffCursorID) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Cursor.SetToEnd;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorSetToKey(aCursorID     : TffCursorID;
                                        aSearchAction : TffSearchKeyAction;
                                        aDirectKey    : boolean;
                                        aFieldCount   : integer;
                                        aPartialLen   : integer;
                                        aKeyData      : PffByteArray) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        if (Cursor.IndexID = 0) then
          Result := DBIERR_NOASSOCINDEX
        else begin
          FFSetRetry(Cursor.Timeout);
          Result := Cursor.SetToKey(aSearchAction, aDirectKey,
                                    aFieldCount, aPartialLen, aKeyData);
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.CursorSwitchToIndex(aCursorID  : TffCursorID;
                                             aIndexName : TffDictItemName;
                                             aIndexID   : integer;
                                             aPosnOnRec : boolean) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    {get the cursor}
    Result := CheckCursorIDAndGet(aCursorID, Cursor);

    if (Result = DBIERR_NONE) then
      try
        {validate the index information; if the index name is non-blank
         it must exist and will supercede the index number; if the index
         name is blank the index number must exist}
        if (aIndexName <> '') then begin
          aIndexID := Cursor.Table.Dictionary.GetIndexFromName(aIndexName);
          if (aIndexID = -1) then
            Result := DBIERR_NOSUCHINDEX;
        end
        else if (0 > aIndexID) or
                (aIndexID >= Cursor.Table.Dictionary.IndexCount) then
          Result := DBIERR_NOSUCHINDEX;

        {switch indexes}
        if (Result = DBIERR_NONE) then
          if (aIndexID <> Cursor.IndexID) then begin
            FFSetRetry(Cursor.Timeout);
            Result := Cursor.SwitchToIndex(aIndexID, aPosnOnRec);
          end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.DatabaseAddAlias(const aAlias      : TffName;
                                          const aPath       : TffPath;
                                                aCheckSpace : Boolean;  {!!.11}
                                          const aClientID   : TffClientID)
                                                            : TffResult;
var
  Client : TffSrClient;
begin
  try
    Result := CheckClientIDAndGet(aClientID, Client);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Client.Timeout);
        Result := Client.NotifyExtenders(ffeaBeforeDBInsert, ffeaDBInsertFail);
        if (Result = DBIERR_NONE) then begin
          seConfig.AliasList.BeginWrite;
          try
            Result := seDatabaseAddAliasPrim(aAlias,
                                             aPath,
                                             aCheckSpace); {!!.11}
            if (Result = DBIERR_NONE) then
              WriteAliasData
            else
              Client.NotifyExtenders(ffeaDBInsertFail, ffeaNoAction);
          finally
            seConfig.AliasList.EndWrite;
          end;
        end;
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.seDatabaseAliasListPrim(aList : TList) : TffResult;
var
  Inx       : integer;
  AliasItem : TffAliasItem;
  TempDescr : PffAliasDescriptor;
begin
  { Assumption: Thread-safeness enforced at a higher level. }
  Result := DBIERR_NONE;
  for Inx := 0 to pred(seConfig.AliasList.Count) do begin
    FFGetMem(TempDescr, sizeOf(TffAliasDescriptor));
    AliasItem := seConfig.AliasList[Inx];
    with AliasItem do begin
      TempDescr^.adAlias := KeyAsStr;
      TempDescr^.adPath := Path;
    end;
    aList.add(TempDescr);
  end;
end;
{--------}
function TffServerEngine.DatabaseAliasList(aList     : TList;
                                           aClientID : TffClientID) : TffResult;
var
  Client    : TffSrClient;
begin
  try
    Result := CheckClientIDandGet(aClientID, Client);
    if Result = DBIERR_NONE then
      try
        FFSetRetry(Client.Timeout);
        Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);
        if Result = DBIERR_NONE then begin
          seConfig.AliasList.BeginRead;
          try
            Result := seDatabaseAliasListPrim(aList);
          finally
            seConfig.AliasList.EndRead;
          end;
        end;  { if }
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.RecoveryAliasList(aList     : TList;
                                           aClientID : TffClientID) : TffResult;
var
  Client    : TffSrClient;
begin
  try
    Result := seCheckClientIDandGet(aClientID, Client);
    if Result = DBIERR_NONE then begin
      FFSetRetry(Client.Timeout);
      Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);
      if Result = DBIERR_NONE then begin
        seConfig.AliasList.BeginRead;
        try
          Result := seDatabaseAliasListPrim(aList);
        finally
          seConfig.AliasList.EndRead;
        end;
      end;  { if }
    end;  { if }
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.DatabaseChgAliasPath(aAlias      : TffName;
                                              aNewPath    : TffPath;
                                              aCheckSpace : Boolean;    {!!.11}
                                              aClientID   : TffClientID)
                                                          : TffResult;
var
  Client : TffSrClient;
begin
  try
    Result := CheckClientIDandGet(aClientID, Client);
    if Result = DBIERR_NONE then
      try
        FFSetRetry(Client.Timeout);
        {check whether the alias exists}
        seConfig.AliasList.BeginWrite;
        try
          if not seConfig.AliasList.AliasExists(aAlias) then begin
            Result := DBIERR_UNKNOWNDB;
            Exit;
          end;
          {delete the old alias}
          seConfig.AliasList.DeleteAlias(aAlias);

          {add the Alias again and its new path}
          seConfig.AddAlias(aAlias, aNewPath, aCheckSpace);            {!!.11}
          WriteAliasData;
        finally
          seConfig.AliasList.EndWrite;
        end;
        Result := DBIERR_NONE;
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.DatabaseClose(aDatabaseID : TffDatabaseID) : TffResult;
var
  DB  : TffSrDatabase;
begin
  try
    Result := seCheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result = DBIERR_NONE) then begin
      FFSetRetry(DB.Timeout);
      { We can free the database if there are no open cursors
        & if the database is not active.
        Note: We are protected by the TableOpen method's behavior.
              If a table is in the process of being opened
              then DB's state will be ffosActive & we won't free the
              database. }
      if DB.CanClose(True) then begin
        DB.Free;
        TableList.RemoveUnusedTables;
      end else
        DB.RequestClose;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.seDatabaseDeleteAliasPrim(aAlias : TffName) : TffResult;
begin
  { Assumption: Thread-safeness enforced at a higher level. }
  Result := DBIERR_NONE;

  { Does the alias exist? }
  if not seConfig.AliasList.AliasExists(aAlias) then
    { No.  Notify client. }
    Result := DBIERR_UNKNOWNDB
  else
    { Delete the old alias}
    seConfig.AliasList.DeleteAlias(aAlias);
end;
{--------}
function TffServerEngine.DatabaseDeleteAlias(aAlias    : TffName;
                                             aClientID : TffClientID) : TffResult;
var
  Client : TffSrClient;
begin
  try
    Result := CheckClientIDandGet(aClientID, Client);
    if Result = DBIERR_NONE then
      try
        FFSetRetry(Client.Timeout);
        Result := Client.NotifyExtenders(ffeaBeforeDBDelete, ffeaDBDeleteFail);
        if Result = DBIERR_NONE then begin
          seConfig.AliasList.BeginWrite;
          try
            Result := seDatabaseDeleteAliasPrim(aAlias);
            if Result = DBIERR_NONE then
              WriteAliasData
            else
              Client.NotifyExtenders(ffeaDBDeleteFail, ffeaNoAction);
          finally
            seConfig.AliasList.EndWrite;
          end;
          Result := DBIERR_NONE;
        end;  { if }
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.DatabaseOpen(aClientID   : TffClientID;
                                const aAlias      : TffName;
                                const aOpenMode   : TffOpenMode;
                                const aShareMode  : TffShareMode;
                                const aTimeout    : Longint;
                                  var aDatabaseID : TffDatabaseID)
                                                  : TffResult;
var
  aDatabase  : TffSrDatabase;
  aSession   : TffSrSession;
  Folder     : TffSrFolder;
  Client     : TffSrClient;
  DB         : TffSrDatabase;
  UNCPath    : TffPath;
  CheckSpace : Boolean;                                                {!!.11}
begin
  aDatabase := nil;
  Folder := nil;
  try
    {the client must exist}
    Result := CheckClientIDAndGet(aClientID, Client);
    if (Result <> DBIERR_NONE) then
      Exit;

    try
      FFSetRetry(Client.Timeout);
      Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);

      if Result = DBIERR_NONE then begin
        {get the current session}
        Client.SessionList.BeginRead;
        try
          aSession := Client.SessionList.CurrentSession;
        finally
          Client.SessionList.EndRead;
        end;
        {check to see whether the Alias exists}
        seConfig.AliasList.BeginRead;
        try
          if not seConfig.AliasList.AliasExists(aAlias) then begin
            Result := DBIERR_UNKNOWNDB;
            Exit;
          end;
          {get the Alias path}
          UNCPath := seConfig.AliasList.Path[aAlias];
          CheckSpace := seConfig.AliasList.CheckDiskSpace(aAlias);     {!!.11}
        finally
          seConfig.AliasList.EndRead;
        end;
        {check to see whether the directory exists}
        if not FFDirectoryExists(UNCPath) then begin
          Result := DBIERR_INVALIDDIR;
          Exit;
        end;
        {get a path id for this path}
        FolderList.BeginWrite;
        try
          Folder := FolderList.AddFolder(UNCPath, IsReadOnly, seBufMgr);
        finally
          FolderList.EndWrite;
        end;
        UNCPath := Folder.Path;

        {check to see whether this Alias has already been opened and in
         a non-compatible state (ie we or some other client/session
         wants it opened exclusively)}
        DatabaseList.BeginWrite;
        try
          DB := DatabaseList.GetDatabaseForFolder(Folder);
          if assigned(DB) then begin
            if ((DB.ShareMode = smExclusive) or (aShareMode = smExclusive)) and
               ((TffSrClient(DB.Client).ClientID <> aClientID) or
                (DB.Session <> aSession)) then begin
              Result := DBIERR_NEEDEXCLACCESS;
              Exit;
            end;
          end;
          {create a new database object, add it to the global list}
          aDatabase := seDatabaseOpenPrim(aSession,
                                          Folder,
                                          aAlias,
                                          aOpenMode,
                                          aShareMode,
                                          aTimeout,
                                          CheckSpace);                 {!!.11}
          aDatabaseID := aDatabase.DatabaseID;
        finally
          DatabaseList.EndWrite;
        end;
      end;
    finally
      Client.Deactivate;
    end;
  except
    on E : Exception do begin
      if (aDatabase <> nil) then
        aDatabase.Free
      else {aDatabase was never created}
        if (Folder <> nil) then begin
          FolderList.BeginWrite;
          try
            FolderList.DeleteFolderByID(Folder.FolderID);
          finally
            FolderList.EndWrite;
          end;
        end;
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.DatabaseOpenNoAlias(aClientID   : TffClientID;
                                       const aPath       : TffPath;
                                       const aOpenMode   : TffOpenMode;
                                       const aShareMode  : TffShareMode;
                                       const aTimeout    : Longint;
                                         var aDatabaseID : TffDatabaseID)
                                                         : TffResult;
var
  aDatabase      : TffSrDatabase;
  anAlias        : TffName;
  aSession       : TffSrSession;
  Folder         : TffSrFolder;
  Client         : TffSrClient;
  DatabaseExists : Boolean;
  DB             : TffSrDatabase;
  UNCPath        : TffPath;
  CheckSpace     : Boolean;                                            {!!.11}
begin
  aDatabase := nil;
  Folder := nil;
  try
    { The path cannot be empty. }
    if (aPath = '') then begin
      Result := DBIERR_INVALIDDIR;
      Exit;
    end;

    { The client must exist. }
    Result := CheckClientIDAndGet(aClientID, Client);
    if (Result <> DBIERR_NONE) then
      Exit;

    try
      FFSetRetry(Client.Timeout);
      Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);

      if Result = DBIERR_NONE then begin
        {get the current session}
        Client.SessionList.BeginRead;
        try
          aSession := Client.SessionList.CurrentSession;
        finally
          Client.SessionList.EndRead;
        end;
        {check to see whether the directory exists}
        if not FFDirectoryExists(aPath) then begin
          Result := DBIERR_INVALIDDIR;
          Exit;
        end;
        {get a folder for this path}
        FolderList.BeginWrite;
        try
          Folder := FolderList.AddFolder(aPath, IsReadOnly, seBufMgr);
        finally
          FolderList.EndWrite;
        end;
        UNCPath := Folder.Path;
        {check to see whether this path has already been opened and in
         a non-compatible state (ie we or some other client/session
         wants it opened exclusively)}
        anAlias := '';
        CheckSpace := True;                                            {!!.11}
        DatabaseList.BeginWrite;
        try
          DB := DatabaseList.GetDatabaseForFolder(Folder);
          DatabaseExists := assigned(DB);
          if DatabaseExists then begin
            CheckSpace := DB.CheckSpace;                               {!!.11}
            if ((DB.ShareMode = smExclusive) or (aShareMode = smExclusive)) and
               ((TffSrClient(DB.Client).ClientID <> aClientID) or
                (DB.Session <> aSession)) then begin
              Result := DBIERR_NEEDEXCLACCESS;
              Exit;
            end;
            anAlias := DB.Alias;
          end;
          { Create a new database object, add it to the global list. }
          aDatabase := seDatabaseOpenPrim(aSession,
                                          Folder,
                                          anAlias,
                                          aOpenMode,
                                          aShareMode,
                                          aTimeout,
                                          CheckSpace);                 {!!.11}
          aDatabaseID := aDatabase.DatabaseID;
        finally
          DatabaseList.EndWrite;
        end;
      end;
    finally
      Client.Deactivate;
    end;
  except
    on E : Exception do begin
      if assigned(aDatabase) then
        aDatabase.Free
      else {database was never created}
        if (Folder <> nil) then begin
          FolderList.BeginWrite;
          try
            FolderList.DeleteFolderByID(Folder.FolderID);
          finally
            FolderList.EndWrite;
          end;
        end;
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.DatabaseSetTimeout(const aDatabaseID : TffDatabaseID;
                                            const aTimeout    : Longint) : TffResult;
var
  DB : TffSrDatabase;
begin
  try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if Result = DBIERR_NONE then
      try
        DB.Timeout := aTimeout;
      finally
        DB.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.DatabaseTableExists(aDatabaseID : TffDatabaseID;
                                       const aTableName  : TffTableName;
                                         var aExists     : Boolean) : TffResult;
var
  DB : TffSrDatabase;
  SearchPath : TffPath;                                              
begin
  try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result <> DBIERR_NONE) then
      Exit;

    try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      if Result = DBIERR_NONE then begin
        SearchPath := DB.Folder.Path;
        if (SearchPath[length(SearchPath)] <> '\') then
          FFShStrAddChar(SearchPath, '\');
        aExists := FFFileExists(SearchPath + FFForceExtension(aTableName, ffc_ExtForData));
      end;

    finally
      DB.Deactivate;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}

end;
{Begin !!.11}
{--------}
function TffServerEngine.seTableExistsPrim(aDB : TffSrDatabase;
                                     const aTableName: TffTableName) : Boolean;
var
  SearchPath : TffPath;
begin
  { The table name must be a valid file name without extension. }
  if not FFVerifyFileName(aTableName) then
    FFRaiseException(EffException, ffstrResServer,
                     fferrInvalidTableName, [aTableName]);
                     
  SearchPath := aDB.Folder.Path;
  if (SearchPath[length(SearchPath)] <> '\') then
    FFShStrAddChar(SearchPath, '\');
  Result := FFFileExists(SearchPath +
                         FFForceExtension(aTableName, ffc_ExtForData));
end;
{End !!.11}
{--------}
function TffServerEngine.DatabaseTableList(aDatabaseID : TffDatabaseID;
                                     const aMask       : TffFileNameExt;
                                           aList       : TList) : TffResult;
var
  DB         : TffSrDatabase;
  FindRes    : integer;
  TableDesc  : PffTableDescriptor;
  SearchRec  : TffSearchRec;
  SearchMask : TffPath;
begin
  try

    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result <> DBIERR_NONE) then
      exit;

    try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      if Result = DBIERR_NONE then begin
        SearchMask := DB.Folder.Path;
        if (SearchMask[length(SearchMask)] <> '\') then
          FFShStrAddChar(SearchMask, '\');
        if (aMask = '') then begin
          FFShStrConcat(SearchMask, '*.');
          FFShStrConcat(SearchMask, ffc_ExtForData);
        end
        else begin                                                     {BEGIN !!.01}
          FFShStrConcat(SearchMask, aMask);
          {$IFDEF OnlyRetrieveTables}
          FFForceExtension(SearchMask, ffc_ExtForData);
          {$ENDIF}
        end;                                                           {END !!.01}
        FindRes := FFFindFirst(SearchMask, [ditFile], diaAnyAttr, SearchRec);
        while (FindRes = 0) do begin
          FFGetMem(TableDesc, sizeOf(TffTableDescriptor));
          with SearchRec do begin
            TableDesc^.tdTableName := FFExtractFileName(srName);
            TableDesc^.tdExt := FFExtractExtension(srName);
            TableDesc^.tdSizeLo := srSize;
            TableDesc^.tdSizeHi := srSizeHigh;
            TableDesc^.tdTimeStamp := srTime;
          end;
          aList.Add(TableDesc);
          FindRes := FFFindNext(SearchRec);
        end;
        FFFindClose(SearchRec);
      end;
    finally
      DB.Deactivate;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.DatabaseTableLockedExclusive(aDatabaseID : TffDatabaseID;
                                                const aTableName  : TffTableName;
                                                  var aLocked     : Boolean) : TffResult;
var
  DB    : TffSrDatabase;
  Table : TffSrBaseTable;
begin
  aLocked := False;
  try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result <> DBIERR_NONE) then
      exit;

    try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      Table := GetTableInstance(DB.Folder, aTableName);

     { Is the table open? }
      if Assigned(Table) then
        aLocked := Table.Folder.LockMgr.TableLockGranted(Table.TableID) = ffsltExclusive;
    finally
      DB.Deactivate;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.GetTableInstance(aFolder    : TffSrFolder;
                                    const aTableName : TffTableName) : TffSrBaseTable;
var
  Inx : integer;
begin
  { Assumption: Calling routine has locked TableList appropriately. }
  for Inx := 0 to pred(TableList.TableCount) do begin
    Result := TableList[ftFromIndex, Inx];
    with Result do
      if (Folder = aFolder) and
         (FFCmpShStrUC(BaseName, aTableName, 255) = 0) then
        Exit;
  end;
  Result := nil;
end;
{--------}
function TffServerEngine.IndexClear(aCursorID : TffCursorID) : TffResult;
{Restructured !!.01}
var
  Cursor : TffSrBaseCursor;
  StartedTrans : boolean;
  TransID : TffTransID;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if (Result = DBIERR_NONE) then
    try
      StartedTrans := false;
      try
        FFSetRetry(Cursor.Timeout);
        { Make sure a read-only transaction is active. }
        if not assigned(Cursor.Database.Transaction) then begin
          Result := seTransactionStart(Cursor.Database, false,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := (Result = DBIERR_NONE);
        end;
        if Result = DBIERR_NONE then begin
          Cursor.ClearIndex;
          if StartedTrans then
            seTransactionCommit(Cursor.Database);
        end;
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.IsTableNameOpen(aFolder    : TffSrFolder;
                                   const aTableName : TffTableName) : boolean;
var
  Inx : integer;
begin
  Result := true;
  TableList.BeginRead;
  try
    for Inx := 0 to pred(TableList.TableCount) do
      with TableList[ftFromIndex, Inx] do
        if (Folder = aFolder) and
           (FFCmpShStrUC(BaseName, aTableName, 255) = 0) then
          Exit;
  finally
    TableList.EndRead;
  end;
  Result := false;
end;
{--------}
function TffServerEngine.seCheckSessionIDAndGet(aSessionID : TffSessionID;
                                            var aSession   : TffSrSession) : TffResult;
begin
  Result := DBIERR_FF_UnknownSession;
  try
    if TObject(aSessionID) is TffSrSession then begin
      aSession := TffSrSession(aSessionID);
      Result := DBIERR_NONE;
    end;
  except
    { An exception may be raised if the ID is bogus.  Swallow the exception.}
  end;
end;
{--------}
function TffServerEngine.CheckSessionIDAndGet(aClientID : TffClientID;
                                              aSessionID : TffSessionID;
                                          var aClient  : TffSrClient;
                                          var aSession : TffSrSession) : TffResult;
begin
  if State <> ffesStarted then begin
    Result := DBIERR_FF_ServerUnavail;
    Exit;
  end;

  Result := CheckClientIDAndGet(aClientID, aClient);
  if (Result = DBIERR_NONE) then
    Result := seCheckSessionIDAndGet(aSessionID, aSession);
end;
{--------}
procedure TffServerEngine.lcSetEventLog(anEventLog : TffBaseLog);
begin
  inherited lcSetEventLog(anEventLog);
  seSetLoggingState;
end;
{--------}
procedure TffServerEngine.lcSetLogEnabled(const aEnabled : boolean);
begin
  inherited lcSetLogEnabled(aEnabled);
  seSetLoggingState;
end;
{--------}
function TffServerEngine.RebuildRegister(aClientID     : TffClientID;
                                         aTotalRecords : Longint) : TffSrRebuildStatus;
begin
  Result := seRebuildList.AddRebuildStatus(aClientID, aTotalRecords);
end;
{--------}
procedure TffServerEngine.RebuildDeregister(aRebuildID : Longint);
begin
  seRebuildList.MarkRebuildStatusFinished(aRebuildID);
end;
{--------}
function TffServerEngine.RebuildGetStatus(aRebuildID : Longint;
                                    const aClientID  : TffClientID;
                                      var aIsPresent : boolean;
                                      var aStatus    : TffRebuildStatus) : TffResult;
var
  Client : TffSrClient;
begin
  Result := seCheckClientIDAndGet(aClientID, Client);
  if Result = DBIERR_NONE then begin
    aIsPresent := seRebuildList.GetRebuildStatus(aRebuildID, aStatus);
    Result := DBIERR_NONE;
  end;
end;
{--------}
function TffServerEngine.RecordDelete(aCursorID : TffCursorID; aData : PffByteArray) : TffResult;
var
  Cursor : TffSrBaseCursor;
  Trans : TffSrTransaction;
  TransID : TffTransID;
  StartedTrans : boolean;
begin
  StartedTrans := false;
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if Result = DBIERR_NONE then
      try
        FFSetRetry(Cursor.Timeout);
        if (Result = DBIERR_NONE) then begin
          Result := Cursor.EnsureWritable(True, True);                 {!!.02}
          if (Result = DBIERR_NOACTIVETRAN) or Cursor.NeedNestedTransaction then begin {!!.03}
            Result := seTransactionStart(Cursor.Database, false,
                                         ffcl_TrImplicit, TransID);
            StartedTrans := Result = DBIERR_NONE;
          end;
          try
            if (Result = DBIERR_NONE) then begin
              Result := Cursor.DeleteRecord(aData);
              if (Result <> DBIERR_NONE) and not StartedTrans then begin
                Trans := Cursor.Database.Transaction;
                Trans.IsCorrupt := true;
              end;
            end;
          finally
            if StartedTrans then
              if (Result = DBIERR_NONE) then
                Result := seTransactionCommit(Cursor.Database)
              else
                seTransactionRollback(Cursor.Database);
          end;{try..finally}
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  end;{try..except}
end;
{--------}
function TffServerEngine.RecordDeleteBatch(aCursorID : TffCursorID;
                                           aBMCount  : Longint;
                                           aBMLen    : Longint;
                                           aData     : PffByteArray;
                                           aErrors   : PffLongintArray
                                          ) : TffResult;
{Restructured !!.10}
var
  Cursor   : TffSrBaseCursor;
  TransID  : TffTransID;
  Offset   : Longint;
  IRRes    : TffResult;
  RecInx   : integer;
  StartedTrans : boolean;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if (Result = DBIERR_NONE) then
    try
      StartedTrans := false;
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False);                 {!!.02}
        if (Result = DBIERR_NOACTIVETRAN) or
           Cursor.NeedNestedTransaction then begin                     {!!.03}
          Result := seTransactionStart(Cursor.Database, false,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := true;
        end;
        if (Result = DBIERR_NONE) then begin
          try
            Offset := 0;
            for RecInx := 0 to pred(aBMCount) do begin
              IRRes := CursorSetToBookmark(aCursorID,                  {!!.10}
                                           PffByteArray(@aData^[Offset])); {!!.10}
              if IRRes = DBIERR_NONE then
                IRRes := RecordDelete(aCursorID, nil);
              aErrors^[RecInx] := IRRes;
              inc(Offset, aBMLen);
            end;
          finally
            if StartedTrans then
              Result := seTransactionCommit(Cursor.Database);
          end;{try..finally}
        end;
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.RecordExtractKey(aCursorID : TffCursorID;
                                          aData     : PffByteArray;
                                          aKey      : PffByteArray) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        if Result = DBIERR_NONE then
          if (Cursor.IndexID = 0) then
            Result := DBIERR_NOASSOCINDEX
          else begin
            Result := Cursor.ExtractKey(aData, aKey);
            if Result = DBIERR_NONE then
              Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
          end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.RecordGet(aCursorID : TffCursorID;
                                   aLockType : TffLockType;
                                   aData     : PffByteArray) : TffResult;
var
  Cursor : TffSrBaseCursor;
  ServerLockType : TffSrLockType;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if Result = DBIERR_NONE then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        if (Result = DBIERR_NONE) then begin
          ServerLockType := FFMapLock(aLockType, false);
          Result := Cursor.GetRecord(aData, ServerLockType);
          if Result = DBIERR_NONE then
            Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.RecordGetBatch(aCursorID : TffCursorID;
                                        aRecCount : Longint;
                                        aRecLen   : Longint;
                                    var aRecRead  : Longint;
                                        aData     : PffByteArray;
                                    var aError    : TffResult) : TffResult;
var
  Cursor : TffSrBaseCursor;
  Offset : Longint;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        if Result = DBIERR_NONE then begin
          Offset := 0;
          aError := Cursor.GetNextRecord(PffByteArray(@aData^[Offset]), ffsltNone);
          if (aError = DBIERR_NONE) then
            aRecRead := 1
          else
            aRecRead := 0;
          if aError = DBIERR_FF_FilterTimeout then
            Result := aError;
          while (aError = DBIERR_NONE) and (aRecRead < aRecCount) do begin
            inc(Offset, aRecLen);
            aError := Cursor.GetNextRecord(PffByteArray(@aData^[Offset]), ffsltNone);
            if (aError = DBIERR_NONE) then
              inc(aRecRead);
          end; {while}
        end; {if}
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.RecordGetForKey(aCursorID   : TffCursorID;
                                         aDirectKey  : boolean;
                                         aFieldCount : integer;
                                         aPartialLen : integer;
                                         aKeyData    : PffByteArray;
                                         aData       : PffByteArray;
                                         aFirstCall  : Boolean) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        if Result = DBIERR_NONE then begin
          Result := Cursor.GetRecordForKey(aDirectKey,
                                           aFieldCount,
                                           aPartialLen,
                                           aKeyData,
                                           aData,
                                           aFirstCall);
          if Result = DBIERR_NONE then
            Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.RecordGetNext(aCursorID : TffCursorID;
                                       aLockType : TffLockType;
                                       aData     : PffByteArray) : TffResult;
var
  Cursor : TffSrBaseCursor;
  ServerLockType : TffSrLockType;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        if Result = DBIERR_NONE then begin
          ServerLockType := FFMapLock(aLockType, false);
          Result := Cursor.GetNextRecord(aData, ServerLockType);
          if Result = DBIERR_NONE then
            Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.RecordGetNextSeq(aCursorID : TffCursorID;
                                      var aRefNr    : TffInt64;
                                          aData     : PffByteArray) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        if Result = DBIERR_NONE then
          Cursor.Table.GetNextRecordSeq(Cursor.Database.TransactionInfo, aRefNr, aData);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.RecordGetPrior(aCursorID : TffCursorID;
                                        aLockType : TffLockType;
                                        aData : PffByteArray) : TffResult;
var
  Cursor : TffSrBaseCursor;
  ServerLockType : TffSrLockType;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeRecRead, ffeaNoAction);
        if Result = DBIERR_NONE then begin
          ServerLockType := FFMapLock(aLockType, false);
          Result := Cursor.GetPriorRecord(aData, ServerLockType);
          if Result = DBIERR_NONE then
            Cursor.NotifyExtenders(ffeaAfterRecRead, ffeaNoAction);
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.RecordInsert(aCursorID : TffCursorID;
                                      aLockType : TffLockType;
                                      aData     : PffByteArray) : TffResult;
{Restructured !!.10}
var
  Cursor : TffSrBaseCursor;
  ServerLockType : TffSrLockType;
  StartedTrans : boolean;
  TransID : TffTransID;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if (Result = DBIERR_NONE) then
    try
      StartedTrans := False;
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, True);                  {!!.02}
        { Need to start an implicit transaction? }
        if (Result = DBIERR_NOACTIVETRAN) or                           {!!.03}
           Cursor.NeedNestedTransaction then begin                     {!!.03}
          Result := seTransactionStart(Cursor.Database, false,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := (Result = DBIERR_NONE);
        end;
//        else                                                         {Deleted !!.11}
//          Cursor.Table.UseInternalRollback := True;                  {Deleted !!.11}

//        try                                                          {Deleted !!.11}
        if (Result = DBIERR_NONE) then begin
          ServerLockType := FFMapLock(aLockType, false);
          Result := Cursor.InsertRecord(aData, ServerLockType);
        end;
//        finally                                                      {Deleted !!.11}
{Begin !!.05}
//          Cursor.Table.UseInternalRollback := False;                 {Deleted !!.11}
//        end;
        if StartedTrans then begin
          if (Result = DBIERR_NONE) then
            Result := seTransactionCommit(Cursor.Database)
          else
            seTransactionRollback(Cursor.Database);
        end;
{End !!.05}
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.RecordInsertBatch(aCursorID : TffCursorID;
                                           aRecCount : Longint;
                                           aRecLen   : Longint;
                                           aData     : PffByteArray;
                                           aErrors   : PffLongintArray) : TffResult;
{Restructured !!.10}
var
  Cursor : TffSrBaseCursor;
  TransID : TffTransID;
  Offset : Longint;
  IRRes  : TffResult;
  RecInx : integer;
  StartedTrans : boolean;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if (Result = DBIERR_NONE) then
    try
      StartedTrans := false;
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(False, False);                 {!!.02}
        if (Result = DBIERR_NOACTIVETRAN) or
           Cursor.NeedNestedTransaction then begin                     {!!.03}
          Result := seTransactionStart(Cursor.Database, false,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := true;
        end;
        if (Result = DBIERR_NONE) then begin
          try
            Offset := 0;
            for RecInx := 0 to pred(aRecCount) do begin
              IRRes := RecordInsert( aCursorID, ffltWriteLock,
                                     PffByteArray(@aData^[Offset]));
              aErrors^[RecInx] := IRRes;
              inc(Offset, aRecLen);
            end;
          finally
            if StartedTrans then
              Result := seTransactionCommit(Cursor.Database);
          end;{try..finally}
        end;
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.RecordIsLocked(aCursorID : TffCursorID; aLockType : TffLockType;
                                    var aIsLocked : boolean) : TffResult;
var
  Cursor : TffSrBaseCursor;
  ServerLockType : TffSrLockType;
begin
  Result := DBIERR_NONE;
  aIsLocked := false;
  if (aLockType = ffltNoLock) then
    Exit;

  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        if Result = DBIERR_NONE then begin
          ServerLockType := FFMapLock(aLockType, true);

          aIsLocked := Cursor.IsRecordLocked(ServerLockType);
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.RecordModify(aCursorID : TffCursorID;
                                      aData     : PffByteArray;
                                      aRelLock  : boolean) : TffResult;
{Restructured !!.10}
var
  Cursor : TffSrBaseCursor;
  TransID : TffTransID;
  StartedTrans : boolean;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if (Result = DBIERR_NONE) then
    try
      StartedTrans := false;
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.EnsureWritable(True, False);                  {!!.02}
        { Need to start an implicit transaction? }
        if (Result = DBIERR_NOACTIVETRAN) or                           {!!.03}
           Cursor.NeedNestedTransaction then begin                     {!!.03}
          Result := seTransactionStart(Cursor.Database, false,
                                       ffcl_TrImplicit, TransID);
          StartedTrans := Result = DBIERR_NONE;
{Begin !!.03}
        end;
//        else                                                         {Deleted !!.11}
//          Cursor.Table.UseInternalRollback := True;                  {Deleted !!.11}
{End !!.03}

//        try                                                          {Deleted !!.11}
        if (Result = DBIERR_NONE) then begin
          Result := Cursor.ModifyRecord(aData, aRelLock);
        end;
{Begin !!.05}
//        finally                                                      {Deleted !!.11}
//          Cursor.Table.UseInternalRollback := False;                   {!!.03}{Deleted !!.11}
//        end;{try..finally}                                           {Deleted !!.11}
        if StartedTrans then begin
          if (Result = DBIERR_NONE) then
            Result := seTransactionCommit(Cursor.Database)
          else
            seTransactionRollback(Cursor.Database);
        end;  { if }
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
{End !!.05}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
function TffServerEngine.RecordRelLock(aCursorID : TffCursorID; aAllLocks : boolean) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  { Assumption: Transaction is active. }
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Cursor.RelRecordLock(aAllLocks);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.seDatabaseOpenPrim(Session     : TffSrSession;
                                            Folder      : TffSrFolder;
                                            anAlias     : TffName;
                                            aOpenMode   : TffOpenMode;
                                            aShareMode  : TffShareMode;
                                            aTimeout    : Longint;
                                            aCheckSpace : Boolean)     {!!.11}
                                                        : TffSrDatabase;
var
  aMonitor    : TffBaseEngineMonitor;
  anExtender  : TffBaseEngineExtender;
  anIndex     : Longint;
  MonitorList : TffList;
begin
  Result := TffSrDatabase.Create(Self,
                                 Session,
                                 Folder,
                                 anAlias,
                                 aOpenMode,
                                 aShareMode,
                                 aTimeout,
                                 aCheckSpace);                         {!!.11}
  { Assumption: Calling routine has gained write access to the database list. }
  DatabaseList.BeginWrite;
  try
    DatabaseList.AddDatabase(Result);
  finally
    DatabaseList.EndWrite;
  end;

  { If there are any monitors interested in databases then see if they
    are interested in this database. }
  MonitorList := GetInterestedMonitors(TffSrDatabase);
  if assigned(MonitorList) then begin
    for anIndex := 0 to pred(MonitorList.Count) do begin
      aMonitor := TffBaseEngineMonitor
                    (TffIntListItem(MonitorList[anIndex]).KeyAsInt);
      try
        anExtender := aMonitor.Interested(Result);
        if assigned(anExtender) then
          Result.dbAddExtender(anExtender);
      except
        on E:Exception do
          seForce('Monitor [%s] exception, seDatabaseOpenPrim: %s',    {!!.06 - Start}
                  [aMonitor.ClassName,E.message],
                  bseGetReadOnly);                                     {!!.06 - End}
      end;
    end;
    MonitorList.Free;
  end;

end;
{--------}
function TffServerEngine.SQLAlloc(aClientID : TffClientID;
                                  aDatabaseID : TffDatabaseID;
                                  aTimeout : Longint;
                              var aStmtID : TffSqlStmtID) : TffResult;
var
  Client : TffSrClient;
  DB : TffSrDatabase;
begin
  try
    Result := CheckClientIDAndGet(aClientID, Client);
    if Result = DBIERR_NONE then
      try
        Result := seCheckDatabaseIDAndGet(aDatabaseID, DB);
        if Result = DBIERR_NONE then begin
          FFSetRetry(5000);
          Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
          if Result = DBIERR_NONE then begin
            if assigned(seSQLEngine) then
              Result := seSQLEngine.Alloc(Self, aClientID, aDatabaseID,
                                         aTimeout, aStmtID)
            else
              FFRaiseException(EffServerException, ffStrResServer,
                               fferrNoSQLEngine, [seGetServerName]);
          end;
        end;
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SQLExec(aStmtID : TffSqlStmtID;
                                 aOpenMode : TffOpenMode;
                             var aCursorID : TffCursorID;
                                 aStream : TStream) : TffResult;
begin
  Result := DBIERR_NONE;
  try
    { Note: Timeout set in SQLAlloc. }
    if assigned(seSQLEngine) then
      Result := seSQLEngine.Exec(aStmtID, aOpenMode, aCursorID, aStream)
    else
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrNoSQLEngine, [seGetServerName]);
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SQLExecDirect(aClientID   : TffClientID;
                                       aDatabaseID : TffDatabaseID;
                                       aQueryText  : PChar;
                                       aTimeout    : Longint;
                                       aOpenMode   : TffOpenMode;
                                   var aCursorID   : TffCursorID;
                                       aStream     : TStream) : TffResult;
var
  Client : TffSrClient;
  DB : TffSrDatabase;
begin
  try
    Result := CheckClientIDAndGet(aClientID, Client);
    if Result = DBIERR_NONE then
      try
        Result := seCheckDatabaseIDAndGet(aDatabaseID, DB);
        if Result = DBIERR_NONE then begin
          FFSetRetry(aTimeout);
          Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
          if Result = DBIERR_NONE then begin
            if assigned(seSQLEngine) then
              Result := seSQLEngine.ExecDirect(Self, aClientID, aDatabaseID,
                                              aQueryText, aOpenMode, aTimeout,
                                              aCursorID, aStream)
            else
              FFRaiseException(EffServerException, ffStrResServer,
                               fferrNoSQLEngine, [seGetServerName]);
          end;
        end;
      finally
        Client.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SQLFree(aStmtID : TffSqlStmtID) : TffResult;
begin
  Result := DBIERR_NONE;
  try
    FFSetRetry(5000);
    if assigned(seSQLEngine) then
      Result := seSQLEngine.FreeStmt(aStmtID)
    else
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrNoSQLEngine, [seGetServerName]);
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SQLPrepare(aStmtID    : TffSqlStmtID;
                                    aQueryText : PChar;
                                    aStream    : TStream) : TffResult;
begin
  Result := DBIERR_NONE;
  try
    FFSetRetry(5000);
    if assigned(seSQLEngine) then
      Result := seSQLEngine.Prepare(aStmtID, aQueryText, aStream)
    else
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrNoSQLEngine, [seGetServerName]);
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.SQLSetParams(aStmtID     : TffSqlStmtID;
                                      aNumParams  : word;
                                      aParamDescs : Pointer;
                                      aDataBuffer : PffByteArray;
                                      aDataLen    : integer;
                                      aStream     : TStream) : TffResult;
begin
  Result := DBIERR_NONE;
  try
    FFSetRetry(5000);
    if assigned(seSQLEngine) then
      Result := seSQLEngine.SetParams(aStmtID, aNumParams, aParamDescs,
                                     aDataBuffer, aStream)
    else
      FFRaiseException(EffServerException, ffStrResServer,
                       fferrNoSQLEngine, [seGetServerName]);
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.TableAddIndex(const aDatabaseID : TffDatabaseID;
                                       const aCursorID   : TffCursorID;
                                       const aTableName  : TffTableName;
                                       const aIndexDesc  : TffIndexDescriptor) : TffResult;
{Restructured !!.10}
var
  Cursor       : TffSrBaseCursor;
  DB           : TffSrDatabase;
  StartedTrans : boolean;
  tmpCursorID  : TffCursorID;
  tmpTablename : string;
  TransID      : TffTransID;
  FI           : PffFileInfo;
  FileHeader   : PffBlockHeaderFile;
  aRelMethod   : TffReleaseMethod;
begin
  {choice of two here: if the cursor ID is set, use it. Otherwise
   use the databaseID/tablename}
  if (aCursorID <> 0) then begin
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        StartedTrans := false;
        try
          FFSetRetry(Cursor.Timeout);
          Result := Cursor.NotifyExtenders(ffeaBeforeAddInx, ffeaTabAddInxFail);
          if Result = DBIERR_NONE then begin
            tmpTableName := Cursor.Table.BaseName;
            Result := seTransactionStart(Cursor.Database, false,
                                         ffcl_TrImplicit, TransID);
            if (Result = DBIERR_NONE) then begin
              StartedTrans := true;
              Result := Cursor.AddIndexToTable(aIndexDesc);
{Begin !!.13}
              if (Result = DBIERR_NONE) then begin
                {update the file header}
                TableList.BeginRead;
                try
                  FI := TableList.GetTableFromName(aTableName).Files[0];
                finally
                  TableList.EndRead;
                end;
                FileHeader :=
                  PffBlockHeaderFile(BufferManager.GetBlock(FI, 0, DB.dbTI,
                                                            True,
                                                            aRelMethod));
                inc(FileHeader^.bhfIndexCount);
                aRelMethod(PffBlock(FileHeader));
                seTransactionCommit(Cursor.Database)
              end
{End !!.13}
              else begin
                Cursor.NotifyExtenders(ffeaTabAddInxFail, ffeaNoAction);
                seTransactionRollback(Cursor.Database)
              end;
            end;
          end;
        except
          on E : Exception do begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            if StartedTrans then
              seTransactionRollback(Cursor.Database);
          end;
        end;{try..except}
      finally
        Cursor.Deactivate;
      end;
  end
  else {use databaseID/tablename} begin
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result = DBIERR_NONE) then
      try
        StartedTrans := False;
        try
          FFSetRetry(DB.Timeout);
          Result := TableOpen(aDatabaseID, aTableName,
                              false, '', 0, omReadWrite, smExclusive, DB.Timeout,
                              tmpCursorID, nil);
          if (Result = DBIERR_NONE) then
            try
              Result := seCheckCursorIDAndGet(tmpCursorID, Cursor);
              if (Result = DBIERR_NONE) then begin
                Result := Cursor.NotifyExtenders(ffeaBeforeAddInx, ffeaTabAddInxFail);
                if Result = DBIERR_NONE then begin
                  Result := seTransactionStart(DB, false, ffcl_TrImplicit,
                                               TransID);
                  if (Result = DBIERR_NONE) then begin
                    StartedTrans := true;
                    Result := Cursor.AddIndexToTable(aIndexDesc);
                    if (Result = DBIERR_NONE) then begin
                      {update the file header}
                      TableList.BeginRead;
                      try
                        FI := TableList.GetTableFromName(aTableName).Files[0];
                      finally
                        TableList.EndRead;
                      end;
                      FileHeader :=
                        PffBlockHeaderFile(BufferManager.GetBlock(FI, 0, DB.dbTI,
                                                                  True,
                                                                  aRelMethod));
                      inc(FileHeader^.bhfIndexCount);
                      aRelMethod(PffBlock(FileHeader));
                      seTransactionCommit(Cursor.Database)
                    end else begin
                      Cursor.NotifyExtenders(ffeaTabAddInxFail, ffeaNoAction);
                      seTransactionRollback(Cursor.Database)
                    end;
                  end;
                end;
              end;
            finally
              CursorClose(tmpCursorID);
            end;
        except
          on E : Exception do begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            if StartedTrans then
              seTransactionRollback(DB);
          end;
        end;{try..except}
      finally
        DB.Deactivate;
      end;
  end;
end;
{--------}
function TffServerEngine.TableBuild(aDatabaseID : TffDatabaseID;
                                    aOverWrite  : boolean;
                              const aTableName  : TffTableName;
                                    aForServer  : boolean;
                                    aDictionary : TffDataDictionary) : TffResult;
var
  DB : TffSrDatabase;
begin
  if IsReadOnly then begin                                             {!!.01 - Start}
    Result := DBIERR_READONLYDB;
    Exit;
  end;                                                                 {!!.01 - End}
  try
    {the database ID must exist}
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result <> DBIERR_NONE) then
      exit;

    try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabInsert, ffeaTabInsertFail);

      if Result = DBIERR_NONE then begin
        {the database must be open in readwrite mode}
        if (DB.OpenMode = omReadOnly) then begin
          Result := DBIERR_READONLYDB;
          Exit;
        end;
        Result := seTableBuildPrim(DB, aOverwrite, aTableName, aForServer,
                                   aDictionary);
        if Result <> DBIERR_NONE then
          DB.NotifyExtenders(ffeaTabInsertFail, ffeaNoAction);
      end;
    finally
      DB.Deactivate;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.seTableBuildPrim(aDB        : TffSrDatabase;
                                          aOverwrite : boolean;
                                    const aTableName : TffTableName;
                                          aForServer : boolean;
                                          aDict      : TffDataDictionary) : TffResult;
var
  Table         : TffSrBaseTable;
  TableDataFile : TffFileNameExt;
  TransID       : TffTransID;
begin
  { Obtain write access to the table list.  Our purpose is to make sure
    the table is not opened. We have to obtain write access, instead of read
    access, just in case we need to call TableList.RemoveIfUnused. }
  TableList.BeginWrite;
  try
    { Is the table open? }
    Table := GetTableInstance(aDB.Folder, aTableName);
    if (Table <> nil) then begin
      { Yes. See if it can be closed. }
      TableList.RemoveIfUnused(Table);
      if GetTableInstance(aDB.Folder, aTableName) <> nil then begin
        Result := DBIERR_TABLEOPEN;
        Exit;
      end;
    end;
    {the table name must be a valid file name without extension}
    if not FFVerifyFileName(aTableName) then begin
      Result := DBIERR_INVALIDTABLENAME;
      Exit;
    end;
    {the table's data file connot exist within the database}
    TableDataFile := FFMakeFileNameExt(aTableName, ffc_ExtForData);
    if FFFileExists(FFMakeFullFileName(aDB.Folder.Path, TableDataFile)) then begin
      if aOverWrite then begin
        {we want to overwrite this table - we have to delete it first}
        {table exists, is not open - we can delete the table and all files}
        seDeleteTable(aDB, aTableName);
      end
      else begin
        {table exists, and we're not going to overwrite it}
        Result := DBIERR_TABLEEXISTS;
        Exit;
      end;
    end;

    { Create the table. }
    Table := TffSrTable.Create(Self, aTableName, aDB.Folder, seBufMgr,
                               omReadWrite);

    try
      { Start a transaction. Note that if one is already active for this
        database object, this will be a nested transaction. }
      Result := seTransactionStart(aDB, false, ffcl_TrImplicit, TransID);
      if Result <> DBIERR_NONE then
        Exit;

      try
        { Create files making up the table. }
        Table.BuildFiles(aDB.TransactionInfo, aForServer, aDict, [], nil);
        { Commit the transaction. }
        seTransactionCommit(aDB);

        { If we are in a nested transaction then the table will not have
          been written out to disk. Make sure the changes are written to
          disk. }
        if aDB.Transaction <> nil then
          Table.CommitChanges(aDB.TransactionInfo);
      except
        on E:Exception do begin
          seTransactionRollback(aDB);
          raise;
        end;
      end;{try..except}
    finally
      { Destroy the table object. This will close all the files. }
      Table.Free;
    end;{try..finally}
  finally
    TableList.EndWrite;
  end;
end;
{--------}
function TffServerEngine.seTableDeletePrim(DB : TffSrDatabase;
                                     const aTableName : TffTableName) : TffResult;
var
  Table : TffSrBaseTable;
begin

  Result := DBIERR_NONE;

  { If no tablename specified then exit otherwise a lower level routine
    (FFFindClose) will go into an infinite loop. }
  if aTableName = '' then begin
    Result := DBIERR_INVALIDTABLENAME;
    exit;
  end;

  { Obtain write access to the table list.  This is our way of making sure
    nobody opens the table in between our determining the table is NOT open
    and deleting the table. }
  TableList.BeginWrite;
  try
    { Is the table open? }
    Table := GetTableInstance(DB.Folder, aTableName);
    if (Table <> nil) then begin
      { Yes. Can it be closed? }
      TableList.RemoveIfUnused(Table);
      if GetTableInstance(DB.Folder, aTableName) <> nil then begin
        { No. Return an error. }
        Result := DBIERR_TABLEOPEN;
        Exit;
      end;
    end;
    seDeleteTable(DB, aTableName)
  finally
    TableList.EndWrite;
  end;
end;
{--------}
function TffServerEngine.TableDelete(aDatabaseID : TffDatabaseID;
                               const aTableName  : TffTableName) : TffResult;
var
  DB : TffSrDatabase;
begin
  if IsReadOnly then begin                                             {!!.01 - Start}
    Result := DBIERR_TABLEREADONLY;
    Exit;
  end;                                                                 {!!.01 - End}
  try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if Result = DBIERR_NONE then
      try
        FFSetRetry(DB.Timeout);
        Result := DB.NotifyExtenders(ffeaBeforeTabDelete, ffeaTabDeleteFail);
        if (Result = DBIERR_NONE) then begin
          Result := seTableDeletePrim(DB, aTableName);
          if Result <> DBIERR_NONE then
            DB.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
        end;
      finally
        DB.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.TableDropIndex(aDatabaseID : TffDatabaseID;
                                        aCursorID   : TffCursorID;
                                  const aTableName  : TffTableName;
                                  const aIndexName  : TffDictItemName;
                                        aIndexID    : Longint) : TffResult;
{Restructured !!.10}
var
  aTable : TffSrBaseTable;                                             {!!.02}
  Cursor : TffSrBaseCursor;
  DB : TffSrDatabase;
  StartedTrans : boolean;
  TransID : TffTransID;
begin
  { Assumption: Table has been opened for Exclusive use.  This is verified
                in Cursor.DropIndexFromTable. }
  {choice of two here: if the cursor ID is set use that, otherwise
   use the databaseID/tablename}
  if (aCursorID <> 0) then begin
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        StartedTrans := false;
        try
          FFSetRetry(Cursor.Timeout);
          Result := Cursor.NotifyExtenders(ffeaBeforeTabDelete, ffeaTabDeleteFail);
          if Result = DBIERR_NONE then begin
            Result := seTransactionStart(Cursor.Database, false,
                                         ffcl_TrImplicit, TransID);
            if (Result = DBIERR_NONE) then begin
              StartedTrans := true;
              Result := Cursor.DropIndexFromTable(aIndexName, aIndexID);
              if (Result = DBIERR_NONE) then begin
                seTransactionCommit(Cursor.Database);
              end else begin
                Cursor.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
                seTransactionRollback(Cursor.Database);
              end;  { if }
            end;  { if }
          end;  { if }
        except
          on E : Exception do begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            if StartedTrans then
              seTransactionRollback(Cursor.Database);
          end;
        end;{try..except}
      finally
        Cursor.Deactivate;
      end;
  end
  else {use databaseID/tablename} begin
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if Result = DBIERR_NONE then
      try
        StartedTrans := false;
        try
          FFSetRetry(DB.Timeout);
          Result := TableOpen(aDatabaseID, aTableName,
                              false, '', 0, omReadWrite, smExclusive, DB.Timeout,
                              aCursorID, nil);
          if (Result = DBIERR_NONE) then
            try
              Result := seCheckCursorIDAndGet(aCursorID, Cursor);
              if (Result = DBIERR_NONE) then begin
                Result := Cursor.NotifyExtenders(ffeaBeforeTabDelete, ffeaTabDeleteFail);
                if Result = DBIERR_NONE then begin
                  Result := seTransactionStart(DB, false, ffcl_TrImplicit,
                                               TransID);
                  if (Result = DBIERR_NONE) then begin
                    StartedTrans := true;
{Begin !!.02}
                    try
                      Result := Cursor.DropIndexFromTable(aIndexName, aIndexID);
                      if (Result = DBIERR_NONE) then
                        seTransactionCommit(Cursor.Database)
                      else begin
                        Cursor.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
                        seTransactionRollback(Cursor.Database)
                      end;
                    except
                      Cursor.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
                      seTransactionRollback(Cursor.Database);
                      StartedTrans := False;
                      raise;
                    end;
{End !!.02}
                  end;
                end;
              end;  { if }
            finally
              aTable := Cursor.Table;                                  {!!.02}
              CursorClose(aCursorID);
              TableList.RemoveIfUnused(aTable);                        {!!.02}
            end;
        except
          on E : Exception do begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            if StartedTrans then
              seTransactionRollback(DB);
          end;
        end;{try..except}
    finally
      DB.Deactivate;
    end;
  end;
end;
{--------}
function TffServerEngine.TableEmpty(aDatabaseID : TffDatabaseID;
                                    aCursorID   : TffCursorID;
                              const aTableName  : TffTableName) : TffResult;
{Restructured !!.10}
var
  Cursor  : TffSrBaseCursor;
  DB      : TffSrDatabase;
  Dict    : TffDataDictionary;
  Trans   : TffSrTransaction;
  TransID : TffTransID;
begin
  if IsReadOnly then begin                                             {!!.01 - Start}
    Result := DBIERR_TABLEREADONLY;
    Exit;
  end;                                                                 {!!.01 - End}

  { Choice of two here: if the cursor ID is set use that, otherwise
    use the databaseID/tablename. }
  if (aCursorID <> 0) then begin
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if Result = DBIERR_NONE then
      try
        Trans := nil;
        try
          FFSetRetry(Cursor.Timeout);
          DB := Cursor.Database;
          Result := Cursor.NotifyExtenders(ffeaBeforeTabDelete, ffeaTabDeleteFail);

          { Verify the cursor is writable & start an implicit transaction if
            necessary. }
          if (Result = DBIERR_NONE) then begin
            Result := Cursor.EnsureWritable(False, False);             {!!.02}
            if Result = DBIERR_NOACTIVETRAN then
              Result := seTransactionStart(Cursor.Database, false,
                                           ffcl_TrImplicit, TransID);
            Trans := Cursor.Database.Transaction;
          end;
          if (Result = DBIERR_NONE) then begin
            Result := Cursor.Empty;
            { If this was an implicit transaction then commit/rollback. }
            if (Result = DBIERR_NONE) and Trans.IsImplicit then
              seTransactionCommit(DB)
            else begin
              Cursor.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
              seTransactionRollback(DB);
            end;  { if }
          end;  { if }
        except
          on E : Exception do begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            if assigned(Trans) and Trans.IsImplicit then
              seTransactionRollback(Cursor.Database);
          end;
        end;{try..except}
      finally
        Cursor.Deactivate;
      end;
  end
  else {use databaseID/tablename} begin
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if Result = DBIERR_NONE then
      try
        Trans := nil;
        try
          FFSetRetry(DB.Timeout);
          Result := DB.NotifyExtenders(ffeaBeforeTabDelete, ffeaTabDeleteFail);
          if Result = DBIERR_NONE then begin
            Dict := TffDataDictionary.Create(4096);
            try
              Result := seGetDictionary(DB, aTableName, Dict);
              if (Result = DBIERR_NONE) then
                Result := seTableBuildPrim(DB, true, aTableName, false, Dict);
              if Result <> DBIERR_NONE then
                DB.NotifyExtenders(ffeaTabDeleteFail, ffeaNoAction);
            finally
              Dict.Free;
            end;
          end;
        except
          on E : Exception do begin
            Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
            if assigned(Trans) and Trans.IsImplicit then
              seTransactionRollback(DB);
          end;
        end;{try..except}
      finally
        DB.Deactivate;
      end;
  end;
end;
{--------}
function TffServerEngine.TableGetAutoInc(aCursorID : TffCursorID;
                                     var aValue    : TffWord32) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        if (Result = DBIERR_NONE) then
          Cursor.ReadAutoIncValue(aValue);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.TableGetDictionary(aDatabaseID : TffDatabaseID;
                                      const aTableName  : TffTableName;
                                            aForServer  : boolean;
                                            aStream     : TStream) : TffResult;
var
  DB       : TffSrDatabase;
  Dict     : TffDataDictionary;
begin
  try
    {the database ID must exist}
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result <> DBIERR_NONE) then
      exit;

    try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      if Result = DBIERR_NONE then begin
        { We must obtain write access on the engine's table list.  Why?
          Because another thread may be looking for the table at the same
          time.  If the table has not been opened, we don't want that thread
          to open the table while we are opening the table. }
        Dict := TffServerDataDict.Create(4096);
        TableList.BeginWrite;
        try
          Result := seGetDictionary(DB, aTableName, Dict);
          if Result = DBIERR_NONE then
            Dict.WriteToStream(aStream);
        finally
          TableList.EndWrite;
          Dict.Free;
        end;
      end;
    finally
      DB.Deactivate;
    end;
  except
    on E : Exception do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  end;{try..except}
end;
{--------}
function TffServerEngine.TableGetRecCount(aCursorID : TffCursorID;
                                      var aRecCount : Longint) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        if Result = DBIERR_NONE then
          Result := Cursor.GetRecordCount(aRecCount);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.TableIsLocked(aCursorID : TffCursorID;
                                       aLockType : TffLockType;
                                   var aIsLocked : boolean) : TffResult;
var
  Cursor : TffSrBaseCursor;
  ServerLockType : TffSrLockType;
begin
  Result := DBIERR_NONE;
  aIsLocked := false;
  if (aLockType = ffltNoLock) then
    Exit;

  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
        if Result = DBIERR_NONE then begin
          ServerLockType := FFMapLock(aLockType, true);
          aIsLocked := Cursor.Table.HasLock(Cursor.CursorID, ServerLockType);
        end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.TableLockAcquire(aCursorID : TffCursorID;
                                          aLockType : TffLockType) : TffResult;
var
  Cursor      : TffSrBaseCursor;
  ServerLockType : TffSrLockType;
begin
  Result := DBIERR_NONE;
  if (aLockType = ffltNoLock) then
    Exit;

  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Result := Cursor.NotifyExtenders(ffeaBeforeTableLock, ffeaTableLockFail);
        if Result = DBIERR_NONE then
          try
            ServerLockType := FFMapLock(aLockType, True);
            Cursor.Table.AcqClientLock(aCursorID, ServerLockType, False);
            Cursor.NotifyExtenders(ffeaAfterTableLock, ffeaNoAction);
          except
            Cursor.NotifyExtenders(ffeaTableLockFail, ffeaNoAction);
            raise;
          end;
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.TableLockRelease(aCursorID : TffCursorID; aAllLocks : Boolean) : TffResult;
var
  Cursor : TffSrBaseCursor;
begin
  try
    Result := CheckCursorIDAndGet(aCursorID, Cursor);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(Cursor.Timeout);
        Cursor.Table.RelClientLock(aCursorID, aAllLocks);
      finally
        Cursor.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.TableOpen(const aDatabaseID : TffDatabaseID;
                                   const aTableName  : TffTableName;
                                   const aForServer  : Boolean;
                                   const aIndexName  : TffName;
                                         aIndexID    : Longint;
                                   const aOpenMode   : TffOpenMode;
                                         aShareMode  : TffShareMode;
                                   const aTimeout    : Longint;
                                     var aCursorID   : TffCursorID;
                                         aStream     : TStream)
                                                     : TffResult;
var
  Cursor   : TffSrBaseCursor;                                          {!!.06}
  DB       : TffSrDatabase;
  IndexID  : Longint;
  OpenMode : TffOpenMode;
begin
  try
    { The database must exist. }
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result <> DBIERR_NONE) then
      exit;

    try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);

      { Change the open mode to ReadOnly if the Server is ReadOnly. }
      if Result = DBIERR_NONE then begin
        if seConfig.GeneralInfo^.giReadOnly then
          OpenMode := omReadOnly
        else
          OpenMode := aOpenMode;

        { The database and table open and share modes must 'match'. }
        if (DB.OpenMode = omReadOnly) and (OpenMode <> omReadOnly) then begin
          Result := DBIERR_READONLYDB;
          Exit;
        end;
        if (DB.ShareMode = smExclusive) then
          aShareMode := smExclusive;

        { Create a cursor for the table and return it, add it to the
          server's cursor list. }
        Cursor := CursorClass.Create(Self, DB, aTimeout);              {!!.06}
        try
          Cursor.Open(aTableName, aIndexName, aIndexID, OpenMode, aShareMode,
                      aForServer, False, []);

          CursorList.BeginWrite;
          try
            CursorList.AddCursor(Cursor);
          finally
            CursorList.EndWrite;
          end;

          { Get the cursor ID. }
          aCursorID := Cursor.CursorID;

          { Write the information out to the stream - caller's responsibility to
            create and destroy the stream - also to rewind it. }
          if (aStream <> nil) then begin
            { First, the cursor ID. }
            aStream.Write(aCursorID, sizeof(aCursorID));

            { Next, the data dictionary. }
            Cursor.Dictionary.WriteToStream(aStream);

            { Finally the IndexID for the cursor. }
            IndexID := Cursor.IndexID;
            aStream.Write(IndexID, sizeof(IndexID));
          end;
        except
          Cursor.Free;
          raise;
        end;
      end; { if }
    finally
      DB.Deactivate;
    end;
  except
    on E : Exception do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  end;{try..except}
end;
{--------}
function TffServerEngine.seTableRenamePrim(DB : TffSrDatabase;
                                     const aOldName, aNewName : TffName) : TffResult;

var
  Dict : TffDataDictionary;
  Table : TffSrBaseTable;
begin
  Dict := TffDataDictionary.Create(4096);
  TableList.BeginWrite;
  try
    { Is the table open? }
    Table := GetTableInstance(DB.Folder, aOldName);
    if (Table <> nil) then begin
      { Yes. Can it be closed? }
      TableList.RemoveIfUnused(Table);
      if GetTableInstance(DB.Folder, aOldName) <> nil then begin
        { No. Return an error. }
        Result := DBIERR_TABLEOPEN;
        Exit;
      end;
    end;
    Result := seGetDictionary(DB, aOldName, Dict);
    { Retrieved the dictionary? }
    if Result = DBIERR_NONE then begin
      { Yes. Delete the files specified by the dictionary. }
      FFTblHlpRename(DB.Folder.Path, aOldName, aNewName, Dict);
      Result := DBIERR_NONE;
    end
  finally
    TableList.EndWrite;
    Dict.Free;
  end;
end;
{--------}
function TffServerEngine.TableRename(aDatabaseID        : TffDatabaseID;
                               const aOldName, aNewName : TffName) : TffResult;
var
  DB  : TffSrDatabase;
begin
  try
    { The table name must be a valid file name without extension. }
    if not FFVerifyFileName(aNewName) then begin
      Result := DBIERR_INVALIDTABLENAME;
      Exit;
    end;

    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(DB.Timeout);
        Result := DB.NotifyExtenders(ffeaBeforeTabUpdate, ffeaTabUpdateFail);
        if Result = DBIERR_NONE then begin
          Result := seTableRenamePrim(DB, aOldName, aNewName);
          if Result <> DBIERR_NONE then
            DB.NotifyExtenders(ffeaTabUpdateFail, ffeaNoAction);
        end;
      finally
        DB.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.TableSetAutoInc(aCursorID   : TffCursorID;
                                         aValue      : TffWord32) : TffResult;
{Restructured !!.10}
var
  Cursor : TffSrBaseCursor;
  StartedTrans: Boolean;
  TransID: TffTransID;
begin
  Result := CheckCursorIDAndGet(aCursorID, Cursor);
  if (Result = DBIERR_NONE) then
    try
      StartedTrans := false;
      try
        FFSetRetry(Cursor.Timeout);
        StartedTrans := False;
        Result := Cursor.NotifyExtenders(ffeaBeforeTabUpdate, ffeaTabUpdateFail);
        if Result = DBIERR_NONE then begin
          Result := Cursor.EnsureWritable(False, False);               {!!.02}
          if (Result = DBIERR_NOACTIVETRAN) or
             Cursor.NeedNestedTransaction then begin                   {!!.03}
            Result := seTransactionStart(Cursor.Database, false,
                                         ffcl_TrImplicit, TransID);
            StartedTrans := (Result = DBIERR_NONE);
          end;

          if (Result = DBIERR_NONE) then begin
            try
              Cursor.SetAutoIncValue(aValue);
            except
              Cursor.NotifyExtenders(ffeaTabUpdateFail, ffeaNoAction);
              raise;
            end;
            if StartedTrans then
              Result := seTransactionCommit(Cursor.Database);
          end;
        end;  { if }
      except
        on E : Exception do begin
          Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
          if StartedTrans then
            seTransactionRollback(Cursor.Database);
        end;
      end;{try..except}
    finally
      Cursor.Deactivate;
    end;
end;
{--------}
{Begin !!.11}
function TffServerEngine.TableVersion(aDatabaseID : TffDatabaseID;
                                const aTableName  : TffTableName;
                                  var aVersion : Longint) : TffResult;
var
  DB : TffSrDatabase;
  FI : TffFileInfo;
  FileHandle : THandle;
  Table : TffSrBaseTable;
  TableDataFile : TffFullFileName;
  PTableDataFile : PAnsiChar;
  Header : TffBlockHeaderFile;
begin
  PTableDataFile := nil;
  try
    {the database ID must exist}
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result <> DBIERR_NONE) then
      exit;

    try
      FFSetRetry(DB.Timeout);
      Result := DB.NotifyExtenders(ffeaBeforeTabRead, ffeaNoAction);
      if Result = DBIERR_NONE then begin
        { If the table is already open then return the version number from the
          internal file data structure. Otherwise, open the main file for the
          table & retrieve the version number from its header block. }
        seTableList.BeginWrite;
        try
          { Try & find the open table in the engine's table list. If it exists already
            then reference the existing table. }
          Table := GetTableInstance(DB.Folder, aTableName);

          { Is the table open? }
          if assigned(Table) then
            { Yes. Return version # from in-memory information. }
            aVersion := Table.Files[0].fiFFVersion
          else if seTableExistsPrim(DB, aTableName) then begin
            { Table exists. Open the file directly & retrieve the version number
              from its header block. }
            TableDataFile := FFMakeFullFileName
                               (DB.Folder.Path,
                                FFMakeFileNameExt(aTableName, ffc_ExtForData));
            FFGetMem(PTableDataFile, Length(TableDataFile) + 1);
            StrPCopy(PTableDataFile, TableDataFile);
            FileHandle := FFOpenFilePrim(PTableDataFile, omReadOnly,
                                         smShareRead, False, False);
            try
              FI.fiHandle := FileHandle;
              FI.fiName := FFShStrAlloc(TableDataFile);
              FFReadFilePrim(@FI, SizeOf(TffBlockHeaderFile), Header);
              aVersion := Header.bhfFFVersion;
            finally
              FFCloseFilePrim(@FI);
              FFShStrFree(FI.fiName);
            end;
          end
          else
            { The file does not exist. Raise an error. }
            FFRaiseException(EffException, ffstrResServer, fferrUnknownTable,
                             [aTableName, DB.Alias]);
        finally
          if PTableDataFile <> nil then
            FFFreeMem(PTableDataFile, StrLen(PTableDataFile) + 1);
          seTableList.EndWrite;
        end;
      end;  { if }

    finally
      DB.Deactivate;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{End !!.11}
{--------}
function TffServerEngine.seConvertSingleField(aSourceBuf,
                                              aTargetBuf: PffByteArray;
                                              aSourceCursorID,
                                              aTargetCursorID: Longint;
                                              aSourceFldNr,
                                              aTargetFldNr: Integer;
                                              aBLOBBuffer: Pointer;
                                              aBLOBBufLen: Longint): TffResult;
var
  SourceValue: Pointer;
  TargetValue: Pointer;
  SourceType: TffFieldType;
  TargetType: TffFieldType;
  SourceLength: Longint;
  TargetLength: Longint;
  SourceCursor,
  TargetCursor: TffSrBaseCursor;
begin
  Result := DBIERR_NONE;
  try
    seCheckCursorIDAndGet(aSourceCursorID, SourceCursor);
    seCheckCursorIDAndGet(aTargetCursorID, TargetCursor);

    SourceValue := nil;
    TargetValue := nil;

    with SourceCursor.Table.Dictionary do begin
      if Assigned(aSourceBuf) then begin

        { If input field is a null, then output is automatically a null
          regardless of datatype. }
        if IsRecordFieldNull(aSourceFldNr, aSourceBuf) then begin
          TargetCursor.Table.Dictionary.SetRecordField(aTargetFldNr, aTargetBuf, nil);
          Exit;
        end;

        {Begin !!.10}
        { also count input field as null if it's a stringtype, the field
          conains the empty string, and output field is a blob. }
        if (TargetCursor.Table.Dictionary.FieldType[aTargetFldNr] in [fftBLOB..ffcLastBlobType]) and
         (((FieldType[aSourceFldNr] in [fftNullString, fftNullAnsiStr]) and
           (Byte(aSourceBuf^[FieldOffset[aSourceFldNr]])=0)) or
          ((FieldType[aSourceFldNr] in [fftShortString, fftShortAnsiStr]) and
           (Byte(aSourceBuf^[FieldOffset[aSourceFldNr]+1])=0)) or
          ((FieldType[aSourceFldNr] in [fftWideString]) and
           (WideChar(aSourceBuf^[FieldOffset[aSourceFldNr]])=''))) then begin
          TargetCursor.Table.Dictionary.SetRecordField(aTargetFldNr, aTargetBuf, nil);
          Exit;
        end;
        {End !!.10}

        SourceValue := Addr(aSourceBuf^[FieldOffset[aSourceFldNr]]);
      end;
      SourceType := FieldType[aSourceFldNr];
      SourceLength := FieldLength[aSourceFldNr];
    end;

    with TargetCursor.Table.Dictionary do begin
      if Assigned(aTargetBuf) then
        TargetValue := Addr(aTargetBuf^[FieldOffset[aTargetFldNr]]);

      TargetType := FieldType[aTargetFldNr];
      TargetLength := FieldLength[aTargetFldNr];
    end;

    Result := FFConvertSingleField(SourceValue, TargetValue,
                                   SourceType, TargetType,
                                   SourceLength, TargetLength);

    if Assigned(aTargetBuf) and (Result = DBIERR_NONE) then begin

      { Field is not null }
      with TargetCursor.Table.Dictionary do
        FFClearBit(@aTargetBuf^[LogicalRecordLength], aTargetFldNr);

      { Handle BLOB targets }
      if TargetType in [fftBLOB..ffcLastBLOBType] then begin
        Result := BLOBCreate(TargetCursor.CursorID, TffInt64(TargetValue^));
        if Result = DBIERR_NONE then
          if SourceType in [fftBLOB..ffcLastBLOBType] then
            Result := seBLOBCopy(SourceCursor,
                                 TargetCursor,
                                 TffInt64(SourceValue^),
                                 TffInt64(TargetValue^),
                                 aBLOBBuffer,
                                 aBLOBBufLen)
          else
          {Begin !!.10}
          if SourceType in [fftShortString, fftShortAnsiStr] then begin
            { skip lengthbyte }
            SourceValue := Pointer(Succ(Integer(SourceValue)));
            Result := TargetCursor.BLOBWrite(TffInt64(TargetValue^),
                                             0,
                                             SourceLength-1,
                                             SourceValue^);
          end
          else
//          if SourceType in [fftShortString, fftShortAnsiStr] begin
          {End !!.10}
            Result := TargetCursor.BLOBWrite(TffInt64(TargetValue^),
                                             0,
                                             SourceLength,
                                             SourceValue^);
      end;
    end;
  except
    {Begin !!.13}
    on E : EOverFlow do
      Result := DBIERR_INVALIDFLDXFORM;
    {$IFOPT R+}
    on E : ERangeError do
      Result := DBIERR_INVALIDFLDXFORM;
    {$ENDIF}
    {End !!.13}
    on E : Exception do begin
      if Result = DBIERR_NONE then
        Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;
end;
{--------}
{ Include code for asynchronous requests. } 
{$i ffsrridx.inc}
{$i ffsrpack.inc}
{$i ffsrrest.inc}
{$i ffsrrcnt.inc}                                                      {!!.10}
{--------}
function TffServerEngine.TransactionCommit(const aDatabaseID : TffDatabaseID) : TffResult;
var
  DB : TffSrDatabase;
begin
  try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(DB.Timeout);
        if DB.Transaction = nil then
          Result := DBIERR_NOACTIVETRAN
        else if DB.Transaction.IsCorrupt then begin
            DB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
            seTransactionRollback(DB);
            Result := DBIERR_FF_CorruptTrans;
            DB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
          end
        else begin
          Result := DB.NotifyExtenders(ffeaBeforeCommit, ffeaCommitFail);{!!.06}
          if Result = DBIERR_NONE then begin                          {!!.06}
            seTransactionCommit(DB);                                  {!!.06}
            DB.NotifyExtenders(ffeaAfterCommit, ffeaNoAction);        {!!.06}
          end;                                                        {!!.06}
        end;
      finally
        DB.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.TransactionCommitSubset(const aDatabaseID : TffDatabaseID) : TffResult;
var
  DB : TffSrDatabase;
begin
  try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result = DBIERR_NONE) then
      try
        FFSetRetry(DB.Timeout);
        Result := seTransactionCommitSubset(DB);
      finally
        DB.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{Begin !!.01
{--------}
function TffServerEngine.TransactionCommitSQL(const aDatabaseID : TffDatabaseID;
                                              const notifyExtenders : Boolean) : TffResult;
var
  aDB : TffSrDatabase;
begin
  aDB := TffSrDatabase(aDatabaseID);
  if aDB.Transaction.IsCorrupt then begin
      if notifyExtenders then
        aDB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
      seTransactionRollback(aDB);
      Result := DBIERR_FF_CorruptTrans;
      if notifyExtenders then
        aDB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
    end
  else begin
    if notifyExtenders then
      aDB.NotifyExtenders(ffeaBeforeCommit, ffeaNoAction);
    Result := seTransactionCommit(aDB);
    if notifyExtenders then
      aDB.NotifyExtenders(ffeaAfterCommit, ffeaNoAction);
  end;
end;
{End !!.01}
{--------}
function TffServerEngine.seTransactionCommitSubset(const aDB : TffSrDatabase) : TffResult;
{ Rewritten !!.03}
var
  aContainer : TffTransContainer;
  aInx : Longint;
  aTable : TffSrTable;
  aTableList : TffPointerList;
  Nested : Boolean;
begin
  Result := DBIERR_NONE;
  if aDB.Transaction.IsCorrupt then begin
    aDB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
    seTransactionRollback(aDB);
    Result := DBIERR_FF_CorruptTrans;
    aDB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
  end
  else begin
    aTableList := TffPointerList.Create;
    aContainer := TffTransContainer(aDB.Transaction.TransLockContainer);
    Nested := aDB.Transaction.Nested;

    try
      { Determine which tables were affected by the transaction. We will
        commit the changes to their BLOB mgr's in-memory deleted chain. }
      if assigned(aContainer) and (not Nested) then
        for aInx := 0 to pred(aContainer.ContentCount) do
          if aContainer.ContentLockType[aInx] = ffsltExclusive then begin
            aTable := TffSrTable(aContainer.ContentTable[aInx]);
            aTableList.Append(Pointer(aTable));
          end;

      aDB.NotifyExtenders(ffeaBeforeCommit, ffeaNoAction);
      seBufMgr.CommitTransactionSubset(aDB.Transaction);

      { Nested transaction? }
      if (not Nested) then begin
        { No. Release transaction locks. For each table involved, commit the
          changes to the BLOB resource manager's in-memory deleted chain. }
        aDB.Folder.LockMgr.ReleaseTransactionLocks(aDB.Transaction, True);
        for aInx := 0 to pred(aTableList.Count) do begin
          aTable := TffSrTable(aTableList.List[aInx]);
          aTable.btCommitBLOBMgr;
        end;
      end;

      aDB.NotifyExtenders(ffeaAfterCommit, ffeaNoAction);
    finally
      aTableList.Free;
    end;
  end;
end;
{--------}
function TffServerEngine.TransactionRollback(const aDatabaseID : TffDatabaseID) : TffResult;
var
  DB     : TffSrDatabase;
begin
  try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result = DBIERR_NONE) then
      try

        if not assigned(DB.Transaction) then begin
          Result := DBIERR_NOACTIVETRAN;
          exit;
        end;

        FFSetRetry(DB.Timeout);

        DB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
        seTransactionRollback(DB);
        DB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
      finally
        DB.Deactivate;
      end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{Begin !!.01}
{--------}
function TffServerEngine.TransactionRollbackSQL(const aDatabaseID : TffDatabaseID;
                                                const notifyExtenders : Boolean) : TffResult;
var
  aDB : TffSrDatabase;
begin
  aDB := TffSrDatabase(aDatabaseID);
  if notifyExtenders then
    aDB.NotifyExtenders(ffeaBeforeRollback, ffeaNoAction);
  Result := seTransactionRollback(aDB);
  if notifyExtenders then
    aDB.NotifyExtenders(ffeaAfterRollback, ffeaNoAction);
end;
{End !!.01}
{--------}
function TffServerEngine.bseGetAutoSaveCfg : Boolean;
begin
  Result := seConfig.GeneralInfo^.giNoAutoSaveCfg;
end;
{--------}
function TffServerEngine.bseGetReadOnly : boolean;
begin
  Result := seConfig.GeneralInfo^.giReadOnly;
end;
{--------}
procedure TffServerEngine.bseSetAutoSaveCfg(aValue : Boolean);         {!!.01 - Start}
begin
  seConfig.GeneralInfo^.giNoAutoSaveCfg := aValue;
end;
{--------}
procedure TffServerEngine.bseSetReadOnly(aValue : Boolean);
begin
  seConfig.GeneralInfo^.giReadOnly := aValue;
end;
{--------}                                                             {!!.01 - End}
function TffServerEngine.TransactionStart(const aDatabaseID    : TffDatabaseID;
                                          const aFailSafe      : Boolean) : TffResult;
var
  DB : TffSrDatabase;
  TransID : TffTransID;
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
  if (Result = DBIERR_NONE) then
    try
      FFSetRetry(DB.Timeout);
      Result := seTransactionStart(DB, aFailSafe, ffcl_TrExplicit,
                                   TransID);
      if Result = DBIERR_NONE then
        DB.NotifyExtenders(ffeaAfterStartTrans, ffeaNoAction);
    finally
      DB.Deactivate;
    end;
end;
{Begin !!.01}
{--------}
function TffServerEngine.TransactionStartSQL(const aDatabaseID : TffDatabaseID;
                                             const notifyExtenders : boolean) : TffResult;
var
  aTransID : TffTransID;
begin
  Result := seTransactionStart(TffSrDatabase(aDatabaseID), false, true, aTransID);
{Begin !!.06}
  if (Result = DBIERR_NONE) then begin
    TffSrDatabase(aDatabaseID).Transaction.IsReadOnly := True;
    if notifyExtenders then
      TffSrDatabase(aDatabaseID).NotifyExtenders(ffeaAfterStartTrans, ffeaNoAction);
  end;
{End !!.06}
end;
{End !!.01}
{Begin !!.10}
{--------}
function TffServerEngine.TransactionStartWith(const aDatabaseID : TffDatabaseID;
                                              const aFailSafe : Boolean;
                                              const aCursorIDs : TffPointerList) : TffResult;
var
  RetryUntil : DWORD;
  DB : TffSrDatabase;
  TransID : TffTransID;
  Limit,
  anIndex : Longint;
  aCursorID : TffCursorID;
  Cursor : TffSrBaseCursor;
  Lock : TffPadlock;
  GetCursorResult : TffResult;                                         {!!.13}
begin
  Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
  if (Result = DBIERR_NONE) then
    try
      FFSetRetry(DB.Timeout);
      Result := seTransactionStart(DB, aFailSafe, ffcl_TrExplicit,
                                   TransID);
      if Result = DBIERR_NONE then
        try
          Lock :=  DB.Folder.LockMgr.StartWithLock;
          { Retry this operation until it is successful or we reach the database
            timeout limit. }
          RetryUntil := FFGetRetry;
          repeat
            if Result <> DBIERR_NONE then
              Sleep(ffc_StartTranWithDelay);
            Limit := 0;
            Lock.Lock;
            try
              for anIndex := 0 to pred(aCursorIDs.Count) do begin
                aCursorID := TffCursorID(aCursorIDs[anIndex]);
                Result := CheckCursorIDAndGet(aCursorID, Cursor);
                if Result = DBIERR_NONE then
                  try
                    Result := Cursor.AcqExclContentLock;
                    if Result <> DBIERR_NONE then begin
                      Limit := pred(anIndex);
                      Break;
                    end;
                  finally
                    Cursor.Deactivate;
                  end
                else
                  Break;
              end;  { for }
              if Result <> DBIERR_NONE then
                for anIndex := 0 to Limit do begin
                  aCursorID := TffCursorID(aCursorIDs[anIndex]);
                  GetCursorResult := CheckCursorIDAndGet(aCursorID, Cursor); {!!.13}
                  if GetCursorResult = DBIERR_NONE then begin                {!!.13}
                    Cursor.RelContentLock(ffclmWrite);
                  end;
                end;  { for }
            finally
              Lock.Unlock;
            end;

          until (Result = DBIERR_NONE) or
                (RetryUntil <= (GetTickCount - 10));

          if Result = DBIERR_NONE then
            DB.NotifyExtenders(ffeaAfterStartTrans, ffeaNoAction)
          else begin
            seTransactionRollback(DB);
            if Result = fferrLockRejected then
              Result := DBIERR_LOCKED;
          end;

        except
          on E : Exception do begin
            Result := ConvertServerExceptionEx(E, FEventLog,
                                               bseGetReadOnly);
            seTransactionRollback(DB);
          end;
        end;
    finally
      DB.Deactivate;
    end;
end;
{End !!.10}
{--------}
function TffServerEngine.seTransactionStart(const aDB                  : TffSrDatabase;
                                            const aFailSafe, aImplicit : boolean;
                                              var aTransactionID       : TffTransID) : TffResult;
var
  aTrans : TffSrTransaction;
begin
  try
    Result := aDB.Folder.TransactionMgr.StartTransaction
                    (aDB.DatabaseID, aFailSafe, aImplicit,
                     false, aDB.Folder.Path, aTrans);
    aDB.Transaction := aTrans;
    aTransactionID := aTrans.TransactionID;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{===Script processing================================================}
function TffServerEngine.CalcPriorityIndex(const PriorityStr : TffShStr) : integer;
const
  PriorityValues : array [0..6] of string[12] = (
              'LOWEST',
              'BELOW NORMAL',
              'NORMAL',
              'ABOVE NORMAL',
              'HIGHEST',
              'BELOWNORMAL',
              'ABOVENORMAL');
var
  Inx : integer;
begin
  for Inx := low(PriorityValues) to high(PriorityValues) do
    if (PriorityStr = PriorityValues[Inx]) then begin
      Result := Inx - 2;
      if Result = 3 then
        Result := -1
      else if Result = 4 then
        Result := 1;
      Exit;
    end;
  Result := 0;
end;
{--------}
function TffServerEngine.CalcKeyIndex(const KeyStr : TffShStr) : integer;
const
  KeyValues : array [0..21] of string[13] = (
              'SERVERNAME',
              'MAXRAM',
              'USESINGLEUSER',
              'USEIPXSPX',
              'USETCPIP',
              'USELOGIN',
              'AUTOUPSERVER',
              'AUTOMINIMIZE',
              'IPXSPXLFB',
              'TCPIPLFB',
              'ALLOWENCRYPT',
              'READONLY',
              'LASTMSGINTVAL',
              'ALIVEINTERVAL',
              'ALIVERETRIES',
              'PRIORITY',
              'DELETESCRIPT',
              'TCPINTERFACE',
              'NOAUTOSAVECFG',
              'TEMPSTORESIZE',
              'COLLECTENABLD',
              'COLLECTFREQ');
var
  Inx : integer;
begin
  for Inx := low(KeyValues) to high(KeyValues) do
    if (KeyStr = KeyValues[Inx]) then begin
      Result := Inx;
      Exit;
    end;
  Result := -1;
end;
{--------}
procedure TffServerEngine.GetServerNames(aList: TStrings;
                                         aTimeout : Longint);
begin
  aList.Clear;
  aList.Add('Direct');
end;
{--------}
function TffServerEngine.seDatabaseGetAliasPathPrim
  (aAlias : TffName; var aPath :TffPath) : TffResult;
var
  aList    : TList;
  Count    : Integer;
  AliasDes : PffAliasDescriptor;
begin
  { Assumption: Thread-safeness enforced at a higher level. }

  { Retrieve the alias list, and return the path for the matching entry }
  aPath := '';
  aList := TList.Create;
  try
    Result := seDatabaseAliasListPrim(aList);
    if Result = DBIERR_NONE then
      for Count := 0 to Pred(aList.Count) do begin
        AliasDes := PffAliasDescriptor(aList.Items[Count]);
        if FFAnsiCompareText(AliasDes^.adAlias, aAlias) = 0 then begin  {!!.03, !!.10}
          aPath := AliasDes^.adPath;
          Break;
        end;
      end;
  finally
    aList.Free;
  end;
end;
{--------}
function TffServerEngine.DatabaseGetAliasPath(aAlias    : TffName;
                                          var aPath     : TffPath;
                                              aClientID : TFFClientID)
                                                        : TffResult;
var
  Client : TffSrClient;
begin
  try
    Result := CheckClientIDandGet(aClientID, Client);
    if Result = DBIERR_NONE then begin
      FFSetRetry(Client.Timeout);
      try
        seConfig.AliasList.BeginRead;
        try
          Result := Client.NotifyExtenders(ffeaBeforeDBRead, ffeaNoAction);
          if Result = DBIERR_NONE then
            Result := seDatabaseGetAliasPathPrim(aAlias, aPath);
        finally
          seConfig.AliasList.EndRead;
        end;
      finally
        Client.Deactivate;
      end;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.DatabaseGetFreeSpace(const aDatabaseID : TffDatabaseID;
                                                var aFreeSpace  : Longint)
                                                                : TffResult;
{!!.11 - Rewritten}
var
  DB : TffSrDatabase;
begin
  try
    Result := CheckDatabaseIDAndGet(aDatabaseID, DB);
    if (Result = DBIERR_NONE) then begin
      try
        aFreeSpace := FFGetDiskFreeSpace(DB.dbFolder.Path);
      finally
        DB.Deactivate;
      end;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;
end;
{--------}
function TffServerEngine.DatabaseModifyAlias(const aClientID   : TffClientID;
                                             const aAlias      : TffName;
                                             const aNewName    : TffName;
                                             const aNewPath    : TffPath;
                                                   aCheckSpace : Boolean)  {!!.11}
                                                               : TffResult;
var
  Client : TffSrClient;
  Name   : TffName;
  Path   : TffPath;
begin
  try
    Result := CheckClientIDandGet(aClientID, Client);
    if Result = DBIERR_NONE then begin
      FFSetRetry(Client.Timeout);
      try
        seConfig.AliasList.BeginWrite;
        try
          Result := Client.NotifyExtenders(ffeaBeforeDBDelete,
                                           ffeaDBDeleteFail);
          if Result = DBIERR_NONE then begin
            Name := aAlias;
            Result := seDatabaseGetAliasPathPrim(aAlias, Path);
            if Result = DBIERR_NONE then begin

              { Does the alias have a new name? }
              if aNewName <> '' then
                Name := aNewName;

              { Does the alias have a new path? }
              if aNewPath <> '' then
                Path := aNewPath;

              Result := seDatabaseDeleteAliasPrim(aAlias);

              if (Result = DBIERR_NONE) then begin
                Result := Client.NotifyExtenders(ffeaBeforeDBInsert,
                                                 ffeaDBInsertFail);
                if Result = DBIERR_NONE then begin
                  Result := seDatabaseAddAliasPrim(Name,
                                                   Path,
                                                   aCheckSpace);       {!!.11}
                  if Result = DBIERR_NONE then
                    WriteAliasData
                  else
                    Client.NotifyExtenders(ffeaDBInsertFail,
                                           ffeaNoAction);
                end;
              end else
                Client.NotifyExtenders(ffeaDBDeleteFail, ffeaNoAction);
            end else { if got existing alias path }
              Client.NotifyExtenders(ffeaDBDeleteFail, ffeaNoAction);
          end;  { if no clients complained about rights }
        finally
          seConfig.AliasList.EndWrite;
        end;
      finally
        Client.Deactivate;
      end;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
    end;
  end;{try..except}
end;
{--------}
function TffServerEngine.GetServerDateTime(var aDateTime: TDateTime): TffResult;
begin
  Result := DBIERR_NONE;
  aDateTime := Now;
end;
{--------}                                                     {begin !!.10}
function TffServerEngine.GetServerSystemTime(var aSystemTime : TSystemTime)
                                            : TffResult;
begin
  Result := DBIERR_NONE;
  GetSystemTime(aSystemTime);
end;
{--------}
function TffServerEngine.GetServerGUID(var aGUID : TGUID) : TffResult;
begin
  Result := DBIERR_NONE;
  CoCreateGuid(aGuid);
end;
{--------}
function TffServerEngine.GetServerID(var aUniqueID : TGUID) : TffResult;
begin
  Result := DBIERR_NONE;
  aUniqueID := seUniqueID;
end;
{--------}
function TffServerEngine.GetServerStatistics(var aStats : TffServerStatistics)
                                            : TffResult;
begin
  aStats.ssName := Configuration.ServerName;
  aStats.ssVersion := ffVersionNumber;
  aStats.ssState := FFMapStateToString(State);
  aStats.ssClientCount := ClientList.ClientCount;
  aStats.ssSessionCount := SessionList.SessionCount;
  aStats.ssOpenDatabasesCount := DatabaseList.DatabaseCount;
  aStats.ssOpenTablesCount := TableList.TableCount;
  aStats.ssOpenCursorsCount := CursorList.CursorCount;
  aStats.ssRamUsed := BufferManager.RAMUsed;
  aStats.ssMaxRam := BufferManager.MaxRAM;
  aStats.ssUpTimeSecs := (GetTickCount - seStartTime) div 1000;
  aStats.ssCmdHandlerCount := CmdHandlerCount;
  Result := DBIERR_NONE;
end;
{--------}
function TffServerEngine.GetCommandHandlerStatistics(const aCmdHandlerIdx : Integer;
                                                       var aStats : TffCommandHandlerStatistics)
                                                    : TffResult;
begin
  if (aCmdHandlerIdx < 0) or
     (aCmdHandlerIdx > Pred(CmdHandlerCount)) then
    Result := DBIERR_OBJNOTFOUND
  else begin
    aStats.csTransportCount := CmdHandler[aCmdHandlerIdx].TransportCount;
    Result := DBIERR_NONE;
  end;
end;
{--------}
function TffServerEngine.GetTransportStatistics(const aCmdHandlerIdx : Integer;
                                                const aTransportIdx  : Integer;
                                                  var aStats : TffTransportStatistics)
                                               : TffResult; 
var
  Trans : TffBaseTransport;
begin
  if (aCmdHandlerIdx < 0) or
     (aCmdHandlerIdx > Pred(CmdHandlerCount)) then
    Result := DBIERR_OBJNOTFOUND
  else begin
    if (aTransportIdx < 0) or
       (aTransportIdx > Pred(CmdHandler[aCmdHandlerIdx].TransportCount)) then
      Result := DBIERR_OBJNOTFOUND
    else begin
      Trans := CmdHandler[aCmdHandlerIdx].Transports[aTransportIdx];
      aStats.tsName := Trans.GetName;
      aStats.tsState := FFMapStateToString(Trans.State);
      aStats.tsAddress := Trans.ServerName;
      aStats.tsClientCount := Trans.ConnectionCount;
      aStats.tsMessageCount := Trans.MsgCount;
      aStats.tsMessagesPerSec :=  Trans.MsgCount / ((GetTickCount - seStartTime) div 1000);
      Result := DBIERR_NONE;
    end;
  end;
end;
{--------}                                                       {end !!.10}
function TffServerEngine.ValBoolean(const BoolStr : TffShStr;
                                    var   BoolValue : boolean) : boolean;
var
  UpperBoolStr : TffShStr;
begin
  {only values allowed are 0, 1, YES, NO, TRUE, FALSE}
  UpperBoolStr := FFShStrUpper(BoolStr);
  Result := true;
  BoolValue := false;
  if (UpperBoolStr = '0') or
     (UpperBoolStr = 'NO') or
     (UpperBoolStr = 'FALSE') then
    Exit;
  BoolValue := true;
  if (UpperBoolStr = '1') or
     (UpperBoolStr = 'YES') or
     (UpperBoolStr = 'TRUE') then
    Exit;
  Result := false;
end;
{--------}
procedure TffServerEngine.ProcessScriptCommand(const KeyStr,
                                                     ValueStr     : TffShStr;
                                                 var DeleteScript : Boolean);
var
  KeyInx   : Integer;
  WorkInt  : Longint;
  ec       : Integer;
  WorkBool : Boolean;
  UpperStr : TffShStr;
begin
  DeleteScript := False;
  {uppercase the key}
  UpperStr := FFShStrUpper(KeyStr);
  {is it one of the strings we allow?}
  KeyInx := CalcKeyIndex(UpperStr);
  {if it is, process the command}
  if (KeyInx >= 0) then begin
    case KeyInx of
      0 : {server name}
        begin
          Configuration.GeneralInfo^.giServerName := ValueStr;
        end;
      1 : {Max RAM}
        begin
          Val(ValueStr, WorkInt, ec);
          if (ec = 0) and (WorkInt >= 1) then
            Configuration.GeneralInfo^.giMaxRAM := WorkInt;
        end;
      2 : {Use Single User Protocol}
        begin                                                          {!!.01 - Start}
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giSingleUser := WorkBool;
        end;                                                           {!!.01 - End}
      3 : {Use IPX/SPX Protocol}
        begin                                                          {!!.01 - Start}
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giIPXSPX := WorkBool;
        end;                                                           {!!.01 - End}
      4 : {Use TCP/IP Protocol}
        begin                                                          {!!.01 - Start}
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giTCPIP := WorkBool;
        end;                                                           {!!.01 - End}
      5 : {Login security?}
        begin
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giIsSecure := WorkBool;
        end;
      6 : {Auto Up?}
        begin
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giAutoUp := WorkBool;
        end;
      7 : {Auto Minimize?}
        begin
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giAutoMini := WorkBool;
        end;
      8 : {Enable IPX/SPX use broadcasts?}
        begin
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giIPXSPXLFB := WorkBool;
        end;
      9 : {Enable TCP/IP use broadcasts?}
        begin
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giTCPIPLFB := WorkBool;
        end;

      10 : {Allow encrypted tables to be created?}
        begin
          {$IFDEF SecureServer}
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giAllowEncrypt := WorkBool;
          {$ENDIF}
        end;
      11 : {ReadOnly?}
        begin
          if ValBoolean(ValueStr, WorkBool) then begin
            Configuration.GeneralInfo^.giReadOnly := WorkBool;
            seSetLoggingState;
          end;
        end;
      12 : {Last message interval}
        begin
          Val(ValueStr, WorkInt, ec);
          if (ec = 0) and (WorkInt >= 1000) and (WorkInt <= 86400000) then
            Configuration.GeneralInfo^.giLastMsgInterval := WorkInt;
        end;
     13 : {keep alive interval}
        begin
          Val(ValueStr, WorkInt, ec);
          if (ec = 0) and (WorkInt >= 1000) and (WorkInt <= 86400000) then
            Configuration.GeneralInfo^.giKAInterval := WorkInt;
        end;
     14 : {keep alive retries}
        begin
          Val(ValueStr, WorkInt, ec);
          if (ec = 0) and (WorkInt >= 1) and (WorkInt <= 100) then
            Configuration.GeneralInfo^.giKARetries := WorkInt;
        end;
     15 : {Priority}
        begin
          UpperStr := FFShStrUpper(ValueStr);
          Configuration.GeneralInfo^.giPriority :=
             CalcPriorityIndex(UpperStr);
        end;
     16 : {Delete script}
        begin                                                         {!!.01 - Start}
          if ValBoolean(ValueStr, WorkBool) then
          DeleteScript := WorkBool;
        end;                                                          {!!.01 - End}
     17 : {TCP/IP Interface}
        begin
          Val(ValueStr, WorkInt, ec);
          Configuration.GeneralInfo^.giTCPInterface := WorkInt;
        end;
     18 : {NoAutoSaveCfg}
        begin
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giNoAutoSaveCfg := WorkBool;
        end;
     19 : {giTempStoreSize}
        begin
          Val(ValueStr, WorkInt, ec);
          {Temp storage must be between 1 meg and 2 gigs.}
          if (ec = 0) and (WorkInt > 0) and (WorkInt < 2049) then
            Configuration.GeneralInfo^.giTempStoreSize := WorkInt;
        end;
     20 : {giCollectEnabled}
        begin
          if ValBoolean(ValueStr, WorkBool) then
            Configuration.GeneralInfo^.giCollectEnabled := WorkBool;
        end;
     21 : {giCollectFreq}
        begin
          Val(ValueStr, WorkInt, ec);
          {Garbage collection frequency should be between 30 seconds
           and 60 minutes.}
          if (ec = 0) and (WorkInt > 30000) and (WorkInt < 3600000) then
            Configuration.GeneralInfo^.giCollectFreq := WorkInt;
        end;
    end;{case}
  end
  {if it isn't it must be an alias definition}
  else begin
    if FFDirectoryExists(ValueStr) then
      { Assumption: This routine happens on sever startup therefore we
        do not need to ensure thread-safeness. }
      seDatabaseAddAliasPrim(KeyStr, ValueStr, False);                 {!!.11}
  end;
end;
{--------}
procedure TffServerEngine.ProcessAliasScript;
var
  CurPath       : TffPath;
  ScriptFile    : TffFullFileName;
  ScriptItems   : TStrings;
  Alias         : TffName;
  Path          : TffPath;
  i, iPos, iLen : Integer;
  DeleteScript  : Boolean;
begin
  { Get the application's directory. }
  CurPath := FFExtractPath(FFGetExeName);
  { Create the script filename. }
  ScriptFile := FFMakeFullFileName(CurPath, ffc_AliasScript);

  { Does the alias script file (FFAlias.sc$) exist in the directory? }
  if FFFileExists( ScriptFile ) then begin
    { Yes. Process it. }
    ScriptItems := TStringList.Create;
    try
      ScriptItems.LoadFromFile( ScriptFile );
      { For each item in the file, try to parse it. }
      for i := 0 to pred( ScriptItems.Count ) do begin
        { Only process lines with some length. }
        iLen := Length( ScriptItems[i] );
        if iLen > 2 then begin
          { Find the semicolon. }
          iPos := Pos( ';', ScriptItems[i] );
          { Only process lines with length before and after the semicolon. }
          if ( iPos > 1 ) and ( iPos < iLen )then begin
            { Get the alias. }
            Alias := Copy( ScriptItems[i], 1, pred( iPos ) );
            { Get the path. }
            Path := Copy( ScriptItems[i], succ( iPos ), iLen - iPos );
            { Add the alias. }
            ProcessScriptCommand(Alias, Path, DeleteScript);
          end;
        end;
      end;
    finally
      ScriptItems.Free;
    end;
  end;
end;
{--------}
procedure TffServerEngine.ProcessFullScript(const ScriptFileName : TffFullFileName);
var
  AfterStr      : TffShStr;
  AppliesToSelf : Boolean;
    { If True then script command applies to this server.  Becomes True when
      encounters a section header bearing the same server name.  Becomes False
      when encounters a section header bearing a different server name. }
  DeleteScript  : Boolean;
  Inx           : Integer;
  Len           : Integer;
  Line          : TffShStr;
  PosEquals     : Integer;
  ScriptItems   : TStrings;
  UServerName   : TffShStr;
begin
  AppliesToSelf := True;
    { Default to True since the script may contain leading items that apply
      to all server engines. }
  DeleteScript := False;
  UServerName := Uppercase(Self.Name);
  { Does the script file exist? }
  if FFFileExists(ScriptFileName) then begin
    { Yes. Process it. }
    ScriptItems := TStringList.Create;
    try
      ScriptItems.LoadFromFile(ScriptFileName);
      { For each item in the file, try to parse it. }
      for Inx := 0 to pred(ScriptItems.Count) do begin
        { Only process lines with some length. }
        Line := Trim(ScriptItems[Inx]);
        Len := length(Line);
        if (Len > 2) then begin
          { Is this a section header? }
          if (Pos('[', Line) = 1) and
             (Pos(']', Line) = Len) then begin
            { Yes.  Does the section apply to us? }
            AppliesToSelf := (UpperCase(Copy(Line, 2, Len - 2)) = UServerName);
          end
          else
            { Not a section header. Does this item apply to this server
              engine? }
            if AppliesToSelf then begin
              { Yes. Find the equals sign. }
              PosEquals := Pos('=', Line);
              { Only process lines with length before and after the = char. }
              if (PosEquals > 1) and (PosEquals < Len) then begin
                { Get the before and after strings. }
                AfterStr := Copy(Line, succ(PosEquals), Len - PosEquals);
                SetLength(Line, pred(PosEquals));
                { Process the script command. }
                ProcessScriptCommand(Line, AfterStr, DeleteScript);
                if (DeleteScript) then
                  DeleteFile(ScriptFileName);
              end;
            end;  { if AppliesToSelf }
        end;
      end;
    finally
      ScriptItems.Free;
    end;
  end;
end;
{--------}
procedure TffServerEngine.ReadAliasData;
var
  aClientID  : TffClientID;
  Alias      : TffName;
  Client     : TffSrClient;
  Cursor     : TffSrBaseCursor;                                        {!!.06}
  DB         : TffSrDatabase;
  DBIResult  : TffResult;
  Dict       : TffDataDictionary;
  Folder     : TffSrFolder;
  Hash       : TffWord32;
  IsNull     : Boolean;
  MyRec      : PffByteArray;
  Path       : TffPath;
  SearchPath : TffPath;
  CheckDisk  : Boolean;                                                {!!.11}
begin
  Folder := nil;
  DB := nil;
  Client := nil;
  Cursor := nil;
  try
    {create ourselves a client}
    DBIResult := ClientAdd(aClientID, '', ffc_AdminUserID, 1000, Hash);
    if (DBIResult <> DBIERR_NONE) then
      Exit;

    {open a database to the server engine directory}
    Client := TffSrClient(aClientID);
    Folder := TffSrFolder.Create(ConfigDir, True, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
                             Folder,
                             '',
                             omReadWrite,
                             smExclusive,
                             1000,
                             False);                                   {!!.11}
    if (DBIResult = DBIERR_NONE) then
      FFSetRetry(DB.Timeout)
    else
      Exit;

    { Read the records. }
    Configuration.AliasList.BeginWrite;
    try
      Configuration.AliasList.Empty;
    finally
      Configuration.AliasList.EndWrite;
    end;

    { If the table exists then read it. }
    SearchPath := Folder.Path;
    if (SearchPath[length(SearchPath)] <> '\') then
      FFShStrAddChar(SearchPath, '\');
    if (FFFileExists(SearchPath +
                    FFForceExtension(ffc_AliasTableName,
                                     ffc_ExtForData))) then begin
      Cursor := CursorClass.Create(Self, DB, 1000);                    {!!.06}
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Cursor.Open(ffc_AliasTableName,
                  '',
                  0,
                  omReadOnly,
                  smExclusive,
                  True,
                  False, []);
      Cursor.CloseTable := True;
      Dict := Cursor.Dictionary;
      FFGetMem(MyRec, Dict.RecordLength);
      try
        FFSetRetry(Cursor.Timeout);
        Cursor.SetToBegin;
        FFSetRetry(Cursor.Timeout);                                    {!!.01}
        DBIResult := Cursor.GetNextRecord(MyRec, ffsltNone);
        while (DBIResult = DBIERR_NONE) do begin
          Dict.GetRecordField(0, MyRec, IsNull, @Alias);
          Dict.GetRecordField(1, MyRec, IsNull, @Path);
          if (Dict.FieldCount > 2) then                                {!!.11}
            Dict.GetRecordField(2, MyRec, IsNull, @CheckDisk)          {!!.11}
          else                                                         {!!.11}
            CheckDisk := False;                                        {!!.11}
          { Assumption: This is one of the first things happening when the
            server starts so no thread-safeness need be enforced. }
          Configuration.AddAlias(Alias, Path, CheckDisk);              {!!.11}
          FFSetRetry(Cursor.Timeout);                                  {!!.01}
          DBIResult := Cursor.GetNextRecord(MyRec, ffsltNone);
        end;
      finally
        FFFreeMem(MyRec, Dict.RecordLength);
      end;{try..finally}
    end;
  finally

    { Close the cursor. }
    if assigned(Cursor) then
      Cursor.Free;

    DB.Free;
    Folder.Free;

    { Remove the client. }
    seClientRemovePrim(Client);

  end;
end;
{--------}
function TffServerEngine.WriteAliasData : TffResult;
label
  Cleanup,
  InnerCleanup;
var
  aClientID  : TffClientID;
  AliasItem  : TffAliasItem;
  Buffer     : TffShStr;
  Dict       : TffDataDictionary;
  Folder     : TffSrFolder;
  Hash       : TffWord32;
  i          : integer;
  MyRec      : PffByteArray;
  State      : integer;
  TransID    : TffTransID;
  Client     : TffSrClient;
  DB         : TffSrDatabase;
  Cursor     : TffSrBaseCursor;                                        {!!.06}
begin
  Result := DBIERR_NONE;
  with Configuration.GeneralInfo^ do
    if giReadOnly or giNoAutoSaveCfg then
      Exit;

  State := 0;
  DB := nil;
  Client := nil;
  Dict := nil;
  Folder := nil;
  Cursor := nil;
  try

    { Strategy: Create a temporary table and write the data to that
      table.  If that works, rename the existing table and replace it with
      the temporary table.  If that succeeds, get rid of the old table.
      If a failure occurs at any point, the old table must be put back
      in its original place. }

    {create ourselves a client}
    Result := ClientAdd(aClientID, '', ffc_AdminUserID, 1000, Hash);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 100;  { client added }

    {open a database (no alias) to the server engine directory}
    Client := TffSrClient(aClientID);
    Folder := TffSrFolder.Create(ConfigDir, False, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
                             Folder,
                             '',
                             omReadWrite,
                             smExclusive,
                             1000,
                             False);                                   {!!.11}
    if (Result = DBIERR_NONE) then
      FFSetRetry(DB.Timeout)
    else
      goto Cleanup;

    State := 200;  { database opened }

    {Make sure prior instances of the saved and temporary tables are deleted. }
    seTableDeletePrim(DB, ffc_SavedAliasTableName);
    seTableDeletePrim(DB, ffc_TempAliasTableName);

    {Prepare a data dictionary.}
    Dict := TffServerDataDict.Create(4096);

    State := 300;  { dictionary created }

    {Create the new alias table as a temporary file. }
    with Dict do begin
      AddField('Alias',     '', fftShortString, pred(sizeof(TffName)), 0, True, nil);
      AddField('Path',      '', fftShortString, pred(sizeof(TffPath)), 0, True, nil);
      AddField('CheckDisk', '', fftBoolean,     SizeOf(Boolean),       0, True, nil); {!!.11}
    end;
    Dict.IsEncrypted := Configuration.GeneralInfo^.giAllowEncrypt;

    Result := seTableBuildPrim(DB,
                               True,
                               ffc_TempAliasTableName,
                               True,
                               Dict);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 400;  { temporary table created }

    {start a transaction before opening the alias table}
    Result := seTransactionStart(DB, False, ffcl_TrImplicit, TransID);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 500;  { transaction started for opening alias table }

    Configuration.AliasList.BeginRead;

    try
      FFGetMem(MyRec, Dict.RecordLength);

      State := 600;

      Cursor := CursorClass.Create(Self, DB, 1000);                    {!!.06}
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Cursor.Open(ffc_TempAliasTableName,
                  '',
                  0,
                  omReadWrite,
                  smExclusive,
                  True,
                  False,
                  []);
      Cursor.CloseTable := True;

      {Insert new records.}
      for i := 0 to pred(Configuration.AliasList.Count) do begin
        Cursor.Dictionary.InitRecord(MyRec);
        AliasItem := Configuration.AliasList[i];
        Buffer := AliasItem.Alias;
        Cursor.Dictionary.SetRecordField(0, MyRec, @Buffer);
        Buffer := AliasItem.Path;
        Cursor.Dictionary.SetRecordField(1, MyRec, @Buffer);
        Cursor.Dictionary.SetRecordField(2, MyRec, @AliasItem.CheckSpace); {!!.11}
        FFSetRetry(Cursor.Timeout);                                    {!!.01}
        Result := Cursor.InsertRecord(MyRec, ffsltExclusive);
        if (Result <> DBIERR_NONE) then
          goto InnerCleanup;
      end;

      State := 750;

      { Commit the transaction. }
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Result := seTransactionCommit(DB);
      if Result = DBIERR_NONE then
        State := 800;  { transaction committed }

      InnerCleanup:

    finally
      Configuration.AliasList.EndRead;

      { Rollback the transaction. }
      if (State >= 500) and (State < 750) then
        seTransactionRollback(DB);

      if State >= 600 then
        FFFreeMem(MyRec, Dict.RecordLength);

      {close the cursor}
      if assigned(Cursor) then
        Cursor.Free;

    end;{try..finally}

    { If the record insertions did not complete then jump to cleanup. }
    if State < 800 then
      goto Cleanup;

    { Rename the existing table. }
    Result := seTableRenamePrim(DB, ffc_AliasTableName, ffc_SavedAliasTableName);
    if (Result <> DBIERR_NOSUCHTABLE) and (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 1000;  { renamed system table to saved table }

    { Replace the original table with the temporary table. }
    Result := seTableRenamePrim(DB, ffc_TempAliasTableName, ffc_AliasTableName);
    if Result <> DBIERR_NONE then
      goto Cleanup;

    State := 1100;  { renamed temp table to system table }

    { The new alias table is now in place.  Get rid of the saved, original
      table.  Ignore errors. }
    if not IsTableNameOpen(DB.Folder, ffC_SavedAliasTableName) then
      seDeleteTable(DB, ffC_SavedAliasTableName)
    else
      Result := DBIERR_TABLEOPEN;

    { The code jumps to this point if an error is detected in a ServerEngine
      method. }
    Cleanup:

  except
    {If an exception occurs, get the error code and fall through to the
     cleanup code below.  The error code will be returned to the calling
     object. }
    on E : Exception do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  end;

  { Put System table back into its rightful place if a failure occurred
    after it was renamed to the saved table. }
  if (State >= 1000) and (State < 1100) then
    seTableRenamePrim(DB, ffc_SavedAliasTableName, ffc_AliasTableName);

  {delete temporary table if it did not replace system table}
  if (State >= 400) and (State < 1100) then
    if not IsTableNameOpen(DB.Folder, ffc_TempAliasTableName) then
      seDeleteTable(DB, ffc_TempAliasTableName)
    else
      Result := DBIERR_TABLEOPEN;

  Dict.Free;
  DB.Free;
  Folder.Free;

  {remove the client}
  if State >= 100 then
    seClientRemovePrim(Client);

end;
{=====================================================================}


{== Read/Write User data from table ==================================}
procedure TffServerEngine.ReadUserData;
var
  aClientID  : TffClientID;
  BufFirst   : TffName;
  BufHash    : TffWord32;
  BufLast    : TffName;
  BufRights  : TffUserRights;
  BufUserID  : TffName;
  Client     : TffSrClient;
  Cursor     : TffSrBaseCursor;                                        {!!.06}
  DBIResult  : TffResult;
  DB         : TffSrDatabase;
  Dict       : TffDataDictionary;
  Folder     : TffSrFolder;
  IsNull     : boolean;
  MyRec      : PffByteArray;
  SearchPath : TffPath;
begin
  Client := nil;
  Folder := nil;
  DB := nil;
  Cursor := nil;
  try
    {create ourselves a client}
    DBIResult := ClientAdd(aClientID,
                           '',
                           ffc_AdminUserID,
                           1000,
                           BufHash);
    if (DBIResult <> DBIERR_NONE) then
      Exit;

    {open a database (no User) to the server engine directory}
    Client := TffSrClient(aClientID);
    Folder := TffSrFolder.Create(ConfigDir, True, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
                             Folder,
                             '',
                             omReadWrite,
                             smExclusive,
                             1000,
                             False);                                   {!!.11}
    if (DBIResult = DBIERR_NONE) then
      FFSetRetry(DB.Timeout)
    else
      Exit;

    Configuration.UserList.Empty;

    { If the table exists then read it. }
    SearchPath := Folder.Path;
    if (SearchPath[length(SearchPath)] <> '\') then
      FFShStrAddChar(SearchPath, '\');
    if FFFileExists(SearchPath + FFForceExtension(ffc_UserTableName, ffc_ExtForData)) then begin
      Cursor := CursorClass.Create(Self, DB, 1000);                    {!!.06}
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Cursor.Open(ffc_UserTableName, '', 0, omReadOnly, smExclusive,
                  true, False, []);
      Cursor.CloseTable := True;
      Dict := Cursor.Dictionary;
      FFGetMem(MyRec, Dict.RecordLength);
      try
        FFSetRetry(Cursor.Timeout);
        Cursor.SetToBegin;
        FFSetRetry(Cursor.Timeout);                                    {!!.01}
        DBIResult := Cursor.GetNextRecord(MyRec, ffsltNone);
        while (DBIResult = DBIERR_NONE) do begin
          Dict.GetRecordField(0, MyRec, IsNull, @BufUserID);
          Dict.GetRecordField(1, MyRec, IsNull, @BufLast);
          Dict.GetRecordField(2, MyRec, IsNull, @BufFirst);
          Dict.GetRecordField(3, MyRec, IsNull, @BufHash);
          Dict.GetRecordField(4, MyRec, IsNull, @BufRights);
          Configuration.AddUser(BufUserID, BufLast, BufFirst, BufHash, BufRights);
          FFSetRetry(Cursor.Timeout);                                  {!!.01}
          DBIResult := Cursor.GetNextRecord(MyRec, ffsltNone);
        end;
      finally
        FFFreeMem(MyRec, Dict.RecordLength);
      end;{try..finally}
    end;
  finally

    { Close the cursor. }
    if assigned(Cursor) then
      Cursor.Free;

    DB.Free;
    Folder.Free;

    { Remove the client. }
    seClientRemovePrim(Client);

  end;
end;
{--------}
function TffServerEngine.WriteUserData : TffResult;
label
  Cleanup,
  InnerCleanup;
var
  aClientID  : TffClientID;
  BufHash    : TffWord32;
  BufRights  : TffUserRights;
  BufStr     : TffShStr;
  Dict       : TffDataDictionary;
  Folder     : TffSrFolder;
  Hash       : TffWord32;
  i          : integer;
  MyRec      : PffByteArray;
  State      : integer;
  TransID    : TffTransID;
  UserItem   : TffUserItem;
  Client     : TffSrClient;
  DB         : TffSrDatabase;
  Cursor     : TffSrBaseCursor;                                        {!!.06}
begin
  Result := DBIERR_NONE;
  with Configuration.GeneralInfo^ do
    if giReadOnly or giNoAutoSaveCfg then
      Exit;

  Client := nil;
  DB := nil;
  Dict := nil;
  Folder := nil;
  Cursor := nil;
  State := 0;

  try
    { Strategy: Create a temporary table and write the data to that
      table.  If that works, rename the existing table and replace it with
      the temporary table.  If that succeeds, get rid of the old table.
      If a failure occurs at any point, the old table must be put back
      in its original place. }

    {create ourselves a client}
    Result := ClientAdd(aClientID, '', ffc_AdminUserID, 1000, Hash);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 100;  { client added }

    {open a database (no alias) to the server engine directory}
    Client := TffSrClient(aClientID);
    Folder := TffSrFolder.Create(ConfigDir, False, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
                             Folder,
                             '',
                             omReadWrite,
                             smExclusive,
                             1000,
                             False);                                   {!!.11}
    if (Result = DBIERR_NONE) then
      FFSetRetry(DB.Timeout)
    else
      goto Cleanup;

    State := 200;  { database opened }

    {Make sure prior instances of the saved and temporary tables are deleted. }
    seTableDeletePrim(DB, ffc_SavedUserTableName);
    seTableDeletePrim(DB, ffc_TempUserTableName);

    {create a dictionary}
    Dict := TffServerDataDict.Create(4096);

    State := 300;  { dictionary created }

    with Dict do begin
      AddField('User',      '', fftShortString, pred(sizeof(TffName)), 0, true, nil);
      AddField('LastName',  '', fftShortString, pred(sizeof(TffName)), 0, true, nil);
      AddField('FirstName', '', fftShortString, pred(sizeof(TffName)), 0, true, nil);
      AddField('PwdHash',   '', fftWord32, 0, 0, true, nil);
      AddField('Rights',    '', fftByteArray, sizeof(TffUserRights), 0, true, nil);
    end;
    Dict.IsEncrypted := Configuration.GeneralInfo^.giAllowEncrypt;

    {Create the new table as a temporary file. }
    Result := seTableBuildPrim(DB, true, ffc_TempUserTableName, True, Dict);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 400;  { temporary table created }

    {start a transaction before opening the table}
    Result := seTransactionStart(DB, false, ffcl_TrImplicit, TransID);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 500;  { transaction started for opening table }

    try
      FFGetMem(MyRec, Dict.RecordLength);

      State := 600;

      {Insert new records.}
      Cursor := CursorClass.Create(Self, DB, 1000);                    {!!.06}
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Cursor.Open(ffc_TempUserTableName, '', 0, omReadWrite, smExclusive,
                  True, False, []);
      Cursor.CloseTable := True;
      for i := 0 to pred(Configuration.UserList.Count) do begin
        Cursor.Dictionary.InitRecord(MyRec);
        UserItem := Configuration.UserList[i];
        BufStr := UserItem.UserID;
        Cursor.Dictionary.SetRecordField(0, MyRec, @BufStr);
        BufStr := UserItem.LastName;
        Cursor.Dictionary.SetRecordField(1, MyRec, @BufStr);
        BufStr := UserItem.FirstName;
        Cursor.Dictionary.SetRecordField(2, MyRec, @BufStr);
        BufHash := UserItem.PasswordHash;
        Cursor.Dictionary.SetRecordField(3, MyRec, @BufHash);
        BufRights := UserItem.Rights;
        Cursor.Dictionary.SetRecordField(4, MyRec, @BufRights);
        FFSetRetry(Cursor.Timeout);                                    {!!.01}
        Cursor.InsertRecord(MyRec, ffsltExclusive);
        if (Result <> DBIERR_NONE) then
          goto InnerCleanup;
      end;

      State := 750;

      { Commit the transaction. }
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Result := seTransactionCommit(DB);
      if Result = DBIERR_NONE then
        State := 800;  { transaction committed }

      InnerCleanup:

    finally
      { Rollback the transaction. }
      if (State >= 500) and (State < 750) then
        seTransactionRollback(DB);

      if State >= 600 then
        FFFreeMem(MyRec, Dict.RecordLength);

      {close the cursor}
      if assigned(Cursor) then
        Cursor.Free;

    end;{try..finally}

    { If the record insertions did not complete then jump to cleanup. }
    if State < 800 then
      goto Cleanup;

    { Rename the existing table. }
    Result := seTableRenamePrim(DB, ffc_UserTableName, ffc_SavedUserTableName);
    if (Result <> DBIERR_NOSUCHTABLE) and (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 1000;  { renamed system table to saved table }

    { Replace the original table with the temporary table. }
    Result := seTableRenamePrim(DB, ffc_TempUserTableName, ffc_UserTableName);
    if Result <> DBIERR_NONE then
      goto Cleanup;

    State := 1100;  { renamed temp table to system table }

    { The new table is now in place.  Get rid of the saved, original
      table.  Ignore errors. }
    if not IsTableNameOpen(DB.Folder, ffc_SavedUserTableName) then
      seDeleteTable(DB, ffc_SavedUserTableName)
    else
      Result := DBIERR_TABLEOPEN;

    { The code jumps to this point if an error is detected in a ServerEngine
      method. }
    Cleanup:

  except
    {If an exception occurs, get the error code and fall through to the
     cleanup code below.  The error code will be returned to the calling
     object. }
    on E : Exception do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  end;

  { Put System table back into its rightful place if a failure occurred
    after it was renamed to the saved table. }
  if (State >= 1000) and (State < 1100) then
    seTableRenamePrim(DB, ffc_SavedUserTableName, ffc_UserTableName);

  {delete temporary table if it did not replace system table}
  if (State >= 400) and (State < 1100) then
    if not IsTableNameOpen(DB.Folder, ffc_TempUserTableName) then
      seDeleteTable(DB, ffc_TempUserTableName)
    else
      Result := DBIERR_TABLEOPEN;

  Dict.Free;
  DB.Free;
  Folder.Free;

  {remove the client}
  if State >= 100 then
    seClientRemovePrim(Client);

end;
{=====================================================================}


{== Read/write general info from tables ==============================}
const
  ffc_GeneralClientID = -1;
{--------}
procedure TffServerEngine.ReadGeneralInfo;
var
  aClientID  : TffClientID;
  Client     : TffSrClient;
  Cursor     : TffSrBaseCursor;                                        {!!.06}
  DB         : TffSrDatabase;
  DBIResult  : TffResult;
  Dict       : TffDataDictionary;
  Folder     : TffSrFolder;
  Hash       : TffWord32;
  IsNull     : boolean;
  MyRec      : PffByteArray;
  SearchPath : TffPath;
begin
  Client := nil;
  DB := nil;
  Folder := nil;
  Cursor := nil;
  try
    {create ourselves a client}
    DBIResult := ClientAdd(aClientID, '', ffc_AdminUserID, 1000, Hash);
    if (DBIResult <> DBIERR_NONE) then
      Exit;

    {open a database (no User) to the server engine directory}
    Client := TffSrClient(aClientID);
    Folder := TffSrFolder.Create(ConfigDir, True, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
                             Folder,
                             '',
                             omReadWrite,
                             smExclusive,
                             1000,
                             False);                                   {!!.11}
    if (DBIResult = DBIERR_NONE) then
      FFSetRetry(DB.Timeout)
    else
      Exit;

    { If the table exists then read it. }
    SearchPath := Folder.Path;
    if (SearchPath[length(SearchPath)] <> '\') then
      FFShStrAddChar(SearchPath, '\');
    if FFFileExists(SearchPath + FFForceExtension(ffc_GenInfoTableName, ffc_ExtForData)) then begin
      { Open a cursor to read the records. }
      Cursor := CursorClass.Create(Self, DB, 1000);                    {!!.06}
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Cursor.Open(ffc_GenInfoTableName, '', 0, omReadOnly, smExclusive,
                  true, False, []);
      Cursor.CloseTable := True;
      Dict := Cursor.Dictionary;
      FFGetMem(MyRec, Dict.RecordLength);
      try
        FFSetRetry(Cursor.Timeout);
        Cursor.SetToBegin;
        FFSetRetry(Cursor.Timeout);
        DBIResult := Cursor.GetNextRecord(MyRec, ffsltNone);
        if DBIResult = DBIERR_NONE then
          with Configuration.GeneralInfo^ do begin
            Dict.GetRecordField(0, MyRec, IsNull, @giServerName);
            Dict.GetRecordField(1, MyRec, IsNull, @giMaxRAM);
            Dict.GetRecordField(2, MyRec, IsNull, @giIsSecure);
            Dict.GetRecordField(3, MyRec, IsNull, @giAutoUp);
            Dict.GetRecordField(4, MyRec, IsNull, @giAutoMini);
            Dict.GetRecordField(5, MyRec, IsNull, @giDebugLog);
            Dict.GetRecordField(6, MyRec, IsNull, @giSingleUser);
            Dict.GetRecordField(7, MyRec, IsNull, @giIPXSPX);
            Dict.GetRecordField(8, MyRec, IsNull, @giIPXSPXLFB);
            Dict.GetRecordField(9, MyRec, IsNull, @giTCPIP);
            Dict.GetRecordField(10, MyRec, IsNull, @giTCPIPLFB);
            Dict.GetRecordField(11, MyRec, IsNull, @giTCPPort);
            Dict.GetRecordField(12, MyRec, IsNull, @giUDPPortSr);
            Dict.GetRecordField(13, MyRec, IsNull, @giUDPPortCl);
            Dict.GetRecordField(14, MyRec, IsNull, @giIPXSocketSr);
            Dict.GetRecordField(15, MyRec, IsNull, @giIPXSocketCl);
            Dict.GetRecordField(16, MyRec, IsNull, @giSPXSocket);
            Dict.GetRecordField(17, MyRec, IsNull, @giAllowEncrypt);
            Dict.GetRecordField(18, MyRec, IsNull, @giReadOnly);
            Dict.GetRecordField(19, MyRec, IsNull, @giLastMsgInterval);
            Dict.GetRecordField(20, MyRec, IsNull, @giKAInterval);
            Dict.GetRecordField(21, MyRec, IsNull, @giKARetries);
            Dict.GetRecordField(22, MyRec, IsNull, @giPriority);
            Dict.GetRecordField(23, MyRec, IsNull, @giTCPInterface);
            Dict.GetRecordField(24, MyRec, IsNull, @giNoAutoSaveCfg);
            Dict.GetRecordField(25, MyRec, IsNull, @giTempStoreSize);
            Dict.GetRecordField(26, MyRec, IsNull, @giCollectEnabled);
            Dict.GetRecordField(27, MyRec, IsNull, @giCollectFreq);
          end;  { with }
      finally
        FFFreeMem(MyRec, Dict.RecordLength);
      end;{try..finally}
    end;
  finally

    { Close the cursor. }
    if assigned(Cursor) then
      Cursor.Free;

    DB.Free;
    Folder.Free;

    { Remove the client. }
    seClientRemovePrim(Client);

    { Update the logging state. }
    seSetLoggingState;

  end;
end;
{--------}
function TffServerEngine.WriteGeneralInfo(aOverrideRO : Boolean)
                                                      : TffResult;
label
  Cleanup,
  InnerCleanup;
var
  aClientID     : TffClientID;
  MyRec         : PffByteArray;
  Folder        : TffSrFolder;
  Hash          : TffWord32;
  TransID       : TffTransID;
  Dict          : TffServerDataDict;
  Client        : TffSrClient;
  DB            : TffSrDatabase;
  Cursor        : TffSrBaseCursor;                                     {!!.06}
  State         : integer;
begin
  Result := DBIERR_NONE;

  {aOverrideRO is used to override the giReadOnly setting. If we didn't
   have this option, there would be no way of saving the change when
   setting giReadOnly from False to True}
  with Configuration.GeneralInfo^ do
    if ((giReadOnly or giNoAutoSaveCfg) and
        (not aOverrideRO)) then
      Exit;

  State := 0;
  Client := nil;
  DB := nil;
  Cursor := nil;
  Dict := nil;
  Folder := nil;

  try

    { Strategy: Create a temporary table and write the data to that
      table.  If that works, rename the existing table and replace it with
      the temporary table.  If that succeeds, get rid of the old table.
      If a failure occurs at any point, the old table must be put back
      in its original place. }

    {create ourselves a client}
    Result := ClientAdd(aClientID, '', ffc_AdminUserID, 1000, Hash);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 100; {client added}

    {open a database (no alias) to the server engine directory}
    Client := TffSrClient(aClientID);
    Folder := TffSrFolder.Create(ConfigDir, False, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
                             Folder,
                             '',
                             omReadWrite,
                             smExclusive,
                             1000,
                             False);                                   {!!.11}
    if (Result = DBIERR_NONE) then
      FFSetRetry(DB.Timeout)
    else
      goto Cleanup;

    State := 200; {database opened}

    {Make sure prior instances of the saved and temporary tables are deleted. }
    seTableDeletePrim(DB, ffc_SavedGenInfoTableName);
    seTableDeletePrim(DB, ffc_TempGenInfoTableName);

    {build a new data dictionary (don't bother with an index)}
    Dict := TffServerDataDict.Create(4096);

    State := 300; {dict created}

    with Dict do begin
      AddField('ServerName',    '', fftShortString, pred(sizeof(TffNetName)), 0, true, nil);
      AddField('MaxRAM',        '', fftWord32,  0, 0, True, nil);
      AddField('IsSecure',      '', fftBoolean, 0, 0, True, nil);
      AddField('AutoUp',        '', fftBoolean, 0, 0, True, nil);
      AddField('AutoMini',      '', fftBoolean, 0, 0, True, nil);
      AddField('DebugLog',      '', fftBoolean, 0, 0, True, nil);
      AddField('UseSingleUser', '', fftBoolean, 0, 0, True, nil);
      AddField('UseIPXSPX',     '', fftBoolean, 0, 0, True, nil);
      AddField('IPXSPXLFB',     '', fftBoolean, 0, 0, True, nil);
      AddField('UseTCPIP',      '', fftBoolean, 0, 0, True, nil);
      AddField('TCPIPLFB',      '', fftBoolean, 0, 0, True, nil);
      AddField('TCPPort',       '', fftInt32,   0, 0, True, nil);
      AddField('UDPPortSr',     '', fftInt32,   0, 0, True, nil);
      AddField('UDPPortCl',     '', fftInt32,   0, 0, True, nil);
      AddField('IPXSocketSr',   '', fftInt32,   0, 0, True, nil);
      AddField('IPXSocketCl',   '', fftInt32,   0, 0, True, nil);
      AddField('SPXSocket',     '', fftInt32,   0, 0, True, nil);
      AddField('UseEncrypt',    '', fftBoolean, 0, 0, True, nil);
      AddField('ReadOnly',      '', fftBoolean, 0, 0, True, nil);
      AddField('LstMsgIntvl',   '', fftInt32,   0, 0, True, nil);
      AddField('KAInterval',    '', fftInt32,   0, 0, True, nil);
      AddField('KARetries',     '', fftInt32,   0, 0, True, nil);
      AddField('Priority',      '', fftInt32,   0, 0, True, nil);
      AddField('TCPInterface',  '', fftInt32,   0, 0, True, nil);
      AddField('NoAutoSaveCfg', '', fftBoolean, 0, 0, True, nil);
      Addfield('TempStoreSize', '', fftInt16,   0, 0, True, nil);
      AddField('CollectEnabld', '', fftBoolean, 0, 0, True, nil);
      AddField('CollectFreq',   '', fftInt32,   0, 0, True, nil);
    end;
    Dict.IsEncrypted := Configuration.GeneralInfo^.giAllowEncrypt;

    {build a new alias table}
    Result := seTableBuildPrim(DB, True, ffc_TempGenInfoTableName, True,
                               Dict);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 400; {temporary table created}

    {start a transaction before opening the table}
    Result := seTransactionStart(DB, False, ffcl_TrImplicit, TransID);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 500;  { transaction started for opening table }

    try
      {First, delete all existing records.}
      FFGetMem(MyRec, Dict.RecordLength);

      State := 600; {memory allocated for MyRec}

      {Insert new record.}
      Cursor := CursorClass.Create(Self, DB, 1000);                    {!!.06}
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Cursor.Open(ffc_TempGenInfoTableName, '', 0, omReadWrite, smExclusive,
                  True, False, []);
      Cursor.CloseTable := True;
      Cursor.Dictionary.InitRecord(MyRec);
      with Configuration.GeneralInfo^, Cursor.Dictionary do begin
        SetRecordField(0, MyRec, @giServerName);
        SetRecordField(1, MyRec, @giMaxRAM);
        SetRecordField(2, MyRec, @giIsSecure);
        SetRecordField(3, MyRec, @giAutoUp);
        SetRecordField(4, MyRec, @giAutoMini);
        SetRecordField(5, MyRec, @giDebugLog);
        SetRecordField(6, MyRec, @giSingleUser);
        SetRecordField(7, MyRec, @giIPXSPX);
        SetRecordField(8, MyRec, @giIPXSPXLFB);
        SetRecordField(9, MyRec, @giTCPIP);
        SetRecordField(10, MyRec, @giTCPIPLFB);
        SetRecordField(11, MyRec, @giTCPPort);
        SetRecordField(12, MyRec, @giUDPPortSr);
        SetRecordField(13, MyRec, @giUDPPortCl);
        SetRecordField(14, MyRec, @giIPXSocketSr);
        SetRecordField(15, MyRec, @giIPXSocketCl);
        SetRecordField(16, MyRec, @giSPXSocket);
        SetRecordField(17, MyRec, @giAllowEncrypt);
        SetRecordField(18, MyRec, @giReadOnly);
        SetRecordField(19, MyRec, @giLastMsgInterval);
        SetRecordField(20, MyRec, @giKAInterval);
        SetRecordField(21, MyRec, @giKARetries);
        SetRecordField(22, MyRec, @giPriority);
        SetRecordField(23, MyRec, @giTCPInterface);
        SetRecordField(24, MyRec, @giNoAutoSaveCfg);
        SetRecordField(25, MyRec, @giTempStoreSize);
        SetRecordField(26, MyRec, @giCollectEnabled);
        SetRecordField(27, MyRec, @giCollectFreq);
      end;
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Result := Cursor.InsertRecord(MyRec, ffsltExclusive);
      if Result <> DBIERR_NONE then
        goto InnerCleanup;

      State := 750;

      { Commit the transaction. }
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Result := seTransactionCommit(DB);
      if Result = DBIERR_NONE then
        State := 800;  { transaction committed }

      InnerCleanup:

    finally
      {rollback the transaction}
      if (State >= 500) and (State < 750) then
        seTransactionRollback(DB);

      {free memory for MyRec}
      if State >= 600 then
        FFFreeMem(MyRec, Dict.RecordLength);

      {close the cursor}
      if assigned(Cursor) then
        Cursor.Free;

    end; {try..finally}

    {if the record wasn't inserted, goto cleanup}
    if State < 800 then
      goto Cleanup;

    { Rename the existing table. }
    Result := seTableRenamePrim(DB, ffc_GenInfoTableName, ffc_SavedGenInfoTableName);
    if (Result <> DBIERR_NOSUCHTABLE) and (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 1000; {table renamed}

    { Replace the original table with the temporary table. }
    Result := seTableRenamePrim(DB, ffc_TempGenInfoTableName, ffc_GenInfoTableName);
    if Result <> DBIERR_NONE then
      goto Cleanup;

    State := 1100; {renamed existing table}

    { The new table is now in place.  Get rid of the saved, original
      table.  Ignore errors. }
    if not IsTableNameOpen(DB.Folder, ffc_SavedGenInfoTableName) then
      seDeleteTable(DB, ffc_SavedGenInfoTableName)
    else
      Result := DBIERR_TABLEOPEN;

    Cleanup:

  except
    {If an error occurs at any point, we raise an exception.  The
     exception handling just falls through to the cleanup code below.}
    on E: Exception do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  end;

  { Put System table back into its rightful place if a failure occurred
    after it was renamed to the saved table. }
  if (State >= 1000) and (State < 1100) then
    seTableRenamePrim(DB, ffc_SavedGenInfoTableName, ffc_GenInfoTableName);

  {delete the temporary table if it didn't replace the system table}
  if (State >= 400) and (State < 1100) then
    if not IsTableNameOpen(DB.Folder, ffc_TempGenInfoTableName) then
      seDeleteTable(DB, ffc_TempGenInfoTableName)
    else
      Result := DBIERR_TABLEOPEN;

  Dict.Free;
  DB.Free;
  Folder.Free;

  {remove the client}
  if State >= 100 then
    seClientRemovePrim(Client);

end;
{=====================================================================}


{== Read/write key proc info from/to tables ==========================}
const
  ffc_KeyProcClientID = -1;
{--------}
procedure TffServerEngine.ReadKeyProcData;
var
  aClientID  : TffClientID;
  BufBuild   : TffName;
  BufCompare : TffName;
  BufDLL     : TffFullFileName;
  BufIndexID : Longint;
  BufPath    : TffPath;
  BufTable   : TffTableName;
  Client     : TffSrClient;
  Cursor     : TffSrBaseCursor;                                        {!!.06}
  DB         : TffSrDatabase;
  DBIResult  : TffResult;
  Dict       : TffDataDictionary;
  Folder     : TffSrFolder;
  Hash       : TffWord32;
  IsNull     : boolean;
  MyRec      : PffByteArray;
  SearchPath : TffPath;
begin
  Client := nil;
  Folder := nil;
  Cursor := nil;
  DB := nil;
  try
    {create ourselves a client}
    DBIResult := ClientAdd(aClientID, '', ffc_AdminUserID, 1000, Hash);
    if (DBIResult <> DBIERR_NONE) then
      Exit;

    {open a database (no User) to the server engine directory}
    Client := TffSrClient(aClientID);
    Folder := TffSrFolder.Create(ConfigDir, True, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
                             Folder,
                             '',
                             omReadWrite,
                             smExclusive,
                             1000,
                             False);                                   {!!.11}
    if (DBIResult = DBIERR_NONE) then
      FFSetRetry(DB.Timeout)
    else
      Exit;

    {read the records}
    Configuration.KeyProcList.Empty;

    { If the table exists then read it. }
    SearchPath := Folder.Path;
    if (SearchPath[length(SearchPath)] <> '\') then
      FFShStrAddChar(SearchPath, '\');
    if FFFileExists(SearchPath + FFForceExtension(ffc_IndexTableName, ffc_ExtForData)) then begin
      Cursor := CursorClass.Create(Self, DB, 1000);                    {!!.06}
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Cursor.Open(ffc_IndexTableName, '', 0, omReadOnly, smExclusive,
                  True, False, []);
      Cursor.CloseTable := True;
      Dict := Cursor.Dictionary;
      FFGetMem(MyRec, Dict.RecordLength);
      try
        FFSetRetry(Cursor.Timeout);
        Cursor.SetToBegin;
        FFSetRetry(Cursor.Timeout);                                    {!!.01}
        DBIResult := Cursor.GetNextRecord(MyRec, ffsltNone);
        while (DBIResult = DBIERR_NONE) do begin
          Dict.GetRecordField(0, MyRec, IsNull, @BufPath);
          Dict.GetRecordField(1, MyRec, IsNull, @BufTable);
          Dict.GetRecordField(2, MyRec, IsNull, @BufIndexID);
          Dict.GetRecordField(3, MyRec, IsNull, @BufDLL);
          Dict.GetRecordField(4, MyRec, IsNull, @BufBuild);
          Dict.GetRecordField(5, MyRec, IsNull, @BufCompare);
          Configuration.AddKeyProc(BufPath, BufTable, BufIndexID,
                                   BufDLL, BufBuild, BufCompare);
          FFSetRetry(Cursor.Timeout);                                  {!!.01}
          DBIResult := Cursor.GetNextRecord(MyRec, ffsltNone);
        end;
      finally
        FFFreeMem(MyRec, Dict.RecordLength);
      end; {try..finally}
    end;
  finally

    { Close the cursor. }
    if assigned(Cursor) then
      Cursor.Free;

    DB.Free;
    Folder.Free;

    { Remove the client. }
    seClientRemovePrim(Client);

  end;
end;
{--------}
function TffServerEngine.WriteKeyProcData : TffResult;
label
  Cleanup,
  InnerCleanup;
var
  aClientID   : TffClientID;
  BufInt      : Longint;
  BufStr      : TffShStr;
  Dict        : TffDataDictionary;
  Folder      : TffSrFolder;
  Hash        : TffWord32;
  i           : integer;
  KeyProcItem : TffKeyProcItem;
  MyRec       : PffByteArray;
  State       : integer;
  TransID     : TffTransID;
  Client      : TffSrClient;
  DB          : TffSrDatabase;
  Cursor      : TffSrBaseCursor;                                       {!!.06}
begin

  Result := DBIERR_NONE;
  with Configuration.GeneralInfo^ do
    if giReadOnly or giNoAutoSaveCfg then
      Exit;

  Client := nil;
  DB := nil;
  Dict := nil;
  Folder := nil;
  Cursor := nil;
  State := 0;
  try
    { Strategy: Create a temporary table and write the data to that
      table.  If that works, rename the existing table and replace it with
      the temporary table.  If that succeeds, get rid of the old table.
      If a failure occurs at any point, the old table must be put back
      in its original place. }

    {create ourselves a client}
    Result := ClientAdd(aClientID, '', ffc_AdminUserID, 1000, Hash);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 100;  { client added }

    {open a database (no alias) to the server engine directory}
    Client := TffSrClient(aClientID);
    Folder := TffSrFolder.Create(ConfigDir, False, seBufMgr);
    DB := seDatabaseOpenPrim(Client.clSessionList.CurrentSession,
                             Folder,
                             '',
                             omReadWrite,
                             smExclusive,
                             1000,
                             False);                                   {!!.11}
    if (Result = DBIERR_NONE) then
      FFSetRetry(DB.Timeout)
    else
      goto Cleanup;

    State := 200;  { database opened }

    {Make sure prior instances of the saved and temporary tables are deleted. }
    seTableDeletePrim(DB, ffc_SavedIndexTableName);
    seTableDeletePrim(DB, ffc_TempIndexTableName);

    {Prepare a data dictionary.}
    Dict := TffServerDataDict.Create(4096);

    State := 300;  { dictionary created }

    {Create the new table as a temporary file. }

    with Dict do begin
      AddField('Path',       '', fftShortString, pred(sizeof(TffPath)), 0, true, nil);
      AddField('Table',      '', fftShortString, pred(sizeof(TffTableName)), 0, true, nil);
      AddField('IndexID',    '', fftInt32, 0, 0, true, nil);
      AddField('DLL',        '', fftShortString, pred(sizeof(TffFullFileName)), 0, true, nil);
      AddField('BuildKey',   '', fftShortString, pred(sizeof(TffName)), 0, true, nil);
      AddField('CompareKey', '', fftShortString, pred(sizeof(TffName)), 0, true, nil);
    end;
    Dict.IsEncrypted := Configuration.GeneralInfo^.giAllowEncrypt;

    Result := seTableBuildPrim(DB, true, ffc_TempIndexTableName, True, Dict);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 400;  { temporary table created }

    { Start a transaction before opening the table. }
    Result := seTransactionStart(DB, false, ffcl_TrImplicit, TransID);
    if (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 500;  { transaction started for opening table }

    try
      FFGetMem(MyRec, Dict.RecordLength);

      State := 600;

      Cursor := CursorClass.Create(Self, DB, 1000);                    {!!.06}
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Cursor.Open(ffc_TempIndexTableName, '', 0, omReadWrite, smExclusive,
                  True, False, []);
      Cursor.CloseTable := True;
      {Insert new records.}
      for i := 0 to pred(Configuration.KeyProcList.Count) do begin
        Cursor.Dictionary.InitRecord(MyRec);
        KeyProcItem := Configuration.KeyProcList[i];
        BufStr := KeyProcItem.Path;
        Cursor.Dictionary.SetRecordField(0, MyRec, @BufStr);
        BufStr := KeyProcItem.Table;
        Cursor.Dictionary.SetRecordField(1, MyRec, @BufStr);
        BufInt := KeyProcItem.IndexID;
        Cursor.Dictionary.SetRecordField(2, MyRec, @BufInt);
        BufStr := KeyProcItem.DLLName;
        Cursor.Dictionary.SetRecordField(3, MyRec, @BufStr);
        BufStr := KeyProcItem.BuildKeyName;
        Cursor.Dictionary.SetRecordField(4, MyRec, @BufStr);
        BufStr := KeyProcItem.CompareKeyName;
        Cursor.Dictionary.SetRecordField(5, MyRec, @BufStr);
        FFSetRetry(Cursor.Timeout);                                    {!!.01}
        Result := Cursor.InsertRecord(MyRec, ffsltExclusive);
        if (Result <> DBIERR_NONE) then
          goto InnerCleanup;
      end;

      State := 750;

      { Commit the transaction. }
      FFSetRetry(Cursor.Timeout);                                      {!!.01}
      Result := seTransactionCommit(DB);
      if Result = DBIERR_NONE then
        State := 800;  { transaction committed }

      InnerCleanup:

    finally
      { Rollback the transaction. }
      if (State >= 500) and (State < 750) then
        seTransactionRollback(DB);

      if State >= 600 then
        FFFreeMem(MyRec, Dict.RecordLength);

      {close the cursor}
      if assigned(Cursor) then
        Cursor.Free;

    end;{try..finally}

    { If the record insertions did not complete then jump to cleanup. }
    if State < 800 then
      goto Cleanup;

    { Rename the existing table. }
    Result := seTableRenamePrim(DB, ffc_IndexTableName, ffc_SavedIndexTableName);
    if (Result <> DBIERR_NOSUCHTABLE) and (Result <> DBIERR_NONE) then
      goto Cleanup;

    State := 1000;  { renamed system table to saved table }

    { Replace the original table with the temporary table. }
    Result := seTableRenamePrim(DB, ffc_TempIndexTableName, ffc_IndexTableName);
    if Result <> DBIERR_NONE then
      goto Cleanup;

    State := 1100;  { renamed temp table to system table }

    { The new table is now in place.  Get rid of the saved, original
      table.  Ignore errors. }
    if not IsTableNameOpen(DB.Folder, ffc_SavedIndexTableName) then
      seDeleteTable(DB, ffc_SavedIndexTableName)
    else
      Result := DBIERR_TABLEOPEN;

    { The code jumps to this point if an error is detected in a ServerEngine
      method. }
    Cleanup:
  except
    {If an exception occurs, get the error code and fall through to the
     cleanup code below.  The error code will be returned to the calling
     object. }
    on E : Exception do
      Result := ConvertServerExceptionEx(E, FEventLog, bseGetReadOnly);
  end;

  { Put System table back into its rightful place if a failure occurred
    after it was renamed to the saved table. }
  if (State >= 1000) and (State < 1100) then
    seTableRenamePrim(DB, ffc_SavedIndexTableName, ffc_IndexTableName);

  { Delete temporary table if it did not replace system table. }
  if (State >= 400) and (State < 1100) then
    if not IsTableNameOpen(DB.Folder, ffc_TempIndexTableName) then
      seDeleteTable(DB, ffc_TempIndexTableName)
    else
      Result := DBIERR_TABLEOPEN;

  Dict.Free;
  DB.Free;
  Folder.Free;

  {remove the client}
  if State >= 100 then
    seClientRemovePrim(Client);

end;
{--------}
procedure TffServerEngine.CreateAdminUser(SaveToDisk : Boolean);
var
  Hash : TffWord32;
begin
  Hash := FFCalcShStrELFHash('flashfiler');
  Configuration.AddUser(ffc_AdminUserID,
                        'Administrator',
                        '',
                        Hash,
                        ffc_AdminRights);
  if SaveToDisk then
    WriteUserData;
end;
{====================================================================}


{===Initialization===================================================}
procedure InitializeUnit;
var
  i    : integer;
  Temp : string[5];
begin
  {a simple encryption to thwart casual hackers: 'ojneb' will appear
   in the EXE, not 'admin'}
  Temp := 'ojneb';
  ffc_AdminUserID[0] := #5;
  for i := 1 to 5 do
    ffc_AdminUserID[i] := char(ord(Temp[6-i]) - 1);
end;
{====================================================================}

initialization
  InitializeUnit;

end.

