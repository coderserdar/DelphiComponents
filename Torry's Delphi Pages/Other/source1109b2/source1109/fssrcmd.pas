{$I fsdefine.inc}

Unit fssrcmd;

Interface

Uses
  Classes,
  Windows,
  SysUtils,
  fsconst,
  fshash, {!!.05}
  fsllbase,
  fslleng,
  fsllcomm,
  fsllprot,
  fsnetmsg,
  fsdtmsgq,
  fssrbase,
  fssrbde,
  fssrintm,
  fssrtran,
  fsdictserveraccess,
  fsserverclass;

Type
  TFSHandler = Class(TfsIntermediateCommandHandler)
  Protected {private}
    fsExtraRecInfo: pfsExtraRecInfo;
    schSavedAddClientEvents: TfsThreadHash; {!!.05}

  Protected
    {client message handling}
    Procedure nmAcqTableLock(Var Msg: TfsDataMessage);
      Message fsnmAcqTableLock;
    Procedure nmAddIndex(Var Msg: TfsDataMessage);
      Message fsnmAddIndex;
    Procedure nmAddFileBLOB(Var Msg: TfsDataMessage);
      Message fsnmAddFileBLOB;
    Procedure nmBuildTable(Var Msg: TfsDataMessage);
      Message fsnmBuildTable;
    Procedure nmCheckSecureComms(Var Msg: TfsDataMessage);
      Message fsnmCheckSecureComms;
    Procedure nmClientSetTimeout(Var Msg: TfsDataMessage);
      Message fsnmClientSetTimeout;
    Procedure nmCreateBLOB(Var Msg: TfsDataMessage);
      Message fsnmCreateBLOB;
    Procedure nmCursorClone(Var Msg: TfsDataMessage);
      Message fsnmCursorClone;
    Procedure nmCursorClose(Var Msg: TfsDataMessage);
      Message fsnmCursorClose;
    Procedure nmCursorCompareBMs(Var Msg: TfsDataMessage);
      Message fsnmCursorCompareBMs;
    Procedure nmCursorCopyRecords(Var Msg: TfsDataMessage); {!!.02}
    Message fsnmCursorCopyRecords; {!!.02}
    Procedure nmCursorDeleteRecords(Var Msg: TfsDataMessage); {!!.06}
    Message fsnmCursorDeleteRecords; {!!.06}
    {Begin !!.03}
    Procedure nmCursorGetBLOBFreeSpace(Var Msg: TfsDataMessage);
      Message fsnmListBLOBFreeSpace;
    {End !!.03}
    Procedure nmCursorGetBookmark(Var Msg: TfsDataMessage);
      Message fsnmCursorGetBookmark;
    Procedure nmCursorOverrideFilter(Var Msg: TfsDataMessage);
      Message fsnmCursorOverrideFilter;
    Procedure nmCursorResetRange(Var Msg: TfsDataMessage);
      Message fsnmCursorResetRange;
    Procedure nmCursorRestoreFilter(Var Msg: TfsDataMessage);
      Message fsnmCursorRestoreFilter;
    Procedure nmCursorSetRange(Var Msg: TfsDataMessage);
      Message fsnmCursorSetRange;
    Procedure nmCursorSetTimeout(Var Msg: TfsDataMessage);
      Message fsnmCursorSetTimeout;
    Procedure nmCursorSetToBegin(Var Msg: TfsDataMessage);
      Message fsnmCursorSetToBegin;
    Procedure nmCursorSetToBookmark(Var Msg: TfsDataMessage);
      Message fsnmCursorSetToBookmark;
    Procedure nmCursorSetToCursor(Var Msg: TfsDataMessage);
      Message fsnmCursorSetToCursor;
    Procedure nmCursorSetToEnd(Var Msg: TfsDataMessage);
      Message fsnmCursorSetToEnd;
    Procedure nmCursorSetToKey(Var Msg: TfsDataMessage);
      Message fsnmCursorSetToKey;
    Procedure nmCursorSwitchToIndex(Var Msg: TfsDataMessage);
      Message fsnmCursorSwitchToIndex;
    Procedure nmCursorSetFilter(Var Msg: TfsDataMessage);
      Message fsnmCursorSetFilter;
    Procedure nmDatabaseAddAlias(Var Msg: TfsDataMessage);
      Message fsnmDatabaseAddAlias;
    Procedure nmDatabaseAliasList(Var Msg: TfsDataMessage);
      Message fsnmDatabaseAliasList;
    Procedure nmDatabaseChgAliasPath(Var Msg: TfsDataMessage);
      Message fsnmDatabaseChgAliasPath;
    Procedure nmDatabaseClose(Var Msg: TfsDataMessage);
      Message fsnmDatabaseClose;
    Procedure nmDatabaseDeleteAlias(Var Msg: TfsDataMessage);
      Message fsnmDatabaseDeleteAlias;
    Procedure nmDatabaseGetAliasPath(Var Msg: TfsDataMessage);
      Message fsnmDatabaseGetAliasPath;
    Procedure nmDatabaseGetFreeSpace(Var Msg: TfsDataMessage);
      Message fsnmDatabaseGetFreeSpace;
    Procedure nmDatabaseModifyAlias(Var Msg: TfsDataMessage);
      Message fsnmDatabaseModifyAlias;
    Procedure nmDatabaseOpen(Var Msg: TfsDataMessage);
      Message fsnmDatabaseOpen;
    Procedure nmDatabaseOpenNoAlias(Var Msg: TfsDataMessage);
      Message fsnmDatabaseOpenNoAlias;
    Procedure nmDatabaseSetTimeout(Var Msg: TfsDataMessage);
      Message fsnmDatabaseSetTimeout;
    Procedure nmDatabaseTableExists(Var Msg: TfsDataMessage);
      Message fsnmDatabaseTableExists;
    Procedure nmDatabaseTableList(Var Msg: TfsDataMessage);
      Message fsnmDatabaseTableList;
    Procedure nmDatabaseTableLockedExclusive(Var Msg: TfsDataMessage);
      Message fsnmDatabaseTableLockedExclusive;
    Procedure nmDeleteBLOB(Var Msg: TfsDataMessage);
      Message fsnmDeleteBLOB;
    Procedure nmDeleteTable(Var Msg: TfsDataMessage);
      Message fsnmDeleteTable;
    Procedure nmDetachServerJIC(Var Msg: TfsDataMessage);
      Message fsnmDetachServerJIC;
    Procedure nmDropIndex(Var Msg: TfsDataMessage);
      Message fsnmDropIndex;
    Procedure nmEmptyTable(Var Msg: TfsDataMessage);
      Message fsnmEmptyTable;
    Procedure nmEndTransaction(Var Msg: TfsDataMessage);
      Message fsnmEndTransaction;
    Procedure nmInTransaction(Var Msg: TfsDataMessage);
      Message fsnmInTransaction;
    Procedure nmTransactionCorrupted(Var Msg: TfsDataMessage);
      Message fsnmTransactionCorrupted;
    Procedure nmFreeBLOB(Var Msg: TfsDataMessage);
      Message fsnmFreeBLOB;
    Procedure nmGetTableAutoIncValue(Var Msg: TfsDataMessage);
      Message fsnmGetTableAutoIncValue;
    Procedure nmGetTableMaxRecordsValue(Var Msg: TfsDataMessage);
      Message fsnmGetTableMaxRecordsValue;
    Procedure nmGetTableTableFlagsValue(Var Msg: TfsDataMessage);
      Message fsnmGetTableTableFlagsValue;
    Procedure nmGetTableTablePasswordValue(Var Msg: TfsDataMessage);
      Message fsnmGetTableTablePasswordValue;
    Procedure nmGetTableTablePasswordRestValue(Var Msg: TfsDataMessage);
      Message fsnmGetTableTablePasswordRestValue;
    Procedure nmGetTableTableDBIDValue(Var Msg: TfsDataMessage);
      Message fsnmGetTableTableDBIDValue;

    Procedure nmGetBLOBLength(Var Msg: TfsDataMessage);
      Message fsnmGetBLOBLength;
    Procedure nmGetRebuildStatus(Var Msg: TfsDataMessage);
      Message fsnmGetRebuildStatus;
    Procedure nmGetServerDateTime(Var Msg: TfsDataMessage);
      Message fsnmGetServerDateTime;
    {begin !!.07}
    Procedure nmGetServerSystemTime(Var Msg: TfsDataMessage);
      Message fsnmGetServerSystemTime;
    Procedure nmGetServerGUID(Var Msg: TfsDataMessage);
      Message fsnmGetServerGUID;
    Procedure nmGetServerID(Var Msg: TfsDataMessage);
      Message fsnmGetServerID; {end !!.07}
    Procedure nmGetTableDictionary(Var Msg: TfsDataMessage);
      Message fsnmGetTableDictionary;
    Procedure nmGetTableRecCount(Var Msg: TfsDataMessage);
      Message fsnmGetTableRecCount;
    Procedure nmGetTableRecCountAsync(Var Msg: TfsDataMessage); {!!.07}
    Message fsnmGetTableRecCountAsync; {!!.07}
    {Begin !!.11}
    Procedure nmGetTableVersion(Var Msg: TfsDataMessage);
      Message fsnmGetTableVersion;
    {End !!.11}
    Procedure nmIsTableLocked(Var Msg: TfsDataMessage);
      Message fsnmIsTableLocked;
    {Begin !!.03}
    Procedure nmListBLOBSegments(Var Msg: TfsDataMessage);
      Message fsnmListBLOBSegments;
    {End !!.03}
    Procedure nmOpenTable(Var Msg: TfsDataMessage);
      Message fsnmOpenTable;
    Procedure nmPackTable(Var Msg: TfsDataMessage);
      Message fsnmPackTable;
    Procedure nmReadBLOB(Var Msg: TfsDataMessage);
      Message fsnmReadBLOB;
    Procedure nmRecordDelete(Var Msg: TfsDataMessage);
      Message fsnmRecordDelete;
    Procedure nmRecordDeleteBatch(Var Msg: TfsDataMessage);
      Message fsnmRecordDeleteBatch;
    Procedure nmRecordExtractKey(Var Msg: TfsDataMessage);
      Message fsnmRecordExtractKey;
    Procedure nmRecordGet(Var Msg: TfsDataMessage);
      Message fsnmRecordGet;
    Procedure nmRecordGetBatch(Var Msg: TfsDataMessage);
      Message fsnmRecordGetBatch;
    Procedure nmRecordGetForKey(Var Msg: TfsDataMessage);
      Message fsnmRecordGetForKey;
    Procedure nmRecordGetForKey2(Var Msg: TfsDataMessage);
      Message fsnmRecordGetForKey2;
    Procedure nmRecordGetNext(Var Msg: TfsDataMessage);
      Message fsnmRecordGetNext;
    Procedure nmRecordGetPrev(Var Msg: TfsDataMessage);
      Message fsnmRecordGetPrev;
    Procedure nmRecordGetSetPosition(Var Msg: TfsDataMessage);
      Message fsnmRecordGetSetPosition;
    Procedure nmRecordInsert(Var Msg: TfsDataMessage);
      Message fsnmRecordInsert;
    Procedure nmRecordInsertBatch(Var Msg: TfsDataMessage);
      Message fsnmRecordInsertBatch;
    Procedure nmRecordIsLocked(Var Msg: TfsDataMessage);
      Message fsnmRecordIsLocked;
    Procedure nmRecordModify(Var Msg: TfsDataMessage);
      Message fsnmRecordModify;
    Procedure nmRecordRelLock(Var Msg: TfsDataMessage);
      Message fsnmRecordRelLock;
    Procedure nmReindexTable(Var Msg: TfsDataMessage);
      Message fsnmReindexTable;
    Procedure nmRelTableLock(Var Msg: TfsDataMessage);
      Message fsnmRelTableLock;
    Procedure nmRenameTable(Var Msg: TfsDataMessage);
      Message fsnmRenameTable;
    Procedure nmRestructureTable(Var Msg: TfsDataMessage);
      Message fsnmRestructureTable;
    Procedure nmServerIsReadOnly(Var Msg: TfsDataMessage);
      Message fsnmServerIsReadOnly;
    {begin !!.07}
    Procedure nmServerStatistics(Var Msg: TfsDataMessage);
      Message fsnmServerStatistics;
    Procedure nmCmdHandlerStatistics(Var Msg: TfsDataMessage);
      Message fsnmCmdHandlerStatistics;
    Procedure nmTransportStatistics(Var Msg: TfsDataMessage);
      Message fsnmTransportStatistics; {end !!.07}
    Procedure nmSessionAdd(Var Msg: TfsDataMessage);
      Message fsnmSessionAdd;
    Procedure nmSessionClose(Var Msg: TfsDataMessage);
      Message fsnmSessionClose;
    Procedure nmSessionCloseInactiveTables(Var Msg: TfsDataMessage);
      Message fsnmSessionCloseInactTbl;
    Procedure nmSessionGetCurrent(Var Msg: TfsDataMessage);
      Message fsnmSessionGetCurrent;
    Procedure nmSessionSetCurrent(Var Msg: TfsDataMessage);
      Message fsnmSessionSetCurrent;
    Procedure nmSessionSetTimeout(Var Msg: TfsDataMessage);
      Message fsnmSessionSetTimeout;
    Procedure nmSetTableAutoIncValue(Var Msg: TfsDataMessage);
      Message fsnmSetTableAutoIncValue;
    Procedure nmSetTableMaxRecordsValue(Var Msg: TfsDataMessage);
      Message fsnmSetTableMaxRecordsValue;
    Procedure nmSetTableTableFlagsValue(Var Msg: TfsDataMessage);
      Message fsnmSetTableTableFlagsValue;
    Procedure nmSetTableTablePasswordValue(Var Msg: TfsDataMessage);
      Message fsnmSetTableTablePasswordValue;
    Procedure nmSetTableTablePasswordRestValue(Var Msg: TfsDataMessage);
      Message fsnmSetTableTablePasswordRestValue;
    Procedure nmSetTableTableDBIDValue(Var Msg: TfsDataMessage);
      Message fsnmSetTableTableDBIDValue;
    Procedure nmSQLAlloc(Var Msg: TfsDataMessage);
      Message fsnmSQLAlloc;
    Procedure nmSQLPrepare(Var Msg: TfsDataMessage);
      Message fsnmSQLPrepare;
    Procedure nmSQLExec(Var Msg: TfsDataMessage);
      Message fsnmSQLExec;
    Procedure nmSQLExecDirect(Var Msg: TfsDataMessage);
      Message fsnmSQLExecDirect;
    Procedure nmSQLSetParams(Var Msg: TfsDataMessage);
      Message fsnmSQLSetParams;
    Procedure nmSQLFree(Var Msg: TfsDataMessage);
      Message fsnmSQLFree;
    Procedure nmStartTransaction(Var Msg: TfsDataMessage);
      Message fsnmStartTransaction;
    Procedure nmStartTransactionWith(Var Msg: TfsDataMessage); {!!.10}
    Message fsnmStartTransactionWith; {!!.10}
    Procedure nmTruncateBLOB(Var Msg: TfsDataMessage);
      Message fsnmTruncateBLOB;
    Procedure nmWriteBLOB(Var Msg: TfsDataMessage);
      Message fsnmWriteBLOB;

    Procedure schDisposeRecord(Sender: TfsBaseHashTable; {!!.05}
      aData: Pointer); {!!.05}

    Procedure schOnAddClient(Listener: TFSBaseTransport;
      Const userID: TffName;
      Const timeout: Longint;
      Const clientVersion: Longint;
      Var passwordHash: TffWord32;
      Var aClientID: TffClientID;
      Var errorCode: TffResult;
      Var isSecure: boolean;
      Var aVersion: Longint;
      Var aRights: TffUserRights);
    { This method is called when the transport needs to establish a new
      client. }

    Procedure schOnRemoveClient(Listener: TFSBaseTransport;
      Const aClientID: TffClientID;
      Var errorCode: TffResult);
    { This method is called when the transport needs to remove an existing
      client. }

  Protected

    {State methods}
    Procedure scInitialize; Override;
    { This method is called when the command handler is to perform
      its initialization. }

    Procedure scPrepareForShutdown; Override;
    { This method is called when the command handler is to prepare for
      shutdown. }

    Procedure scShutdown; Override;
    { This method is called when the command handler is to stop processing
      requests. }

    Procedure scStartup; Override;
    { This method is called when the command handler is to start processing
      requests. }

  Public
    Constructor Create(aOwner: TComponent); Override;

    Destructor Destroy; Override;

    Procedure DefaultHandler(Var Message); Override;
    { If this command handler does not have a method specifically for
      a received message, the TObject.Dispatch method will pass the
      message to this method.  This method hands the message of to this
      class' ancestor so that default handling may be applied (i.e., see
      if the plugins or engine manager recognize the message. }

    Procedure FFAddDependent(ADependent: TFSSpecComp); Override; {!!.11}
    { This overridden method sets the OnAddclient and OnRemoveClient events
      of the registering transport. }

    Procedure Process(Msg: PFSDataMessage); Override;
    { This method is called by the transport in order to process a message.
      The message is first routed to the server engine.  If the server engine
      does not handle the message then it is forwarded to the plugin(s).  If
      a plugin does not handle the message, it is finally forwarded to the
      engine manager(s).}

  End;

Implementation

Uses
  ComObj,
  fssqlbas,
  fssrlock;

Const
  { Logging constants }
  csBlobNr = '  BLOBNr    %d:%d';
  csClientID = '  ClientID  %d';
  csCursorID = '  CursorID  %d';
  csErr = '*ERROR*  %x';

  {===TFSHandler==========================================}

Constructor TFSHandler.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);
  schSavedAddClientEvents := TfsThreadHash.Create(fsc_Size59); {!!.05}
  schSavedAddClientEvents.OnDisposeData := schDisposeRecord; {!!.05}
  If Not Assigned(fsExtraRecInfo) Then
    Begin
      ffgetmem(fsExtraRecInfo, SizeOf(TfsExtraRecInfo));
      fillchar(fsExtraRecInfo^, SizeOf(TfsExtraRecInfo), 0);
    End;
End;
{--------}

Destructor TFSHandler.Destroy;
Begin
  schSavedAddClientEvents.Clear; {!!.05}
  schSavedAddClientEvents.Free; {!!.05}
  schSavedAddClientEvents := Nil; {!!.05}
  If Assigned(fsExtraRecInfo) Then
    fffreemem(fsExtraRecInfo, SizeOf(TfsExtraRecInfo));
  Inherited Destroy;
End;
{--------}

Procedure TFSHandler.DefaultHandler(Var Message);
Begin
  { The server engine does not handle this message.  Hand it off to our
    ancestor class for default handling. }
  Inherited Process(@Message);
End;
{--------}

Procedure TFSHandler.Process(Msg: PFSDataMessage);
Begin
  Dispatch(Msg^);
  bchFreeMsg(Msg);
End;
{--------}

Procedure TFSHandler.nmAcqTableLock(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmAcqTableLockReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['AcqTableLock',
          Format(csClientID, [dmClientID]),
            Format(csCursorID, [CursorID]),
            Format('  LockType %d', [Byte(LockType)])]);

      Error := FServerEngine.TableLockAcquire(CursorID, LockType);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

      TFSBaseTransport.Reply(fsnmAcqTableLock, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmAddFileBLOB(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmAddFileBLOBRpy;
Begin
  With Msg, PfsnmAddFileBLOBReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['AddFileBLOB',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  FileName %s', [FileName])]);

      Error := FServerEngine.FileBLOBAdd(CursorID, FileName, Reply.BLOBNr);
      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt(csBLOBNr, [Reply.BLOBNr.iHigh, Reply.BLOBNr.iLow]); {!!.03}
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmAddFileBLOB, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmAddIndex(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmAddIndexReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['AddIndex',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format(csCursorID, [CursorID]),
            format('  TblName  [%s]', [TableName])]);

      Error := FServerEngine.TableAddIndex(DatabaseID, CursorID, TableName, IndexDesc);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmAddIndex, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmBuildTable(Var Msg: TfsDataMessage);
{ Input stream is expected to be:
      DatabaseId (longint)
      OverWrite  (Boolean)
      TableName  (TfsTableName)
      Dictionary (TFSInfoServerDict or TffDataDictionary)
      FieldMap   (one TffShStr for each field map entry; final entry
                  followed by a zero byte to signal end-of-list.  If
                  no field map is given, then a single zero byte must be
                  present
}
Var
  Error: TffResult;
  Stream: TMemoryStream;
  DatabaseID: Longint;
  OverWrite: Boolean;
  TableName: TfsTableName;
  Dictionary: TFSInfoServerDict;
  DictionaryStart: Longint;
Begin
  With Msg Do
    Begin
      Stream := TMemoryStream.Create;
      Stream.Write(dmData^, dmDataLen);
      Stream.Position := 0;
      Stream.Read(DatabaseID, SizeOf(DatabaseID));
      Stream.Read(OverWrite, SizeOf(OverWrite));
      Stream.Read(TableName, SizeOf(TableName));
      Dictionary := TFSInfoServerDict.Create(4096);
      Try
        DictionaryStart := Stream.Position;
        Dictionary.ReadFromStream(Stream);

        If FLogEnabled Then
          Begin
            ichLogAll(['BuildTable',
              format(csClientID, [dmClientID]),
                format('  DBase ID %d', [DatabaseID]),
                format('  OverWrite %d', [ord(OverWrite)]),
              format('  TblName  [%s]', [TableName])]);
            ichLogBlock('  Dictionary',
              Addr(PffByteArray(Stream.Memory)^[DictionaryStart]),
              Stream.Size - DictionaryStart);
          End;

        Error := FServerEngine.TableBuild(DatabaseID,
          OverWrite,
          TableName,
          False,
          Dictionary);
        If FLogEnabled Then
          ichLogFmt(csErr, [Error]);
        TFSBaseTransport.Reply(fsnmBuildTable, Error, Nil, 0);
      Finally
        Dictionary.Free;
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmCheckSecureComms(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg Do
    Begin
      If FLogEnabled Then
        ichLogAll(['CheckSecureComms',
          format(csClientID, [dmClientID])]);

      {Note: If we get this message the client's password must have been
             OK; the transport will hangup if the clientID is unknown.}
      Error := DBIERR_NONE;
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCheckSecureComms, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmClientSetTimeout(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmClientSetTimeoutReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['ClientSetTimeout',
          format(csClientID, [dmClientID]),
            format('  Timeout  %d', [Timeout])]);

      Error := FServerEngine.ClientSetTimeout(dmClientID, Timeout);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmClientSetTimeout, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmCreateBLOB(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmCreateBLOBRpy;
Begin
  With Msg, PfsnmCreateBLOBReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['CreateBLOB',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);

      Error := FServerEngine.BLOBCreate(CursorID, Reply.BLOBNr);
      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt(csBLOBNr, [Reply.BLOBNr.iHigh, Reply.BLOBNr.iLow]); {!!.03}
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmCreateBLOB, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmCursorClone(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  aNewCursorID: TffCursorID;
  Reply: TfsnmCursorCloneRpy;
Begin
  With Msg, PfsnmCursorCloneReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['CursorClone',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  OpenMode %d', [Byte(OpenMode)])]);

      Error := FServerEngine.CursorClone(CursorID, OpenMode, aNewCursorID);
      If (Error = 0) Then
        Reply.CursorID := aNewCursorID;
      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt(csCursorID, [Reply.CursorID]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmCursorClone, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmCursorClose(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorCloseReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['CursorClose',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);

      Error := FServerEngine.CursorClose(CursorID);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCursorClose, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmCursorCompareBMs(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  BM2: PffByteArray;
  Reply: TfsnmCursorCompareBMsRpy;
Begin
  With Msg, PfsnmCursorCompareBMsReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['CompareBookmarks',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  BM Size  %d', [BookmarkSize])]);

      BM2 := PffByteArray(PAnsiChar(@Bookmark1) + BookmarkSize);
      If FLogEnabled Then
        Begin
          ichLogBlock('  BM1', @Bookmark1, BookmarkSize);
          ichLogBlock('  BM2', BM2, BookmarkSize);
        End;
      Error := FServerEngine.CursorCompareBookmarks(CursorID, @Bookmark1, BM2, Reply.CompareResult);
      If (Reply.CompareResult < 0) Then
        Reply.CompareResult := -1
      Else If (Reply.CompareResult > 0) Then
        Reply.CompareResult := 1;
      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  Compare  %d', [Reply.CompareResult]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmCursorCompareBMs, Error, @Reply, sizeof(Reply));
    End;
End;
{Begin !!.02}
{--------}

Procedure TFSHandler.nmCursorCopyRecords(Var Msg: TfsDataMessage);
Var
  CopyBLOBsStr: String;
  Error: TffResult;
Begin
  With Msg, PfsnmCursorCopyRecordsReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        Begin
          If CopyBLOBs Then
            CopyBLOBsStr := 'yes'
          Else
            CopyBLOBsStr := 'no';
          ichLogAll(['CopyRecords',
            format(csClientID, [dmClientID]),
              format('  SrcCursorID  %d', [SrcCursorID]),
              format('  DestCursorID %d', [DestCursorID]),
              format('  Copy blobs   %s', [CopyBLOBsStr])]);
        End;

      Error := FServerEngine.CursorCopyRecords(SrcCursorID, DestCursorID, CopyBLOBs, CountPerTrans);
      TFSBaseTransport.Reply(fsnmCursorCopyRecords, Error, Nil, 0);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
    End;
End;
{End !!.02}
{Begin !!.06
{--------}

Procedure TFSHandler.nmCursorDeleteRecords(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorDeleteRecordsReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['DeleteRecords',
          format(csClientID, [dmClientID]),
            format('  CursorID  %d', [CursorID])]);

      Error := FServerEngine.CursorDeleteRecords(CursorID, CountPerTrans);
      TFSBaseTransport.Reply(fsnmCursorDeleteRecords, Error, Nil, 0);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
    End;
End;
{End !!.06}
{Begin !!.03}
{--------}

Procedure TFSHandler.nmCursorGetBLOBFreeSpace(Var Msg: TfsDataMessage);
Var
  aBuffer: pointer;
  Error: TffResult;
  aStream: TMemoryStream;
  StreamSize: Longint;
Begin
  With Msg, PfsnmGetBLOBFreeSpaceReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['CursorGetBLOBFreeSpace',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);

      aStream := TMemoryStream.Create;
      Try
        Error := FServerEngine.CursorListBLOBFreeSpace(CursorID, InMemory,
          aStream);
        StreamSize := aStream.Size;
        FFGetMem(aBuffer, StreamSize);
        aStream.Position := 0;
        aStream.Read(aBuffer^, StreamSize);

        If FLogEnabled And (Error = 0) Then
          ichLogBlock('  List', aStream.Memory, StreamSize);

        TFSBaseTransport.Reply(fsnmListBLOBFreeSpace, Error, aBuffer,
          StreamSize);
        FFFreeMem(aBuffer, StreamSize);

      Finally
        aStream.Free;
      End; {try..finally}

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
    End;
End;
{End !!.03}
{--------}

Procedure TFSHandler.nmCursorGetBookmark(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  BM: PffByteArray;
Begin
  With Msg, PfsnmCursorGetBookmarkReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['GetBookmark',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  BM Size  %d', [BookmarkSize])]);

      FFGetMem(BM, BookmarkSize);
      Try
        Error := FServerEngine.CursorGetBookmark(CursorID, BM);
        If FLogEnabled Then
          If (Error = 0) Then
            ichLogBlock('  Bookmark', BM, BookmarkSize);
        TFSBaseTransport.Reply(fsnmCursorGetBookmark, Error, BM, BookmarkSize);
      Finally
        FFFreeMem(BM, BookmarkSize);
      End; {try..finally}

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
    End;
End;
{--------}

Procedure TFSHandler.nmCursorOverrideFilter(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Expression: pCANExpr;
Begin
  With Msg, PfsnmCursorOverrideFilterReq(dmData)^ Do
    Begin
      Expression := pCANExpr(@ExprTree);
      If FLogEnabled Then
        Begin
          ichLogAll(['OverrideFilter',
            format('  ClientID %d', [dmClientID]),
              format('  CursorID %d', [CursorID]),
              format('  Timeout  %d', [Timeout])]);
          ichLogBlock('  Data', Expression, Expression^.iTotalSize);
        End;

      If Expression^.iTotalSize <= SizeOf(CANExpr) Then
        Expression := Nil;

      Error := FServerEngine.CursorOverrideFilter(CursorID, Expression, Timeout);
      TFSBaseTransport.Reply(fsnmCursorOverrideFilter, Error, Nil, 0);

      If FLogEnabled Then
        ichLogFmt('  *ERROR*  %x', [Error]);
    End;
End;
{--------}

Procedure TFSHandler.nmCursorResetRange(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorResetRangeReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['ResetRange',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);

      Error := FServerEngine.CursorResetRange(CursorID);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCursorResetRange, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmCursorRestoreFilter(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorRestoreFilterReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['RestoreFilter',
          format('  CursorID %d', [CursorID])]);

      Error := FServerEngine.CursorRestoreFilter(CursorID);
      TFSBaseTransport.Reply(fsnmCursorRestoreFilter, Error, Nil, 0);
      If FLogEnabled Then
        ichLogFmt('  *ERROR*  %x', [Error]);
    End;
End;
{-------}

Procedure TFSHandler.nmCursorSetRange(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  pKey1, pKey2: Pointer;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
Begin
  With Msg, PfsnmCursorSetRangeReq(dmData)^ Do
    Begin
      If KeyLen1 = 0 Then
        pKey1 := Nil
      Else
        pKey1 := @KeyData1;
      If KeyLen2 = 0 Then
        pKey2 := Nil
      Else
        pKey2 := PffByteArray(PAnsiChar(@KeyData1) + KeyLen1);
      If FLogEnabled Then
        Begin
          ichLogAll(['SetRange',
            format(csClientID, [dmClientID]),
              format(csCursorID, [CursorID]),
              format('  DirectKey %d', [Byte(DirectKey)]),
            format('  KeyLen1 %d', [KeyLen1]),
              format('  FieldCount1 %d', [FieldCount1]),
              format('  PartialLen1 %d', [PartialLen1]),
              format('  KeyIncl1 %d', [Byte(KeyIncl1)])]);
          ichLogBlock('  Key1', pKey1, KeyLen1);
          ichLogAll([format('  KeyLen2 %d', [KeyLen2]),
            format('  FieldCount2 %d', [FieldCount2]),
              format('  PartialLen2 %d', [PartialLen2]),
              format('  KeyIncl2 %d', [Byte(KeyIncl2)])]);
          ichLogBlock('  Key2', pKey2, KeyLen2);
        End;

      MsgSize := (2 * fsc_SubMsgHeaderSize);
      FFGetMem(MsgData, MsgSize);
      Try
        { do the SetRange First }
        SubMsg := PfssmHeader(MsgData);
        Error := FServerEngine.CursorSetRange(CursorID, DirectKey,
          FieldCount1, PartialLen1,
          pKey1, KeyIncl1,
          FieldCount2, PartialLen2,
          pKey2, KeyIncl2);

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmCursorSetRange,
          Error,
          nmdByteArray,
          Nil,
          0);
        If FLogEnabled Then
          ichLogAll([format(csErr, [Error]),
            'SetToBegin (multipart)']);

        Error := FServerEngine.CursorSetToBegin(CursorID);
        FFCreateSubMessage(SubMsg,
          fsnmCursorSetToBegin,
          Error,
          nmdByteArray,
          Nil,
          0);

        If FLogEnabled Then
          ichLogFmt(csErr, [Error]);

        TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);
      Finally
        FFFreeMem(MsgData, MsgSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmCursorSetTimeout(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorSetTimeoutReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['CursorSetTimeout',
          format(csCursorID, [CursorID]),
            format('  Timeout  %d', [Timeout])]);

      Error := FServerEngine.CursorSetTimeout(CursorID, Timeout);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCursorSetTimeout, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmCursorSetToBegin(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorSetToBeginReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['SetToBegin',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);

      Error := FServerEngine.CursorSetToBegin(CursorID);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCursorSetToBegin, Error, Nil, 0);

    End;
End;
{-------}

Procedure TFSHandler.nmCursorSetToBookmark(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorSetToBookmarkReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        Begin
          ichLogAll(['SetToBookmark',
            format(csClientID, [dmClientID]),
              format(csCursorID, [CursorID]),
              format('  BM Size  %d', [BookmarkSize])]);
          ichLogBlock('  Bookmark', @Bookmark, BookmarkSize);
        End;

      Error := FServerEngine.CursorSetToBookmark(CursorID, @Bookmark);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCursorSetToBookmark, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmCursorSetToCursor(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorSetToCursorReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['SetToCursor',
          format(csClientID, [dmClientID]),
            format('  DestCursor %d', [DestCursorID]),
            format('  SrcCursor  %d', [SrcCursorID])]);

      Error := FServerEngine.CursorSetToCursor(DestCursorID, SrcCursorID);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCursorSetToCursor, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmCursorSetToEnd(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorSetToEndReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SetToEnd',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);

      Error := FServerEngine.CursorSetToEnd(CursorID);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCursorSetToEnd, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmCursorSetToKey(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmCursorSetToKeyReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        Begin
          ichLogAll(['SetToKey',
            format(csClientID, [dmClientID]),
              format(csCursorID, [CursorID]),
              format('  Action   %d', [Byte(Action)]),
            format('  DrctKey  %d', [Byte(DirectKey)]),
            format('  FldCount %d', [FieldCount]),
              format('  PartLen  %d', [PartialLen]),
              format('  DataLen  %d', [KeyDataLen])]);
          ichLogBlock('  Data', @KeyData, KeyDataLen);
        End;

      Error := FServerEngine.CursorSetToKey(CursorID, Action, DirectKey,
        FieldCount, PartialLen, @KeyData);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCursorSetToKey, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmCursorSwitchToIndex(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
Begin
  With Msg, PfsnmCursorSwitchToIndexReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['SwitchToIndex',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  InxName  [%s]', [IndexName]),
            format('  InxNum   %d', [IndexNumber]),
            format('  PosnRec  %d', [Byte(PosnOnRec)])]);

      If Byte(PosnOnRec) <> 0 Then
        Begin
          Error := FServerEngine.CursorSwitchToIndex(CursorID,
            IndexName, IndexNumber,
            PosnOnRec);
          If FLogEnabled Then
            ichLogFmt(csErr, [Error]);

          TFSBaseTransport.Reply(fsnmCursorSwitchToIndex, Error, Nil, 0);

        End
      Else
        Begin
          MsgSize := (2 * fsc_SubMsgHeaderSize);
          FFGetMem(MsgData, MsgSize);
          Try
            { do the SwitchToIndex First }
            SubMsg := PfssmHeader(MsgData);
            Error := FServerEngine.CursorSwitchToIndex(CursorID,
              IndexName, IndexNumber,
              PosnOnRec);
            SubMsg := FFCreateSubMessage(SubMsg,
              fsnmCursorSwitchToIndex,
              Error,
              nmdByteArray,
              Nil,
              0);
            If FLogEnabled Then
              ichLogAll([format(csErr, [Error]),
                'SetToBegin (multipart)']);

            Error := FServerEngine.CursorSetToBegin(CursorID);
            FFCreateSubMessage(SubMsg,
              fsnmCursorSetToBegin,
              Error,
              nmdByteArray,
              Nil,
              0);
            If FLogEnabled Then
              ichLogFmt(csErr, [Error]);

            TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);
          Finally
            FFFreeMem(MsgData, MsgSize);
          End;
        End;
    End;
End;
{--------}

Procedure TFSHandler.nmCursorSetFilter(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Expression: pCANExpr;
Begin
  With Msg, PfsnmCursorSetFilterReq(dmData)^ Do
    Begin
      Expression := pCANExpr(@ExprTree);

      If FLogEnabled Then
        Begin
          ichLogAll(['SetFilter',
            format(csClientID, [dmClientID]),
              format(csCursorID, [CursorID]),
              format('  Timeout  %d', [Timeout])]);
          ichLogBlock('  Data', Expression, Expression^.iTotalSize);
        End;

      //    if Expression^.iTotalSize <= SizeOf(CANExpr) then                {Deleted !!.01}
      //      Expression:= nil;                                              {Deleted !!.01}

      Error := FServerEngine.CursorSetFilter(CursorID, Expression, Timeout);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmCursorSetFilter, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseAddAlias(Var Msg: TfsDataMessage);
{ Rewritten !!.11}
Var
  Error: TffResult;
Begin
  If Msg.dmDataLen = SizeOf(TfsnmOldDatabaseAddAliasReq) Then
    With Msg, PfsnmOldDatabaseAddAliasReq(dmData)^ Do
      Begin
        If FLogEnabled Then
          ichLogAll(['DatabaseAddAlias - Old',
            format(csClientID, [dmClientID]),
              format('  Alias    [%s]', [Alias]),
              format('  Path     [%s]', [Path])]);

        Error := FServerEngine.DatabaseAddAlias(Alias,
          Path,
          False,
          dmClientID);
      End { with }
  Else
    With Msg, PfsnmDatabaseAddAliasReq(dmData)^ Do
      Begin
        If FLogEnabled Then
          ichLogAll(['DatabaseAddAlias',
            format(csClientID, [dmClientID]),
              format('  Alias     [%s]', [Alias]),
              format('  Path      [%s]', [Path]),
              format('  Checkdisk [%d]', [Byte(CheckDisk)])]); {!!.13}

        Error := FServerEngine.DatabaseAddAlias(Alias,
          Path,
          CheckDisk,
          dmClientID);
      End; { with }

  If FLogEnabled Then
    ichLogFmt(csErr, [Error]);
  TFSBaseTransport.Reply(fsnmDatabaseAddAlias, Error, Nil, 0);
End;
{--------}

Procedure TFSHandler.nmDatabaseAliasList(Var Msg: TfsDataMessage);
Var
  aBuffer: pointer;
  aList: TList;
  anAlias: PffAliasDescriptor;
  Error: TffResult;
  Index: Longint;
  Stream: TMemoryStream;
  StreamSize: Longint;
Begin
  With Msg Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseAliasList',
          format(csClientID, [dmClientID])]);

      Stream := TMemoryStream.Create;
      aList := TList.Create;
      Try
        Error := FServerEngine.DatabaseAliasList(aList, dmClientID);
        {Write the list of alias information to the stream. }
        For Index := 0 To pred(aList.Count) Do
          Begin
            anAlias := PffAliasDescriptor(aList.Items[Index]);
            Stream.WriteBuffer(anAlias^, sizeOf(TffAliasDescriptor));
          End;

        { Free the returned items. }
        For Index := pred(aList.Count) Downto 0 Do
          Begin
            anAlias := PffAliasDescriptor(aList.Items[Index]);
            FFFreeMem(anAlias, sizeOf(TffAliasDescriptor));
          End;

        StreamSize := Stream.Size;
        FFGetMem(aBuffer, StreamSize);
        Stream.Position := 0;
        Stream.Read(aBuffer^, StreamSize);

        If FLogEnabled And (Error = 0) Then
          ichLogBlock('  List', Stream.Memory, StreamSize);

        TFSBaseTransport.Reply(fsnmDatabaseAliasList, Error, aBuffer, StreamSize);
        FFFreeMem(aBuffer, StreamSize);

      Finally
        Stream.Free;
        aList.Free;
      End; {try..finally}

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseChgAliasPath(Var Msg: TfsDataMessage);
{Rewritten !!.11}
Var
  Error: TffResult;
Begin
  If Msg.dmDataLen = SizeOf(TfsnmOldDatabaseChgAliasPathReq) Then
    With Msg, PfsnmOldDatabaseChgAliasPathReq(dmData)^ Do
      Begin

        If FLogEnabled Then
          ichLogAll(['DatabaseChgAliasPath - Old',
            format(csClientID, [dmClientID]),
              format('  Alias    [%s]', [Alias]),
              format('  NewPath  [%s]', [NewPath])]);

        Error := FServerEngine.DatabaseChgAliasPath(Alias,
          NewPath,
          False,
          dmClientID);
      End { with }
  Else
    With Msg, PfsnmDatabaseChgAliasPathReq(dmData)^ Do
      Begin

        If FLogEnabled Then
          ichLogAll(['DatabaseChgAliasPath',
            format(csClientID, [dmClientID]),
              format('  Alias     [%s]', [Alias]),
              format('  NewPath   [%s]', [NewPath]),
              format('  Checkdisk [%s]', [Byte(CheckDisk)])]);

        Error := FServerEngine.DatabaseChgAliasPath(Alias,
          NewPath,
          CheckDisk,
          dmClientID);
      End; { with }

  If FLogEnabled Then
    ichLogFmt(csErr, [Error]);
  TFSBaseTransport.Reply(fsnmDatabaseChgAliasPath, Error, Nil, 0);

End;
{--------}

Procedure TFSHandler.nmDatabaseClose(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmDatabaseCloseReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseClose',
          format(csClientID, [dmClientID]),
            format('  DBaseID  %d', [DatabaseID])]);

      Error := FServerEngine.DatabaseClose(DatabaseID);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmDatabaseClose, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseDeleteAlias(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmDatabaseDeleteAliasReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseDeleteAlias',
          format(csClientID, [dmClientID]),
            format('  Alias    [%s]', [Alias])]);

      Error := FServerEngine.DatabaseDeleteAlias(Alias, dmClientID);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmDatabaseDeleteAlias, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseGetAliasPath(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmDatabaseGetAliasPathRpy;
  Path: TffPath;
Begin
  With Msg, PfsnmDatabaseGetAliasPathReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseGetAliasPath', {!!.10}
          Format(csClientID, [dmClientID]),
            Format('    Alias  %s', [Alias])]);

      Error := FServerEngine.DatabaseGetAliasPath(Alias, Path, dmClientID);

      If (Error = 0) Then
        Reply.Path := Path;

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('      Path %s', [Reply.Path]); {!!.02}
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmDatabaseGetAliasPath, Error, @Reply, SizeOf(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseGetFreeSpace(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmDatabaseGetFreeSpaceRpy;
  FreeSpace: Int64;
Begin
  With Msg, PfsnmDatabaseGetFreeSpaceReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseGetFreespace',
          Format(csClientID, [dmClientID]),
            Format('  DBaseID  %d', [DatabaseID])]);

      Error := FServerEngine.DatabaseGetFreeSpace(DatabaseID, FreeSpace);

      If (Error = 0) Then
        Reply.FreeSpace := FreeSpace;

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  Free Space %d', [Reply.FreeSpace]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmDatabaseGetFreeSpace, Error, @Reply, SizeOf(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseModifyAlias(Var Msg: TfsDataMessage);
{Rewritten !!.11}
Var
  Error: TffResult;
Begin
  If Msg.dmDataLen = SizeOf(TfsnmOldDatabaseModifyAliasReq) Then
    With Msg, PfsnmOldDatabaseModifyAliasReq(dmData)^ Do
      Begin
        If FLogEnabled Then
          ichLogAll(['DatabaseModifyAlias - Old',
            format(csClientID, [ClientID]),
              format('  Alias Name  [%s]', [Alias]),
              format('  New Name    [%s]', [NewName]),
              format('  New Path    [%s]', [NewPath])]);

        Error := FServerEngine.DatabaseModifyAlias(ClientID,
          Alias,
          NewName,
          NewPath,
          False);
      End { while }
  Else
    With Msg, PfsnmDatabaseModifyAliasReq(dmData)^ Do
      Begin
        If FLogEnabled Then
          ichLogAll(['DatabaseModifyAlias',
            format(csClientID, [ClientID]),
              format('  Alias Name  [%s]', [Alias]),
              format('  New Name    [%s]', [NewName]),
              format('  New Path    [%s]', [NewPath]),
              format('  Check Disk  [%s]', [Byte(CheckDisk)])]);

        Error := FServerEngine.DatabaseModifyAlias(ClientID,
          Alias,
          NewName,
          NewPath,
          CheckDisk);
      End; { while }

  If FLogEnabled Then
    ichLogFmt(csErr, [Error]);
  TFSBaseTransport.Reply(fsnmDatabaseModifyAlias, Error, Nil, 0);
End;
{--------}

Procedure TFSHandler.nmDetachServerJIC(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg Do
    Begin
      If FLogEnabled Then
        ichLogAll(['DetachServer - just in case',
          format(csClientID, [dmClientID])]);
      Error := FServerEngine.ClientRemove(dmClientID);
      { No response necessary. }

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseOpen(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  aDatabaseID: TffDatabaseID;
  Reply: TfsnmDatabaseOpenRpy;
Begin
  With Msg, PfsnmDatabaseOpenReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseOpen',
          format(csClientID, [dmClientID]),
            format('  Alias    [%s]', [Alias]),
            format('  OpenMode %d', [Byte(OpenMode)]),
          format('  ShrMode  %d', [Byte(ShareMode)]),
          format('  Timeout  %d', [Timeout])]);

      Error := FServerEngine.DatabaseOpen(dmClientID,
        Alias,
        OpenMode,
        ShareMode,
        Timeout,
        TransIsolation,
        TransLocking,
        aDatabaseID);
      If (Error = 0) Then
        Reply.DatabaseID := aDatabaseID;

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  DBase ID %d', [Reply.DatabaseID]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmDatabaseOpen, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseOpenNoAlias(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  aDatabaseID: TffDatabaseID;
  Reply: TfsnmDatabaseOpenNoAliasRpy;
Begin
  With Msg, PfsnmDatabaseOpenNoAliasReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseOpenNoAlias',
          format(csClientID, [dmClientID]),
            format('  Path     [%s]', [Path]),
            format('  OpenMode %d', [Byte(OpenMode)]),
          format('  ShrMode  %d', [Byte(ShareMode)]),
          format('  Timeout  %d', [Timeout])]);

      Error := FServerEngine.DatabaseOpenNoAlias(dmClientID,
        Path,
        OpenMode,
        ShareMode,
        Timeout,
        TransIsolation,
        TransLocking,
        aDatabaseID);
      If (Error = 0) Then
        Reply.DatabaseID := aDatabaseID;

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  DBase ID %d', [Reply.DatabaseID]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmDatabaseOpenNoAlias, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseSetTimeout(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmDatabaseSetTimeoutReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseSetTimeout',
          format('  DatabaseID %d', [DatabaseID]),
            format('  Timeout  %d', [Timeout])]);

      Error := FServerEngine.DatabaseSetTimeout(DatabaseID, Timeout);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

      TFSBaseTransport.Reply(fsnmDatabaseSetTimeout, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseTableExists(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmDatabaseTableExistsRpy;
Begin
  With Msg, PfsnmDatabaseTableExistsReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseTableExists',
          format('  DatabaseID %d', [DatabaseID]),
            format('  TblName  %s', [TableName])]); {!!.01}

      Error := FServerEngine.DatabaseTableExists(DatabaseID, TableName, Reply.Exists);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

      TFSBaseTransport.Reply(fsnmDatabaseTableExists, Error, @Reply, sizeof(Reply)); {!!.01}
    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseTableList(Var Msg: TfsDataMessage);
Var
  aBuffer: Pointer;
  aList: TList;
  aTable: PffTableDescriptor;
  Error: TffResult;
  Index: Longint;
  Stream: TMemoryStream;
  StreamSize: Longint;
Begin
  With Msg, PfsnmDatabaseTableListReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseTableList',
          format(csClientID, [dmClientID]),
            format('  DBaseID  %d', [DatabaseID]),
            format('  Mask     [%s]', [Mask])]);

      aList := TList.Create;
      Stream := TMemoryStream.Create;
      Try
        Error := FServerEngine.DatabaseTableList(DatabaseID, Mask, aList);
        { Write the table descriptions to the stream. }
        For Index := 0 To pred(aList.Count) Do
          Begin
            aTable := PffTableDescriptor(aList.Items[Index]);
            Stream.WriteBuffer(aTable^, sizeOf(TfsTableDescriptor));
          End;

        { Free the table descriptions. }
        For Index := pred(aList.Count) Downto 0 Do
          Begin
            aTable := PffTableDescriptor(aList.Items[Index]);
            FFFreeMem(aTable, sizeOf(TfsTableDescriptor));
          End;

        StreamSize := Stream.Size;
        FFGetMem(aBuffer, StreamSize);
        Stream.Position := 0;
        Stream.Read(aBuffer^, StreamSize);

        If FLogEnabled And (Error = 0) Then
          ichLogBlock('  List', Stream.Memory, StreamSize);
        TFSBaseTransport.Reply(fsnmDatabaseTableList, Error, aBuffer, StreamSize);
        FFFreeMem(aBuffer, StreamSize);

      Finally
        aList.Free;
        Stream.Free;
      End;

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

    End;
End;
{--------}

Procedure TFSHandler.nmDatabaseTableLockedExclusive(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmDatabaseTableLockedExclusiveRpy;
Begin
  With Msg, PfsnmDatabaseTableLockedExclusiveReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DatabaseTableExists',
          format('  DatabaseID %d', [DatabaseID]),
            format('  TblName  %d', [TableName])]);

      Error := FServerEngine.DatabaseTableLockedExclusive(DatabaseID, TableName, Reply.Locked);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

      TFSBaseTransport.Reply(fsnmDatabaseTableLockedExclusive, Error, Nil, 0);
    End;
End;

{--------}

Procedure TFSHandler.nmDeleteBLOB(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmDeleteBLOBReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DeleteBLOB',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format(csBLOBNr, [BLOBNr.iHigh, BLOBNr.iLow])]); {!!.03}

      Error := FServerEngine.BLOBDelete(CursorID, BLOBNr);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmDeleteBLOB, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmDeleteTable(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmDeleteTableReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DeleteTable',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format('  TblName  [%s]', [TableName])]);

      Error := FServerEngine.TableDelete(DatabaseID, TableName);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmDeleteTable, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmDropIndex(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmDropIndexReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['DropIndex',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format(csCursorID, [CursorID]),
            format('  TblName  [%s]', [TableName]),
            format('  InxName  [%s]', [IndexName]),
            format('  IndexID  [%d]', [IndexNumber])]);

      Error := FServerEngine.TableDropIndex(DatabaseID, CursorID, TableName,
        IndexName, IndexNumber);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmDropIndex, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmEmptyTable(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmEmptyTableReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['EmptyTable',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format(csCursorID, [CursorID]),
            format('  TblName  [%s]', [TableName])]);

      Error := FServerEngine.TableEmpty(DatabaseID, CursorID, TableName);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmEmptyTable, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmEndTransaction(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmEndTransactionReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['EndTransaction',
          format('  ClientID    %d', [dmClientID]),
            format('  Database ID %d', [DatabaseID]),
            format('  Commit?     %d', [Byte(ToBeCommitted)])]);

      If ToBeCommitted Then
        Error := FServerEngine.TransactionCommit(DatabaseID, RemoveFiles)
      Else
        Error := FServerEngine.TransactionRollback(DatabaseID);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmEndTransaction, Error, Nil, 0);

    End;
End;

Procedure TFSHandler.nmInTransaction(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmInTransactionReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['InTransaction',
          format('  ClientID    %d', [dmClientID]),
            format('  Database ID %d', [DatabaseID])]);

      Error := FServerEngine.InTransaction(DatabaseID, TransLevel);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmInTransaction, Error, Nil, 0);

    End;
End;

Procedure TFSHandler.nmTransactionCorrupted(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmTransactionCorruptedReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['TransactionCorrupted',
          format('  ClientID    %d', [dmClientID]),
            format('  Database ID %d', [DatabaseID])]);

      Error := FServerEngine.TransactionCorrupted(DatabaseID);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmTransactionCorrupted, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmFreeBLOB(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmFreeBLOBReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['FreeBLOB',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format(csBlobNr, [BLOBNr.iHigh, BLOBNr.iLow]), {!!.03}
          format('  Read-Only %d', [Byte(ReadOnly)])]);

      Error := FServerEngine.BLOBFree(CursorID, BLOBNr, ReadOnly);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmFreeBLOB, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmGetTableAutoIncValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetTableAutoIncValueRpy;
Begin
  With Msg, PfsnmGetTableAutoIncValueReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['GetTableAutoIncValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);
      Error := FServerEngine.TableGetAutoInc(CursorID, Reply.AutoInc64Value, Reply.AutoInc64StepValue);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  AutoInc  %d', [Reply.AutoInc64Value]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetTableAutoIncValue, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmGetTableMaxRecordsValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetTableMaxRecordsValueRpy;
Begin
  With Msg, PfsnmGetTableMaxRecordsValueReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['GetTableMaxRecordsValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);
      Error := FServerEngine.TableGetMaxRecords(CursorID, Reply.MaxRecords);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  MaxRecords  %d', [Reply.MaxRecords]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetTableMaxRecordsValue, Error, @Reply, sizeof(Reply));
    End;
End;

Procedure TFSHandler.nmGetTableTableFlagsValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetTableTableFlagsValueRpy;
Begin
  With Msg, PfsnmGetTableTableFlagsValueReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['GetTableTableFlagsValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);
      Error := FServerEngine.TableGetTableFlags(CursorID, Reply.TableFlags);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  TableFlags  %d', [Reply.TableFlags]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetTableTableFlagsValue, Error, @Reply, sizeof(Reply));
    End;
End;

Procedure TFSHandler.nmGetTableTablePasswordValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetTableTablePasswordValueRpy;
Begin
  With Msg, PfsnmGetTableTablePasswordValueReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['GetTableTablePasswordValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);
      Error := FServerEngine.TableGetTablePassword(CursorID, Reply.TablePassword);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  TablePassword  %d', [Reply.TablePassword]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetTableTablePasswordValue, Error, @Reply, sizeof(Reply));
    End;
End;

Procedure TFSHandler.nmGetTableTablePasswordRestValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetTableTablePasswordRestValueRpy;
Begin
  With Msg, PfsnmGetTableTablePasswordRestValueReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['GetTableTablePasswordRestValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);
      Error := FServerEngine.TableGetTablePasswordRest(CursorID, Reply.TablePassword);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  TablePassword  %d', [Reply.TablePassword]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetTableTablePasswordRestValue, Error, @Reply, sizeof(Reply));
    End;
End;

Procedure TFSHandler.nmGetTableTableDBIDValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetTableTableDBIDValueRpy;
Begin
  With Msg, PfsnmGetTableTableDBIDValueReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['GetTableTableDBIDValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);
      Error := FServerEngine.TableGetTableDBID(CursorID, Reply.TableDBID);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  TableDBID  %d', [Reply.TableDBID]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetTableTableDBIDValue, Error, @Reply, sizeof(Reply));
    End;
End;

Procedure TFSHandler.nmGetBLOBLength(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetBLOBLengthRpy;
Begin
  With Msg, PfsnmGetBLOBLengthReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['GetBLOBLength',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format(csBLOBNr, [BLOBNr.iHigh, BLOBNr.iLow])]); {!!.03}

      Error := FServerEngine.BLOBGetLength(CursorID, BLOBNr, Reply.BLOBLength);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  BLOBLen  %d', [Reply.BLOBLength]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetBLOBLength, Error, @Reply, sizeof(Reply));

    End;
End;
{--------}

Procedure TFSHandler.nmGetRebuildStatus(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetRebuildStatusRpy;
Begin
  With Msg, PfsnmGetRebuildStatusReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['GetRebuildStatus',
          format(csClientID, [dmClientID]),
            format('  RebldID  %d', [RebuildID])]);

      Error := FServerEngine.RebuildGetStatus(RebuildID, dmClientID, Reply.IsPresent, Reply.Status);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  IsThere  %d', [ord(Reply.IsPresent)]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetRebuildStatus, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmGetServerDateTime(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetServerDateTimeRpy;
Begin
  With Msg Do
    Begin

      If FLogEnabled Then
        ichLog('GetServerDateTime');

      Reply.ServerNow := Now;
      Error := 0;

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  DateTime %s', [DateTimeToStr(Reply.ServerNow)]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetServerDateTime, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}{begin !!.07}

Procedure TFSHandler.nmGetServerSystemTime(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetServerSystemTimeRpy;
Begin
  With Msg Do
    Begin

      If FLogEnabled Then
        ichLog('GetServerSystemTime');

      Error := FServerEngine.GetServerSystemTime(Reply.ServerNow);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  SystemTime %s', [DateTimeToStr(SystemTimeToDateTime(Reply.ServerNow))]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetServerSystemTime, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmGetServerGUID(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetServerGUIDRpy;
Begin
  With Msg Do
    Begin

      If FLogEnabled Then
        ichLog('GetServerGUID');

      Error := FServerEngine.GetServerGUID(Reply.GUID);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  GUID %s', [GuidToString(Reply.GUID)]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetServerGUID, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}{end !!.07}

Procedure TFSHandler.nmGetServerID(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetServerIDRpy;
Begin
  With Msg Do
    Begin

      If FLogEnabled Then
        ichLog('GetServerID');

      Error := FServerEngine.GetServerID(Reply.UniqueID);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  UniqueID %s', [GuidToString(Reply.UniqueID)]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetServerID, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}{end !!.07}

Procedure TFSHandler.nmGetTableDictionary(Var Msg: TfsDataMessage);
Var
  aBuffer: Pointer;
  Error: TffResult;
  Stream: TMemoryStream;
  StreamSize: Longint;
Begin
  With Msg, PfsnmGetTableDictionaryReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['GetTableDictionary',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format('  TblName  [%s]', [TableName])]);

      Stream := TMemoryStream.Create;
      Try
        Error := FServerEngine.TableGetDictionary(DatabaseID,
          TableName,
          False,
          Stream);
        StreamSize := Stream.Size;
        FFGetMem(aBuffer, StreamSize);
        Stream.Position := 0;
        Stream.Read(aBuffer^, StreamSize);

        If FLogEnabled And (Error = 0) Then
          ichLogBlock('  Dictionary', Stream.Memory, Stream.Size);
        TFSBaseTransport.Reply(fsnmGetTableDictionary, Error, aBuffer, StreamSize);
        FFFreeMem(aBuffer, StreamSize);

      Finally
        Stream.Free;
      End; {try..finally}

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

    End;
End;
{--------}

Procedure TFSHandler.nmGetTableRecCount(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetTableRecCountRpy;
Begin
  With Msg, PfsnmGetTableRecCountReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['GetTableRecCount',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);

      Error := FServerEngine.TableGetRecCount(CursorID, Reply.RecCount);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  Count   %d', [Byte(Reply.RecCount)]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetTableRecCount, Error, @Reply, sizeof(Reply));

    End;
End;
{Begin !!.07}
{--------}

Procedure TFSHandler.nmGetTableRecCountAsync(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetTableRecCountAsyncRpy;
Begin
  With Msg, PfsnmGetTableRecCountAsyncReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['GetTableRecCountAsync',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID])]);

      Error := FServerEngine.TableGetRecCountAsync(CursorID, Reply.RebuildID);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('RebuildID %d', [Reply.RebuildID]);
          ichLogFmt(csErr, [Error]);
        End; { if }

      TFSBaseTransport.Reply(fsnmGetTableRecCountAsync, Error, @Reply,
        SizeOf(Reply));
    End; { with }
End;
{End !!.07}
{Begin !!.11}
{--------}

Procedure TFSHandler.nmGetTableVersion(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmGetTableVersionRpy;
Begin
  With Msg, PfsnmGetTableVersionReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['GetTableVersion',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format('  TblName  [%s]', [TableName])]);

      Error := FServerEngine.TableVersion(DatabaseID, TableName, Reply.Version);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  Version  %d', [Reply.Version]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmGetTableVersion, Error, @Reply, sizeof(Reply));

    End;
End;
{End !!.11}
{--------}

Procedure TFSHandler.nmIsTableLocked(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmIsTableLockedRpy;
Begin
  With Msg, PfsnmIsTableLockedReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['IsTableLocked',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  LockType %d', [Byte(LockType)])]);

      Error := FServerEngine.TableIsLocked(CursorID, LockType, Reply.IsLocked);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  Locked?  %d', [Byte(Reply.IsLocked)]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmIsTableLocked, Error, @Reply, sizeof(Reply));

    End;
End;
{Begin !!.03}
{--------}

Procedure TFSHandler.nmListBLOBSegments(Var Msg: TfsDataMessage);
Var
  aBuffer: pointer;
  Error: TffResult;
  aStream: TMemoryStream;
  StreamSize: Longint;
Begin
  With Msg, PfsnmListBLOBSegmentsReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['ListBLOBSegments',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format(csBLOBNr, [BLOBNr.iHigh, BLOBNr.iLow])]);

      aStream := TMemoryStream.Create;
      Try
        Error := FServerEngine.BLOBListSegments(CursorID, BLOBNr, aStream);
        StreamSize := aStream.Size;
        FFGetMem(aBuffer, StreamSize);
        aStream.Position := 0;
        aStream.Read(aBuffer^, StreamSize);

        If FLogEnabled And (Error = 0) Then
          ichLogBlock('  List', aStream.Memory, StreamSize);

        TFSBaseTransport.Reply(fsnmListBLOBSegments, Error, aBuffer,
          StreamSize);
        FFFreeMem(aBuffer, StreamSize);

      Finally
        aStream.Free;
      End; {try..finally}

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
    End;
End;
{End !!.03}
{--------}

Procedure TFSHandler.nmOpenTable(Var Msg: TfsDataMessage);
Var
  aBuffer: pointer;
  CursorID: TffCursorID;
  Error: TffResult;
  Stream: TMemoryStream;
  StreamSize: Longint;
Begin
  With Msg, PfsnmOpenTableReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['OpenTable',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format('  TblName  [%s]', [TableName]),
            format('  InxName  [%s]', [IndexName]),
            format('  InxNum   %d', [IndexNumber]),
            format('  OpenMode %d', [Byte(OpenMode)]),
          format('  Timeout  %d', [Timeout]), {!!.06}
          format('  ShrMode  %d', [Byte(ShareMode)])]);

      Stream := TMemoryStream.Create;
      Try
        Error := FServerEngine.TableOpen(DatabaseID,
          TableName,
          False,
          IndexName,
          IndexNumber,
          OpenMode,
          ShareMode,
          Timeout,
          CursorID,
          Stream);
        { Note that TFSServer.TableOpen writes the cursorID to the
          stream. }
        If Stream.Size > 0 Then
          Begin
            StreamSize := Stream.Size;
            FFGetMem(aBuffer, StreamSize);
            Stream.Position := 0;
            Stream.Read(aBuffer^, StreamSize);
          End
        Else
          Begin
            aBuffer := Nil;
            StreamSize := 0;
          End;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Dictionary, etc', Stream.Memory, StreamSize);
            ichLogFmt(csErr, [Error]);
          End;

        TFSBaseTransport.Reply(fsnmOpenTable, Error, aBuffer, StreamSize);

        If assigned(aBuffer) Then
          FFFreeMem(aBuffer, StreamSize);

      Finally
        Stream.Free;
      End; {try..finally}

      //    if FLogEnabled then         {duplicated from a few lines above}  {!!.06}
      //      ichLogFmt(csErr, [Error]);                                     {!!.06}

    End;
End;
{--------}

Procedure TFSHandler.nmPackTable(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmPackTableRpy;
Begin
  With Msg, PfsnmPackTableReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['PackTable',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format('  TblName  [%s]', [TableName])]);

      Error := FServerEngine.TablePack(DatabaseID, TableName, Reply.RebuildID, UndeleteRecords, OnlyDeleted);

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  RbldID   %d', [Reply.RebuildID]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmPackTable, Error, @Reply, sizeof(Reply));

    End;
End;
{--------}

Procedure TFSHandler.nmReadBLOB(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: PfsnmReadBLOBRpy;
  RpyLen: Longint;
Begin
  With Msg, PfsnmReadBLOBReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['ReadBLOB',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format(csBlobNr, [BLOBNr.iLow, BLOBNr.iHigh]),
            format('  Offset %d', [Offset]),
            format('  Len %d', [Len])]);

      FFGetMem(Reply, Len + sizeof(Longint));
      Try
        Error := FServerEngine.BLOBRead(CursorID, FieldNo, BLOBNr, Offset, Len,
          Reply^.BLOB, Reply^.BytesRead);
        If Error = 0 Then
          RpyLen := Reply^.BytesRead + sizeof(Longint)
        Else
          RpyLen := 0;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              Begin
                ichLogFmt('  BytesRead %d', [Reply^.BytesRead]);
                ichLogBlock('  BLOB', @Reply^.BLOB, Reply^.BytesRead);
              End;
            ichLogFmt(csErr, [Error]);
          End;
        TFSBaseTransport.Reply(fsnmReadBLOB, Error, Reply, RpyLen);

      Finally
        FFFreeMem(Reply, Len + sizeof(Longint));
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordDelete(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  pData: PffByteArray;
Begin
  With Msg, PfsnmRecordDeleteReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordDelete',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  RecLen   %d', [RecLen])]);

      If (RecLen <> 0) Then
        FFGetMem(pData, RecLen)
      Else
        pData := Nil;
      Try
        Error := FServerEngine.RecordDelete(CursorID, pData);

        If FLogEnabled And (Error = 0) And (RecLen <> 0) Then
          ichLogBlock('  Record', pData, RecLen);
        TFSBaseTransport.Reply(fsnmRecordDelete, Error, pData, RecLen);

      Finally
        If (RecLen <> 0) Then
          FFFreeMem(pData, RecLen);
      End; {try..finally}

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

    End;
End;
{--------}

Procedure TFSHandler.nmRecordDeleteBatch(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: PffLongintArray;
  DataSize: Longint;
Begin
  With Msg, PfsnmRecordDeleteBatchReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordDeleteBatch',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('   BMCount %d', [BMCount]),
            format('   BMLen   %d', [BMLen])]);

      DataSize := BMCount * sizeof(Longint);
      FFGetMem(Reply, DataSize);
      Try
        Error := FServerEngine.RecordDeleteBatch(CursorID, BMCount, BMLen,
          PffByteArray(@BMArray),
          Reply);
        If FLogEnabled Then
          ichLogFmt(csErr, [Error]);
        TFSBaseTransport.Reply(fsnmRecordDeleteBatch, Error, Reply, DataSize);

      Finally
        FFFreeMem(Reply, DataSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordExtractKey(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  pKey: PffByteArray;
Begin
  With Msg, PfsnmRecordExtractKeyReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordExtractKey',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  KeyLen   %d', [KeyLen]),
            format('  ForCurrRec %d', [ord(ForCurrentRecord)])]);

      If (KeyLen <> 0) Then
        FFGetMem(pKey, KeyLen)
      Else
        pKey := Nil;
      Try
        If ForCurrentRecord Then
          Error := FServerEngine.RecordExtractKey(CursorID, Nil, pKey)
        Else
          Error := FServerEngine.RecordExtractKey(CursorID, @Data, pKey);

        If FLogEnabled And (Error = 0) Then
          ichLogBlock('  Key', pKey, KeyLen);
        TFSBaseTransport.Reply(fsnmRecordExtractKey, Error, pKey, KeyLen);

      Finally
        If (KeyLen <> 0) Then
          FFFreeMem(pKey, KeyLen);
      End;

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

    End;
End;
{--------}

Function GetVerClient(Engine: TfsServer; ClientId: TffClientId): Integer;
Var
  i: Integer;
Begin
  Result := 0;
  If Engine.ClientList.ClientCount > 0 Then
    Begin
      For i := 0 To Engine.ClientList.ClientCount - 1 Do
        Begin
          If Engine.ClientList.Client[ftFromIndex, i].ClientId = ClientId Then
            Begin
              Result := Engine.ClientList.Client[ftFromIndex, i].ClientVersion;
              break;
            End;
        End;
    End;
End;

Procedure TFSHandler.nmRecordGet(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
  Buffer: PffByteArray;
  flag: Byte;
  aRefNr: TffInt64;
  ExtraRecInfo: Integer;
Begin

  With Msg, PfsnmRecordGetReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordGet',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  LockType %d', [Byte(LockType)]),
          format('  RecLen   %d', [RecLen]),
            format('  BMSize   %d', [BookmarkSize])]);

      {we shall be sending back a multipart message: get record followed
       by getbookmark}
      ExtraRecInfo := SizeOf(TfsExtraRecInfo);
      //ver := GetVerClient(TfsServer(FServerEngine), dmClientID);
      //If ver >= 1068 Then ExtraRecInfo:= 1 + 4 + 8 + 2;
      MsgSize := (2 * fsc_SubMsgHeaderSize) + RecLen + BookmarkSize + ExtraRecInfo;
      FFGetMem(MsgData, MsgSize);
      Try
        SubMsg := PfssmHeader(MsgData);
        If (RecLen = 0) Then
          Buffer := Nil
        Else
          Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.RecordGet(CursorID, LockType, UserLockType, Buffer, Flag, aRefNr, aUser);
        If (Error <> 0) Then
          Begin

            If FLogEnabled Then
              ichLogFmt(csErr, [Error]);
            TFSBaseTransport.Reply(fsnmRecordGet, Error, Nil, 0);

            Exit;
          End;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Record', Buffer, RecLen);
            ichLogFmt(csErr, [Error]);
          End;

        fsExtraRecInfo^.eFlagRowInfo := Flag;
        fsExtraRecInfo^.eRecNo := 0;
        fsExtraRecInfo^.eRefNr := aRefNr;
        fsExtraRecInfo^.eRecVersion := 0;
        fsExtraRecInfo^.eFlagCollInfoSize := 0;

        If Assigned(fsExtraRecInfo) Then
          Move(fsExtraRecInfo^, PffByteArray(@SubMsg^.smhData)[RecLen], ExtraRecInfo);

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordGet,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          RecLen + ExtraRecInfo);

        If FLogEnabled Then
          ichLog('CursorGetBookmark (multipart)');

        If (BookmarkSize <> 0) Then
          Begin
            Buffer := PffByteArray(@SubMsg^.smhData);
            Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
          End
        Else
          Error := DBIERR_INVALIDBOOKMARK;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Bookmark', Buffer, BookmarkSize);
            ichLogFmt(csErr, [Error]);
          End;

        FFCreateSubMessage(SubMsg,
          fsnmCursorGetBookmark,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          BookmarkSize);
        TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);

      Finally
        FFFreeMem(MsgData, MsgSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordGetBatch(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  pData: PfsnmRecordGetBatchRpy;
  DataSize: Longint;
Begin
  With Msg, PfsnmRecordGetBatchReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordGetBatch',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  RecLen   %d', [RecLen]),
            format('  RecCount %d', [RecCount])]);

      DataSize := 2 * sizeof(Longint) + (RecLen * RecCount);
      FFGetMem(pData, DataSize);
      Try
        pData^.RecCount := 0; { just to be safe }
        Error := FServerEngine.RecordGetBatch(CursorID, RecCount,
          RecLen,
          pData^.RecCount,
          PffByteArray(@pData^.RecArray),
          pData^.Error);
        If FLogEnabled Then
          ichLogAll([format('  RecCount %d', [pData^.RecCount]),
            format('  Error    %x', [pData^.Error]),
              format(csErr, [Error])]);

        TFSBaseTransport.Reply(fsnmRecordGetBatch, Error,
          pData, (pdata^.RecCount * RecLen) + 2 * Sizeof(Longint));

      Finally
        If (DataSize <> 0) Then
          FFFreeMem(pData, DataSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordGetForKey(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
  Buffer: PffByteArray;
Begin
  With Msg, PfsnmRecordGetForKeyReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        Begin
          ichLogAll(['RecordGetForKey',
            format(csClientID, [dmClientID]),
              format(csCursorID, [CursorID]),
              format('  DrctKey  %d', [Byte(DirectKey)]),
            format('  FldCount %d', [FieldCount]),
              format('  PartLen  %d', [PartialLen]),
              format('  RecLen   %d', [RecLen]),
              format('  DataLen  %d', [KeyDataLen]),
              format('  BMSize   %d', [BookmarkSize])]);
          ichLogBlock('  Data', @KeyData, KeyDataLen);
        End;

      {we shall be sending back a multipart message: RecordGetForKey}
      {followed by getbookmark}
      MsgSize := (2 * fsc_SubMsgHeaderSize) + RecLen + BookmarkSize;
      FFGetMem(MsgData, MsgSize);
      Try
        { do the RecordGetForKey First }
        SubMsg := PfssmHeader(MsgData);
        If (RecLen = 0) Then
          Buffer := Nil
        Else
          Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.RecordGetForKey(CursorID, DirectKey, FieldCount,
          PartialLen, @KeyData, Buffer, True);
        If (Error <> 0) Then
          Begin
            If FLogEnabled Then
              ichLogFmt(csErr, [Error]);
            TFSBaseTransport.Reply(fsnmRecordGetForKey, Error, Nil, 0);
            Exit;
          End;

        If FLogEnabled Then
          Begin
            If Error = 0 Then
              ichLogBlock('  Record', Buffer, RecLen);
            ichLogFmt(csErr, [Error]);
          End;

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordGetForKey,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          RecLen);
        {Now do the GetBookmark }

        If FLogEnabled Then
          ichLog('CursorGetBookmark (multipart)');

        If (BookmarkSize <> 0) Then
          Begin
            Buffer := PffByteArray(@SubMsg^.smhData);
            Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
          End
        Else
          Error := DBIERR_INVALIDBOOKMARK;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Bookmark', Buffer, BookmarkSize);
            ichLogFmt(csErr, [Error]);
          End;

        FFCreateSubMessage(SubMsg,
          fsnmCursorGetBookmark,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          BookmarkSize);
        TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);
      Finally
        FFFreeMem(MsgData, MsgSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordGetForKey2(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
  Buffer: PffByteArray;
Begin
  With Msg, PfsnmRecordGetForKeyReq2(dmData)^ Do
    Begin

      If FLogEnabled Then
        Begin
          ichLogAll(['RecordGetForKey2',
            format(csClientID, [dmClientID]),
              format(csCursorID, [CursorID]),
              format('  DrctKey  %d', [Byte(DirectKey)]),
            format('  FldCount %d', [FieldCount]),
              format('  PartLen  %d', [PartialLen]),
              format('  RecLen   %d', [RecLen]),
              format('  DataLen  %d', [KeyDataLen]),
              format('  BMSize   %d', [BookmarkSize]),
              format('  FirstCl  %d', [Byte(FirstCall)])]);
          ichLogBlock('  Data', @KeyData, KeyDataLen);
        End;

      {we shall be sending back a multipart message: RecordGetForKey2}
      {followed by getbookmark}
      MsgSize := (2 * fsc_SubMsgHeaderSize) + RecLen + BookmarkSize;
      FFGetMem(MsgData, MsgSize);
      Try
        { do the RecordGetForKey First }
        SubMsg := PfssmHeader(MsgData);
        If (RecLen = 0) Then
          Buffer := Nil
        Else
          Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.RecordGetForKey(CursorID, DirectKey, FieldCount,
          PartialLen, @KeyData, Buffer, FirstCall);
        If (Error <> 0) Then
          Begin
            If FLogEnabled Then
              ichLogFmt(csErr, [Error]);
            TFSBaseTransport.Reply(fsnmRecordGetForKey2, Error, Nil, 0);
            Exit;
          End;

        If FLogEnabled Then
          Begin
            If Error = 0 Then
              ichLogBlock('  Record', Buffer, RecLen);
            ichLogFmt(csErr, [Error]);
          End;

        {we don't need a multipart message in case of a error...}
        If Error <> DBIERR_NONE Then
          Begin
            TFSBaseTransport.Reply(fsnmRecordGetForKey2, Error, Nil, 0);
            Exit;
          End;
        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordGetForKey2,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          RecLen);
        {Now do the GetBookmark }

        If FLogEnabled Then
          ichLog('CursorGetBookmark (multipart)');

        If (BookmarkSize <> 0) Then
          Begin
            Buffer := PffByteArray(@SubMsg^.smhData);
            Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
          End
        Else
          Error := DBIERR_INVALIDBOOKMARK;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Bookmark', Buffer, BookmarkSize);
            ichLogFmt(csErr, [Error]);
          End;

        FFCreateSubMessage(SubMsg,
          fsnmCursorGetBookmark,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          BookmarkSize);
        TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);
      Finally
        FFFreeMem(MsgData, MsgSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordGetNext(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
  Buffer: PffByteArray;
  flag: Byte;
  aRefNr: TffInt64;
  ExtraRecInfo: Integer;
Begin
  With Msg, PfsnmRecordGetNextReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordGetNext',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  LockType %d', [Byte(LockType)]),
          format('  RecLen   %d', [RecLen]),
            format('  BMSize   %d', [BookmarkSize])]);

      {check the rights}

      {we shall be sending back a multipart message: getnextrecord
       followed by getbookmark}
      ExtraRecInfo := SizeOf(TfsExtraRecInfo); //1 + 4 + 8 + 2;
      //ver := GetVerClient(TfsServer(FServerEngine), dmClientID);
      //If ver >= 1068 Then ExtraRecInfo:= 1 + 4 + 8 + 2;
      MsgSize := (2 * fsc_SubMsgHeaderSize) + RecLen + BookmarkSize + ExtraRecInfo;
      FFGetMem(MsgData, MsgSize);

      Try
        SubMsg := PfssmHeader(MsgData);
        If (RecLen = 0) Then
          Buffer := Nil
        Else
          Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.RecordGetNext(CursorID, LockType, Buffer, Flag, aRefNr);

        If (Error <> 0) Then
          Begin
            If FLogEnabled Then
              ichLogFmt(csErr, [Error]);
            TFSBaseTransport.Reply(fsnmRecordGetNext, Error, Nil, 0);
            Exit;
          End;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Record', Buffer, RecLen);
            ichLogFmt(csErr, [Error]);
          End;

        fsExtraRecInfo^.eFlagRowInfo := Flag;
        fsExtraRecInfo^.eRecNo := 0;
        fsExtraRecInfo^.eRefNr := aRefNr;
        fsExtraRecInfo^.eRecVersion := 0;
        fsExtraRecInfo^.eFlagCollInfoSize := 0;

        If Assigned(fsExtraRecInfo) Then
          Move(fsExtraRecInfo^, PffByteArray(@SubMsg^.smhData)[RecLen], ExtraRecInfo);

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordGetNext,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          RecLen + ExtraRecInfo);

        If FLogEnabled Then
          ichLog('CursorGetBookmark (multipart)');

        If (BookmarkSize <> 0) Then
          Begin
            Buffer := PffByteArray(@SubMsg^.smhData);
            Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
          End
        Else
          Error := DBIERR_INVALIDBOOKMARK;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Bookmark', Buffer, BookmarkSize);
            ichLogFmt(csErr, [Error]);
          End;

        FFCreateSubMessage(SubMsg,
          fsnmCursorGetBookmark,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          BookmarkSize);
        TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);
      Finally
        FFFreeMem(MsgData, MsgSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordGetPrev(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
  Buffer: PffByteArray;
  flag: Byte;
  aRefNr: TffInt64;
  ExtraRecInfo: Integer;
Begin
  With Msg, PfsnmRecordGetPrevReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordGetPrev',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  LockType %d', [Byte(LockType)]),
          format('  RecLen   %d', [RecLen]),
            format('  BMSize   %d', [BookmarkSize])]);

      {check the rights}

      {we shall be sending back a multipart message: getnextrecord
       followed by getbookmark}
      ExtraRecInfo := SizeOf(TfsExtraRecInfo);
      //ver := GetVerClient(TfsServer(FServerEngine), dmClientID);
      //If ver >= 1068 Then ExtraRecInfo:= 1 + 4 + 8 + 2;
      MsgSize := (2 * fsc_SubMsgHeaderSize) + RecLen + BookmarkSize + ExtraRecInfo;
      FFGetMem(MsgData, MsgSize);
      Try
        SubMsg := PfssmHeader(MsgData);
        If (RecLen = 0) Then
          Buffer := Nil
        Else
          Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.RecordGetPrior(CursorID, LockType, Buffer, Flag, aRefNr);
        If (Error <> 0) Then
          Begin
            If FLogEnabled Then
              ichLogFmt(csErr, [Error]);
            TFSBaseTransport.Reply(fsnmRecordGetPrev, Error, Nil, 0);
            Exit;
          End;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Record', Buffer, RecLen);
            ichLogFmt(csErr, [Error]);
          End;

        fsExtraRecInfo^.eFlagRowInfo := Flag;
        fsExtraRecInfo^.eRecNo := 0;
        fsExtraRecInfo^.eRefNr := aRefNr;
        fsExtraRecInfo^.eRecVersion := 0;
        fsExtraRecInfo^.eFlagCollInfoSize := 0;

        If Assigned(fsExtraRecInfo) Then
          Move(fsExtraRecInfo^, PffByteArray(@SubMsg^.smhData)[RecLen], ExtraRecInfo);

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordGetPrev,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          RecLen + ExtraRecInfo);

        If FLogEnabled Then
          ichLog('CursorGetBookmark (multipart)');

        If (BookmarkSize <> 0) Then
          Begin
            Buffer := PffByteArray(@SubMsg^.smhData);
            Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
          End
        Else
          Error := DBIERR_INVALIDBOOKMARK;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Bookmark', Buffer, BookmarkSize);
            ichLogFmt(csErr, [Error]);
          End;

        FFCreateSubMessage(SubMsg,
          fsnmCursorGetBookmark,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          BookmarkSize);
        TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);
      Finally
        FFFreeMem(MsgData, MsgSize);
      End;
    End;
End;

Procedure TFSHandler.nmRecordGetSetPosition(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
  Buffer: PffByteArray;
  flag: Byte;
  aRefNr: TffInt64;
  RecNo: LongWord;
  ExtraRecInfo: Integer;
Begin
  With Msg, PfsnmRecordGetSetPositionReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordGetSetPosition',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  LockType %d', [Byte(LockType)]),
          format('  RecLen   %d', [RecLen]),
            format('  BMSize   %d', [BookmarkSize])]);

      ExtraRecInfo := SizeOf(TfsExtraRecInfo);
      //ver := GetVerClient(TfsServer(FServerEngine), dmClientID);
      //If ver >= 1068 Then ExtraRecInfo:= 1 + 4 + 8 + 2;
      MsgSize := (2 * fsc_SubMsgHeaderSize) + RecLen + BookmarkSize + ExtraRecInfo;
      FFGetMem(MsgData, MsgSize);
      Try
        SubMsg := PfssmHeader(MsgData);
        If (RecLen = 0) Then
          Buffer := Nil
        Else
          Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.RecordGetSetPosition(Value, CursorID, LockType, Buffer, Flag, RecNo, aRefNr, InfoGetSetPosition, aSet);
        If (Error <> 0) Then
          Begin
            If FLogEnabled Then
              ichLogFmt(csErr, [Error]);
            TFSBaseTransport.Reply(fsnmRecordGetSetPosition, Error, Nil, 0);
            Exit;
          End;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Record', Buffer, RecLen);
            ichLogFmt(csErr, [Error]);
          End;

        fsExtraRecInfo^.eFlagRowInfo := Flag;
        fsExtraRecInfo^.eRecNo := RecNo;
        fsExtraRecInfo^.eRefNr:= aRefNr;
        fsExtraRecInfo^.eRecVersion := 0;
        fsExtraRecInfo^.eFlagCollInfoSize := 0;

        If Assigned(fsExtraRecInfo) Then
          Move(fsExtraRecInfo^, PffByteArray(@SubMsg^.smhData)[RecLen], ExtraRecInfo);

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordGetSetPosition,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          RecLen + ExtraRecInfo);

        If FLogEnabled Then
          ichLog('CursorGetBookmark (multipart)');

        If (BookmarkSize <> 0) Then
          Begin
            Buffer := PffByteArray(@SubMsg^.smhData);
            Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
          End
        Else
          Error := DBIERR_INVALIDBOOKMARK;

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Bookmark', Buffer, BookmarkSize);
            ichLogFmt(csErr, [Error]);
          End;

        FFCreateSubMessage(SubMsg,
          fsnmCursorGetBookmark,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          BookmarkSize);
        TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);
      Finally
        FFFreeMem(MsgData, MsgSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordInsert(Var Msg: TfsDataMessage);
Var
  aFlag: Byte;
  aRefNr: TffInt64;
  Error: TffResult;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
  Buffer: PffByteArray;
  ExtraRecInfo: Integer;
Begin
  aFlag := 0;
  With Msg, PfsnmRecordInsertReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordInsert',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  RecLen   %d', [RecLen]),
            format('  BMSize   %d', [BookmarkSize]),
            format('  LockType %d', [Byte(LockType)])]);

      {try and insert record}
      Error := FServerEngine.RecordInsert(CursorID, LockType, @Data, Undelete, aRefNr);
      If (Error <> 0) Then
        Begin
          If FLogEnabled Then
            ichLogFmt(csErr, [Error]);
          TFSBaseTransport.Reply(fsnmRecordInsert, Error, Nil, 0);
          Exit;
        End;
      fsExtraRecInfo^.eRefNr:= aRefNr;
      {we shall be sending back a multipart message: insertrecord,
       followed by getrecord, followed by getbookmark}
      ExtraRecInfo := SizeOf(TfsExtraRecInfo);
      //ver := GetVerClient(TfsServer(FServerEngine), dmClientID);
      //If ver >= 1068 Then ExtraRecInfo:= 1 + 4 + 8 + 2;
      MsgSize := (3 * fsc_SubMsgHeaderSize) + RecLen + BookmarkSize + ExtraRecInfo;
      FFGetMem(MsgData, MsgSize);
      Try
        SubMsg := PfssmHeader(MsgData);
        {write the results of the insertrecord}

        If FLogEnabled Then
          ichLogFmt(csErr, [Error]);

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordInsert,
          Error,
          nmdByteArray,
          Nil,
          0);

        If FLogEnabled Then
          ichLog('RecordGet (multipart)');

        Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.RecordGet(CursorID, ffltNoLock, tluDatabase, Buffer, aFlag, aRefNr, False);

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Record', Buffer, RecLen);
            ichLogFmt(csErr, [Error]);
          End;

        fsExtraRecInfo^.eFlagRowInfo := aFlag;
        fsExtraRecInfo^.eRecNo := 0;
        fsExtraRecInfo^.eRecVersion := 0;
        fsExtraRecInfo^.eFlagCollInfoSize := 0;

        If Assigned(fsExtraRecInfo) Then
          Move(fsExtraRecInfo^, PffByteArray(@SubMsg^.smhData)[RecLen], ExtraRecInfo);

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordGet,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          RecLen + ExtraRecInfo);

        If FLogEnabled Then
          ichLog('CursorGetBookmark (multipart)');

        Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Bookmark', Buffer, BookmarkSize);
            ichLogFmt(csErr, [Error]);
          End;

        FFCreateSubMessage(SubMsg,
          fsnmCursorGetBookmark,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          BookmarkSize);
        TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);
      Finally
        FFFreeMem(MsgData, MsgSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordInsertBatch(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: PffLongintArray;
  DataSize: Longint;
Begin
  With Msg, PfsnmRecordInsertBatchReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordInsertBatch',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  RecCount %d', [RecCount]),
            format('  RecLen   %d', [RecLen])]);

      DataSize := RecCount * sizeof(Longint);
      FFGetMem(Reply, DataSize);
      Try
        Error := FServerEngine.RecordInsertBatch(CursorID, RecCount, RecLen,
          PffByteArray(@RecArray),
          Reply);
        If FLogEnabled Then
          ichLogFmt(csErr, [Error]);
        TFSBaseTransport.Reply(fsnmRecordInsertBatch, Error, Reply, DataSize);

      Finally
        FFFreeMem(Reply, DataSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordIsLocked(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmRecordIsLockedRpy;
Begin
  With Msg, PfsnmRecordIsLockedReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordIsLocked',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  LockType %d', [Byte(LockType)])]);

      Error := FServerEngine.RecordIsLocked(CursorID, LockType, Reply.IsLocked);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

      TFSBaseTransport.Reply(fsnmRecordIsLocked, Error, @Reply, SizeOf(Reply)); {!!.03}

    End;
End;
{--------}

Procedure TFSHandler.nmRecordModify(Var Msg: TfsDataMessage);
Var
  aRefNr: TffInt64;
  Error: TffResult;
  MsgSize: Longint;
  MsgData: PffByteArray;
  SubMsg: PfssmHeader;
  Buffer: PffByteArray;
  ExtraRecInfo: Integer;
Begin
  With Msg, PfsnmRecordModifyReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordModify',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  RecLen   %d', [RecLen]),
            format('  BMSize   %d', [BookmarkSize]),
            format('  RelLock  %d', [Byte(RelLock)])]);

      {try and modify record}
      Error := FServerEngine.RecordModify(CursorID, @Data, RelLock, UserLockType, aFlag, aSet, aUse);
      If (Error <> 0) Then
        Begin
          If FLogEnabled Then
            ichLogFmt(csErr, [Error]);
          TFSBaseTransport.Reply(fsnmRecordModify, Error, Nil, 0);
          Exit;
        End;

      {we shall be sending back a multipart message: modifyrecord,
       followed by getrecord, followed by getbookmark}
      ExtraRecInfo := SizeOf(TfsExtraRecInfo);
      //ver := GetVerClient(TfsServer(FServerEngine), dmClientID);
      //If ver >= 1068 Then ExtraRecInfo:= 1 + 4 + 8 + 2;
      MsgSize := (3 * fsc_SubMsgHeaderSize) + RecLen + BookmarkSize + ExtraRecInfo;
      FFGetMem(MsgData, MsgSize);
      Try
        SubMsg := PfssmHeader(MsgData);
        {write the results of the insertrecord}

        If FLogEnabled Then
          ichLogFmt(csErr, [Error]);

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordModify,
          Error,
          nmdByteArray,
          Nil,
          0);

        If FLogEnabled Then
          ichLog('RecordGet (multipart)');

        Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.RecordGet(CursorID, ffltNoLock, UserLockType, Buffer, aFlag, aRefNr, False);

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Record', Buffer, RecLen);
            ichLogFmt(csErr, [Error]);
          End;

        fsExtraRecInfo^.eFlagRowInfo := aFlag;
        fsExtraRecInfo^.eRecNo := 0;
        fsExtraRecInfo^.eRefNr:= aRefNr;
        fsExtraRecInfo^.eRecVersion := 0;
        fsExtraRecInfo^.eFlagCollInfoSize := 0;

        If Assigned(fsExtraRecInfo) Then
          Move(fsExtraRecInfo^, PffByteArray(@SubMsg^.smhData)[RecLen], ExtraRecInfo);

        SubMsg := FFCreateSubMessage(SubMsg,
          fsnmRecordGet,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          RecLen + ExtraRecInfo);

        If FLogEnabled Then
          ichLog('CursorGetBookmark (multipart)');

        Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogBlock('  Bookmark', Buffer, BookmarkSize);
            ichLogFmt(csErr, [Error]);
          End;

        FFCreateSubMessage(SubMsg,
          fsnmCursorGetBookmark,
          Error,
          nmdByteArray,
          @SubMsg^.smhData,
          BookmarkSize);
        TFSBaseTransport.Reply(fsnmMultiPartMessage, 0, MsgData, MsgSize);
      Finally
        FFFreeMem(MsgData, MsgSize);
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmRecordRelLock(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmRecordRelLockReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RecordRelLock',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  AllLocks %d', [Byte(AllLocks)])]);

      Error := FServerEngine.RecordRelLock(CursorID, AllLocks);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

      TFSBaseTransport.Reply(fsnmRecordRelLock, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmReindexTable(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmReindexTableRpy;
Begin
  With Msg, PfsnmReindexTableReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['ReindexTable',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format('  TblName  [%s]', [TableName]),
            format('  InxName  [%s]', [IndexName]),
            format('  InxNum   %d', [IndexNumber])]);

      Error := FServerEngine.TableRebuildIndex(DatabaseID,
        TableName,
        IndexName,
        IndexNumber,
        Reply.RebuildID);
      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  RbldID   %d', [Reply.RebuildID]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmReindexTable, Error, @Reply, sizeof(Reply));

    End;
End;
{--------}

Procedure TFSHandler.nmRelTableLock(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmRelTableLockReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RelTableLock',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  AllLocks %d', [Byte(AllLocks)]),
          format('  LockType %d', [Byte(LockType)])]);

      Error := FServerEngine.TableLockRelease(CursorID, AllLocks);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmRelTableLock, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmRenameTable(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmRenameTableReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['RenameTable',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format('  OldTblName  [%s]', [OldTableName]),
            format('  NewTblName  [%s]', [NewTableName])]);

      Error := FServerEngine.TableRename(DatabaseID, OldTableName, NewTableName);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmRenameTable, Error, Nil, 0);

    End;
End;
{--------}

Procedure TFSHandler.nmRestructureTable(Var Msg: TfsDataMessage);
{ Input stream is expected to be:
      DatabaseId (longint)
      TableName  (TfsTableName)
      Dictionary (TFSInfoServerDict or TffDataDictionary)
      FieldMap   (one TffShStr for each field map entry; final entry
                  followed by a zero byte to signal end-of-list.  If
                  no field map is given, then a single zero byte must be
                  present
}
Var
  Reply: TfsnmRestructureTableRpy;
  Error: TffResult;
  Stream: TMemoryStream;
  DatabaseID: Longint;
  TableName: TfsTableName;
  Dictionary: TFSInfoServerDict;

  DictionaryStart: Integer;
  DictionaryEnd: Integer;
  I: Integer;

  FieldMap: TFSSpecStringList;
  LenByte: Byte;
  FieldMapEntry: TffShStr;
  RangeError: Boolean;
Begin
  With Msg Do
    Begin
      Stream := TMemoryStream.Create;
      Stream.Write(dmData^, dmDataLen);
      Stream.Position := 0;
      Stream.Read(RangeError, 2);
      Stream.Read(DatabaseID, SizeOf(DatabaseID));
      Stream.Read(TableName, SizeOf(TableName));
      Dictionary := TFSInfoServerDict.Create(4096);
      Try

        DictionaryStart := Stream.Position;

        Dictionary.ReadFromStream(Stream);

        DictionaryEnd := Stream.Position;

        FieldMap := Nil;
        Stream.Read(LenByte, SizeOf(LenByte));
        If LenByte <> 0 Then
          Begin
            FieldMap := TFSSpecStringList.Create;
            Try
              Repeat
                Stream.Position := Stream.Position - SizeOf(LenByte);
                Stream.Read(FieldMapEntry, LenByte + 1);
                FieldMap.Insert(FieldMapEntry);
                Stream.Read(LenByte, SizeOf(LenByte));
              Until LenByte = 0;
            Except
              FieldMap.Free;
              Raise;
            End;
          End;
        Try

          If FLogEnabled Then
            Begin
              ichLogAll(['RestructureTable',
                format(csClientID, [dmClientID]),
                  format('  DBase ID %d', [DatabaseID]),
                  format('  TblName  [%s]', [TableName])]);
              ichLogBlock('  Dictionary',
                Addr(PffByteArray(Stream.Memory)^[DictionaryStart]),
                DictionaryEnd - DictionaryStart);
              If Not Assigned(FieldMap) Then
                ichLog('  FieldMap nil')
              Else
                Begin
                  ichLogFmt('  FieldMap [%s]', [FieldMap.Strings[0]]);
                  For I := 1 To FieldMap.Count - 1 Do
                    ichLogFmt('           [%s]', [FieldMap.Strings[I]]);
                End;
            End;

          Error := FServerEngine.TableRestructure(DatabaseID,
            TableName,
            Dictionary,
            FieldMap,
            Reply.RebuildID,
            RangeError);
          If FLogEnabled Then
            Begin
              If (Error = 0) Then
                ichLogFmt('  ReBldID %d', [Reply.RebuildID]);
              ichLogFmt(csErr, [Error]);
            End;

          TFSBaseTransport.Reply(fsnmRestructureTable, Error, @Reply, SizeOf(Reply));

        Finally
          FieldMap.Free;
        End;
      Finally
        Dictionary.Free;
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmServerIsReadOnly(Var Msg: TfsDataMessage);
Var
  Reply: TfsnmServerIsReadOnlyRpy;
Begin
  With Msg Do
    Begin

      If FLogEnabled Then
        ichLogAll(['ServerIsReadOnly',
          format(csClientID, [dmClientID])]);

      Reply.IsReadOnly := FServerEngine.IsReadOnly;
      If FLogEnabled Then
        ichLogFmt(csErr, [0]);
      TFSBaseTransport.Reply(fsnmServerIsReadOnly, 0, @Reply, SizeOf(Reply)); {!!.01}

    End;
End;
{--------}{begin !!.07}

Procedure TFSHandler.nmServerStatistics(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmServerStatisticsRpy;
Begin
  With Msg Do
    Begin

      If FLogEnabled Then
        ichLogAll(['ServerStatistics',
          format(csClientID, [dmClientID])]);

      Error := FServerEngine.GetServerStatistics(Reply.Stats);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

      TFSBaseTransport.Reply(fsnmServerStatistics, Error, @Reply, SizeOf(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmCmdHandlerStatistics(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmCmdHandlerStatisticsRpy;
Begin
  With Msg, PfsnmCmdHandlerStatisticsReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['CmdHandlerStatistics',
          Format(csClientID, [dmClientID]),
            Format('  CmdHandlerIdx %d', [CmdHandlerIdx])]);

      Error := FServerEngine.GetCommandHandlerStatistics(CmdHandlerIdx, Reply.Stats);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

      TFSBaseTransport.Reply(fsnmCmdHandlerStatistics, Error, @Reply, SizeOf(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmTransportStatistics(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmTransportStatisticsRpy;
Begin
  With Msg, PfsnmTransportStatisticsReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['TransportStatistics',
          Format(csClientID, [dmClientID]),
            Format('  CmdHandlerIdx %d', [CmdHandlerIdx]),
            Format('  TramsportIdx  %d', [TransportIdx])]);

      Error := FServerEngine.GetTransportStatistics(CmdHandlerIdx,
        TransportIdx,
        Reply.Stats);

      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);

      TFSBaseTransport.Reply(fsnmTransportStatistics, Error, @Reply, SizeOf(Reply));
    End;
End;
{--------}{end !!.07}

Procedure TFSHandler.nmSessionAdd(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  SessionID: TffSessionID;
  Reply: TfsnmSessionAddRpy;
  il, t: Longint;
  ClientDb: Array[0..9] Of Char;
Begin
  With Msg, PfsnmSessionAddReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['SessionAdd',
          format(csClientID, [dmClientID]),
            format('  Timeout  %d', [Timeout])]);
      il := 0;
      ClientDb[0] := '#';
      ClientDb[1] := 'C';
      ClientDb[2] := 'L';
      ClientDb[3] := 'I';
      ClientDb[4] := 'E';
      ClientDb[5] := 'N';
      ClientDb[6] := 'T';
      ClientDb[7] := 'D';
      ClientDb[8] := 'B';
      ClientDb[9] := '#';
      If IsClientDb = ClientDb Then
        il := DataLength;
      t := Timeout;
      Error := FServerEngine.SessionAdd(dmClientID, t, SessionID, @Data, il);
      If (Error = 0) Then
        Reply.SessionID := SessionID;
      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  Session  %d', [Reply.SessionID]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmSessionAdd, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmSessionClose(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSessionCloseReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SessionClose',
          format(csClientID, [dmClientID]),
            format('  Session  %d', [SessionID])]);

      Error := FServerEngine.SessionRemove(dmClientID, SessionID);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSessionClose, Error, Nil, 0);
    End;
End;
{Begin !!.06}
{--------}

Procedure TFSHandler.nmSessionCloseInactiveTables(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSessionCloseInactiveTblReq(dmData)^ Do
    Begin
      If FLogEnabled Then
        ichLogAll(['SessionCloseInactiveTables',
          format(csClientID, [dmClientID]),
            format('  Session  %d', [SessionID])]);

      Error := FServerEngine.SessionCloseInactiveTables(dmClientID);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSessionCloseInactTbl, Error, Nil, 0);
    End;
End;
{End !!.06}
{--------}

Procedure TFSHandler.nmSessionGetCurrent(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmSessionGetCurrentRpy;
Begin
  With Msg Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SessionGetCurrent',
          format(csClientID, [dmClientID])]);

      Error := FServerEngine.SessionGetCurrent(dmClientID, Reply.SessionID);
      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt('  Session  %d', [Reply.SessionID]);
          ichLogFmt(csErr, [Error]);
        End;
      TFSBaseTransport.Reply(fsnmSessionGetCurrent, Error, @Reply, sizeof(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmSessionSetCurrent(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSessionSetCurrentReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SessionSetCurrent',
          format(csClientID, [dmClientID]),
            format('  Session  %d', [SessionID])]);

      Error := FServerEngine.SessionSetCurrent(dmClientID, SessionID);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSessionSetCurrent, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmSessionSetTimeout(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSessionSetTimeoutReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SessionSetTimeout',
          format(csClientID, [dmClientID]),
            format('  Session  %d', [SessionID]),
            format('  Timeout  %d', [Timeout])]);

      Error := FServerEngine.SessionSetTimeout(dmClientID, SessionID, Timeout);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSessionSetTimeout, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmSetTableAutoIncValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSetTableAutoIncValueReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SetTableAutoIncValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  Value %d', [AutoInc64Value])]);

      Error := FServerEngine.TableSetAutoInc(CursorID, AutoInc64Value, AutoInc64StepValue);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSetTableAutoIncValue, Error, Nil, 0);
    End;
End;

Procedure TFSHandler.nmSetTableMaxRecordsValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSetTableMaxRecordsValueReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SetTableMaxRecordsValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  Value %d', [MaxRecords])]);

      Error := FServerEngine.TableSetMaxRecords(CursorID, MaxRecords);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSetTableMaxRecordsValue, Error, Nil, 0);
    End;
End;

Procedure TFSHandler.nmSetTableTableFlagsValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSetTableTableFlagsValueReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SetTableTableFlagsValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  Value %d', [TableFlags])]);

      Error := FServerEngine.TableSetTableFlags(CursorID, TableFlags);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSetTableTableFlagsValue, Error, Nil, 0);
    End;
End;

Procedure TFSHandler.nmSetTableTablePasswordValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSetTableTablePasswordValueReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SetTableTablePasswordValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  Value %d', [TablePassword])]);

      Error := FServerEngine.TableSetTablePassword(CursorID, TablePassword);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSetTableTablePasswordValue, Error, Nil, 0);
    End;
End;

Procedure TFSHandler.nmSetTableTablePasswordRestValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSetTableTablePasswordRestValueReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SetTableTablePasswordRestValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  Value %d', [TablePassword])]);

      Error := FServerEngine.TableSetTablePasswordRest(CursorID, TablePassword);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSetTableTablePasswordRestValue, Error, Nil, 0);
    End;
End;

Procedure TFSHandler.nmSetTableTableDBIDValue(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSetTableTableDBIDValueReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SetTableTableDBIDValue',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format('  Value %d', [TableDBID])]);

      Error := FServerEngine.TableSetTableDBID(CursorID, TableDBID);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSetTableTableDBIDValue, Error, Nil, 0);
    End;
End;

{--------}

Procedure TFSHandler.nmStartTransaction(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmStartTransactionReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['StartTransaction',
          format(csClientID, [dmClientID]),
            format('  DBase ID %d', [DatabaseID]),
            format('  FailSafe %d', [Byte(FailSafe)])]);

      Error := FServerEngine.TransactionStart(DatabaseID,
        FailSafe);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmStartTransaction, Error, Nil, 0);
    End;
End;
{Begin !!.10}
{--------}

Procedure TFSHandler.nmStartTransactionWith(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Inx,
    CursorCount: Integer;
  Reader: TReader;
  Stream: TMemoryStream;
  DbID: TffDatabaseID;
  FailSafe: Boolean;
  CursorIDList: TfsPointerList;
  CursorIDStr: String;
Begin
  With Msg Do
    Begin
      CursorIDList := TfsPointerList.Create;
      Try
        Stream := TMemoryStream.Create;
        Try
          Stream.Write(dmData^, dmDataLen);
          Stream.Position := 0;
          Reader := TReader.Create(Stream, 4096);
          Try
            DbID := Reader.ReadInteger;
            FailSafe := Reader.ReadBoolean;
            CursorCount := Reader.ReadInteger;
            For Inx := 1 To CursorCount Do
              CursorIDList.Append(Pointer(Reader.ReadInteger));
          Finally
            Reader.Free;
          End;
        Finally
          Stream.Free;
        End;

        If FLogEnabled Then
          Begin
            CursorIDStr := '';
            For Inx := 0 To Pred(CursorIDList.Count) Do
              Begin
                If CursorIDStr <> '' Then
                  CursorIDStr := CursorIDStr + ',';
                CursorIDStr := CursorIDStr + IntToStr(Integer(CursorIDList[Inx]));
              End; { for }
            ichLogAll(['StartTransactionWith',
              format(csClientID, [dmClientID]),
                format('  DBase ID  %d', [DbID]),
                format('  FailSafe  %d', [Byte(FailSafe)]),
              format('  CursorIDs %s', [CursorIDStr])]);
          End;

        Error := FServerEngine.TransactionStartWith(DbID,
          FailSafe,
          CursorIDList);
        If FLogEnabled Then
          ichLogFmt(csErr, [Error]);
        TFSBaseTransport.Reply(fsnmStartTransactionWith, Error, Nil, 0);
      Finally
        CursorIDList.Free;
      End;
    End; { with }
End;
{End !!.10}
{--------}

Procedure TFSHandler.nmSQLAlloc(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
  Reply: TfsnmSQLAllocRpy;
Begin
  With Msg, PfsnmSQLAllocReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SQLAlloc',
          format(csClientID, [dmClientID]),
            format('  DBaseID  %d', [DatabaseID]), {!!.01}
          format('  Timeout  %d', [Timeout])]); {!!.01}

      Error := FServerEngine.SQLAlloc(dmClientID, DatabaseID, Timeout,
        Reply.StmtID);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSQLAlloc, Error, @Reply, SizeOf(Reply));
    End;
End;
{--------}

Procedure TFSHandler.nmSQLExec(Var Msg: TfsDataMessage);
Var
  aBuffer: pointer;
  Error: TffResult;
  CursorID: TffCursorID;
  Stream: TMemoryStream;
  StreamSize: Longint;
Begin
  With Msg, PfsnmSQLExecReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SQLExec',
          format(csClientID, [dmClientID]),
            format('  StmtID   %d', [StmtID]),
            format('  OpenMode %d', [Ord(OpenMode)])]);

      Stream := TMemoryStream.Create;
      Try
        Error := FServerEngine.SQLExec(StmtID, OpenMode, CursorID, Stream);
        //      if CursorID = 0 then                                           {!!.01}
        //        TFSBaseTransport.Reply(fsnmSQLExec, Error, nil, 0)           {!!.01}
        //      else begin                                                     {!!.01}
        StreamSize := Stream.Size;
        FFGetMem(aBuffer, StreamSize);
        Stream.Position := 0;
        Stream.Read(aBuffer^, StreamSize);
        TFSBaseTransport.Reply(fsnmSQLExec, Error, aBuffer, StreamSize);
        FFFreeMem(aBuffer, StreamSize);
        //      end;                                                           {!!.01}
      Finally
        Stream.Free;
      End;

      If FLogEnabled Then
        Begin
          If (Error = 0) Then
            ichLogFmt(csCursorID, [CursorID]);
          ichLogFmt(csErr, [Error]);
        End;

    End;
End;
{--------}

Procedure TFSHandler.nmSQLExecDirect(Var Msg: TfsDataMessage);
Var
  aBuffer: pointer;
  Error: TffResult;
  QueryText: PChar;
  CursorID: TffCursorID;
  Stream: TMemoryStream;
  StreamSize: Longint;
Begin
  With Msg, PfsnmSQLExecDirectReq(dmData)^ Do
    Begin
      QueryText := @Query;

      If FLogEnabled Then
        ichLogAll(['SQLExecDirect',
          format(csClientID, [dmClientID]),
            format('  DBase ID [%d]', [DatabaseID]),
            format('  Query    [%s]', [StrPas(QueryText)]),
          format('  Timeout  %d', [Timeout]),
            format('  OpenMode [%d]', [Ord(OpenMode)])]);

      Stream := TMemoryStream.Create;
      Try
        Error := FServerEngine.SQLExecDirect(dmClientID, DatabaseID, QueryText,
          Timeout, OpenMode, CursorID, Stream);
        StreamSize := Stream.Size;
        FFGetMem(aBuffer, StreamSize);
        Stream.Position := 0;
        Stream.Read(aBuffer^, StreamSize);

        If FLogEnabled Then
          Begin
            If (Error = 0) Then
              ichLogFmt(csCursorID, [CursorID]);
            ichLogFmt(csErr, [Error]);
          End;

        TFSBaseTransport.Reply(fsnmSQLExecDirect, Error, aBuffer, StreamSize);
        FFFreeMem(aBuffer, StreamSize);
      Finally
        Stream.Free;
      End;

    End;
End;
{--------}

Procedure TFSHandler.nmSQLFree(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmSQLFreeReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SQLFree',
          format(csClientID, [dmClientID]),
            format('  StmtID   %d', [StmtID])]);

      Error := FServerEngine.SQLFree(StmtID);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmSQLFree, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmSQLPrepare(Var Msg: TfsDataMessage);
Var
  aBuffer: pointer;
  Error: TffResult;
  Stream: TMemoryStream;
  StreamSize: Longint;
Begin
  With Msg, PfsnmSQLPrepareReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['SQLPrepare',
          format(csClientID, [dmClientID]),
            format('  StmtID   %d', [StmtID]),
            format('  Query    [%s]', [StrPas(@Query)])]);

      Stream := TMemoryStream.Create;
      Try
        Error := FServerEngine.SQLPrepare(StmtID, @Query, Stream);

        StreamSize := Stream.Size;
        aBuffer := Nil;
        If StreamSize > 0 Then
          Begin
            FFGetMem(aBuffer, StreamSize);
            Stream.Position := 0;
            Stream.Read(aBuffer^, StreamSize);
          End;

        If FLogEnabled Then
          ichLogFmt(csErr, [Error]);
        TFSBaseTransport.Reply(fsnmSQLPrepare, Error, aBuffer, StreamSize);
        If assigned(aBuffer) Then
          FFFreeMem(aBuffer, StreamSize);
      Finally
        Stream.Free;
      End;
    End;
End;
{--------}

Procedure TFSHandler.nmSQLSetParams(Var Msg: TfsDataMessage);
{ Input stream is expected to be:
      StmtID     (longint)
      NumParams  (word)
      ParamList  (array of TfsSqlParamInfo)
      BufLen     (longint; size of DataBuffer)
      DataBuffer (data buffer)
}
Var
  aBuffer: pointer;
  Error: TffResult;
  OutStream: TMemoryStream;
  OutStreamSize: Longint;
  Stream: TMemoryStream;
  StmtID: Longint;
  NumParams: Word;
  ParamDescs: PfsSqlParamInfoList;
  DataBuffer: PffByteArray;
  BufLen: Longint;
Begin
  With Msg Do
    Begin
      Stream := TMemoryStream.Create;
      {Begin !!.03}
      Try
        Stream.Write(dmData^, dmDataLen);
        Stream.Position := 0;
        Stream.Read(StmtID, SizeOf(StmtID));
        Stream.Read(NumParams, SizeOf(NumParams));
        ParamDescs := Pointer(Longint(Stream.Memory) + Stream.Position);
        Stream.Position := Stream.Position + NumParams * SizeOf(TfsSqlParamInfo);
        Stream.Read(BufLen, SizeOf(BufLen));
        DataBuffer := Pointer(Longint(Stream.Memory) + Stream.Position);

        If FLogEnabled Then
          Begin
            ichLogAll(['SQLSetParams',
              format(csClientID, [dmClientID]),
                format('  StmtID    %d', [StmtID]),
                format('  NumParams %d', [NumParams])]);
            ichLogBlock('  ParamDescs  ', ParamDescs, NumParams * SizeOf(TfsSqlParamInfo));
            ichLogBlock('  DataBuf   ', DataBuffer, BufLen);
          End;

        OutStream := TMemoryStream.Create;
        Try
          Error := FServerEngine.SQLSetParams(StmtID, NumParams, ParamDescs,
            DataBuffer, BufLen, OutStream);
          OutStreamSize := Stream.Size;
          aBuffer := Nil;
          If OutStreamSize > 0 Then
            Begin
              FFGetMem(aBuffer, OutStreamSize);
              Stream.Position := 0;
              Stream.Read(aBuffer^, OutStreamSize);
            End;

          If FLogEnabled And (Error <> 0) Then
            ichLogFmt(csErr, [Error]);
          TFSBaseTransport.Reply(fsnmSQLSetParams, Error, aBuffer, OutStreamSize);
          If assigned(aBuffer) Then
            FFFreeMem(aBuffer, OutStreamSize);
        Finally
          OutStream.Free;
        End;
      Finally
        Stream.Free;
      End;
      {End !!.03}
    End;
End;
{--------}

Procedure TFSHandler.nmTruncateBLOB(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmTruncateBLOBReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        ichLogAll(['TruncateBLOB',
          format(csClientID, [dmClientID]),
            format(csCursorID, [CursorID]),
            format(csBlobNr, [BLOBNr.iLow, BLOBNr.iHigh]),
            format('  BLOBLen  %d', [BLOBLength])]);

      Error := FServerEngine.BLOBTruncate(CursorID, BLOBNr, BLOBLength);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmTruncateBLOB, Error, Nil, 0);
    End;
End;
{--------}

Procedure TFSHandler.nmWriteBLOB(Var Msg: TfsDataMessage);
Var
  Error: TffResult;
Begin
  With Msg, PfsnmWriteBLOBReq(dmData)^ Do
    Begin

      If FLogEnabled Then
        Begin
          ichLogAll(['WriteBLOB',
            format(csClientID, [dmClientID]),
              format(csCursorID, [CursorID]),
              format(csBlobNr, [BLOBNr.iLow, BLOBNr.iHigh]),
              format('  Offset %d', [Offset]),
              format('  Len %d', [Len])]);
          ichLogBlock('  BLOB', @BLOB, Len);
        End;

      Error := FServerEngine.BLOBWrite(CursorID, FieldNo, BLOBNr, Offset, Len, BLOB);
      If FLogEnabled Then
        ichLogFmt(csErr, [Error]);
      TFSBaseTransport.Reply(fsnmWriteBLOB, Error, Nil, 0);
    End;
End;
{--------}
{Rewritten !!.11}

Procedure TFSHandler.FFAddDependent(ADependent: TFSSpecComp);
Var
  Method: PffInt64;
  aTransport: TFSBaseTransport;
Begin
  Inherited;
  If (ADependent Is TFSBaseTransport) Then
    Begin
      aTransport := TFSBaseTransport(ADependent);
      If Assigned(aTransport.OnAddClient) Then
        Begin
          FFGetMem(Method, SizeOf(TffInt64));
          Method^ := TffInt64(aTransport.OnAddClient);
          schSavedAddClientEvents.BeginWrite;
          Try
            schSavedAddClientEvents.Add(Longint(aTransport), Method);
          Finally
            schSavedAddClientEvents.EndWrite;
          End;
        End;
      aTransport.OnAddClient := schOnAddClient;
      aTransport.OnRemoveClient := schOnRemoveClient;
    End; { if }
End;
{Begin !!.05}
{--------}

Procedure TFSHandler.schDisposeRecord(Sender: TfsBaseHashTable;
  aData: Pointer);
Begin
  FFFreeMem(aData, SizeOf(TffInt64));
End;
{End !!.05}
{--------}

Procedure TFSHandler.schOnAddClient
  (Listener: TFSBaseTransport;
  Const userID: TffName;
  Const timeout: Longint;
  Const clientVersion: Longint;
  Var passwordHash: TffWord32;
  Var aClientID: TffClientID;
  Var errorCode: TffResult;
  Var isSecure: boolean;
  Var aVersion: Longint;
  Var aRights: TffUserRights);
Var {!!.05}
  Method: PffInt64; {!!.05}
Begin
  If FLogEnabled Then
    ichLogAll(['AddClientEvent',
      format('  UserID   [%s]', [UserID]),
        format('  timeout  [%d]', [Timeout]),
        format('  clientVersion  [%d]', [ClientVersion])]);

  {Begin !!.05}
    { See if there is a saved event for the listener. }
  schSavedAddClientEvents.BeginRead; {begin !!.05}
  Try
    Method := schSavedAddClientEvents.Get(Longint(Listener));
  Finally
    schSavedAddClientEvents.EndRead;
  End; {end !!.05}
  If Method <> Nil Then
    Begin
      errorCode := DBIERR_NONE;
      TfsAddClientEvent(Method^)
        (Listener, userID, timeout, clientVersion,
        passwordHash, aClientID, errorCode, isSecure,
        aVersion, aRights);
      If errorCode <> DBIERR_NONE Then
        Exit;
    End;
  {End !!.05}

  aClientID := fsc_NoClientID;
  isSecure := False;

  { Is the client a compatible version?
    Reasons for incompatibility:

    1. The server's version number is less than the client's.
    2. The server's major version number is greater than the client's
       major version number (at least in the case of 1.x and 2.x).
  }

  If (fsVersionNumber < clientVersion) Or ((clientVersion < 1101) And (fsVersionNumber >= 1101)) Then
    errorCode := DBIERR_SERVERVERSION
  Else
    errorCode := FServerEngine.ClientAddEx(aClientID, UserID, {!!.11}
      UserID, timeout, {!!.11}
      clientVersion, {!!.11}
      passwordHash,
      aRights, IsSecure); {!!.11}
  If (errorCode = DBIERR_NONE) Then
    isSecure := TFSServer(FServerEngine).Configuration.GeneralInfo^.giIsSecure;
  aVersion := fsVersionNumber;

  If FLogEnabled Then
    Begin
      If (Error = 0) Then
        ichLogAll(['  Successful',
          format(csClientID, [aClientID]),
            format('  IsSecure %d', [ord(isSecure)])]);
      ichLogFmt(csErr, [Error]);
    End;
End;
{--------}

Procedure TFSHandler.schOnRemoveClient
  (Listener: TFSBaseTransport;
  Const aClientID: TffClientID;
  Var errorCode: TffResult);
Begin
  If FLogEnabled Then
    ichLogAll(['RemoveClientEvent',
      format(csClientID, [aClientID])]);

  errorCode := FServerEngine.ClientRemove(aClientID);
  If FLogEnabled Then
    ichLogFmt(csErr, [Error]);
End;
{--------}

Procedure TFSHandler.scInitialize;
Begin
  { do nothing }
End;
{--------}

Procedure TFSHandler.scPrepareForShutdown;
Begin
  { do nothing }
End;
{--------}

Procedure TFSHandler.scShutdown;
Begin
  { do nothing }
End;
{--------}

Procedure TFSHandler.scStartup;
Begin
  { do nothing }
End;

{====================================================================}

End.

