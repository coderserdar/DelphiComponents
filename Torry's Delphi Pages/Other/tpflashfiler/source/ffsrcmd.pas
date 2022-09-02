{*********************************************************}
{* FlashFiler: Server command handler                    *}
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

unit ffsrcmd;

interface

uses
  Classes,
  Windows,
  SysUtils,
  ffconst,
  ffhash,                                                              {!!.05}
  ffllbase,
  fflleng,
  ffllcomm,
  ffllprot,
  ffnetmsg,
  ffdtmsgq,
  ffsrbase,
  ffsrbde,
  ffsrintm,
  ffsrtran,
  fftbdict,
  ffsreng;

type
  TffServerCommandHandler = class(TffIntermediateCommandHandler)
    protected {private}

      schSavedAddClientEvents : TffThreadHash;                               {!!.05}

    protected
      {client message handling}
      procedure nmAcqTableLock(var Msg : TffDataMessage);
                message ffnmAcqTableLock;
      procedure nmAddIndex(var Msg : TffDataMessage);
                message ffnmAddIndex;
      procedure nmAddFileBLOB(var Msg : TffDataMessage);
                message ffnmAddFileBLOB;
      procedure nmBuildTable(var Msg : TffDataMessage);
                message ffnmBuildTable;
      procedure nmCheckSecureComms(var Msg : TffDataMessage);
                message ffnmCheckSecureComms;
      procedure nmClientSetTimeout(var Msg : TffDataMessage);
                message ffnmClientSetTimeout;
      procedure nmCreateBLOB(var Msg : TffDataMessage);
                message ffnmCreateBLOB;
      procedure nmCursorClone(var Msg : TffDataMessage);
                message ffnmCursorClone;
      procedure nmCursorClose(var Msg : TffDataMessage);
                message ffnmCursorClose;
      procedure nmCursorCompareBMs(var Msg : TffDataMessage);
                message ffnmCursorCompareBMs;
      procedure nmCursorCopyRecords(var Msg : TffDataMessage);         {!!.02}
                message ffnmCursorCopyRecords;                         {!!.02}
      procedure nmCursorDeleteRecords(var Msg : TffDataMessage);       {!!.06}
                message ffnmCursorDeleteRecords;                       {!!.06}
{Begin !!.03}
      procedure nmCursorGetBLOBFreeSpace(var Msg : TffDataMessage);
                message ffnmListBLOBFreeSpace;
{End !!.03}
      procedure nmCursorGetBookmark(var Msg : TffDataMessage);
                message ffnmCursorGetBookmark;
      procedure nmCursorOverrideFilter(var Msg : TffDataMessage);
                message ffnmCursorOverrideFilter;
      procedure nmCursorResetRange(var Msg : TffDataMessage);
                message ffnmCursorResetRange;
      procedure nmCursorRestoreFilter(var Msg : TffDataMessage);
                message ffnmCursorRestoreFilter;                 
      procedure nmCursorSetRange(var Msg : TffDataMessage);
                message ffnmCursorSetRange;
      procedure nmCursorSetTimeout(var Msg : TffDataMessage);
                message ffnmCursorSetTimeout;
      procedure nmCursorSetToBegin(var Msg : TffDataMessage);
                message ffnmCursorSetToBegin;
      procedure nmCursorSetToBookmark(var Msg : TffDataMessage);
                message ffnmCursorSetToBookmark;
      procedure nmCursorSetToCursor(var Msg : TffDataMessage);
                message ffnmCursorSetToCursor;
      procedure nmCursorSetToEnd(var Msg : TffDataMessage);
                message ffnmCursorSetToEnd;
      procedure nmCursorSetToKey(var Msg : TffDataMessage);
                message ffnmCursorSetToKey;
      procedure nmCursorSwitchToIndex(var Msg : TffDataMessage);
                message ffnmCursorSwitchToIndex;
      procedure nmCursorSetFilter(var Msg : TffDataMessage);
                message ffnmCursorSetFilter;
      procedure nmDatabaseAddAlias(var Msg : TffDataMessage);
                message ffnmDatabaseAddAlias;
      procedure nmDatabaseAliasList(var Msg : TffDataMessage);
                message ffnmDatabaseAliasList;
      procedure nmDatabaseChgAliasPath(var Msg : TffDataMessage);
                message ffnmDatabaseChgAliasPath;
      procedure nmDatabaseClose(var Msg : TffDataMessage);
                message ffnmDatabaseClose;
      procedure nmDatabaseDeleteAlias(var Msg : TffDataMessage);
                message ffnmDatabaseDeleteAlias;
      procedure nmDatabaseGetAliasPath(var Msg : TffDataMessage);
                message ffnmDatabaseGetAliasPath;
      procedure nmDatabaseGetFreeSpace(var Msg : TffDataMessage);
                message ffnmDatabaseGetFreeSpace;
      procedure nmDatabaseModifyAlias(var Msg : TffDataMessage);
                message ffnmDatabaseModifyAlias;
      procedure nmDatabaseOpen(var Msg : TffDataMessage);
                message ffnmDatabaseOpen;
      procedure nmDatabaseOpenNoAlias(var Msg : TffDataMessage);
                message ffnmDatabaseOpenNoAlias;
      procedure nmDatabaseSetTimeout(var Msg : TffDataMessage);
                message ffnmDatabaseSetTimeout;
      procedure nmDatabaseTableExists(var Msg : TffDataMessage);
                message ffnmDatabaseTableExists;
      procedure nmDatabaseTableList(var Msg : TffDataMessage);
                message ffnmDatabaseTableList;
      procedure nmDatabaseTableLockedExclusive(var Msg : TffDataMessage);
                message ffnmDatabaseTableLockedExclusive;
      procedure nmDeleteBLOB(var Msg : TffDataMessage);
                message ffnmDeleteBLOB;
      procedure nmDeleteTable(var Msg : TffDataMessage);
                message ffnmDeleteTable;
      procedure nmDetachServerJIC(var Msg : TffDataMessage);
                message ffnmDetachServerJIC;
      procedure nmDropIndex(var Msg : TffDataMessage);
                message ffnmDropIndex;
      procedure nmEmptyTable(var Msg : TffDataMessage);
                message ffnmEmptyTable;
      procedure nmEndTransaction(var Msg : TffDataMessage);
                message ffnmEndTransaction;
      procedure nmFreeBLOB(var Msg : TffDataMessage);
                message ffnmFreeBLOB;
      procedure nmGetTableAutoIncValue(var Msg : TffDataMessage);
                message ffnmGetTableAutoIncValue;
      procedure nmGetBLOBLength(var Msg : TffDataMessage);
                message ffnmGetBLOBLength;
      procedure nmGetRebuildStatus(var Msg : TffDataMessage);
                message ffnmGetRebuildStatus;
      procedure nmGetServerDateTime(var Msg : TffDataMessage);
                message ffnmGetServerDateTime;
                                                               {begin !!.07}
      procedure nmGetServerSystemTime(var Msg : TffDataMessage);
                message ffnmGetServerSystemTime;
      procedure nmGetServerGUID(var Msg : TffDataMessage);
                message ffnmGetServerGUID;
      procedure nmGetServerID(var Msg : TffDataMessage);
                message ffnmGetServerID;                         {end !!.07}
      procedure nmGetTableDictionary(var Msg : TffDataMessage);
                message ffnmGetTableDictionary;
      procedure nmGetTableRecCount(var Msg : TffDataMessage);
                message ffnmGetTableRecCount;
      procedure nmGetTableRecCountAsync(var Msg : TffDataMessage);     {!!.07}
                message ffnmGetTableRecCountAsync;                     {!!.07}
{Begin !!.11}
      procedure nmGetTableVersion(var Msg : TffDataMessage);
                message ffnmGetTableVersion;
{End !!.11}
      procedure nmIsTableLocked(var Msg : TffDataMessage);
                message ffnmIsTableLocked;
{Begin !!.03}
      procedure nmListBLOBSegments(var Msg : TffDataMessage);
                message ffnmListBLOBSegments;
{End !!.03}
      procedure nmOpenTable(var Msg : TffDataMessage);
                message ffnmOpenTable;
      procedure nmPackTable(var Msg : TffDataMessage);
                message ffnmPackTable;
      procedure nmReadBLOB( var Msg : TffDataMessage );
                message ffnmReadBLOB;
      procedure nmRecordDelete( var Msg : TffDataMessage );
                message ffnmRecordDelete;
      procedure nmRecordDeleteBatch(var Msg : TffDataMessage);
                message ffnmRecordDeleteBatch;
      procedure nmRecordExtractKey(var Msg : TffDataMessage);
                message ffnmRecordExtractKey;
      procedure nmRecordGet(var Msg : TffDataMessage);
                message ffnmRecordGet;
      procedure nmRecordGetBatch(var Msg : TffDataMessage);
                message ffnmRecordGetBatch;
      procedure nmRecordGetForKey(var Msg : TffDataMessage);
                message ffnmRecordGetForKey;
      procedure nmRecordGetForKey2(var Msg : TffDataMessage);
                message ffnmRecordGetForKey2;
      procedure nmRecordGetNext(var Msg : TffDataMessage);
                message ffnmRecordGetNext;
      procedure nmRecordGetPrev(var Msg : TffDataMessage);
                message ffnmRecordGetPrev;
      procedure nmRecordInsert(var Msg : TffDataMessage);
                message ffnmRecordInsert;
      procedure nmRecordInsertBatch(var Msg : TffDataMessage);
                message ffnmRecordInsertBatch;
      procedure nmRecordIsLocked(var Msg : TffDataMessage);
                message ffnmRecordIsLocked;
      procedure nmRecordModify(var Msg : TffDataMessage);
                message ffnmRecordModify;
      procedure nmRecordRelLock(var Msg : TffDataMessage);
                message ffnmRecordRelLock;
      procedure nmReindexTable(var Msg : TffDataMessage);
                message ffnmReindexTable;
      procedure nmRelTableLock(var Msg : TffDataMessage);
                message ffnmRelTableLock;
      procedure nmRenameTable(var Msg : TffDataMessage);
                message ffnmRenameTable;
      procedure nmRestructureTable(var Msg : TffDataMessage);
                message ffnmRestructureTable;
      procedure nmServerIsReadOnly(var Msg : TffDataMessage);
                message ffnmServerIsReadOnly;
                                                               {begin !!.07}
      procedure nmServerStatistics(var Msg : TffDataMessage);
                message ffnmServerStatistics;
      procedure nmCmdHandlerStatistics(var Msg : TffDataMessage);
                message ffnmCmdHandlerStatistics;
      procedure nmTransportStatistics(var Msg : TffDataMessage);
                message ffnmTransportStatistics;                 {end !!.07}
      procedure nmSessionAdd(var Msg : TffDataMessage);
                message ffnmSessionAdd;
      procedure nmSessionClose(var Msg : TffDataMessage);
                message ffnmSessionClose;
      procedure nmSessionCloseInactiveTables(var Msg : TffDataMessage);
                message ffnmSessionCloseInactTbl;
      procedure nmSessionGetCurrent(var Msg : TffDataMessage);
                message ffnmSessionGetCurrent;
      procedure nmSessionSetCurrent(var Msg : TffDataMessage);
                message ffnmSessionSetCurrent;
      procedure nmSessionSetTimeout(var Msg : TffDataMessage);
                message ffnmSessionSetTimeout;
      procedure nmSetTableAutoIncValue(var Msg : TffDataMessage);
                message ffnmSetTableAutoIncValue;
      procedure nmSQLAlloc(var Msg : TffDataMessage);
                message ffnmSQLAlloc;
      procedure nmSQLPrepare(var Msg : TffDataMessage);
                message ffnmSQLPrepare;
      procedure nmSQLExec(var Msg : TffDataMessage);
                message ffnmSQLExec;
      procedure nmSQLExecDirect(var Msg : TffDataMessage);
                message ffnmSQLExecDirect;
      procedure nmSQLSetParams(var Msg : TffDataMessage);
                message ffnmSQLSetParams;
      procedure nmSQLFree(var Msg : TffDataMessage);
                message ffnmSQLFree;
      procedure nmStartTransaction(var Msg : TffDataMessage);
                message ffnmStartTransaction;
      procedure nmStartTransactionWith(var Msg : TffDataMessage);      {!!.10}
                message ffnmStartTransactionWith;                      {!!.10}
      procedure nmTruncateBLOB(var Msg : TffDataMessage);
                message ffnmTruncateBLOB;
      procedure nmWriteBLOB( var Msg : TffDataMessage );
                message ffnmWriteBLOB;

      procedure schDisposeRecord(Sender : TffBaseHashTable;            {!!.05}
                                 aData : Pointer);                     {!!.05}

      procedure schOnAddClient(Listener : TffBaseTransport;
                         const userID : TffName;
                         const timeout : longInt;
                         const clientVersion : longInt;
                           var passwordHash : TffWord32;
                           var aClientID : TffClientID;
                           var errorCode : TffResult;
                           var isSecure : boolean;
                           var aVersion : longInt);
        { This method is called when the transport needs to establish a new
          client. }

      procedure schOnRemoveClient(Listener : TffBaseTransport;
                            const aClientID : TffClientID;
                              var errorCode : TffResult);
        { This method is called when the transport needs to remove an existing
          client. }

    protected

      {State methods}
      procedure scInitialize; override;
        { This method is called when the command handler is to perform
          its initialization. }

      procedure scPrepareForShutdown; override;
        { This method is called when the command handler is to prepare for
          shutdown. }

      procedure scShutdown; override;
        { This method is called when the command handler is to stop processing
          requests. }

      procedure scStartup; override;
        { This method is called when the command handler is to start processing
          requests. }

    public
      constructor Create(aOwner : TComponent); override;

      destructor Destroy; override;

      procedure DefaultHandler(var Message); override;
        { If this command handler does not have a method specifically for
          a received message, the TObject.Dispatch method will pass the
          message to this method.  This method hands the message of to this
          class' ancestor so that default handling may be applied (i.e., see
          if the plugins or engine manager recognize the message. }

      procedure FFAddDependent(ADependent : TffComponent); override;   {!!.11}
        { This overridden method sets the OnAddclient and OnRemoveClient events
          of the registering transport. }

      procedure Process(Msg : PffDataMessage); override;
        { This method is called by the transport in order to process a message.
          The message is first routed to the server engine.  If the server engine
          does not handle the message then it is forwarded to the plugin(s).  If
          a plugin does not handle the message, it is finally forwarded to the
          engine manager(s).}

  end;

implementation

uses
  ComObj,
  ffsqlbas,
  ffsrlock;

const
  { Logging constants }
  csBlobNr   = '  BLOBNr    %d:%d';
  csClientID = '  ClientID  %d';
  csCursorID = '  CursorID  %d';
  csErr      = '*ERROR*  %x';

{===TffServerCommandHandler==========================================}
constructor TffServerCommandHandler.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
  schSavedAddClientEvents := TffThreadHash.Create(ffc_Size59);               {!!.05}
  schSavedAddClientEvents.OnDisposeData := schDisposeRecord;           {!!.05}
end;
{--------}
destructor TffServerCommandHandler.Destroy;
begin
  schSavedAddClientEvents.Clear;                                      {!!.05}
  schSavedAddClientEvents.Free;                                       {!!.05}
  schSavedAddClientEvents := nil;                                     {!!.05}
  inherited Destroy;
end;
{--------}
procedure TffServerCommandHandler.DefaultHandler(var Message);
begin
  { The server engine does not handle this message.  Hand it off to our
    ancestor class for default handling. }
  inherited Process(@Message);
end;
{--------}
procedure TffServerCommandHandler.Process(Msg : PffDataMessage);
begin
  Dispatch(Msg^);
  bchFreeMsg(Msg);
end;
{--------}
procedure TffServerCommandHandler.nmAcqTableLock(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmAcqTableLockReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['AcqTableLock',
                 Format(csClientID, [dmClientID]),
                 Format(csCursorID, [CursorID]),
                 Format('  LockType %d', [byte(LockType)])]);

    Error := FServerEngine.TableLockAcquire(CursorID, LockType);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

    TffBaseTransport.Reply(ffnmAcqTableLock, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmAddFileBLOB(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmAddFileBLOBRpy;
begin
  with Msg, PffnmAddFileBLOBReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['AddFileBLOB',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  FileName %s', [FileName])]);

    Error := FServerEngine.FileBLOBAdd(CursorID, FileName, Reply.BLOBNr);
    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt(csBLOBNr, [Reply.BLOBNr.iHigh, Reply.BLOBNr.iLow]);  {!!.03}
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmAddFileBLOB, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmAddIndex(var Msg : TffDataMessage);
var
  Error  : TffResult;
begin
  with Msg, PffnmAddIndexReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['AddIndex',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID %d', [DatabaseID]),
                 format(csCursorID, [CursorID]),
                 format('  TblName  [%s]', [TableName])]);

    Error := FServerEngine.TableAddIndex(DatabaseID, CursorID, TableName, IndexDesc);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmAddIndex, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmBuildTable(var Msg : TffDataMessage);
{ Input stream is expected to be:
      DatabaseId (longint)
      OverWrite  (Boolean)
      TableName  (TffTableName)
      Dictionary (TffServerDataDict or TffDataDictionary)
      FieldMap   (one TffShStr for each field map entry; final entry
                  followed by a zero byte to signal end-of-list.  If
                  no field map is given, then a single zero byte must be
                  present
}
var
  Error  : TffResult;
  Stream : TMemoryStream;
  DatabaseID : LongInt;
  OverWrite : Boolean;
  TableName : TffTableName;
  Dictionary : TffServerDataDict;
  DictionaryStart: LongInt;
begin
  with Msg do begin
    Stream := TMemoryStream.Create;
    Stream.Write(dmData^, dmDataLen);
    Stream.Position := 0;
    Stream.Read(DatabaseID, SizeOf(DatabaseID));
    Stream.Read(OverWrite, SizeOf(OverWrite));
    Stream.Read(TableName, SizeOf(TableName));
    Dictionary := TffServerDataDict.Create(4096);
    try
      DictionaryStart := Stream.Position;
      Dictionary.ReadFromStream(Stream);

      if FLogEnabled then begin
        ichLogAll(['BuildTable',
                   format(csClientID, [dmClientID]),
                   format('  DBase ID %d', [DatabaseID]),
                   format('  OverWrite %d', [ord(OverWrite)]),
                   format('  TblName  [%s]', [TableName])]);
        ichLogBlock('  Dictionary',
                    Addr(PffByteArray(Stream.Memory)^[DictionaryStart]),
                    Stream.Size - DictionaryStart);
      end;

      Error := FServerEngine.TableBuild(DatabaseID,
                                        OverWrite,
                                        TableName,
                                        false,
                                        Dictionary);
      if FLogEnabled then
        ichLogFmt(csErr, [Error]);
      TffBaseTransport.Reply(ffnmBuildTable, Error, nil, 0);
    finally
      Dictionary.Free;
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCheckSecureComms(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg do begin
    if FLogEnabled then
      ichLogAll(['CheckSecureComms',
                 format(csClientID, [dmClientID])]);

    {Note: If we get this message the client's password must have been
           OK; the transport will hangup if the clientID is unknown.}
    Error := DBIERR_NONE;
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCheckSecureComms, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmClientSetTimeout(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmClientSetTimeoutReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['ClientSetTimeout',
                 format(csClientID, [dmClientID]),
                 format('  Timeout  %d', [Timeout])]);

    Error := FServerEngine.ClientSetTimeout(dmClientID, Timeout);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmClientSetTimeout, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCreateBLOB(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmCreateBLOBRpy;
begin
  with Msg, PffnmCreateBLOBReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['CreateBLOB',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID])]);

    Error := FServerEngine.BLOBCreate(CursorID, Reply.BLOBNr);
    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt(csBLOBNr, [Reply.BLOBNr.iHigh, Reply.BLOBNr.iLow]);  {!!.03}
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmCreateBLOB, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorClone(var Msg : TffDataMessage);
var
  Error : TffResult;
  aNewCursorID : TffCursorID;
  Reply : TffnmCursorCloneRpy;
begin
  with Msg, PffnmCursorCloneReq( dmData )^ do begin
    if FLogEnabled then
      ichLogAll(['CursorClone',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  OpenMode %d', [byte(OpenMode)])]);

    Error := FServerEngine.CursorClone(CursorID, OpenMode, aNewCursorID);
    if (Error = 0) then
      Reply.CursorID := aNewCursorID;
    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt(csCursorID, [Reply.CursorID]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmCursorClone, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorClose(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmCursorCloseReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['CursorClose',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID])]);

    Error := FServerEngine.CursorClose(CursorID);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCursorClose, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorCompareBMs(var Msg : TffDataMessage);
var
  Error : TffResult;
  BM2   : PffByteArray;
  Reply : TffnmCursorCompareBMsRpy;
begin
  with Msg, PffnmCursorCompareBMsReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['CompareBookmarks',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  BM Size  %d', [BookmarkSize])]);

    BM2 := PffByteArray(PAnsiChar(@Bookmark1) + BookmarkSize);
    if FLogEnabled then begin
      ichLogBlock('  BM1', @Bookmark1, BookmarkSize);
      ichLogBlock('  BM2', BM2, BookmarkSize);
    end;
    Error := FServerEngine.CursorCompareBookmarks(CursorID, @Bookmark1, BM2, Reply.CompareResult);
    if (Reply.CompareResult < 0) then
      Reply.CompareResult := -1
    else if (Reply.CompareResult > 0) then
      Reply.CompareResult := 1;
    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  Compare  %d', [Reply.CompareResult]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmCursorCompareBMs, Error, @Reply, sizeof(Reply));
  end;
end;
{Begin !!.02}
{--------}
procedure TffServerCommandHandler.nmCursorCopyRecords(var Msg : TffDataMessage);
var
  CopyBLOBsStr : string;
  Error : TffResult;
begin
  with Msg, PffnmCursorCopyRecordsReq(dmData)^ do begin
    if FLogEnabled then begin
      if CopyBLOBs then
        CopyBLOBsStr := 'yes'
      else
        CopyBLOBsStr := 'no';
      ichLogAll(['CopyRecords',
                 format(csClientID, [dmClientID]),
                 format('  SrcCursorID  %d', [SrcCursorID]),
                 format('  DestCursorID %d', [DestCursorID]),
                 format('  Copy blobs   %s', [CopyBLOBsStr])]);
    end;

    Error := FServerEngine.CursorCopyRecords(SrcCursorID, DestCursorID, CopyBLOBs);
    TffBaseTransport.Reply(ffnmCursorCopyRecords, Error, nil, 0);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
  end;
end;
{End !!.02}
{Begin !!.06
{--------}
procedure TffServerCommandHandler.nmCursorDeleteRecords(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmCursorDeleteRecordsReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['DeleteRecords',
                 format(csClientID, [dmClientID]),
                 format('  CursorID  %d', [CursorID])]);

    Error := FServerEngine.CursorDeleteRecords(CursorID);
    TffBaseTransport.Reply(ffnmCursorDeleteRecords, Error, nil, 0);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
  end;
end;
{End !!.06}
{Begin !!.03}
{--------}
procedure TffServerCommandHandler.nmCursorGetBLOBFreeSpace(var Msg : TffDataMessage);
var
  aBuffer : pointer;
  Error : TffResult;
  aStream: TMemoryStream;
  StreamSize : longInt;
begin
  with Msg, PffnmGetBLOBFreeSpaceReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['CursorGetBLOBFreeSpace',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID])]);

    aStream := TMemoryStream.Create;
    try
      Error := FServerEngine.CursorListBLOBFreeSpace(CursorID, InMemory,
                                                     aStream);
      StreamSize := aStream.Size;
      FFGetMem(aBuffer, StreamSize);
      aStream.Position := 0;
      aStream.Read(aBuffer^, StreamSize);

      if FLogEnabled and (Error = 0) then
        ichLogBlock('  List', aStream.Memory, StreamSize);

      TffBaseTransport.Reply(ffnmListBLOBFreeSpace, Error, aBuffer,
                             StreamSize);
      FFFreeMem(aBuffer, StreamSize);

    finally
      aStream.Free;
    end;{try..finally}

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
  end;
end;
{End !!.03}
{--------}
procedure TffServerCommandHandler.nmCursorGetBookmark(var Msg : TffDataMessage);
var
  Error : TffResult;
  BM    : PffByteArray;
begin
  with Msg, PffnmCursorGetBookmarkReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['GetBookmark',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  BM Size  %d', [BookmarkSize])]);

    FFGetMem(BM, BookmarkSize);
    try
      Error := FServerEngine.CursorGetBookmark(CursorID, BM);
      if FLogEnabled then
        if (Error = 0) then
          ichLogBlock('  Bookmark', BM, BookmarkSize);
      TffBaseTransport.Reply(ffnmCursorGetBookmark, Error, BM, BookmarkSize);
    finally
      FFFreeMem(BM, BookmarkSize);
    end;{try..finally}

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorOverrideFilter(var Msg : TffDataMessage);
var
  Error      : TffResult;
  Expression : pCANExpr;
begin
  with Msg, PffnmCursorOverrideFilterReq(dmData)^ do begin
    Expression := pCANExpr(@ExprTree);
    if FLogEnabled then begin
      ichLogAll(['OverrideFilter',
                 format('  ClientID %d', [dmClientID]),
                 format('  CursorID %d', [CursorID]),
                 format('  Timeout  %d', [Timeout])]);
      ichLogBlock('  Data', Expression, Expression^.iTotalSize);
    end;

    if Expression^.iTotalSize <= SizeOf(CANExpr) then
      Expression:= nil;

    Error := FServerEngine.CursorOverrideFilter(CursorID, Expression, Timeout);
    TffBaseTransport.Reply(ffnmCursorOverrideFilter, Error, nil, 0);

    if FLogEnabled then
      ichLogFmt('  *ERROR*  %x', [Error]);
  end;
end;
{--------}

procedure TffServerCommandHandler.nmCursorResetRange(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmCursorResetRangeReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['ResetRange',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID])]);

    Error := FServerEngine.CursorResetRange(CursorID);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCursorResetRange, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorRestoreFilter(var Msg : TffDataMessage);
var
  Error      : TffResult;
begin
  with Msg, PffnmCursorRestoreFilterReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['RestoreFilter',
                 format('  CursorID %d', [CursorID])]);

    Error := FServerEngine.CursorRestoreFilter(CursorID);
    TffBaseTransport.Reply(ffnmCursorRestoreFilter, Error, nil, 0);
    if FLogEnabled then
      ichLogFmt('  *ERROR*  %x', [Error]);
  end;
end;
{-------}
procedure TffServerCommandHandler.nmCursorSetRange(var Msg : TffDataMessage);
var
  Error : TffResult;
  pKey1, pKey2 : Pointer;
  MsgSize : longint;
  MsgData : PffByteArray;
  SubMsg  : PffsmHeader;
begin
  with Msg, PffnmCursorSetRangeReq(dmData)^ do begin
    if KeyLen1 = 0 then
      pKey1 := nil
    else
      pKey1 := @KeyData1;
    if KeyLen2 = 0 then
      pKey2 := nil
    else
      pKey2 := PffByteArray(PAnsiChar(@KeyData1) + KeyLen1);
    if FLogEnabled then begin
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
    end;

    MsgSize := (2 * ffc_SubMsgHeaderSize);
    FFGetMem(MsgData, MsgSize);
    try
      { do the SetRange First }
      SubMsg := PffsmHeader(MsgData);
      Error := FServerEngine.CursorSetRange( CursorID, DirectKey,
                                             FieldCount1, PartialLen1,
                                             pKey1, KeyIncl1,
                                             FieldCount2, PartialLen2,
                                             pKey2, KeyIncl2 );

      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmCursorSetRange,
                                   Error,
                                   nmdByteArray,
                                   nil,
                                   0);
      if FLogEnabled then
        ichLogAll([format(csErr, [Error]),
                  'SetToBegin (multipart)']);
                  
      Error := FServerEngine.CursorSetToBegin(CursorID);
      FFCreateSubMessage( SubMsg,
                          ffnmCursorSetToBegin,
                          Error,
                          nmdByteArray,
                          nil,
                          0);

      if FLogEnabled then
        ichLogFmt(csErr, [Error]);

      TffBaseTransport.Reply(ffnmMultiPartMessage, 0, MsgData, MsgSize);
    finally
      FFFreeMem(MsgData, MsgSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorSetTimeout(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmCursorSetTimeoutReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['CursorSetTimeout',
                 format(csCursorID, [CursorID]),
                 format('  Timeout  %d', [Timeout])]);

    Error := FServerEngine.CursorSetTimeout(CursorID, Timeout);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCursorSetTimeout, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorSetToBegin(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmCursorSetToBeginReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['SetToBegin',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID])]);

    Error := FServerEngine.CursorSetToBegin(CursorID);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCursorSetToBegin, Error, nil, 0);

  end;
end;
{-------}
procedure TffServerCommandHandler.nmCursorSetToBookmark(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmCursorSetToBookmarkReq(dmData)^ do begin
    if FLogEnabled then begin
      ichLogAll(['SetToBookmark',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  BM Size  %d', [BookmarkSize])]);
      ichLogBlock('  Bookmark', @Bookmark, BookmarkSize);
    end;

    Error := FServerEngine.CursorSetToBookmark(CursorID, @Bookmark);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCursorSetToBookmark, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorSetToCursor(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmCursorSetToCursorReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['SetToCursor',
                 format(csClientID, [dmClientID]),
                 format('  DestCursor %d', [DestCursorID]),
                 format('  SrcCursor  %d', [SrcCursorID])]);

    Error := FServerEngine.CursorSetToCursor(DestCursorID, SrcCursorID);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCursorSetToCursor, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorSetToEnd(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmCursorSetToEndReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['SetToEnd',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID])]);

    Error := FServerEngine.CursorSetToEnd(CursorID);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCursorSetToEnd, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorSetToKey(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmCursorSetToKeyReq(dmData)^ do begin
    if FLogEnabled then begin
      ichLogAll(['SetToKey',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  Action   %d', [byte(Action)]),
                 format('  DrctKey  %d', [byte(DirectKey)]),
                 format('  FldCount %d', [FieldCount]),
                 format('  PartLen  %d', [PartialLen]),
                 format('  DataLen  %d', [KeyDataLen])]);
      ichLogBlock('  Data', @KeyData, KeyDataLen);
    end;

    Error := FServerEngine.CursorSetToKey(CursorID, Action, DirectKey,
                                          FieldCount, PartialLen, @KeyData);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCursorSetToKey, Error, nil, 0);

  end;
end;
{--------}

procedure TffServerCommandHandler.nmCursorSwitchToIndex(var Msg : TffDataMessage);
var
  Error : TffResult;
  MsgSize : longint;
  MsgData : PffByteArray;
  SubMsg  : PffsmHeader;
begin
  with Msg, PffnmCursorSwitchToIndexReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['SwitchToIndex',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  InxName  [%s]', [IndexName]),
                 format('  InxNum   %d', [IndexNumber]),
                 format('  PosnRec  %d', [byte(PosnOnRec)])]);

    if byte(PosnOnRec) <> 0 then begin
      Error := FServerEngine.CursorSwitchToIndex(CursorID,
                                                 IndexName, IndexNumber,
                                                 PosnOnRec);
      if FLogEnabled then
        ichLogFmt(csErr, [Error]);

      TffBaseTransport.Reply(ffnmCursorSwitchToIndex, Error, nil, 0);
      
    end else begin
      MsgSize := (2 * ffc_SubMsgHeaderSize);
      FFGetMem(MsgData, MsgSize);
      try
        { do the SwitchToIndex First }
        SubMsg := PffsmHeader(MsgData);
        Error := FServerEngine.CursorSwitchToIndex(CursorID,
                                                   IndexName, IndexNumber,
                                                   PosnOnRec);
        SubMsg := FFCreateSubMessage(SubMsg,
                                     ffnmCursorSwitchToIndex,
                                     Error,
                                     nmdByteArray,
                                     nil,
                                     0);
        if FLogEnabled then
          ichLogAll([format(csErr, [Error]),
                     'SetToBegin (multipart)']);

        Error := FServerEngine.CursorSetToBegin(CursorID);
        FFCreateSubMessage( SubMsg,
                            ffnmCursorSetToBegin,
                            Error,
                            nmdByteArray,
                            nil,
                            0);
        if FLogEnabled then
          ichLogFmt(csErr, [Error]);

        TffBaseTransport.Reply(ffnmMultiPartMessage, 0, MsgData, MsgSize);
      finally
        FFFreeMem(MsgData, MsgSize);
      end;
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCursorSetFilter(var Msg : TffDataMessage);
var
  Error      : TffResult;
  Expression : pCANExpr;
begin
  with Msg, PffnmCursorSetFilterReq(dmData)^ do begin
    Expression := pCANExpr(@ExprTree);
    
    if FLogEnabled then begin
      ichLogAll(['SetFilter',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  Timeout  %d', [Timeout])]);
      ichLogBlock('  Data', Expression, Expression^.iTotalSize);
    end;

//    if Expression^.iTotalSize <= SizeOf(CANExpr) then                {Deleted !!.01}
//      Expression:= nil;                                              {Deleted !!.01}

    Error := FServerEngine.CursorSetFilter(CursorID, Expression, Timeout);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmCursorSetFilter, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseAddAlias(var Msg : TffDataMessage);
{ Rewritten !!.11}
var
  Error : TffResult;
begin
  if Msg.dmDataLen = SizeOf(TffnmOldDatabaseAddAliasReq) then
    with Msg, PffnmOldDatabaseAddAliasReq(dmData)^ do begin
      if FLogEnabled then
        ichLogAll(['DatabaseAddAlias - Old',
                   format(csClientID, [dmClientID]),
                   format('  Alias    [%s]', [Alias]),
                   format('  Path     [%s]', [Path])]);

      Error := FServerEngine.DatabaseAddAlias(Alias,
                                              Path,
                                              False,
                                              dmClientID);
    end  { with }
  else
    with Msg, PffnmDatabaseAddAliasReq(dmData)^ do begin
      if FLogEnabled then
        ichLogAll(['DatabaseAddAlias',
                   format(csClientID, [dmClientID]),
                   format('  Alias     [%s]', [Alias]),
                   format('  Path      [%s]', [Path]),
                   format('  Checkdisk [%d]', [Byte(CheckDisk)])]);    {!!.13}

      Error := FServerEngine.DatabaseAddAlias(Alias,
                                              Path,
                                              CheckDisk,
                                              dmClientID);
    end;  { with }

  if FLogEnabled then
    ichLogFmt(csErr, [Error]);
  TffBaseTransport.Reply(ffnmDatabaseAddAlias, Error, nil, 0);
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseAliasList(var Msg : TffDataMessage);
var
  aBuffer : pointer;
  aList : TList;
  anAlias : PffAliasDescriptor;
  Error : TffResult;
  index : longInt;
  Stream: TMemoryStream;
  StreamSize : longInt;
begin
  with Msg do begin

    if FLogEnabled then
      ichLogAll(['DatabaseAliasList',
                 format(csClientID, [dmClientID])]);

    Stream := TMemoryStream.Create;
    aList := TList.Create;
    try
      Error := FServerEngine.DatabaseAliasList(aList, dmClientID);
      {Write the list of alias information to the stream. }
      for index := 0 to pred(aList.count) do begin
        anAlias := PffAliasDescriptor(aList.items[index]);
        Stream.WriteBuffer(anAlias^,sizeOf(TffAliasDescriptor));
      end;

      { Free the returned items. }
      for index := pred(aList.Count) downto 0 do begin
        anAlias := PffAliasDescriptor(aList.items[index]);
        FFFreeMem(anAlias, sizeOf(TffAliasDescriptor));
      end;

      StreamSize := Stream.Size;
      FFGetMem(aBuffer, StreamSize);
      Stream.Position := 0;
      Stream.Read(aBuffer^, StreamSize);

      if FLogEnabled and (Error = 0) then
        ichLogBlock('  List', Stream.Memory, StreamSize);

      TffBaseTransport.Reply(ffnmDatabaseAliasList, Error, aBuffer, StreamSize);
      FFFreeMem(aBuffer, StreamSize);

    finally
      Stream.Free;
      aList.Free;
    end;{try..finally}

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseChgAliasPath(var Msg : TffDataMessage);
{Rewritten !!.11}
var
  Error : TffResult;
begin
  if Msg.dmDataLen = SizeOf(TffnmOldDatabaseChgAliasPathReq) then
    with Msg, PffnmOldDatabaseChgAliasPathReq(dmData)^ do begin

      if FLogEnabled then
        ichLogAll(['DatabaseChgAliasPath - Old',
                   format(csClientID, [dmClientID]),
                   format('  Alias    [%s]', [Alias]),
                   format('  NewPath  [%s]', [NewPath])]);

      Error := FServerEngine.DatabaseChgAliasPath(Alias,
                                                  NewPath,
                                                  False,
                                                  dmClientID);
    end  { with }
  else
    with Msg, PffnmDatabaseChgAliasPathReq(dmData)^ do begin

      if FLogEnabled then
        ichLogAll(['DatabaseChgAliasPath',
                   format(csClientID, [dmClientID]),
                   format('  Alias     [%s]', [Alias]),
                   format('  NewPath   [%s]', [NewPath]),
                   format('  Checkdisk [%s]', [Byte(CheckDisk)])]);

      Error := FServerEngine.DatabaseChgAliasPath(Alias,
                                                  NewPath,
                                                  CheckDisk,
                                                  dmClientID);
    end;  { with }

  if FLogEnabled then
    ichLogFmt(csErr, [Error]);
  TffBaseTransport.Reply(ffnmDatabaseChgAliasPath, Error, nil, 0);

end;
{--------}
procedure TffServerCommandHandler.nmDatabaseClose(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmDatabaseCloseReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['DatabaseClose',
                 format(csClientID, [dmClientID]),
                 format('  DBaseID  %d', [DatabaseID])]);

    Error := FServerEngine.DatabaseClose(DatabaseID);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmDatabaseClose, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseDeleteAlias(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmDatabaseDeleteAliasReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DatabaseDeleteAlias',
                 format(csClientID, [dmClientID]),
                 format('  Alias    [%s]', [Alias])]);

    Error := FServerEngine.DatabaseDeleteAlias(Alias, dmClientID);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmDatabaseDeleteAlias, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseGetAliasPath(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmDatabaseGetAliasPathRpy;
  Path  : TffPath;
begin
  with Msg, PffnmDatabaseGetAliasPathReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DatabaseGetAliasPath',                               {!!.10}
                 Format(csClientID, [dmClientID]),
                 Format('    Alias  %s', [Alias])]);

    Error := FServerEngine.DatabaseGetAliasPath(Alias, Path, dmClientID);

    if (Error = 0) then
      Reply.Path := Path;

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('      Path %s', [Reply.Path]);                      {!!.02}
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmDatabaseGetAliasPath, Error, @Reply, SizeOf(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseGetFreeSpace(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmDatabaseGetFreeSpaceRpy;
  FreeSpace : Longint;
begin
  with Msg, PffnmDatabaseGetFreeSpaceReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DatabaseGetFreespace',
                 Format(csClientID, [dmClientID]),
                 Format('  DBaseID  %d', [DatabaseID])]);

    Error := FServerEngine.DatabaseGetFreeSpace(DatabaseID, FreeSpace);

    if (Error = 0) then
      Reply.FreeSpace := FreeSpace;

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  Free Space %d', [Reply.FreeSpace]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmDatabaseGetFreeSpace, Error, @Reply, SizeOf(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseModifyAlias(var Msg : TffDataMessage);
{Rewritten !!.11}
var
  Error  : TffResult;
begin
  if Msg.dmDataLen = SizeOf(TffnmOldDatabaseModifyAliasReq) then
    with Msg, PffnmOldDatabaseModifyAliasReq(dmData)^ do begin
      if FLogEnabled then
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
    end  { while }
  else
    with Msg, PffnmDatabaseModifyAliasReq(dmData)^ do begin
      if FLogEnabled then
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
    end;  { while }

  if FLogEnabled then
    ichLogFmt(csErr, [Error]);
  TffBaseTransport.Reply(ffnmDatabaseModifyAlias, Error, nil, 0);
end;
{--------}
procedure TffServerCommandHandler.nmDetachServerJIC(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg do begin
    if FLogEnabled then
      ichLogAll(['DetachServer - just in case',
                 format(csClientID, [dmClientID])]);
    Error := FServerEngine.ClientRemove(dmClientID);
    { No response necessary. }

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseOpen(var Msg : TffDataMessage);
var
  Error : TffResult;
  aDatabaseID : TffDatabaseID;
  Reply : TffnmDatabaseOpenRpy;
begin
  with Msg, PffnmDatabaseOpenReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DatabaseOpen',
                 format(csClientID, [dmClientID]),
                 format('  Alias    [%s]', [Alias]),
                 format('  OpenMode %d', [byte(OpenMode)]),
                 format('  ShrMode  %d', [byte(ShareMode)]),
                 format('  Timeout  %d', [Timeout])]);

    Error := FServerEngine.DatabaseOpen(dmClientID,
                                        Alias,
                                        OpenMode,
                                        ShareMode,
                                        Timeout,
                                        aDatabaseID);
    if (Error = 0) then
      Reply.DatabaseID := aDatabaseID;

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  DBase ID %d', [Reply.DatabaseID]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmDatabaseOpen, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseOpenNoAlias(var Msg : TffDataMessage);
var
  Error : TffResult;
  aDatabaseID : TffDatabaseID;
  Reply : TffnmDatabaseOpenNoAliasRpy;
begin
  with Msg, PffnmDatabaseOpenNoAliasReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DatabaseOpenNoAlias',
                 format(csClientID, [dmClientID]),
                 format('  Path     [%s]', [Path]),
                 format('  OpenMode %d', [byte(OpenMode)]),
                 format('  ShrMode  %d', [byte(ShareMode)]),
                 format('  Timeout  %d', [Timeout])]);

    Error := FServerEngine.DatabaseOpenNoAlias(dmClientID,
                                               Path,
                                               OpenMode,
                                               ShareMode,
                                               Timeout,
                                               aDatabaseID);
    if (Error = 0) then
      Reply.DatabaseID := aDatabaseID;

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  DBase ID %d', [Reply.DatabaseID]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmDatabaseOpenNoAlias, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseSetTimeout(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmDatabaseSetTimeoutReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DatabaseSetTimeout',
                 format('  DatabaseID %d', [DatabaseID]),
                 format('  Timeout  %d', [Timeout])]);

    Error := FServerEngine.DatabaseSetTimeout(DatabaseID, Timeout);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

    TffBaseTransport.Reply(ffnmDatabaseSetTimeout, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseTableExists(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmDatabaseTableExistsRpy;
begin
  with Msg, PffnmDatabaseTableExistsReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DatabaseTableExists',
                 format('  DatabaseID %d', [DatabaseID]),
                 format('  TblName  %s', [TableName])]);               {!!.01}

    Error := FServerEngine.DatabaseTableExists(DatabaseID, TableName, Reply.Exists);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

    TffBaseTransport.Reply(ffnmDatabaseTableExists, Error, @Reply, sizeof(Reply)); {!!.01}
  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseTableList(var Msg : TffDataMessage);
var
  aBuffer : Pointer;
  aList : TList;
  aTable : PffTableDescriptor;
  Error : TffResult;
  index : longInt;
  Stream: TMemoryStream;
  StreamSize : longInt;
begin
  with Msg, PffnmDatabaseTableListReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DatabaseTableList',
                 format(csClientID, [dmClientID]),
                 format('  DBaseID  %d', [DatabaseID]),
                 format('  Mask     [%s]', [Mask])]);

    aList := TList.Create;
    Stream := TMemoryStream.Create;
    try
      Error := FServerEngine.DatabaseTableList(DatabaseID, Mask, aList);
      { Write the table descriptions to the stream. }
      for index := 0 to pred(aList.Count) do begin
        aTable := PffTableDescriptor(aList.Items[index]);
        Stream.WriteBuffer(aTable^, sizeOf(TffTableDescriptor));
      end;

      { Free the table descriptions. }
      for index := pred(aList.Count) downto 0 do begin
        aTable := PffTableDescriptor(aList.Items[index]);
        FFFreeMem(aTable, sizeOf(TffTableDescriptor));
      end;

      StreamSize := Stream.Size;
      FFGetMem(aBuffer, StreamSize);
      Stream.Position := 0;
      Stream.Read(aBuffer^, StreamSize);

      if FLogEnabled and(Error = 0) then
        ichLogBlock('  List', Stream.Memory, StreamSize);
      TffBaseTransport.Reply(ffnmDatabaseTableList, Error, aBuffer, StreamSize);
      FFFreeMem(aBuffer, StreamSize);

    finally
      aList.Free;
      Stream.Free;
    end;

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmDatabaseTableLockedExclusive(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmDatabaseTableLockedExclusiveRpy;
begin
  with Msg, PffnmDatabaseTableLockedExclusiveReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DatabaseTableExists',
                 format('  DatabaseID %d', [DatabaseID]),
                 format('  TblName  %d', [TableName])]);

    Error := FServerEngine.DatabaseTableLockedExclusive(DatabaseID, TableName, Reply.Locked);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

    TffBaseTransport.Reply(ffnmDatabaseTableLockedExclusive, Error, nil, 0);
  end;
end;

{--------}
procedure TffServerCommandHandler.nmDeleteBLOB(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmDeleteBLOBReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DeleteBLOB',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format(csBLOBNr, [BLOBNr.iHigh, BLOBNr.iLow])]);      {!!.03}

    Error := FServerEngine.BLOBDelete(CursorID, BLOBNr);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmDeleteBLOB, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmDeleteTable(var Msg : TffDataMessage);
var
  Error  : TffResult;
begin
  with Msg, PffnmDeleteTableReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['DeleteTable',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID %d', [DatabaseID]),
                 format('  TblName  [%s]', [TableName])]);

    Error := FServerEngine.TableDelete(DatabaseID, TableName);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmDeleteTable, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmDropIndex(var Msg : TffDataMessage);
var
  Error  : TffResult;
begin
  with Msg, PffnmDropIndexReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['DropIndex',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID %d', [DatabaseID]),
                 format(csCursorID, [CursorID]),
                 format('  TblName  [%s]', [TableName]),
                 format('  InxName  [%s]', [IndexName]),
                 format('  IndexID  [%d]', [IndexNumber])]);

    Error := FServerEngine.TableDropIndex(DatabaseID, CursorID, TableName,
                                          IndexName, IndexNumber);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmDropIndex, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmEmptyTable(var Msg : TffDataMessage);
var
  Error  : TffResult;
begin
  with Msg, PffnmEmptyTableReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['EmptyTable',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID %d', [DatabaseID]),
                 format(csCursorID, [CursorID]),
                 format('  TblName  [%s]', [TableName])]);

    Error := FServerEngine.TableEmpty(DatabaseID, CursorID, TableName);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmEmptyTable, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmEndTransaction(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmEndTransactionReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['EndTransaction',
                 format('  ClientID    %d', [dmClientID]),
                 format('  Database ID %d', [DatabaseID]),
                 format('  Commit?     %d', [byte(ToBeCommitted)])]);

    if ToBeCommitted then
      Error := FServerEngine.TransactionCommit(DatabaseID)
    else
      Error := FServerEngine.TransactionRollback(DatabaseID);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmEndTransaction, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmFreeBLOB(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmFreeBLOBReq( dmData )^ do begin
    
    if FLogEnabled then
      ichLogAll(['FreeBLOB',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format(csBlobNr, [BLOBNr.iHigh, BLOBNr.iLow]),        {!!.03}
                 format('  Read-Only %d', [byte(readOnly)])]);

    Error := FServerEngine.BLOBFree(CursorID, BLOBNr, readOnly);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmFreeBLOB, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmGetTableAutoIncValue(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetTableAutoIncValueRpy;
begin
  with Msg, PffnmGetTableAutoIncValueReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['GetTableAutoIncValue',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID])]);
    Error := FServerEngine.TableGetAutoInc(CursorID, Reply.AutoIncValue);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  AutoInc  %d', [Reply.AutoIncValue]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmGetTableAutoIncValue, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmGetBLOBLength(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetBLOBLengthRpy;
begin
  with Msg, PffnmGetBLOBLengthReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['GetBLOBLength',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format(csBLOBNr, [BLOBNr.iHigh, BLOBNr.iLow])]);      {!!.03}

    Error := FServerEngine.BLOBGetLength(CursorID, BLOBNr, Reply.BLOBLength);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  BLOBLen  %d', [Reply.BLOBLength]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmGetBLOBLength, Error, @Reply, sizeof(Reply));

  end;
end;
{--------}
procedure TffServerCommandHandler.nmGetRebuildStatus(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetRebuildStatusRpy;
begin
  with Msg, PffnmGetRebuildStatusReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['GetRebuildStatus',
                 format(csClientID, [dmClientID]),
                 format('  RebldID  %d', [RebuildID])]);

    Error := FServerEngine.RebuildGetStatus(RebuildID, dmClientID, Reply.IsPresent, Reply.Status);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  IsThere  %d', [ord(Reply.IsPresent)]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmGetRebuildStatus, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmGetServerDateTime(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetServerDateTimeRpy;
begin
  with Msg do begin

    if FLogEnabled then
      ichLog('GetServerDateTime');

    Reply.ServerNow := Now;
    Error := 0;

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  DateTime %s', [DateTimeToStr(Reply.ServerNow)]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmGetServerDateTime, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}                                                     {begin !!.07}
procedure TffServerCommandHandler.nmGetServerSystemTime(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetServerSystemTimeRpy;
begin
  with Msg do begin

    if FLogEnabled then
      ichLog('GetServerSystemTime');

    Error := FServerEngine.GetServerSystemTime(Reply.ServerNow);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  SystemTime %s', [DateTimeToStr(SystemTimeToDateTime(Reply.ServerNow))]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmGetServerSystemTime, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmGetServerGUID(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetServerGUIDRpy;
begin
  with Msg do begin

    if FLogEnabled then
      ichLog('GetServerGUID');

    Error := FServerEngine.GetServerGUID(Reply.GUID);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  GUID %s', [GuidToString(Reply.GUID)]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmGetServerGUID, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}                                                       {end !!.07}
procedure TffServerCommandHandler.nmGetServerID(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetServerIDRpy;
begin
  with Msg do begin

    if FLogEnabled then
      ichLog('GetServerID');

    Error := FServerEngine.GetServerID(Reply.UniqueID);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  UniqueID %s', [GuidToString(Reply.UniqueID)]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmGetServerID, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}                                                       {end !!.07}
procedure TffServerCommandHandler.nmGetTableDictionary(var Msg : TffDataMessage);
var
  aBuffer : Pointer;
  Error  : TffResult;
  Stream : TMemoryStream;
  StreamSize : longInt;
begin
  with Msg, PffnmGetTableDictionaryReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['GetTableDictionary',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID %d', [DatabaseID]),
                 format('  TblName  [%s]', [TableName])]);

    Stream := TMemoryStream.Create;
    try
      Error := FServerEngine.TableGetDictionary(DatabaseID,
                                                TableName,
                                                false,
                                                Stream);
      StreamSize := Stream.Size;
      FFGetMem(aBuffer, StreamSize);
      Stream.Position := 0;
      Stream.Read(aBuffer^, StreamSize);

      if FLogEnabled and (Error = 0) then
        ichLogBlock('  Dictionary', Stream.Memory, Stream.Size);
      TffBaseTransport.Reply(ffnmGetTableDictionary, Error, aBuffer, StreamSize);
      FFFreeMem(aBuffer, StreamSize);

    finally
      Stream.Free;
    end;{try..finally}
    
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    
  end;
end;
{--------}
procedure TffServerCommandHandler.nmGetTableRecCount(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetTableRecCountRpy;
begin
  with Msg, PffnmGetTableRecCountReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['GetTableRecCount',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID])]);

    Error := FServerEngine.TableGetRecCount(CursorID, Reply.RecCount);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  Count   %d', [byte(Reply.RecCount)]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmGetTableRecCount, Error, @Reply, sizeof(Reply));

  end;
end;
{Begin !!.07}
{--------}
procedure TffServerCommandHandler.nmGetTableRecCountAsync(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetTableRecCountAsyncRpy;
begin
  with Msg, PffnmGetTableRecCountAsyncReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['GetTableRecCountAsync',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID])]);

    Error := FServerEngine.TableGetRecCountAsync(CursorID, Reply.RebuildID);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('RebuildID %d', [Reply.RebuildID]);
      ichLogFmt(csErr, [Error]);
    end;  { if }

    TffBaseTransport.Reply(ffnmGetTableRecCountAsync, Error, @Reply,
                           SizeOf(Reply));
  end;  { with }
end;
{End !!.07}
{Begin !!.11}
{--------}
procedure TffServerCommandHandler.nmGetTableVersion(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmGetTableVersionRpy;
begin
  with Msg, PffnmGetTableVersionReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['GetTableVersion',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID %d', [DatabaseID]),
                 format('  TblName  [%s]', [TableName])]);

    Error := FServerEngine.TableVersion(DatabaseID, TableName, Reply.Version);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  Version  %d', [Reply.Version]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmGetTableVersion, Error, @Reply, sizeof(Reply));

  end;
end;
{End !!.11}
{--------}
procedure TffServerCommandHandler.nmIsTableLocked(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmIsTableLockedRpy;
begin
  with Msg, PffnmIsTableLockedReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['IsTableLocked',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  LockType %d', [byte(LockType)])]);

    Error := FServerEngine.TableIsLocked(CursorID, LockType, Reply.IsLocked);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  Locked?  %d', [byte(Reply.IsLocked)]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmIsTableLocked, Error, @Reply, sizeof(Reply));
    
  end;
end;
{Begin !!.03}
{--------}
procedure TffServerCommandHandler.nmListBLOBSegments(var Msg : TffDataMessage);
var
  aBuffer : pointer;
  Error : TffResult;
  aStream: TMemoryStream;
  StreamSize : longInt;
begin
  with Msg, PffnmListBLOBSegmentsReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['ListBLOBSegments',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format(csBLOBNr,   [BLOBNr.iHigh, BLOBNr.iLow])]);

    aStream := TMemoryStream.Create;
    try
      Error := FServerEngine.BLOBListSegments(CursorID, BLOBNr, aStream);
      StreamSize := aStream.Size;
      FFGetMem(aBuffer, StreamSize);
      aStream.Position := 0;
      aStream.Read(aBuffer^, StreamSize);

      if FLogEnabled and (Error = 0) then
        ichLogBlock('  List', aStream.Memory, StreamSize);

      TffBaseTransport.Reply(ffnmListBLOBSegments, Error, aBuffer,
                             StreamSize);
      FFFreeMem(aBuffer, StreamSize);

    finally
      aStream.Free;
    end;{try..finally}

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
  end;
end;
{End !!.03}
{--------}
procedure TffServerCommandHandler.nmOpenTable(var Msg : TffDataMessage);
var
  aBuffer : pointer;
  CursorID : TffCursorID;
  Error  : TffResult;
  Stream : TMemoryStream;
  StreamSize : longInt;
begin
  with Msg, PffnmOpenTableReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['OpenTable',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID %d', [DatabaseID]),
                 format('  TblName  [%s]', [TableName]),
                 format('  InxName  [%s]', [IndexName]),
                 format('  InxNum   %d', [IndexNumber]),
                 format('  OpenMode %d', [byte(OpenMode)]),
                 format('  Timeout  %d', [Timeout]),                   {!!.06}
                 format('  ShrMode  %d', [byte(ShareMode)])]);

    Stream := TMemoryStream.Create;
    try
      Error := FServerEngine.TableOpen(DatabaseID,
                                       TableName,
                                       false,
                                       IndexName,
                                       IndexNumber,
                                       OpenMode,
                                       ShareMode,
                                       Timeout,
                                       CursorID,
                                       Stream);
      { Note that TffServerEngine.TableOpen writes the cursorID to the
        stream. }
      if Stream.Size > 0 then begin
        StreamSize := Stream.Size;
        FFGetMem(aBuffer, StreamSize);
        Stream.Position := 0;
        Stream.Read(aBuffer^, StreamSize);
      end else begin
        aBuffer := nil;
        StreamSize := 0;
      end;

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Dictionary, etc', Stream.Memory, StreamSize);
        ichLogFmt(csErr, [Error]);
      end;

      TffBaseTransport.Reply(ffnmOpenTable, Error, aBuffer, StreamSize);

      if assigned(aBuffer) then
        FFFreeMem(aBuffer, StreamSize);

    finally
      Stream.Free;
    end;{try..finally}

//    if FLogEnabled then         {duplicated from a few lines above}  {!!.06}
//      ichLogFmt(csErr, [Error]);                                     {!!.06}

  end;
end;
{--------}
procedure TffServerCommandHandler.nmPackTable(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmPackTableRpy;
begin
  with Msg, PffnmPackTableReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['PackTable',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID %d', [DatabaseID]),
                 format('  TblName  [%s]', [TableName])]);
    
    Error := FServerEngine.TablePack(DatabaseID, TableName, Reply.RebuildID);

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  RbldID   %d', [Reply.RebuildID]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmPackTable, Error, @Reply, sizeof(Reply));
    
  end;
end;
{--------}
procedure TffServerCommandHandler.nmReadBLOB( var Msg : TffDataMessage );
var
  Error : TffResult;
  Reply : PffnmReadBLOBRpy;
  RpyLen : longint;
begin
  with Msg, PffnmReadBLOBReq( dmData )^ do begin
    
    if FLogEnabled then
      ichLogAll(['ReadBLOB',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format(csBlobNr, [BLOBNr.iLow, BLOBNr.iHigh]),
                 format('  Offset %d', [Offset]),
                 format('  Len %d', [Len])]);

    FFGetMem(Reply, Len + sizeof(longint));
    try
      Error := FServerEngine.BLOBRead(CursorID, BLOBNr, Offset, Len,
                                      Reply^.BLOB, Reply^.BytesRead );
      if Error = 0 then
        RpyLen := Reply^.BytesRead + sizeof(longint)
      else
        RpyLen := 0;

      if FLogEnabled then begin
        if (Error = 0) then begin
          ichLogFmt('  BytesRead %d', [Reply^.BytesRead]);
          ichLogBlock('  BLOB', @Reply^.BLOB, Reply^.BytesRead);
        end;
        ichLogFmt(csErr, [Error]);
      end;
      TffBaseTransport.Reply(ffnmReadBLOB, Error, Reply, RpyLen);

    finally
      FFFreeMem(Reply, Len + sizeof(longint));
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordDelete(var Msg : TffDataMessage);
var
  Error  : TffResult;
  pData  : PffByteArray;
begin
  with Msg, PffnmRecordDeleteReq( dmData )^ do begin

    if FLogEnabled then
      ichLogAll(['RecordDelete',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  RecLen   %d', [RecLen])]);

    if (RecLen <> 0) then
      FFGetMem(pData, RecLen)
    else
      pData := nil;
    try
      Error := FServerEngine.RecordDelete(CursorID, pData);

      if FLogEnabled and (Error = 0) and (RecLen <> 0) then
        ichLogBlock('  Record', pData, RecLen);
      TffBaseTransport.Reply(ffnmRecordDelete, Error, pData, RecLen);

    finally
      if (RecLen <> 0) then
        FFFreeMem(pData, RecLen);
    end;{try..finally}

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordDeleteBatch(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : PffLongintArray;
  DataSize : longint;
begin
  with Msg, PffnmRecordDeleteBatchReq( dmData )^ do begin

    if FLogEnabled then
      ichLogAll(['RecordDeleteBatch',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('   BMCount %d', [BMCount]),
                 format('   BMLen   %d', [BMLen])]);

    DataSize := BMCount * sizeof(longint);
    FFGetMem(Reply, DataSize);
    try
      Error := FServerEngine.RecordDeleteBatch(CursorID, BMCount, BMLen,
                                               PffByteArray(@BMArray),
                                               Reply);
      if FLogEnabled then
        ichLogFmt(csErr, [Error]);
      TffBaseTransport.Reply(ffnmRecordDeleteBatch, Error, Reply, DataSize);

    finally
      FFFreeMem(Reply, DataSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordExtractKey(var Msg : TffDataMessage);
var
  Error : TffResult;
  pKey : PffByteArray;
begin
  with Msg, PffnmRecordExtractKeyReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['RecordExtractKey',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  KeyLen   %d', [KeyLen]),
                 format('  ForCurrRec %d', [ord(ForCurrentRecord)])]);

    if (KeyLen <> 0) then
      FFGetMem(pKey, KeyLen)
    else
      pKey := nil;
    try
      if ForCurrentRecord then
        Error := FServerEngine.RecordExtractKey(CursorID, nil, pKey)
      else
        Error := FServerEngine.RecordExtractKey(CursorID, @Data, pKey);

      if FLogEnabled and (Error = 0) then
        ichLogBlock('  Key', pKey, KeyLen);
      TffBaseTransport.Reply(ffnmRecordExtractKey, Error, pKey, KeyLen);

    finally
      if (KeyLen <> 0) then
        FFFreeMem(pKey, KeyLen);
    end;

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordGet(var Msg : TffDataMessage);
var
  Error   : TffResult;
  MsgSize : longint;
  MsgData : PffByteArray;
  SubMsg  : PffsmHeader;
  Buffer  : PffByteArray;
begin
  with Msg, PffnmRecordGetReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['RecordGet',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  LockType %d', [byte(LockType)]),
                 format('  RecLen   %d', [RecLen]),
                 format('  BMSize   %d', [BookmarkSize])]);

    {we shall be sending back a multipart message: get record followed
     by getbookmark}
    MsgSize := (2 * ffc_SubMsgHeaderSize) + RecLen + BookmarkSize;
    FFGetMem(MsgData, MsgSize);
    try
      SubMsg := PffsmHeader(MsgData);
      if (RecLen = 0) then
        Buffer := nil
      else
        Buffer := PffByteArray(@SubMsg^.smhData);
      Error := FServerEngine.RecordGet(CursorID, LockType, Buffer);
      if (Error <> 0) then begin

        if FLogEnabled then
          ichLogFmt(csErr, [Error]);
        TffBaseTransport.Reply(ffnmRecordGet, Error, nil, 0);

        Exit;
      end;
      
      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Record', Buffer, RecLen);
        ichLogFmt(csErr, [Error]);
      end;
      

      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmRecordGet,
                                   Error,
                                   nmdByteArray,
                                   @SubMsg^.smhData,
                                   RecLen);
      
      if FLogEnabled then
        ichLog('CursorGetBookmark (multipart)');


      if (BookmarkSize <> 0) then begin
        Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
      end else
        Error := DBIERR_INVALIDBOOKMARK;


      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Bookmark', Buffer, BookmarkSize);
        ichLogFmt(csErr, [Error]);
      end;


      FFCreateSubMessage(SubMsg,
                         ffnmCursorGetBookmark,
                         Error,
                         nmdByteArray,
                         @SubMsg^.smhData,
                         BookmarkSize);
      TffBaseTransport.Reply(ffnmMultiPartMessage, 0, MsgData, MsgSize);

    finally
      FFFreeMem(MsgData, MsgSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordGetBatch(var Msg : TffDataMessage);
var
  Error    : TffResult;
  pData    : PffnmRecordGetBatchRpy;
  DataSize : longint;
begin
  with Msg, PffnmRecordGetBatchReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['RecordGetBatch',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  RecLen   %d', [RecLen]),
                 format('  RecCount %d', [RecCount])]);

    DataSize := 2*sizeof(longint) + (RecLen * RecCount);
    FFGetMem(pData, DataSize);
    try
      pData^.RecCount := 0; { just to be safe }
      Error := FServerEngine.RecordGetBatch(CursorID, RecCount,
                                            RecLen,
                                            pData^.RecCount,
                                            PffByteArray(@pData^.RecArray),
                                            pData^.Error);
      if FLogEnabled then
        ichLogAll([format('  RecCount %d', [pData^.RecCount]),
                   format('  Error    %x', [pData^.Error]),
                   format(csErr, [Error])]);

      TffBaseTransport.Reply(ffnmRecordGetBatch, Error,
                             pData, (pdata^.RecCount * RecLen) + 2*Sizeof(Longint));

    finally
      if (DataSize <> 0) then
        FFFreeMem(pData, DataSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordGetForKey(var Msg : TffDataMessage);
var
  Error : TffResult;
  MsgSize : longint;
  MsgData : PffByteArray;
  SubMsg  : PffsmHeader;
  Buffer  : PffByteArray;
begin
  with Msg, PffnmRecordGetForKeyReq(dmData)^ do begin
    
    if FLogEnabled then begin
      ichLogAll(['RecordGetForKey',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  DrctKey  %d', [byte(DirectKey)]),
                 format('  FldCount %d', [FieldCount]),
                 format('  PartLen  %d', [PartialLen]),
                 format('  RecLen   %d', [RecLen]),
                 format('  DataLen  %d', [KeyDataLen]),
                 format('  BMSize   %d', [BookmarkSize])]);
      ichLogBlock('  Data', @KeyData, KeyDataLen);
    end;


    {we shall be sending back a multipart message: RecordGetForKey}
    {followed by getbookmark}
    MsgSize := (2 * ffc_SubMsgHeaderSize) + RecLen + BookmarkSize;
    FFGetMem(MsgData, MsgSize);
    try
      { do the RecordGetForKey First }
      SubMsg := PffsmHeader(MsgData);
      if (RecLen = 0) then
        Buffer := nil
      else
        Buffer := PffByteArray(@SubMsg^.smhData);
      Error := FServerEngine.RecordGetForKey(CursorID, DirectKey, FieldCount,
                                             PartialLen, @KeyData, Buffer, True);
      if (Error <> 0) then begin
        if FLogEnabled then
          ichLogFmt(csErr, [Error]);
        TffBaseTransport.Reply(ffnmRecordGetForKey, Error, nil, 0);
        exit;
      end;

      if FLogEnabled then begin
        if Error = 0 then
          ichLogBlock('  Record', Buffer, RecLen);
        ichLogFmt(csErr, [Error]);
      end;

      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmRecordGetForKey,
                                   Error,
                                   nmdByteArray,
                                   @SubMsg^.smhData,
                                   RecLen);
      {Now do the GetBookmark }

      if FLogEnabled then
        ichLog('CursorGetBookmark (multipart)');

      if (BookmarkSize <> 0) then begin
        Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
      end
      else
        Error := DBIERR_INVALIDBOOKMARK;

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Bookmark', Buffer, BookmarkSize);
        ichLogFmt(csErr, [Error]);
      end;

      FFCreateSubMessage(SubMsg,
                         ffnmCursorGetBookmark,
                         Error,
                         nmdByteArray,
                         @SubMsg^.smhData,
                         BookmarkSize);
      TffBaseTransport.Reply(ffnmMultiPartMessage, 0, MsgData, MsgSize);
    finally
      FFFreeMem(MsgData, MsgSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordGetForKey2(var Msg : TffDataMessage);
var
  Error : TffResult;
  MsgSize : longint;
  MsgData : PffByteArray;
  SubMsg  : PffsmHeader;
  Buffer  : PffByteArray;
begin
  with Msg, PffnmRecordGetForKeyReq2(dmData)^ do begin
    
    if FLogEnabled then begin
      ichLogAll(['RecordGetForKey2',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  DrctKey  %d', [byte(DirectKey)]),
                 format('  FldCount %d', [FieldCount]),
                 format('  PartLen  %d', [PartialLen]),
                 format('  RecLen   %d', [RecLen]),
                 format('  DataLen  %d', [KeyDataLen]),
                 format('  BMSize   %d', [BookmarkSize]),
                 format('  FirstCl  %d', [Byte(FirstCall)])]);
      ichLogBlock('  Data', @KeyData, KeyDataLen);
    end;
    

    {we shall be sending back a multipart message: RecordGetForKey2}
    {followed by getbookmark}
    MsgSize := (2 * ffc_SubMsgHeaderSize) + RecLen + BookmarkSize;
    FFGetMem(MsgData, MsgSize);
    try
      { do the RecordGetForKey First }
      SubMsg := PffsmHeader(MsgData);
      if (RecLen = 0) then
        Buffer := nil
      else
        Buffer := PffByteArray(@SubMsg^.smhData);
      Error := FServerEngine.RecordGetForKey(CursorID, DirectKey, FieldCount,
                                             PartialLen, @KeyData, Buffer, FirstCall);
      if (Error <> 0) then begin
         if FLogEnabled then
           ichLogFmt(csErr, [Error]);
         TffBaseTransport.Reply(ffnmRecordGetForKey2, Error, nil, 0);
         exit;
      end;

      if FLogEnabled then begin
        if Error = 0 then
          ichLogBlock('  Record', Buffer, RecLen);
        ichLogFmt(csErr, [Error]);
      end;

      {we don't need a multipart message in case of a error...}
      if Error <> DBIERR_NONE then begin
        TffBaseTransport.Reply(ffnmRecordGetForKey2, Error, nil, 0);
        Exit;
      end;
      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmRecordGetForKey2,
                                   Error,
                                   nmdByteArray,
                                   @SubMsg^.smhData,
                                   RecLen);
      {Now do the GetBookmark }

      if FLogEnabled then
        ichLog('CursorGetBookmark (multipart)');

      if (BookmarkSize <> 0) then begin
        Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
      end
      else
        Error := DBIERR_INVALIDBOOKMARK;

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Bookmark', Buffer, BookmarkSize);
        ichLogFmt(csErr, [Error]);
      end;

      FFCreateSubMessage(SubMsg,
                         ffnmCursorGetBookmark,
                         Error,
                         nmdByteArray,
                         @SubMsg^.smhData,
                         BookmarkSize);
      TffBaseTransport.Reply(ffnmMultiPartMessage, 0, MsgData, MsgSize);
    finally
      FFFreeMem(MsgData, MsgSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordGetNext(var Msg : TffDataMessage);
var
  Error   : TffResult;
  MsgSize : longint;
  MsgData : PffByteArray;
  SubMsg  : PffsmHeader;
  Buffer  : PffByteArray;
begin
  with Msg, PffnmRecordGetNextReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['RecordGetNext',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  LockType %d', [byte(LockType)]),
                 format('  RecLen   %d', [RecLen]),
                 format('  BMSize   %d', [BookmarkSize])]);
    
    {check the rights}

    {we shall be sending back a multipart message: getnextrecord
     followed by getbookmark}
    MsgSize := (2 * ffc_SubMsgHeaderSize) + RecLen + BookmarkSize;
    FFGetMem(MsgData, MsgSize);
    try
      SubMsg := PffsmHeader(MsgData);
      if (RecLen = 0) then
        Buffer := nil
      else
        Buffer := PffByteArray(@SubMsg^.smhData);
      Error := FServerEngine.RecordGetNext(CursorID, LockType, Buffer);
      if (Error <> 0) then begin
        if FLogEnabled then
          ichLogFmt(csErr, [Error]);
        TffBaseTransport.Reply(ffnmRecordGetNext, Error, nil, 0);
        Exit;
      end;
      
      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Record', Buffer, RecLen);
        ichLogFmt(csErr, [Error]);
      end;
      
      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmRecordGetNext,
                                   Error,
                                   nmdByteArray,
                                   @SubMsg^.smhData,
                                   RecLen);
      
      if FLogEnabled then
        ichLog('CursorGetBookmark (multipart)');

      if (BookmarkSize <> 0) then begin
        Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
      end
      else
        Error := DBIERR_INVALIDBOOKMARK;

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Bookmark', Buffer, BookmarkSize);
        ichLogFmt(csErr, [Error]);
      end;

      FFCreateSubMessage(SubMsg,
                         ffnmCursorGetBookmark,
                         Error,
                         nmdByteArray,
                         @SubMsg^.smhData,
                         BookmarkSize);
      TffBaseTransport.Reply(ffnmMultiPartMessage, 0, MsgData, MsgSize);
    finally
      FFFreeMem(MsgData, MsgSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordGetPrev(var Msg : TffDataMessage);
var
  Error   : TffResult;
  MsgSize : longint;
  MsgData : PffByteArray;
  SubMsg  : PffsmHeader;
  Buffer  : PffByteArray;
begin
  with Msg, PffnmRecordGetPrevReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['RecordGetPrev',
                  format(csClientID, [dmClientID]),
                  format(csCursorID, [CursorID]),
                  format('  LockType %d', [byte(LockType)]),
                  format('  RecLen   %d', [RecLen]),
                  format('  BMSize   %d', [BookmarkSize])]);

    {check the rights}

    {we shall be sending back a multipart message: getnextrecord
     followed by getbookmark}
    MsgSize := (2 * ffc_SubMsgHeaderSize) + RecLen + BookmarkSize;
    FFGetMem(MsgData, MsgSize);
    try
      SubMsg := PffsmHeader(MsgData);
      if (RecLen = 0) then
        Buffer := nil
      else
        Buffer := PffByteArray(@SubMsg^.smhData);
      Error := FServerEngine.RecordGetPrior(CursorID, LockType, Buffer);
      if (Error <> 0) then begin
        if FLogEnabled then
          ichLogFmt(csErr, [Error]);
        TffBaseTransport.Reply(ffnmRecordGetPrev, Error, nil, 0);
        Exit;
      end;

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Record', Buffer, RecLen);
        ichLogFmt(csErr, [Error]);
      end;

      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmRecordGetPrev,
                                   Error,
                                   nmdByteArray,
                                   @SubMsg^.smhData,
                                   RecLen);

      if FLogEnabled then
        ichLog('CursorGetBookmark (multipart)');

      if (BookmarkSize <> 0) then begin
        Buffer := PffByteArray(@SubMsg^.smhData);
        Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);
      end
      else
        Error := DBIERR_INVALIDBOOKMARK;

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Bookmark', Buffer, BookmarkSize);
        ichLogFmt(csErr, [Error]);
      end;

      FFCreateSubMessage(SubMsg,
                         ffnmCursorGetBookmark,
                         Error,
                         nmdByteArray,
                         @SubMsg^.smhData,
                         BookmarkSize);
      TffBaseTransport.Reply(ffnmMultiPartMessage, 0, MsgData, MsgSize);
    finally
      FFFreeMem(MsgData, MsgSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordInsert(var Msg : TffDataMessage);
var
  Error   : TffResult;
  MsgSize : longint;
  MsgData : PffByteArray;
  SubMsg  : PffsmHeader;
  Buffer  : PffByteArray;
begin
  with Msg, PffnmRecordInsertReq( dmData )^ do begin

    if FLogEnabled then
      ichLogAll(['RecordInsert',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  RecLen   %d', [RecLen]),
                 format('  BMSize   %d', [BookmarkSize]),
                 format('  LockType %d', [byte(LockType)])]);

    {try and insert record}
    Error := FServerEngine.RecordInsert( CursorID, LockType, @Data );
    if (Error <> 0) then begin
      if FLogEnabled then
        ichLogFmt(csErr, [Error]);
      TffBaseTransport.Reply(ffnmRecordInsert, Error, nil, 0);
      Exit;
    end;

    {we shall be sending back a multipart message: insertrecord,
     followed by getrecord, followed by getbookmark}
    MsgSize := (3 * ffc_SubMsgHeaderSize) + RecLen + BookmarkSize;
    FFGetMem(MsgData, MsgSize);
    try
      SubMsg := PffsmHeader(MsgData);
      {write the results of the insertrecord}

      if FLogEnabled then
        ichLogFmt(csErr, [Error]);

      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmRecordInsert,
                                   Error,
                                   nmdByteArray,
                                   nil,
                                   0);

      if FLogEnabled then
        ichLog('RecordGet (multipart)');

      Buffer := PffByteArray(@SubMsg^.smhData);
      Error := FServerEngine.RecordGet(CursorID, ffltNoLock, Buffer);

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Record', Buffer, RecLen);
        ichLogFmt(csErr, [Error]);
      end;

      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmRecordGet,
                                   Error,
                                   nmdByteArray,
                                   @SubMsg^.smhData,
                                   RecLen);

      if FLogEnabled then
        ichLog('CursorGetBookmark (multipart)');

      Buffer := PffByteArray(@SubMsg^.smhData);
      Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Bookmark', Buffer, BookmarkSize);
        ichLogFmt(csErr, [Error]);
      end;

      FFCreateSubMessage(SubMsg,
                         ffnmCursorGetBookmark,
                         Error,
                         nmdByteArray,
                         @SubMsg^.smhData,
                         BookmarkSize);
      TffBaseTransport.Reply(ffnmMultiPartMessage, 0, MsgData, MsgSize);
    finally
      FFFreeMem(MsgData, MsgSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordInsertBatch(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : PffLongintArray;
  DataSize : longint;
begin
  with Msg, PffnmRecordInsertBatchReq( dmData )^ do begin

    if FLogEnabled then
      ichLogAll(['RecordInsertBatch',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  RecCount %d', [RecCount]),
                 format('  RecLen   %d', [RecLen])]);

    DataSize := RecCount * sizeof(longint);
    FFGetMem(Reply, DataSize);
    try
      Error := FServerEngine.RecordInsertBatch(CursorID, RecCount, RecLen,
                                               PffByteArray(@RecArray),
                                               Reply);
      if FLogEnabled then
        ichLogFmt(csErr, [Error]);
      TffBaseTransport.Reply(ffnmRecordInsertBatch, Error, Reply, DataSize);

    finally
      FFFreeMem(Reply, DataSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordIsLocked(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmRecordIsLockedRpy;
begin
  with Msg, PffnmRecordIsLockedReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['RecordIsLocked',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  LockType %d', [Byte(LockType)])]);

    Error := FServerEngine.RecordIsLocked(CursorID, LockType, Reply.IsLocked);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

    TffBaseTransport.Reply(ffnmRecordIsLocked, Error, @Reply, SizeOf(Reply)); {!!.03}

  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordModify(var Msg : TffDataMessage);
var
  Error : TffResult;
  MsgSize : longint;
  MsgData : PffByteArray;
  SubMsg  : PffsmHeader;
  Buffer  : PffByteArray;
begin
  with Msg, PffnmRecordModifyReq( dmData )^ do begin
    
    if FLogEnabled then
      ichLogAll(['RecordModify',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  RecLen   %d', [RecLen]),
                 format('  BMSize   %d', [BookmarkSize]),
                 format('  RelLock  %d', [byte(RelLock)])]);

    {try and modify record}
    Error := FServerEngine.RecordModify( CursorID, @Data, RelLock );
    if (Error <> 0) then begin
      if FLogEnabled then
        ichLogFmt(csErr, [Error]);
      TffBaseTransport.Reply(ffnmRecordModify, Error, nil, 0);
      Exit;
    end;

    {we shall be sending back a multipart message: modifyrecord,
     followed by getrecord, followed by getbookmark}
    MsgSize := (3 * ffc_SubMsgHeaderSize) + RecLen + BookmarkSize;
    FFGetMem(MsgData, MsgSize);
    try
      SubMsg := PffsmHeader(MsgData);
      {write the results of the insertrecord}

      if FLogEnabled then
        ichLogFmt(csErr, [Error]);

      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmRecordModify,
                                   Error,
                                   nmdByteArray,
                                   nil,
                                   0);

      if FLogEnabled then
        ichLog('RecordGet (multipart)');

      Buffer := PffByteArray(@SubMsg^.smhData);
      Error := FServerEngine.RecordGet(CursorID, ffltNoLock, Buffer);

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Record', Buffer, RecLen);
        ichLogFmt(csErr, [Error]);
      end;

      SubMsg := FFCreateSubMessage(SubMsg,
                                   ffnmRecordGet,
                                   Error,
                                   nmdByteArray,
                                   @SubMsg^.smhData,
                                   RecLen);

      if FLogEnabled then
        ichLog('CursorGetBookmark (multipart)');

      Buffer := PffByteArray(@SubMsg^.smhData);
      Error := FServerEngine.CursorGetBookmark(CursorID, Buffer);

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogBlock('  Bookmark', Buffer, BookmarkSize);
        ichLogFmt(csErr, [Error]);
      end;

      FFCreateSubMessage(SubMsg,
                         ffnmCursorGetBookmark,
                         Error,
                         nmdByteArray,
                         @SubMsg^.smhData,
                         BookmarkSize);
      TffBaseTransport.Reply(ffnmMultiPartMessage, 0, MsgData, MsgSize);
    finally
      FFFreeMem(MsgData, MsgSize);
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmRecordRelLock(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmRecordRelLockReq( dmData )^ do begin

    if FLogEnabled then
      ichLogAll(['RecordRelLock',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  AllLocks %d', [byte(AllLocks)])]);

    Error := FServerEngine.RecordRelLock( CursorID, AllLocks );
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

    TffBaseTransport.Reply(ffnmRecordRelLock, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmReindexTable(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmReindexTableRpy;
begin
  with Msg, PffnmReindexTableReq(dmData)^ do begin

    if FLogEnabled then
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
    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  RbldID   %d', [Reply.RebuildID]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmReindexTable, Error, @Reply, sizeof(Reply));

  end;
end;
{--------}
procedure TffServerCommandHandler.nmRelTableLock(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmRelTableLockReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['RelTableLock',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  AllLocks %d', [byte(AllLocks)]),
                 format('  LockType %d', [byte(LockType)])]);

    Error := FServerEngine.TableLockRelease(CursorID, AllLocks);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmRelTableLock, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmRenameTable(var Msg : TffDataMessage);
var
  Error  : TffResult;
begin
  with Msg, PffnmRenameTableReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['RenameTable',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID %d', [DatabaseID]),
                 format('  OldTblName  [%s]', [OldTableName]),
                 format('  NewTblName  [%s]', [NewTableName])]);

    Error := FServerEngine.TableRename(DatabaseID, OldTableName, NewTableName);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmRenameTable, Error, nil, 0);

  end;
end;
{--------}
procedure TffServerCommandHandler.nmRestructureTable(var Msg : TffDataMessage);
{ Input stream is expected to be:
      DatabaseId (longint)
      TableName  (TffTableName)
      Dictionary (TffServerDataDict or TffDataDictionary)
      FieldMap   (one TffShStr for each field map entry; final entry
                  followed by a zero byte to signal end-of-list.  If
                  no field map is given, then a single zero byte must be
                  present
}
var
  Reply : TffnmRestructureTableRpy;
  Error  : TffResult;
  Stream : TMemoryStream;
  DatabaseID : LongInt;
  TableName : TffTableName;
  Dictionary : TffServerDataDict;

  DictionaryStart : Integer;
  DictionaryEnd   : Integer;
  I               : Integer;
  
  FieldMap: TffStringList;
  LenByte: Byte;
  FieldMapEntry: TffShStr;
begin
  with Msg do begin
    Stream := TMemoryStream.Create;
    Stream.Write(dmData^, dmDataLen);
    Stream.Position := 0;
    Stream.Read(DatabaseID, SizeOf(DatabaseID));
    Stream.Read(TableName, SizeOf(TableName));
    Dictionary := TffServerDataDict.Create(4096);
    try
      
      DictionaryStart := Stream.Position;
      
      Dictionary.ReadFromStream(Stream);
      
      DictionaryEnd := Stream.Position;
      

      FieldMap := nil;
      Stream.Read(LenByte, SizeOf(LenByte));
      if LenByte <> 0 then begin
        FieldMap := TffStringList.Create;
        try
          repeat
            Stream.Position := Stream.Position - SizeOf(LenByte);
            Stream.Read(FieldMapEntry, LenByte + 1);
            FieldMap.Insert(FieldMapEntry);
            Stream.Read(LenByte, SizeOf(LenByte));
          until LenByte = 0;
        except
          FieldMap.Free;
          raise;
        end;
      end;
      try

        
        if FLogEnabled then begin
          ichLogAll(['RestructureTable',
                     format(csClientID, [dmClientID]),
                     format('  DBase ID %d', [DatabaseID]),
                     format('  TblName  [%s]', [TableName])]);
          ichLogBlock('  Dictionary',
                        Addr(PffByteArray(Stream.Memory)^[DictionaryStart]),
                        DictionaryEnd - DictionaryStart);
          if not Assigned(FieldMap) then
            ichLog('  FieldMap nil')
          else begin
            ichLogFmt('  FieldMap [%s]', [FieldMap.Strings[0]]);
            for I := 1 to FieldMap.Count - 1 do
              ichLogFmt('           [%s]', [FieldMap.Strings[I]]);
          end;
        end;

        Error := FServerEngine.TableRestructure(DatabaseID,
                                                TableName,
                                                Dictionary,
                                                FieldMap,
                                                Reply.RebuildID);
        if FLogEnabled then begin
          if (Error = 0) then
            ichLogFmt('  ReBldID %d', [Reply.RebuildID]);
          ichLogFmt(csErr, [Error]);
        end;

        TffBaseTransport.Reply(ffnmRestructureTable, Error, @Reply, SizeOf(Reply));

      finally
        FieldMap.Free;
      end;
    finally
      Dictionary.Free;
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmServerIsReadOnly(var Msg : TffDataMessage);
var
  Reply : TffnmServerIsReadOnlyRpy;
begin
  with Msg do begin

    if FLogEnabled then
      ichLogAll(['ServerIsReadOnly',
                 format(csClientID, [dmClientID])]);

    Reply.IsReadOnly := FServerEngine.IsReadOnly;
    if FLogEnabled then
      ichLogFmt(csErr, [0]);
    TffBaseTransport.Reply(ffnmServerIsReadOnly, 0, @Reply, SizeOf(Reply)); {!!.01}

  end;
end;
{--------}                                                     {begin !!.07}
procedure TffServerCommandHandler.nmServerStatistics(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmServerStatisticsRpy;
begin
  with Msg do begin

    if FLogEnabled then
      ichLogAll(['ServerStatistics',
                 format(csClientID, [dmClientID])]);

    Error := FServerEngine.GetServerStatistics(Reply.Stats);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

    TffBaseTransport.Reply(ffnmServerStatistics, Error, @Reply, SizeOf(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmCmdHandlerStatistics(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmCmdHandlerStatisticsRpy;
begin
  with Msg, PffnmCmdHandlerStatisticsReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['CmdHandlerStatistics',
                 Format(csClientID, [dmClientID]),
                 Format('  CmdHandlerIdx %d', [CmdHandlerIdx])]);

    Error := FServerEngine.GetCommandHandlerStatistics(CmdHandlerIdx, Reply.Stats);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

    TffBaseTransport.Reply(ffnmCmdHandlerStatistics, Error, @Reply, SizeOf(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmTransportStatistics(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmTransportStatisticsRpy;
begin
  with Msg, PffnmTransportStatisticsReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['TransportStatistics',
                 Format(csClientID, [dmClientID]),
                 Format('  CmdHandlerIdx %d', [CmdHandlerIdx]),
                 Format('  TramsportIdx  %d', [TransportIdx])]);

    Error := FServerEngine.GetTransportStatistics(CmdHandlerIdx,
                                                  TransportIdx,
                                                  Reply.Stats);

    if FLogEnabled then
      ichLogFmt(csErr, [Error]);

    TffBaseTransport.Reply(ffnmTransportStatistics, Error, @Reply, SizeOf(Reply));
  end;
end;
{--------}                                                       {end !!.07}
procedure TffServerCommandHandler.nmSessionAdd(var Msg : TffDataMessage);
var
  Error   : TffResult;
  SessionID : TffSessionID;
  Reply   : TffnmSessionAddRpy;
begin
  with Msg, PffnmSessionAddReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['SessionAdd',
                 format(csClientID, [dmClientID]),
                 format('  Timeout  %d', [Timeout])]);

    Error := FServerEngine.SessionAdd(dmClientID, Timeout, SessionID);
    if (Error = 0) then
      Reply.SessionID := SessionID;
    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  Session  %d', [Reply.SessionID]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmSessionAdd, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmSessionClose(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmSessionCloseReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['SessionClose',
                 format(csClientID, [dmClientID]),
                 format('  Session  %d', [SessionID])]);

    Error := FServerEngine.SessionRemove(dmClientID, SessionID);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmSessionClose, Error, nil, 0);
  end;
end;
{Begin !!.06}
{--------}
procedure TffServerCommandHandler.nmSessionCloseInactiveTables(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmSessionCloseInactiveTblReq(dmData)^ do begin
    if FLogEnabled then
      ichLogAll(['SessionCloseInactiveTables',
                 format(csClientID, [dmClientID]),
                 format('  Session  %d', [SessionID])]);

    Error := FServerEngine.SessionCloseInactiveTables(dmClientID);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmSessionCloseInactTbl, Error, nil, 0);
  end;
end;
{End !!.06}
{--------}
procedure TffServerCommandHandler.nmSessionGetCurrent(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmSessionGetCurrentRpy;
begin
  with Msg do begin
    
    if FLogEnabled then
      ichLogAll(['SessionGetCurrent',
                 format(csClientID, [dmClientID])]);

    Error := FServerEngine.SessionGetCurrent(dmClientID, Reply.SessionID);
    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt('  Session  %d', [Reply.SessionID]);
      ichLogFmt(csErr, [Error]);
    end;
    TffBaseTransport.Reply(ffnmSessionGetCurrent, Error, @Reply, sizeof(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmSessionSetCurrent(var Msg : TffDataMessage);
var
  Error     : TffResult;
begin
  with Msg, PffnmSessionSetCurrentReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['SessionSetCurrent',
                 format(csClientID, [dmClientID]),
                 format('  Session  %d', [SessionID])]);
    
    Error := FServerEngine.SessionSetCurrent(dmClientID, SessionID);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmSessionSetCurrent, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmSessionSetTimeout(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmSessionSetTimeoutReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['SessionSetTimeout',
                 format(csClientID, [dmClientID]),
                 format('  Session  %d', [SessionID]),
                 format('  Timeout  %d', [Timeout])]);

    Error := FServerEngine.SessionSetTimeout(dmClientID, SessionID, Timeout);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmSessionSetTimeout, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmSetTableAutoIncValue(var Msg : TffDataMessage);
var
  Error  : TffResult;
begin
  with Msg, PffnmSetTableAutoIncValueReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['SetTableAutoIncValue',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format('  Value %d', [AutoIncValue])]);

    Error := FServerEngine.TableSetAutoInc(CursorID, AutoIncValue);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmSetTableAutoIncValue, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmStartTransaction(var Msg : TffDataMessage);
var
  Error       : TffResult;
begin
  with Msg, PffnmStartTransactionReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['StartTransaction',
                  format(csClientID, [dmClientID]),
                  format('  DBase ID %d', [DatabaseID]),
                  format('  FailSafe %d', [byte(FailSafe)])]);

    Error := FServerEngine.TransactionStart(DatabaseID,
                                            FailSafe);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmStartTransaction, Error, nil, 0);
  end;
end;
{Begin !!.10}
{--------}
procedure TffServerCommandHandler.nmStartTransactionWith(var Msg : TffDataMessage);
var
  Error       : TffResult;
  Inx,
  CursorCount : Integer;
  Reader : TReader;
  Stream : TMemoryStream;
  DbID : TffDatabaseID;
  FailSafe : Boolean;
  CursorIDList : TffPointerList;
  CursorIDStr : string;
begin
  with Msg do begin
    CursorIDList := TffPointerList.Create;
    try
      Stream := TMemoryStream.Create;
      try
        Stream.Write(dmData^, dmDataLen);
        Stream.Position := 0;
        Reader := TReader.Create(Stream, 4096);
        try
          DbID := Reader.ReadInteger;
          FailSafe := Reader.ReadBoolean;
          CursorCount := Reader.ReadInteger;
          for Inx := 1 to CursorCount do
            CursorIDList.Append(Pointer(Reader.ReadInteger));
        finally
          Reader.Free;
        end;
      finally
        Stream.Free;
      end;

      if FLogEnabled then begin
        CursorIDStr := '';
        for Inx := 0 to Pred(CursorIDList.Count) do begin
          if CursorIDStr <> '' then
            CursorIDStr := CursorIDStr + ',';
          CursorIDStr := CursorIDStr + IntToStr(Integer(CursorIDList[Inx]));
        end;  { for }
        ichLogAll(['StartTransactionWith',
                    format(csClientID, [dmClientID]),
                    format('  DBase ID  %d', [DbID]),
                    format('  FailSafe  %d', [byte(FailSafe)]),
                    format('  CursorIDs %s', [CursorIDStr])]);
      end;

      Error := FServerEngine.TransactionStartWith(DbID,
                                                  FailSafe,
                                                  CursorIDList);
      if FLogEnabled then
        ichLogFmt(csErr, [Error]);
      TffBaseTransport.Reply(ffnmStartTransactionWith, Error, nil, 0);
    finally
      CursorIDList.Free;
    end;
  end;  { with }
end;
{End !!.10}
{--------}
procedure TffServerCommandHandler.nmSQLAlloc(var Msg : TffDataMessage);
var
  Error : TffResult;
  Reply : TffnmSQLAllocRpy;
begin
  with Msg, PffnmSQLAllocReq(dmData)^ do begin
    
    if FLogEnabled then
      ichLogAll(['SQLAlloc',
                 format(csClientID, [dmClientID]),
                 format('  DBaseID  %d', [DatabaseID]),                {!!.01}
                 format('  Timeout  %d', [Timeout])]);                 {!!.01}

    Error := FServerEngine.SQLAlloc(dmClientID, DatabaseID, Timeout,
                                    Reply.StmtID);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmSQLAlloc, Error, @Reply, SizeOf(Reply));
  end;
end;
{--------}
procedure TffServerCommandHandler.nmSQLExec(var Msg : TffDataMessage);
var
  aBuffer : pointer;
  Error: TffResult;
  CursorID: TffCursorID;
  Stream : TMemoryStream;
  StreamSize : longInt;
begin
  with Msg, PffnmSQLExecReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['SQLExec',
                 format(csClientID, [dmClientID]),
                 format('  StmtID   %d', [StmtID]),
                 format('  OpenMode %d', [Ord(OpenMode)])]);

    Stream := TMemoryStream.Create;
    try
      Error := FServerEngine.SQLExec(StmtID, OpenMode, CursorID, Stream);
//      if CursorID = 0 then                                           {!!.01}
//        TffBaseTransport.Reply(ffnmSQLExec, Error, nil, 0)           {!!.01}
//      else begin                                                     {!!.01}
        StreamSize := Stream.Size;
        FFGetMem(aBuffer, StreamSize);
        Stream.Position := 0;
        Stream.Read(aBuffer^, StreamSize);
        TffBaseTransport.Reply(ffnmSQLExec, Error, aBuffer, StreamSize);
        FFFreeMem(aBuffer, StreamSize);
//      end;                                                           {!!.01}
    finally
      Stream.Free;
    end;

    if FLogEnabled then begin
      if (Error = 0) then
        ichLogFmt(csCursorID, [CursorID]);
      ichLogFmt(csErr, [Error]);
    end;

  end;
end;
{--------}
procedure TffServerCommandHandler.nmSQLExecDirect(var Msg : TffDataMessage);
var
  aBuffer : pointer;
  Error : TffResult;
  QueryText : PChar;
  CursorID : TffCursorID;
  Stream : TMemoryStream;
  StreamSize : longInt;
begin
  with Msg, PffnmSQLExecDirectReq(dmData)^ do begin
    QueryText := @Query;

    if FLogEnabled then
      ichLogAll(['SQLExecDirect',
                 format(csClientID, [dmClientID]),
                 format('  DBase ID [%d]', [DatabaseID]),
                 format('  Query    [%s]', [StrPas(QueryText)]),
                 format('  Timeout  %d', [Timeout]),
                 format('  OpenMode [%d]', [Ord(OpenMode)])]);


    Stream := TMemoryStream.Create;
    try
      Error := FServerEngine.SQLExecDirect(dmClientID, DatabaseID, QueryText,
                                           Timeout, OpenMode, CursorID, Stream);
      StreamSize := Stream.Size;
      FFGetMem(aBuffer, StreamSize);
      Stream.Position := 0;
      Stream.Read(aBuffer^, StreamSize);

      if FLogEnabled then begin
        if (Error = 0) then
          ichLogFmt(csCursorID, [CursorID]);
        ichLogFmt(csErr, [Error]);
      end;

      TffBaseTransport.Reply(ffnmSQLExecDirect, Error, aBuffer, StreamSize);
      FFFreeMem(aBuffer, StreamSize);
    finally
      Stream.Free;
    end;

  end;
end;
{--------}
procedure TffServerCommandHandler.nmSQLFree(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmSQLFreeReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['SQLFree',
                 format(csClientID, [dmClientID]),
                 format('  StmtID   %d', [StmtID])]);

    Error := FServerEngine.SQLFree(StmtID);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmSQLFree, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmSQLPrepare(var Msg : TffDataMessage);
var
  aBuffer : pointer;
  Error : TffResult;
  Stream : TMemoryStream;
  StreamSize : longInt;
begin
  with Msg, PffnmSQLPrepareReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['SQLPrepare',
                 format(csClientID, [dmClientID]),
                 format('  StmtID   %d', [StmtID]),
                 format('  Query    [%s]', [StrPas(@Query)])]);

    Stream := TMemoryStream.Create;
    try
      Error := FServerEngine.SQLPrepare(StmtID, @Query, Stream);

      StreamSize := Stream.Size;
      aBuffer := nil;
      if StreamSize > 0 then begin
        FFGetMem(aBuffer, StreamSize);
        Stream.Position := 0;
        Stream.Read(aBuffer^, StreamSize);
      end;

      if FLogEnabled then
        ichLogFmt(csErr, [Error]);
      TffBaseTransport.Reply(ffnmSQLPrepare, Error, aBuffer, StreamSize);
      if assigned(aBuffer) then
        FFFreeMem(aBuffer, StreamSize);
    finally
      Stream.Free;
    end;
  end;
end;
{--------}
procedure TffServerCommandHandler.nmSQLSetParams(var Msg : TffDataMessage);
{ Input stream is expected to be:
      StmtID     (longint)
      NumParams  (word)
      ParamList  (array of TffSqlParamInfo)
      BufLen     (longint; size of DataBuffer)
      DataBuffer (data buffer)
}
var
  aBuffer : pointer;
  Error : TffResult;
  OutStream : TMemoryStream;
  OutStreamSize : longInt;
  Stream : TMemoryStream;
  StmtID : longint;
  NumParams : Word;
  ParamDescs : PffSqlParamInfoList;
  DataBuffer : PffByteArray;
  BufLen: LongInt;
begin
  with Msg do begin
    Stream := TMemoryStream.Create;
{Begin !!.03}
    try
      Stream.Write(dmData^, dmDataLen);
      Stream.Position := 0;
      Stream.Read(StmtID, SizeOf(StmtID));
      Stream.Read(NumParams, SizeOf(NumParams));
      ParamDescs := Pointer(LongInt(Stream.Memory) + Stream.Position);
      Stream.Position := Stream.Position + NumParams * SizeOf(TffSqlParamInfo);
      Stream.Read(BufLen, SizeOf(BufLen));
      DataBuffer := Pointer(LongInt(Stream.Memory) + Stream.Position);


      if FLogEnabled then begin
        ichLogAll(['SQLSetParams',
                   format(csClientID, [dmClientID]),
                   format('  StmtID    %d', [StmtID]),
                   format('  NumParams %d', [NumParams])]);
        ichLogBlock('  ParamDescs  ', ParamDescs, NumParams * SizeOf(TffSqlParamInfo));
        ichLogBlock('  DataBuf   ', DataBuffer, BufLen);
      end;


      OutStream := TMemoryStream.Create;
      try
        Error := FServerEngine.SQLSetParams(StmtID, NumParams, ParamDescs,
                                            DataBuffer, BufLen, OutStream);
        OutStreamSize := Stream.Size;
        aBuffer := nil;
        if OutStreamSize > 0 then begin
          FFGetMem(aBuffer, OutStreamSize);
          Stream.Position := 0;
          Stream.Read(aBuffer^, OutStreamSize);
        end;

        if FLogEnabled and(Error <> 0) then
          ichLogFmt(csErr, [Error]);
        TffBaseTransport.Reply(ffnmSQLSetParams, Error, aBuffer, OutStreamSize);
        if assigned(aBuffer) then
          FFFreeMem(aBuffer, OutStreamSize);
      finally
        OutStream.Free;
      end;
    finally
      Stream.Free;
    end;
{End !!.03}
  end;
end;
{--------}
procedure TffServerCommandHandler.nmTruncateBLOB(var Msg : TffDataMessage);
var
  Error : TffResult;
begin
  with Msg, PffnmTruncateBLOBReq(dmData)^ do begin

    if FLogEnabled then
      ichLogAll(['TruncateBLOB',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format(csBlobNr, [BLOBNr.iLow, BLOBNr.iHigh]),
                 format('  BLOBLen  %d', [BLOBLength])]);

    Error := FServerEngine.BLOBTruncate(CursorID, BLOBNr, BLOBLength);
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmTruncateBLOB, Error, nil, 0);
  end;
end;
{--------}
procedure TffServerCommandHandler.nmWriteBLOB( var Msg : TffDataMessage );
var
  Error : TffResult;
begin
  with Msg, PffnmWriteBLOBReq( dmData )^ do begin
    
    if FLogEnabled then begin
      ichLogAll(['WriteBLOB',
                 format(csClientID, [dmClientID]),
                 format(csCursorID, [CursorID]),
                 format(csBlobNr, [BLOBNr.iLow, BLOBNr.iHigh]),
                 format('  Offset %d', [Offset]),
                 format('  Len %d', [Len])]);
      ichLogBlock('  BLOB', @BLOB, Len);
    end;

    Error := FServerEngine.BLOBWrite( CursorID, BLOBNr, Offset, Len, BLOB );
    if FLogEnabled then
      ichLogFmt(csErr, [Error]);
    TffBaseTransport.Reply(ffnmWriteBLOB, Error, nil, 0);
  end;
end;
{--------}
{Rewritten !!.11}
procedure TffServerCommandHandler.FFAddDependent(ADependent : TffComponent);
var
  Method : PffInt64;
  aTransport : TffBaseTransport;
begin
  inherited;
  if (ADependent is TffBaseTransport) then begin
    aTransport := TffBaseTransport(ADependent);
    if Assigned(aTransport.OnAddClient) then begin
      FFGetMem(Method, SizeOf(TffInt64));
      Method^ := TffInt64(aTransport.OnAddClient);
      schSavedAddClientEvents.BeginWrite;                               
      try
        schSavedAddClientEvents.Add(Longint(aTransport), Method);
      finally
        schSavedAddClientEvents.EndWrite;
      end;
    end;
    aTransport.OnAddClient := schOnAddClient;
    aTransport.OnRemoveClient := schOnRemoveClient;
  end;  { if }
end;
{Begin !!.05}
{--------}
procedure TffServerCommandHandler.schDisposeRecord(Sender : TffBaseHashTable;
                                                   aData : Pointer);
begin
  FFFreeMem(aData, SizeOf(TffInt64));
end;
{End !!.05}
{--------}
procedure TffServerCommandHandler.schOnAddClient
                               (Listener : TffBaseTransport;
                          const userID : TffName;
                          const timeout : longInt;
                          const clientVersion : longInt;
                            var passwordHash : TffWord32;
                            var aClientID : TffClientID;
                            var errorCode : TffResult;
                            var isSecure : boolean;
                            var aVersion : longInt);
var                                                                    {!!.05}
  Method : PffInt64;                                                   {!!.05}
begin
  if FLogEnabled then
    ichLogAll(['AddClientEvent',
               format('  UserID   [%s]', [UserID]),
               format('  timeout  [%d]', [Timeout]),
               format('  clientVersion  [%d]', [ClientVersion])]);

{Begin !!.05}
  { See if there is a saved event for the listener. }
  schSavedAddClientEvents.BeginRead;                                  {begin !!.05}
  try
    Method := schSavedAddClientEvents.Get(Longint(Listener));
  finally
    schSavedAddClientEvents.EndRead;
  end;                                                                {end !!.05}
  if Method <> nil then begin
    errorCode := DBIERR_NONE;
    TffAddClientEvent(Method^)
        (Listener, userID, timeout, clientVersion,
         passwordHash, aClientID, errorCode, isSecure,
         aVersion);
      if errorCode <> DBIERR_NONE then
        Exit;
  end;
{End !!.05}

  aClientID := ffc_NoClientID;
  isSecure := False;

  { Is the client a compatible version?
    Reasons for incompatibility:

    1. The server's version number is less than the client's.
    2. The server's major version number is greater than the client's
       major version number (at least in the case of 1.x and 2.x).
  }
  if ((ffVersionNumber div 100) < (clientVersion div 100)) or
     ((ffVersionNumber div 10000) > (clientVersion div 10000)) then    {!!.11}
//     (clientversion < 21000) then                                    {!!.10}{Deleted !!.11}
    errorCode := DBIERR_SERVERVERSION
  else
    errorCode := FServerEngine.ClientAddEx(aClientID, UserID,          {!!.11}
                                           UserID, timeout,            {!!.11}
                                           clientVersion,              {!!.11}
                                           passwordHash);              {!!.11}
  if (errorCode = DBIERR_NONE) then
    isSecure := TffServerEngine(FServerEngine).Configuration.GeneralInfo^.giIsSecure;
  aVersion := FFVersionNumber;

  if FLogEnabled then begin
    if (Error = 0) then
      ichLogAll(['  Successful',
                format(csClientID,[aClientID]),
                format('  IsSecure %d', [ord(isSecure)])]);
    ichLogFmt(csErr, [Error]);
  end;
end;
{--------}
procedure TffServerCommandHandler.schOnRemoveClient
                                    (Listener : TffBaseTransport;
                               const aClientID : TffClientID;
                                 var errorCode : TffResult);
begin
  if FLogEnabled then
    ichLogAll(['RemoveClientEvent',
               format(csClientID, [aClientID])]);

  errorCode := FServerEngine.ClientRemove(aClientID);
  if FLogEnabled then
    ichLogFmt(csErr, [Error]);
end;
{--------}
procedure TffServerCommandHandler.scInitialize;
begin
  { do nothing }
end;
{--------}
procedure TffServerCommandHandler.scPrepareForShutdown;
begin
  { do nothing }
end;
{--------}
procedure TffServerCommandHandler.scShutdown;
begin
  { do nothing }
end;
{--------}
procedure TffServerCommandHandler.scStartup;
begin
  { do nothing }
end;

{====================================================================}

end.
