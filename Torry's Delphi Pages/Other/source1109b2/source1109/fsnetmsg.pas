{*********************************************************}
{* FSSQL: Network messaging types & constants          *}
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

Unit fsnetmsg;

Interface

Uses
  Windows,
  Messages,
  SysUtils,
  fsllbase,
  fssrbase;

{===Network message constants===}
Const
  fsm_LostConnection = WM_USER + $0FF1;
  fsm_StartTblReindex = WM_USER + $0FF2;
  fsm_StartTblPack = WM_USER + $0FF3;
  fsm_StartTblRestructure = WM_USER + $0FF4;
  fsm_CopyData = WM_USER + $0FF5;
  fsm_CallSingleUserServer = WM_USER + $0FF6;
  fsm_KeepAlive = WM_USER + $0FF7;

  fsmtRequest = 1; { Request sent from client to server }
  fsmtReply = 2; { Reply sent from server to client }

Const
  {general or non-BDE type}
  fsnmDetachServer = $0002;
  fsnmEndOfStream = $0003;
  fsnmDetachServerJIC = $0004;
  fsnmACK = $0005;
  fsnmRequestServerName = $0006;
  fsnmServerNameReply = $0007;
  fsnmNewServerAdvert = $0008;
  fsnmCheckSecureComms = $0009;
  fsnmCheckConnection = $000A; {!!!!}
  fsnmGetServerDateTime = $000C;
  fsnmCallServer = $000D;
  fsnmClientSetTimeout = $000E;
  fsnmAttachServer = $000F;
  fsnmGetServerSystemTime = $0010; {!!.10}
  fsnmGetServerGUID = $0011; {!!.10}
  fsnmGetServerID = $0012; {!!.10}

  fsnmMultiPartMessage = $00FF;

  {database related}
  fsnmDatabaseOpen = $0100;
  fsnmDatabaseClose = $0101;
  fsnmDatabaseAliasList = $0102;
  fsnmDatabaseTableList = $0103;
  fsnmDatabaseAddAlias = $0104;
  fsnmDatabaseOpenNoAlias = $0105;
  fsnmDatabaseDeleteAlias = $0107;
  fsnmDatabaseChgAliasPath = $0108;
  fsnmDatabaseSetTimeout = $0109;
  fsnmDatabaseGetFreeSpace = $010A;
  fsnmDatabaseModifyAlias = $010B;
  fsnmDatabaseGetAliasPath = $010C;
  fsnmDatabaseTableExists = $010D;
  fsnmDatabaseTableLockedExclusive = $010E;

  {session related}
  fsnmSessionAdd = $0200;
  fsnmSessionClose = $0201;
  fsnmSessionGetCurrent = $0202;
  fsnmSessionSetCurrent = $0203;
  fsnmSessionSetTimeout = $0204;
  fsnmSessionCloseInactTbl = $0205; {!!.06}

  {rebuild processes}
  fsnmReindexTable = $0300;
  fsnmPackTable = $0301;
  fsnmRestructureTable = $0302;
  fsnmGetRebuildStatus = $0303;

  {transaction stuff}
  fsnmStartTransaction = $0400;
  fsnmEndTransaction = $0401;
  fsnmStartTransactionWith = $0402; {!!.10}
  fsnmInTransaction = $0403;
  fsnmTransactionCorrupted = $0404;

  {table stuff}
  fsnmOpenTable = $0500;
  fsnmAcqTableLock = $0510;
  fsnmRelTableLock = $0511;
  fsnmIsTableLocked = $0512;
  fsnmGetTableDictionary = $0513;
  fsnmBuildTable = $0514;
  fsnmDeleteTable = $0515;
  fsnmRenameTable = $0516;
  fsnmGetTableRecCount = $0517;
  fsnmEmptyTable = $0518;
  fsnmAddIndex = $0519;
  fsnmDropIndex = $051A;
  fsnmSetTableAutoIncValue = $051B;
  fsnmGetTableAutoIncValue = $051C;
  fsnmGetTableRecCountAsync = $051D; {!!.10}
  fsnmGetTableVersion = $051E; {!!.11}

  fsnmSetTableMaxRecordsValue = $051F; {1.027} // 1311
  fsnmGetTableMaxRecordsValue = $0520; // 1312

  fsnmSetTableTableFlagsValue = $0521; {1.027} // 1313
  fsnmGetTableTableFlagsValue = $0522; // 1314

  fsnmSetTableTablePasswordValue = $0523; // 1315
  fsnmGetTableTablePasswordValue = $0524; // 1316
  fsnmSetTableTablePasswordRestValue = $0525; // 1317
  fsnmGetTableTablePasswordRestValue = $0526; // 1318

  fsnmSetTableTableDBIDValue = $0527; // 1319
  fsnmGetTableTableDBIDValue = $0528; // 1320

  {BLOB stuff}
  fsnmCreateBLOB = $0600;
  fsnmDeleteBLOB = $0601;
  fsnmReadBLOB = $0602;
  fsnmGetBLOBLength = $0603;
  fsnmTruncateBLOB = $0604;
  fsnmWriteBLOB = $0605;
  fsnmAddFileBLOB = $0607;
  fsnmFreeBLOB = $0608;
  fsnmListBLOBFreeSpace = $0609; {!!.03}
  fsnmListBLOBSegments = $060A; {!!.03}

  {cursor stuff}
  fsnmCursorSetToBegin = $0700;
  fsnmCursorSetToEnd = $0701;
  fsnmCursorClose = $0702;
  fsnmCursorGetBookmark = $0703;
  fsnmCursorSetToBookmark = $0704;
  fsnmCursorSetToKey = $0705;
  fsnmCursorSwitchToIndex = $0706;
  fsnmCursorSetRange = $0707;
  fsnmCursorResetRange = $0708;
  fsnmCursorCompareBMs = $0709;
  fsnmCursorClone = $070A;
  fsnmCursorSetToCursor = $070B;
  fsnmCursorSetFilter = $070C;
  fsnmCursorOverrideFilter = $070D;
  fsnmCursorRestoreFilter = $070E;
  fsnmCursorSetTimeout = $070F;
  fsnmCursorCopyRecords = $0710; {!!.02}
  fsnmCursorDeleteRecords = $0711; {!!.06}

  {record stuff}
  fsnmRecordGet = $0800;
  fsnmRecordGetNext = $0801;
  fsnmRecordGetPrev = $0802;
  fsnmRecordRelLock = $0803;
  fsnmRecordDelete = $0804;
  fsnmRecordInsert = $0805;
  fsnmRecordModify = $0806;
  fsnmRecordExtractKey = $0807;
  fsnmRecordGetForKey = $0808;
  fsnmRecordGetBatch = $0809;
  fsnmRecordInsertBatch = $080A;
  fsnmRecordGetForKey2 = $080C;
  fsnmRecordDeleteBatch = $080D;
  fsnmRecordIsLocked = $080E; // 2062
  fsnmRecordGetSetPosition = $080F;
  fsnmRecordCountLocked = $0810;
  //$0810;

  {SQL stuff}
  fsnmSQLAlloc = $0900; // 2304
  fsnmSQLExec = $0901;
  fsnmSQLExecDirect = $0902;
  fsnmSQLFree = $0903;
  fsnmSQLPrepare = $0904;
  fsnmSQLSetParams = $0905;

  {Server Operations}
  fsnmServerRestart = $0A00;
  fsnmServerShutdown = $0A01;
  fsnmServerStartup = $0A02;
  fsnmServerStop = $0A03;

  { Server Info }
  fsnmServerIsReadOnly = $0B00;
  fsnmServerStatistics = $0B01; {!!.10}
  fsnmCmdHandlerStatistics = $0B02; {!!.10}
  fsnmTransportStatistics = $0B03; {!!.10}

  fsnmUser = $4000;

  {===Network message types===}
Type
  PfsnmHeader = ^TfsnmHeader;
  TfsnmHeader = Packed Record {General message header}
    nmhMsgID: Longint; {..message identifier}
    nmhMsgLen: Longint; {..size of this message, incl. header}
    nmhTotalSize: Longint; {..total size of data over all messages}
    nmhClientID: TffClientID; {..client ID (either from or to)}
    nmhRequestID: Longint; {..client's requestID}
    nmhErrorCode: TffResult; {..BDE error code, or 0 for success}
    nmhTimeout: Longint; {..timeout in milliseconds}
    nmhFirstPart: boolean; {..is this the 1st part of the message?}
    nmhLastPart: boolean; {..is this the last part of the message?}
    nmhDataType: TffNetMsgDataType; {..is message bytearray or stream?}
    nmhMsgType: Byte; {..is this a request or a reply?  Declared as
    byte so that you may create additional msg
    types. }
    nmhData: Byte; {..data marker}
  End;

  PfssmHeader = ^TfssmHeader;
  TfssmHeader = Packed Record {Sub-message header}
    smhMsgID: Longint; {..message identifier}
    smhReplyLen: Longint; {..size of this reply (header + data)}
    smhErrorCode: TffWord16; {..BDE error code, or 0 for success}
    smhDataType: TffNetMsgDataType; {..is message bytearray or stream?}
    smhFiller: Byte; {..filler}
    smhData: Byte; {..data marker}
  End;

Const
  fsc_NetMsgHeaderSize = sizeof(TfsnmHeader) - sizeof(Byte);
  fsc_SubMsgHeaderSize = sizeof(TfssmHeader) - sizeof(Byte);

  {NOTE: all message crackers are in two parts: the request data record
         and the reply data record. If a message cracker has only a
         request record, then all the data for the reply is contained in
         the message header (and is generally just the error code).
         Similarly if cracker only has a reply record then all the data
         for the request is contained in the header (and is generally
         the client ID and the message number). If neither is present,
         the the data for the request and reply is entirely contained in
         the message header.
         }

  {===general or non-BDE type==========================================}
Type
  {attach to server}
  PfsnmAttachServerReq = ^TfsnmAttachServerReq;
  TfsnmAttachServerReq = Packed Record
    {Begin !!.03}
    {$IFDEF IsDelphi}
    ClientName: TffNetName;
    {$ELSE}
    ClientName: TffNetNameShr;
    {$ENDIF}
    {End !!.03}
    UserID: TffName;
    Timeout: Longint;
    ClientVersion: Longint;
  End;
  PfsnmAttachServerRpy = ^TfsnmAttachServerRpy;
  TfsnmAttachServerRpy = Packed Record
    ClientID: TffClientID;
    VersionNumber: Longint;
    Code: Longint;
    LastMsgIntvl: Longint;
    KAIntvl: Longint;
    KARetries: Longint;
    IsSecure: boolean;
    Rights: TffUserRights;
  End;

  {request server name - DATAGRAM ONLY}
  PfsnmRequestServerName = ^TfsnmRequestServerName;
  TfsnmRequestServerName = Packed Record
    MsgID: Longint; {always fsnmRequestServerName}
  End;

  {server name reply - DATAGRAM ONLY}
  PfsnmServerNameReply = ^TfsnmServerNameReply;
  TfsnmServerNameReply = Packed Record
    MsgID: Longint; {always fsnmServerNameReply}
    {Begin !!.03}
    {$IFDEF IsDelphi}
    ServerLocalName: TffNetName;
    ServerNetName: TffNetName;
    {$ELSE}
    ServerLocalName: TffNetNameShr;
    ServerNetName: TffNetNameShr;
    {$ENDIF}
    {End !!.03}
  End;

  PfsnmGetServerDateTimeRpy = ^TfsnmGetServerDateTimeRpy;
  TfsnmGetServerDateTimeRpy = Packed Record
    ServerNow: TDateTime;
  End;

  PfsnmGetServerSystemTimeRpy = ^TfsnmGetServerSystemTimeRpy; {begin !!.10}
  TfsnmGetServerSystemTimeRpy = Packed Record
    ServerNow: TSystemTime;
  End;

  PfsnmGetServerGUIDRpy = ^TfsnmGetServerGUIDRpy;
  TfsnmGetServerGUIDRpy = Packed Record
    GUID: TGUID;
  End;

  PfsnmGetServerIDRpy = ^TfsnmGetServerIDRpy;
  TfsnmGetServerIDRpy = Packed Record
    UniqueID: TGUID;
  End; {end !!.10}

  PfsnmCallServerReq = ^TfsnmCallServerReq;
  TfsnmCallServerReq = Packed Record
    {Begin !!.03}
    {$IFDEF IsDelphi}
    ServerName: TffNetName;
    {$ELSE}
    ServerName: TffNetNameShr;
    {$ENDIF}
    {End !!.03}
  End;

  PfsnmCallServerRpy = ^TfsnmCallServerRpy;
  TfsnmCallServerRpy = Packed Record
    ClientID: TffClientID;
  End;

  { Set a client's timeout value.}
  PfsnmClientSetTimeoutReq = ^TfsnmClientSetTimeoutReq;
  TfsnmClientSetTimeoutReq = Packed Record
    Timeout: Longint;
  End;
  { Reply as an error in message header. }

{===database related=================================================}
Type
  {open database}
  PfsnmDatabaseOpenReq = ^TfsnmDatabaseOpenReq;
  TfsnmDatabaseOpenReq = Packed Record
    Alias: TffName;
    OpenMode: TffOpenMode;
    ShareMode: TffShareMode;
    Timeout: Longint;
    TransIsolation: TfsTransIsolation;
    TransLocking: TfsDataBaseRecLocking;
  End;
  PfsnmDatabaseOpenRpy = ^TfsnmDatabaseOpenRpy;
  TfsnmDatabaseOpenRpy = Packed Record
    DatabaseID: TffDatabaseID;
  End;

  {close database (reply packet contained in header)}
  PfsnmDatabaseCloseReq = ^TfsnmDatabaseCloseReq;
  TfsnmDatabaseCloseReq = Packed Record
    DatabaseID: TffDatabaseID;
  End;

  {get list of tables in database (reply packet is a stream)}
  PfsnmDatabaseTableListReq = ^TfsnmDatabaseTableListReq;
  TfsnmDatabaseTableListReq = Packed Record
    DatabaseID: TffDatabaseID;
    Mask: TffFileNameExt;
  End;

  {add new alias database}
  PfsnmDatabaseAddAliasReq = ^TfsnmDatabaseAddAliasReq;
  TfsnmDatabaseAddAliasReq = Packed Record
    Alias: TffName;
    Path: TffPath;
    PathBlob: TffPath;
    CheckDisk: Boolean;
  End;
  {reply as error in message header}
  PfsnmOldDatabaseAddAliasReq = ^TfsnmOldDatabaseAddAliasReq;
  TfsnmOldDatabaseAddAliasReq = Packed Record
    Alias: TffName;
    Path: TffPath;
    PathBlob: TffPath;
    CheckDisk: Boolean;
  End;

  {open database without alias}
  PfsnmDatabaseOpenNoAliasReq = ^TfsnmDatabaseOpenNoAliasReq;
  TfsnmDatabaseOpenNoAliasReq = Packed Record
    Path: TffPath;
    OpenMode: TffOpenMode;
    ShareMode: TffShareMode;
    Timeout: Longint;
    TransIsolation: TfsTransIsolation;
    TransLocking: TfsDataBaseRecLocking;
  End;
  PfsnmDatabaseOpenNoAliasRpy = ^TfsnmDatabaseOpenNoAliasRpy;
  TfsnmDatabaseOpenNoAliasRpy = Packed Record
    DatabaseID: TffDatabaseID;
  End;

  {delete an alias}
  PfsnmDatabaseDeleteAliasReq = ^TfsnmDatabaseDeleteAliasReq;
  TfsnmDatabaseDeleteAliasReq = Packed Record
    Alias: TffName;
  End;
  {reply as error in message header}

  {retrieve the alias' path}
  PfsnmDatabaseGetAliasPathReq = ^TfsnmDatabaseGetAliasPathReq;
  TfsnmDatabaseGetAliasPathReq = Packed Record
    Alias: TffName;
  End;
  PfsnmDatabaseGetAliasPathRpy = ^TfsnmDatabaseGetAliasPathRpy;
  TfsnmDatabaseGetAliasPathRpy = Packed Record
    Path: TffPath;
    PathBlob: TffPath;
    CheckDisk: Boolean;
  End;

  PfsnmDatabaseChgAliasPathReq = ^TfsnmDatabaseChgAliasPathReq;
  TfsnmDatabaseChgAliasPathReq = Packed Record
    Alias: TffName;
    NewPath: TffPath;
    NewPathBlob: TffPath;
    CheckDisk: Boolean;
  End;
  {reply as error in message header}
  PfsnmOldDatabaseChgAliasPathReq = ^TfsnmOldDatabaseChgAliasPathReq;
  TfsnmOldDatabaseChgAliasPathReq = Packed Record
    Alias: TffName;
    NewPath: TffPath;
    NewPathBlob: TffPath;
    CheckDisk: Boolean;
  End;

  PfsnmDatabaseSetTimeoutReq = ^TfsnmDatabaseSetTimeoutReq;
  TfsnmDatabaseSetTimeoutReq = Packed Record
    DatabaseID: TffDatabaseID;
    Timeout: Longint;
  End;
  { Reply as error in message header. }

  PfsnmDatabaseGetFreeSpaceReq = ^TfsnmDatabaseGetFreeSpaceReq;
  TfsnmDatabaseGetFreeSpaceReq = Packed Record
    DatabaseID: TffDatabaseID;
  End;

  PfsnmDatabaseGetFreeSpaceRpy = ^TfsnmDatabaseGetFreeSpaceRpy;
  TfsnmDatabaseGetFreeSpaceRpy = Packed Record
    FreeSpace: Int64;
  End;

  PfsnmDatabaseModifyAliasReq = ^TfsnmDatabaseModifyAliasReq;
  TfsnmDatabaseModifyAliasReq = Packed Record
    ClientID: TffClientID;
    Alias: TffName;
    NewName: TffName;
    NewPath: TffPath;
    NewPathBlob: TffPath;
    CheckDisk: Boolean;
  End;
  {reply as error in message header}
  PfsnmOldDatabaseModifyAliasReq = ^TfsnmOldDatabaseModifyAliasReq;
  TfsnmOldDatabaseModifyAliasReq = Packed Record
    ClientID: TffClientID;
    Alias: TffName;
    NewName: TffName;
    NewPath: TffPath;
    NewPathBlob: TffPath;
    CheckDisk: Boolean;
  End;


  PfsnmDatabaseTableExistsReq = ^TfsnmDatabaseTableExistsReq;
  TfsnmDatabaseTableExistsReq = Packed Record
    DatabaseID: TffDatabaseID;
    TableName: TfsTableName;
  End;
  PfsnmDatabaseTableExistsRpy = ^TfsnmDatabaseTableExistsRpy;
  TfsnmDatabaseTableExistsRpy = Packed Record
    Exists: Boolean;
  End;

  PfsnmDatabaseTableLockedExclusiveReq = ^TfsnmDatabaseTableLockedExclusiveReq;
  TfsnmDatabaseTableLockedExclusiveReq = Packed Record
    DatabaseID: TffDatabaseID;
    TableName: TfsTableName;
  End;
  PfsnmDatabaseTableLockedExclusiveRpy = ^TfsnmDatabaseTableLockedExclusiveRpy;
  TfsnmDatabaseTableLockedExclusiveRpy = Packed Record
    Locked: Boolean;
  End;

  {===session related==================================================}
Type
  {add session}
  PfsnmSessionAddReq = ^TfsnmSessionAddReq;
  TfsnmSessionAddReq = Packed Record
    Timeout: Longint;
    IsClientDb: Array[0..9] Of Char;
    DataLength: Longint;
    Data: TfsVarMsgField;
  End;
  PfsnmSessionAddRpy = ^TfsnmSessionAddRpy;
  TfsnmSessionAddRpy = Packed Record
    SessionID: TffSessionID;
  End;

  {close session (reply packet contained in header)}
  PfsnmSessionCloseReq = ^TfsnmSessionCloseReq;
  TfsnmSessionCloseReq = Packed Record
    SessionID: TffSessionID;
  End;

  {Begin !!.06}
    { Close unused tables }
  PfsnmSessionCloseInactiveTblReq = ^TfsnmSessionCloseInactiveTblReq;
  TfsnmSessionCloseInactiveTblReq = Packed Record
    SessionID: TffSessionID;
  End;
  {End !!.06}

    {get current session ID (request packet contained in header)}
  PfsnmSessionGetCurrentRpy = ^TfsnmSessionGetCurrentRpy;
  TfsnmSessionGetCurrentRpy = Packed Record
    SessionID: TffSessionID;
  End;

  {set current session ID (reply packet contained in header)}
  PfsnmSessionSetCurrentReq = ^TfsnmSessionSetCurrentReq;
  TfsnmSessionSetCurrentReq = Packed Record
    SessionID: TffSessionID;
  End;

  { Set session's timeout value. }
  PfsnmSessionSetTimeoutReq = ^TfsnmSessionSetTimeoutReq;
  TfsnmSessionSetTimeoutReq = Packed Record
    SessionID: TffSessionID;
    Timeout: Longint;
  End;
  { Reply as error in message header. }

{===rebuild processes================================================}
Type
  {reindex table}
  PfsnmReindexTableReq = ^TfsnmReindexTableReq;
  TfsnmReindexTableReq = Packed Record
    DatabaseID: TffDatabaseID;
    TableName: TfsTableName;
    IndexName: TffDictItemName;
    IndexNumber: Longint;
  End;
  PfsnmReindexTableRpy = ^TfsnmReindexTableRpy;
  TfsnmReindexTableRpy = Packed Record
    RebuildID: Longint;
  End;

  {pack table}
  PfsnmPackTableReq = ^TfsnmPackTableReq;
  TfsnmPackTableReq = Packed Record
    DatabaseID: TffDatabaseID;
    TableName: TfsTableName;
    UndeleteRecords: Boolean;
    OnlyDeleted: boolean;
  End;
  PfsnmPackTableRpy = ^TfsnmPackTableRpy;
  TfsnmPackTableRpy = Packed Record
    RebuildID: Longint;
    UndeleteRecords: Boolean;
    OnlyDeleted: boolean;
  End;

  {restructure table}
  PfsnmRestructureTableRpy = ^TfsnmRestructureTableRpy;
  TfsnmRestructureTableRpy = Packed Record
    RebuildID: Longint;
  End;

  {get rebuild status}
  PfsnmGetRebuildStatusReq = ^TfsnmGetRebuildStatusReq;
  TfsnmGetRebuildStatusReq = Packed Record
    RebuildID: Longint;
  End;
  PfsnmGetRebuildStatusRpy = ^TfsnmGetRebuildStatusRpy;
  TfsnmGetRebuildStatusRpy = Packed Record
    Status: TffRebuildStatus;
    IsPresent: boolean;
  End;

  {===transaction stuff================================================}
Type
  PfsnmStartTransactionReq = ^TfsnmStartTransactionReq;
  TfsnmStartTransactionReq = Packed Record
    DatabaseID: TffDatabaseID;
    FailSafe: boolean;
  End;

  PfsnmEndTransactionReq = ^TfsnmEndTransactionReq;
  TfsnmEndTransactionReq = Packed Record
    DatabaseID: TffTransID;
    ToBeCommitted: boolean;
    RemoveFiles: Boolean;
  End;

  PfsnmInTransactionReq = ^TfsnmInTransactionReq;
  TfsnmInTransactionReq = Packed Record
    DatabaseID: TffDatabaseID;
    TransLevel: Longint;
  End;

  PfsnmTransactionCorruptedReq = ^TfsnmTransactionCorruptedReq;
  TfsnmTransactionCorruptedReq = Packed Record
    DatabaseID: TffDatabaseID;
  End;

  {===table stuff======================================================}
Type
  PfsnmOpenTableReq = ^TfsnmOpenTableReq;
  TfsnmOpenTableReq = Packed Record
    DatabaseID: TffDatabaseID;
    TableName: TfsTableName;
    //Password: String[fscl_PasswdSize];
    IndexName: TffDictItemName;
    IndexNumber: Longint;
    OpenMode: TffOpenMode;
    ShareMode: TffShareMode;
    Timeout: Longint;
  End;
  {open table replies with a stream}

  PfsnmAcqTableLockReq = ^TfsnmAcqTableLockReq;
  TfsnmAcqTableLockReq = Packed Record
    CursorID: TffCursorID;
    LockType: TffLockType;
  End;
  {reply as error in message header}

  PfsnmRelTableLockReq = ^TfsnmRelTableLockReq;
  TfsnmRelTableLockReq = Packed Record
    CursorID: TffCursorID;
    AllLocks: boolean;
    LockType: TffLockType;
  End;
  {reply as error in message header}

  PfsnmIsTableLockedReq = ^TfsnmIsTableLockedReq;
  TfsnmIsTableLockedReq = Packed Record
    CursorID: TffCursorID;
    LockType: TffLockType;
  End;
  PfsnmIsTableLockedRpy = ^TfsnmIsTableLockedRpy;
  TfsnmIsTableLockedRpy = Packed Record
    IsLocked: boolean;
  End;

  PfsnmGetTableDictionaryReq = ^TfsnmGetTableDictionaryReq;
  TfsnmGetTableDictionaryReq = Packed Record
    DatabaseID: TffDatabaseID;
    TableName: TfsTableName;
  End;
  {reply is a stream containing the dictionary}

  PfsnmDeleteTableReq = ^TfsnmDeleteTableReq;
  TfsnmDeleteTableReq = Packed Record
    DatabaseID: TffDatabaseID;
    TableName: TfsTableName;
  End;
  {reply as error in message header}

  PfsnmRenameTableReq = ^TfsnmRenameTableReq;
  TfsnmrenameTableReq = Packed Record
    DatabaseID: TffDatabaseID;
    OldTableName: TfsTableName;
    NewTableName: TfsTableName;
  End;
  {reply as error in message header}

  PfsnmGetTableRecCountReq = ^TfsnmGetTableRecCountReq;
  TfsnmGetTableRecCountReq = Packed Record
    CursorID: TffCursorID;
  End;
  PfsnmGetTableRecCountRpy = ^TfsnmGetTableRecCountRpy;
  TfsnmGetTableRecCountRpy = Packed Record
    RecCount: Longword;
  End;

  {Begin !!.10}
  PfsnmGetTableRecCountAsyncReq = ^TfsnmGetTableRecCountAsyncReq;
  TfsnmGetTableRecCountAsyncReq = Packed Record
    CursorID: Longint;
  End;
  PfsnmGetTableRecCountAsyncRpy = ^TfsnmGetTableRecCountAsyncRpy;
  TfsnmGetTableRecCountAsyncRpy = Packed Record
    RebuildID: Longint;
  End;
  {End !!.10}

  PfsnmEmptyTableReq = ^TfsnmEmptyTableReq;
  TfsnmEmptyTableReq = Packed Record
    DatabaseID: TffDatabaseID;
    CursorID: TffCursorID;
    TableName: TfsTableName;
  End;
  {reply as error in message header}

  PfsnmAddIndexReq = ^TfsnmAddIndexReq;
  TfsnmAddIndexReq = Packed Record
    DatabaseID: TffDatabaseID;
    CursorID: TffCursorID;
    TableName: TfsTableName;
    IndexDesc: TffIndexDescriptor;
  End;
  PfsnmAddIndexRpy = ^TfsnmAddIndexRpy;
  TfsnmAddIndexRpy = Packed Record
    RebuildID: Longint;
  End;

  PfsnmDropIndexReq = ^TfsnmDropIndexReq;
  TfsnmDropIndexReq = Packed Record
    DatabaseID: TffDatabaseID;
    CursorID: TffCursorID;
    TableName: TfsTableName;
    IndexName: TffDictItemName;
    IndexNumber: Longint;
  End;
  {reply as error in message header}

  PfsnmSetTableAutoIncValueReq = ^TfsnmSetTableAutoIncValueReq;
  TfsnmSetTableAutoIncValueReq = Packed Record
    CursorID: TffCursorID;
    AutoInc64Value: Int64;
    AutoInc64StepValue: Longint;
  End;
  {reply as error in message header}

  PfsnmGetTableAutoIncValueReq = ^TfsnmGetTableAutoIncValueReq;
  TfsnmGetTableAutoIncValueReq = Packed Record
    CursorID: TffCursorID;
  End;
  PfsnmGetTableAutoIncValueRpy = ^TfsnmGetTableAutoIncValueRpy;
  TfsnmGetTableAutoIncValueRpy = Packed Record
    AutoInc64Value: Int64;
    AutoInc64StepValue: Longint;
  End;

  {Begin !!.11}
    { Get table version. }
  PfsnmGetTableVersionReq = ^TfsnmGetTableVersionReq;
  TfsnmGetTableVersionReq = Packed Record
    DatabaseID: TffDatabaseID;
    TableName: TfsTableName;
  End;
  PfsnmGetTableVersionRpy = ^TfsnmGetTableVersionRpy;
  TfsnmGetTableVersionRpy = Packed Record
    Version: Longint;
  End;
  {End !!.11}

  PfsnmSetTableMaxRecordsValueReq = ^TfsnmSetTableMaxRecordsValueReq;
  TfsnmSetTableMaxRecordsValueReq = Packed Record
    CursorID: TffCursorID;
    MaxRecords: Longword;
  End;
  {reply as error in message header}

  PfsnmGetTableMaxRecordsValueReq = ^TfsnmGetTableMaxRecordsValueReq;
  TfsnmGetTableMaxRecordsValueReq = Packed Record
    CursorID: TffCursorID;
  End;
  PfsnmGetTableMaxRecordsValueRpy = ^TfsnmGetTableMaxRecordsValueRpy;
  TfsnmGetTableMaxRecordsValueRpy = Packed Record
    MaxRecords: Longword;
  End;

  PfsnmSetTableTablePasswordValueReq = ^TfsnmSetTableTablePasswordValueReq;
  TfsnmSetTableTablePasswordValueReq = Packed Record
    CursorID: TffCursorID;
    TablePassword: Longword;
  End;

  PfsnmGetTableTablePasswordValueReq = ^TfsnmGetTableTablePasswordValueReq;
  TfsnmGetTableTablePasswordValueReq = Packed Record
    CursorID: TffCursorID;
  End;

  PfsnmGetTableTablePasswordValueRpy = ^TfsnmGetTableTablePasswordValueRpy;
  TfsnmGetTableTablePasswordValueRpy = Packed Record
    TablePassword: Longword;
  End;

  PfsnmSetTableTablePasswordRestValueReq = ^TfsnmSetTableTablePasswordRestValueReq;
  TfsnmSetTableTablePasswordRestValueReq = Packed Record
    CursorID: TffCursorID;
    TablePassword: Longword;
  End;

  PfsnmGetTableTablePasswordRestValueReq = ^TfsnmGetTableTablePasswordRestValueReq;
  TfsnmGetTableTablePasswordRestValueReq = Packed Record
    CursorID: TffCursorID;
  End;

  PfsnmGetTableTablePasswordRestValueRpy = ^TfsnmGetTableTablePasswordRestValueRpy;
  TfsnmGetTableTablePasswordRestValueRpy = Packed Record
    TablePassword: Longword;
  End;

  PfsnmSetTableTableDBIDValueReq = ^TfsnmSetTableTableDBIDValueReq;
  TfsnmSetTableTableDBIDValueReq = Packed Record
    CursorID: TffCursorID;
    TableDBID: Longword;
  End;

  PfsnmGetTableTableDBIDValueReq = ^TfsnmGetTableTableDBIDValueReq;
  TfsnmGetTableTableDBIDValueReq = Packed Record
    CursorID: TffCursorID;
  End;

  PfsnmGetTableTableDBIDValueRpy = ^TfsnmGetTableTableDBIDValueRpy;
  TfsnmGetTableTableDBIDValueRpy = Packed Record
    TableDBID: Longword;
  End;

  PfsnmSetTableTableFlagsValueReq = ^TfsnmSetTableTableFlagsValueReq;
  TfsnmSetTableTableFlagsValueReq = Packed Record
    CursorID: TffCursorID;
    TableFlags: Word;
  End;

  {reply as error in message header}

  PfsnmGetTableTableFlagsValueReq = ^TfsnmGetTableTableFlagsValueReq;
  TfsnmGetTableTableFlagsValueReq = Packed Record
    CursorID: TffCursorID;
  End;

  PfsnmGetTableTableFlagsValueRpy = ^TfsnmGetTableTableFlagsValueRpy;
  TfsnmGetTableTableFlagsValueRpy = Packed Record
    TableFlags: Word;
  End;

  {===BLOB stuff=======================================================}
Type
  PfsnmCreateBLOBReq = ^TfsnmCreateBLOBReq;
  TfsnmCreateBLOBReq = Packed Record
    CursorID: TffCursorID;
  End;
  PfsnmCreateBLOBRpy = ^TfsnmCreateBLOBRpy;
  TfsnmCreateBLOBRpy = Packed Record
    BLOBNr: TffInt64;
  End;

  PfsnmDeleteBLOBReq = ^TfsnmDeleteBLOBReq;
  TfsnmDeleteBLOBReq = Packed Record
    CursorID: TffCursorID;
    BLOBNr: TffInt64;
  End;
  {reply as error in message header}

  PfsnmGetBLOBLengthReq = ^TfsnmGetBLOBLengthReq;
  TfsnmGetBLOBLengthReq = Packed Record
    CursorID: TffCursorID;
    BLOBNr: TffInt64;
  End;
  PfsnmGetBLOBLengthRpy = ^TfsnmGetBLOBLengthRpy;
  TfsnmGetBLOBLengthRpy = Packed Record
    BLOBLength: Longint;
  End;

  PfsnmTruncateBLOBReq = ^TfsnmTruncateBLOBReq;
  TfsnmTruncateBLOBReq = Packed Record
    CursorID: TffCursorID;
    FieldNo: TffWord32;
    BLOBNr: TffInt64;
    BLOBLength: Longint;
  End;
  {reply as error in message header}

  PfsnmReadBLOBReq = ^TfsnmReadBLOBReq;
  TfsnmReadBLOBReq = Packed Record
    CursorID: TffCursorID;
    FieldNo: TffWord32;
    BLOBNr: TffInt64;
    Offset: Longint;
    Len: Longint;
  End;
  PfsnmReadBLOBRpy = ^TfsnmReadBLOBRpy;
  TfsnmReadBLOBRpy = Packed Record
    BytesRead: TffWord32; {!!.06}
    BLOB: TfsVarMsgField;
  End;

  PfsnmWriteBLOBReq = ^TfsnmWriteBLOBReq;
  TfsnmWriteBLOBReq = Packed Record
    CursorID: TffCursorID;
    FieldNo: TffWord32;
    BLOBNr: TffInt64;
    Offset: Longint;
    Len: Longint;
    BLOB: TfsVarMsgField;
  End;
  {reply as error in message header}

  PfsnmFreeBLOBReq = ^TfsnmFreeBLOBReq;
  TfsnmFreeBLOBReq = Packed Record
    CursorID: Longint;
    BLOBNr: TffInt64;
    ReadOnly: boolean;
  End;
  {reply as error in message header}

  PfsnmAddFileBLOBReq = ^TfsnmAddFileBLOBReq;
  TfsnmAddFileBLOBReq = Packed Record
    CursorID: TffCursorID;
    FileName: TffFullFileName;
  End;
  PfsnmAddFileBLOBRpy = ^TfsnmAddFileBLOBRpy;
  TfsnmAddFileBLOBRpy = Packed Record
    BLOBNr: TffInt64;
  End;

  {Begin !!.03}
  {get list of free BLOB segments - reply is stream}
  PfsnmGetBLOBFreeSpaceReq = ^TfsnmGetBLOBFreeSpaceReq;
  TfsnmGetBLOBFreeSpaceReq = Packed Record
    CursorID: TffCursorID;
    InMemory: Boolean;
  End;

  {get list of segments used by BLOB - reply is stream}
  PfsnmListBLOBSegmentsReq = ^TfsnmListBLOBSegmentsReq;
  TfsnmListBLOBSegmentsReq = Packed Record
    CursorID: TffCursorID;
    BLOBNr: TffInt64;
  End;
  {End !!.03}

{===Cursor stuff=====================================================}
Type
  PfsnmCursorSetToBeginReq = ^TfsnmCursorSetToBeginReq;
  TfsnmCursorSetToBeginReq = Packed Record
    CursorID: TffCursorID;
  End;
  {reply as error in message header}

  PfsnmCursorSetToEndReq = ^TfsnmCursorSetToEndReq;
  TfsnmCursorSetToEndReq = Packed Record
    CursorID: TffCursorID;
  End;
  {reply as error in message header}

  PfsnmCursorCloseReq = ^TfsnmCursorCloseReq;
  TfsnmCursorCloseReq = Packed Record
    CursorID: TffCursorID;
  End;
  {reply as error in message header}

  PfsnmCursorGetBookmarkReq = ^TfsnmCursorGetBookmarkReq;
  TfsnmCursorGetBookmarkReq = Packed Record
    CursorID: TffCursorID;
    BookmarkSize: Longint;
  End;
  {reply is a byte Array}

  PfsnmCursorSetToBookmarkReq = ^TfsnmCursorSetToBookmarkReq;
  TfsnmCursorSetToBookmarkReq = Packed Record
    CursorID: TffCursorID;
    BookmarkSize: Longint;
    Bookmark: TfsVarMsgField;
  End;
  {reply as error in message header}

  PfsnmCursorCompareBMsReq = ^TfsnmCursorCompareBMsReq;
  TfsnmCursorCompareBMsReq = Packed Record
    CursorID: TffCursorID;
    BookmarkSize: Longint;
    Bookmark1: TfsVarMsgField;
    Bookmark2: TfsVarMsgField;
  End;
  PfsnmCursorCompareBMsRpy = ^TfsnmCursorCompareBMsRpy;
  TfsnmCursorCompareBMsRpy = Packed Record
    CompareResult: Longint;
  End;

  PfsnmCursorSetToKeyReq = ^TfsnmCursorSetToKeyReq;
  TfsnmCursorSetToKeyReq = Packed Record
    CursorID: TffCursorID;
    Action: TffSearchKeyAction;
    DirectKey: boolean;
    FieldCount: Longint;
    PartialLen: Longint;
    KeyDataLen: Longint;
    KeyData: TfsVarMsgField;
  End;
  {reply as error in message header}

  PfsnmCursorSwitchToIndexReq = ^TfsnmCursorSwitchToIndexReq;
  TfsnmCursorSwitchToIndexReq = Packed Record
    CursorID: TffCursorID;
    IndexName: TffDictItemName;
    IndexNumber: Longint;
    PosnOnRec: boolean;
  End;
  {reply as error in message header}

  PfsnmCursorResetRangeReq = ^TfsnmCursorResetRangeReq;
  TfsnmCursorResetRangeReq = Packed Record
    CursorID: TffCursorID;
  End;
  {reply as error in message header}

  PfsnmCursorSetRangeReq = ^TfsnmCursorSetRangeReq;
  TfsnmCursorSetRangeReq = Packed Record
    CursorID: TffCursorID;
    DirectKey: boolean;
    FieldCount1: Longint;
    PartialLen1: Longint;
    KeyLen1: Longint;
    KeyIncl1: boolean;
    FieldCount2: Longint;
    PartialLen2: Longint;
    KeyLen2: Longint;
    KeyIncl2: boolean;
    KeyData1: TfsVarMsgField; {key or record data depending on Direct Key}
    KeyData2: TfsVarMsgField; {key or record data depending on Direct Key}
  End;
  {reply as an error in message header}

  PfsnmCursorCloneReq = ^TfsnmCursorCloneReq;
  TfsnmCursorCloneReq = Packed Record
    CursorID: TffCursorID;
    OpenMode: TffOpenMode;
  End;
  PfsnmCursorCloneRpy = ^TfsnmCursorCloneRpy;
  TfsnmCursorCloneRpy = Packed Record
    CursorID: TffCursorID;
  End;

  PfsnmCursorSetToCursorReq = ^TfsnmCursorSetToCursorReq;
  TfsnmCursorSetToCursorReq = Packed Record
    DestCursorID: TffCursorID;
    SrcCursorID: TffCursorID;
  End;
  {reply as an error in message header}

  PfsnmCursorSetFilterReq = ^TfsnmCursorSetFilterReq;
  TfsnmCursorSetFilterReq = Packed Record
    CursorID: TffCursorID;
    Timeout: TffWord32;
    ExprTree: TfsVarMsgField;
  End;

  PfsnmCursorOverrideFilterReq = ^TfsnmCursorOverrideFilterReq;
  TfsnmCursorOverrideFilterReq = Packed Record
    CursorID: Longint;
    Timeout: TffWord32;
    ExprTree: TfsVarMsgField;
  End;

  PfsnmCursorRestoreFilterReq = ^TfsnmCursorRestoreFilterReq;
  TfsnmCursorRestoreFilterReq = Packed Record
    CursorID: Longint;
  End;

  { Set a cursor's timeout value. }
  PfsnmCursorSetTimeoutReq = ^TfsnmCursorSetTimeoutReq;
  TfsnmCursorSetTimeoutReq = Packed Record
    CursorID: TffCursorID;
    Timeout: Longint;
  End;
  { Reply as an error in message header. }

{Begin !!.02}
  { Copy records from one cursor to another. }
  PfsnmCursorCopyRecordsReq = ^TfsnmCursorCopyRecordsReq;
  TfsnmCursorCopyRecordsReq = Packed Record
    SrcCursorID: TffCursorID;
    DestCursorID: TffCursorID;
    CopyBLOBs: Boolean;
    CountPerTrans: Longint;
    StartValue: TffWord32;
    Count: TffWord32;
  End;
  { Reply as an error in message header. }
{End !!.02}

{Begin !!.06}
  { Delete records from cursor. }
  PfsnmCursorDeleteRecordsReq = ^TfsnmCursorDeleteRecordsReq;
  TfsnmCursorDeleteRecordsReq = Packed Record
    CursorID: TffCursorID;
    CountPerTrans: Longint;
    StartValue: TffWord32;
    Count: TffWord32;
  End;
  { Reply as an error in message header. }
{End !!.06}

{===Record stuff=====================================================}
Type
  PfsnmRecordGetReq = ^TfsnmRecordGetReq;
  TfsnmRecordGetReq = Packed Record
    CursorID: TffCursorID;
    RecLen: Longint;
    BookmarkSize: Longint;
    LockType: TffLockType;
    UserLockType: TfsUserRecLocking;
    PreventForDuplicateLock: boolean;
    aUser: boolean;
    ShowLocked: boolean; // return lock rekord
  End;
  {reply is a byte Array}

  PfsnmRecordGetNextReq = ^TfsnmRecordGetNextReq;
  TfsnmRecordGetNextReq = Packed Record
    CursorID: TffCursorID;
    RecLen: Longint;
    BookmarkSize: Longint;
    LockType: TffLockType;
    ShowLocked: boolean; // return lock rekord
  End;
  {reply is a byte Array}

  PfsnmRecordGetPrevReq = ^TfsnmRecordGetPrevReq;
  TfsnmRecordGetPrevReq = Packed Record
    CursorID: TffCursorID;
    RecLen: Longint;
    BookmarkSize: Longint;
    LockType: TffLockType;
    ShowLocked: boolean; // return lock rekord
  End;
  {reply is a Byte Array}

  PfsnmRecordRelLockReq = ^TfsnmRecordRelLockReq;
  TfsnmRecordRelLockReq = Packed Record
    CursorID: TffCursorID;
    AllLocks: Boolean;
  End;
  {reply as error in message header}

  PfsnmRecordDeleteReq = ^TfsnmRecordDeleteReq;
  TfsnmRecordDeleteReq = Packed Record
    CursorID: TffCursorID;
    RecLen: Longint; {if non 0, record is returned}
  End;
  {reply is a Byte Array}

  PfsnmRecordInsertReq = ^TfsnmRecordInsertReq;
  TfsnmRecordInsertReq = Packed Record
    CursorID: TffCursorID;
    RecLen: Longint;
    BookmarkSize: Longint;
    LockType: TffLockType;
    Undelete: Boolean;
    Data: TfsVarMsgField;
  End;
  {reply as error in message header}

  PfsnmRecordModifyReq = ^TfsnmRecordModifyReq;
  TfsnmRecordModifyReq = Packed Record
    CursorID: TffCursorID;
    RecLen: Longint;
    BookmarkSize: Longint;
    RelLock: Boolean;
    UserLockType: TfsUserRecLocking;
    aFlag: Byte;
    aSet: Boolean;
    aUse: Boolean;
    Data: TfsVarMsgField;
  End;
  {reply as error in message header}

  PfsnmRecordExtractKeyReq = ^TfsnmRecordExtractKeyReq;
  TfsnmRecordExtractKeyReq = Packed Record
    CursorID: TffCursorID;
    KeyLen: Longint;
    ForCurrentRecord: boolean;
    Data: TfsVarMsgField;
  End;
  {reply is a byte array}

  PfsnmRecordGetForKeyReq = ^TfsnmRecordGetForKeyReq;
  TfsnmRecordGetForKeyReq = Packed Record
    CursorID: TffCursorID;
    DirectKey: boolean;
    FieldCount: Longint;
    PartialLen: Longint;
    RecLen: Longint;
    KeyDataLen: Longint;
    BookmarkSize: Longint;
    KeyData: TfsVarMsgField;
  End;
  {reply is a byte array}

  PfsnmRecordGetForKeyReq2 = ^TfsnmRecordGetForKeyReq2;
  TfsnmRecordGetForKeyReq2 = Packed Record
    CursorID: Longint;
    DirectKey: boolean;
    FieldCount: Longint;
    PartialLen: Longint;
    RecLen: Longint;
    KeyDataLen: Longint;
    BookmarkSize: Longint;
    FirstCall: Boolean;
    KeyData: TfsVarMsgField;
  End;
  {reply is a byte array}

  PfsnmRecordGetBatchReq = ^TfsnmRecordGetBatchReq;
  TfsnmRecordGetBatchReq = Packed Record
    CursorID: TffCursorID;
    RecLen: Longint;
    RecCount: Longint; {count of records requested}
    {note: RecLen*RecCount < 64K}
  End;
  PfsnmRecordGetBatchRpy = ^TfsnmRecordGetBatchRpy;
  TfsnmRecordGetBatchRpy = Packed Record
    RecCount: Longint; {count of records read}
    Error: TffResult; {Result of the last GetRecord call}
    RecArray: TfsVarMsgField; {place holder for array of records}
  End;

  PfsnmRecordDeleteBatchReq = ^TfsnmRecordDeleteBatchReq;
  TfsnmRecordDeleteBatchReq = Packed Record
    CursorID: TffCursorID;
    BMCount: Longint;
    BMLen: Longint;
    BMArray: TfsVarMsgField;
  End;
  {reply as a longint array with BMCount elements}

  PfsnmRecordIsLockedReq = ^TfsnmRecordIsLockedReq;
  TfsnmRecordIsLockedReq = Packed Record
    CursorID: TffCursorID;
    LockType: TffLockType;
  End;
  PfsnmRecordIsLockedRpy = ^TfsnmRecordIsLockedRpy;
  TfsnmRecordIsLockedRpy = Packed Record
    IsLocked: Boolean;
    Count: TffWord32;
  End;

  PfsnmRecordCountLockedReq = ^TfsnmRecordCountLockedReq;
  TfsnmRecordCountLockedReq = Packed Record
    CursorID: TffCursorID;
    LockType: TffLockType;
  End;
  PfsnmRecordCountLockedRpy = ^TfsnmRecordCountLockedRpy;
  TfsnmRecordCountLockedRpy = Packed Record
    Count: TffWord32;
  End;

  PfsnmRecordInsertBatchReq = ^TfsnmRecordInsertBatchReq;
  TfsnmRecordInsertBatchReq = Packed Record
    CursorID: TffCursorID;
    RecLen: Longint;
    RecCount: Longint; {count of records requested}
    {note: RecLen*RecCount < 64K}
    RecArray: TfsVarMsgField; {place holder for array of records}
  End;
  {reply is a longint array with RecCount elements}

  PfsnmRecordGetSetPositionReq = ^TfsnmRecordGetSetPositionReq;
  TfsnmRecordGetSetPositionReq = Packed Record
    CursorID: TffCursorID;
    RecLen: Longint;
    BookmarkSize: Longint;
    LockType: TffLockType;
    Value: Longword;
    InfoGetSetPosition: TInfoGetSetPosition;
    aSet: Boolean;
  End;

  {===SQL stuff========================================================}
Type

  PfsnmSQLAllocReq = ^TfsnmSQLAllocReq;
  TfsnmSQLAllocReq = Packed Record
    DatabaseID: TffDatabaseID;
    Timeout: Longint;
  End;
  PfsnmSQLAllocRpy = ^TfsnmSQLAllocRpy;
  TfsnmSQLAllocRpy = Packed Record
    StmtID: TffSqlStmtID;
  End;

  PfsnmSQLExecReq = ^TfsnmSQLExecReq;
  TfsnmSQLExecReq = Packed Record
    StmtID: TffSqlStmtID;
    OpenMode: TffOpenMode;
  End;
  {Exec replies with a stream.  If the execution succeeded, the first item in
   the stream is the server's cursorID & the second item is the cursor's
   data dictionary.  If the execution failed, the first item in the stream is
   the integer length of an error message.  The second item in the stream is
   the error message. }

  PfsnmSQLExecDirectReq = ^TfsnmSQLExecDirectReq;
  TfsnmSQLExecDirectReq = Packed Record
    DatabaseID: TffDatabaseID; {!!.03 - Start}
    Timeout: Longint;
    OpenMode: TffOpenMode;
    Query: TfsVarMsgField; {place holder for ZString query text}
  End; {!!.03 - End}
  {ExecDirect replies with a stream containing a cursorID,
   a data dictionary, and an optional error message.  If cursorID is zero then
   no data dictionary.  Error message is preceded by its length.  If length is
   zero then no error message. }

  PfsnmSQLFreeReq = ^TfsnmSQLFreeReq;
  TfsnmSQLFreeReq = Packed Record
    StmtID: TffSqlStmtID;
  End;
  {reply as error in message header}

  PfsnmSQLPrepareReq = ^TfsnmSQLPrepareReq;
  TfsnmSQLPrepareReq = Packed Record
    StmtID: TffSqlStmtID;
    Query: TfsVarMsgField; { place holder for ZString query text }
  End;
  {Prepare replies with an error code and a stream.  If the error code is
   DBIERR_NONE then the stream is empty.  Otherwise the stream contains an
   error message.  The error message is preceded by its length. }

  { Note: There is no data structure for SetParams.  The parameters are
          transmitted in stream format. }
  {SetParams replies with an error code and a stream.  If the error code is
   DBIERR_NONE then the stream is empty.  Otherwise the stream contains
   an error message.  The error message is preceded by its length. }

{===Server Info stuff================================================}
  { Server Info }
  PfsnmServerIsReadOnlyRpy = ^TfsnmServerIsReadOnlyRpy;
  TfsnmServerIsReadOnlyRpy = Packed Record
    IsReadOnly: boolean;
  End;

  PfsnmServerStatisticsRpy = ^TfsnmServerStatisticsRpy; {begin !!.10}
  TfsnmServerStatisticsRpy = Packed Record
    Stats: TfsServerStatistics;
  End;

  PfsnmCmdHandlerStatisticsReq = ^TfsnmCmdHandlerStatisticsReq;
  TfsnmCmdHandlerStatisticsReq = Packed Record
    CmdHandlerIdx: Integer;
  End;

  PfsnmCmdHandlerStatisticsRpy = ^TfsnmCmdHandlerStatisticsRpy;
  TfsnmCmdHandlerStatisticsRpy = Packed Record
    Stats: TfsCommandHandlerStatistics;
  End;

  PfsnmTransportStatisticsReq = ^TfsnmTransportStatisticsReq;
  TfsnmTransportStatisticsReq = Packed Record
    CmdHandlerIdx: Integer;
    TransportIdx: Integer;
  End;

  PfsnmTransportStatisticsRpy = ^TfsnmTransportStatisticsRpy;
  TfsnmTransportStatisticsRpy = Packed Record
    Stats: TfsTransportStatistics;
  End; {end !!.10}

Implementation

End.

