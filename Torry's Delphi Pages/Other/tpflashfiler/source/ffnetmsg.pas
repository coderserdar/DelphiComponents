{*********************************************************}
{* FlashFiler: Network messaging types & constants       *}
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

unit ffnetmsg;

interface

uses
  Windows,
  Messages,
  SysUtils,
  ffllbase;

{===Network message constants===}
const
  ffm_LostConnection       = WM_USER + $0FF1;
  ffm_StartTblReindex      = WM_USER + $0FF2;
  ffm_StartTblPack         = WM_USER + $0FF3;
  ffm_StartTblRestructure  = WM_USER + $0FF4;
  ffm_CopyData             = WM_USER + $0FF5;
  ffm_CallSingleUserServer = WM_USER + $0FF6;
  ffm_KeepAlive            = WM_USER + $0FF7;

  ffmtRequest              = 1;  { Request sent from client to server }
  ffmtReply                = 2;  { Reply sent from server to client }

const
  {general or non-BDE type}
  ffnmDetachServer         = $0002;
  ffnmEndOfStream          = $0003;
  ffnmDetachServerJIC      = $0004;
  ffnmACK                  = $0005;
  ffnmRequestServerName    = $0006;
  ffnmServerNameReply      = $0007;
  ffnmNewServerAdvert      = $0008;
  ffnmCheckSecureComms     = $0009;
  ffnmCheckConnection      = $000A;  {!!!!}
  ffnmGetServerDateTime    = $000C;
  ffnmCallServer           = $000D;
  ffnmClientSetTimeout     = $000E;
  ffnmAttachServer         = $000F;
  ffnmGetServerSystemTime  = $0010;                                   {!!.10}
  ffnmGetServerGUID        = $0011;                                   {!!.10}
  ffnmGetServerID          = $0012;                                   {!!.10}

  ffnmMultiPartMessage     = $00FF;

  {database related}
  ffnmDatabaseOpen         = $0100;
  ffnmDatabaseClose        = $0101;
  ffnmDatabaseAliasList    = $0102;
  ffnmDatabaseTableList    = $0103;
  ffnmDatabaseAddAlias     = $0104;
  ffnmDatabaseOpenNoAlias  = $0105;
  ffnmDatabaseDeleteAlias  = $0107;
  ffnmDatabaseChgAliasPath = $0108;
  ffnmDatabaseSetTimeout   = $0109;
  ffnmDatabaseGetFreeSpace = $010A;
  ffnmDatabaseModifyAlias  = $010B;
  ffnmDatabaseGetAliasPath = $010C;
  ffnmDatabaseTableExists  = $010D;
  ffnmDatabaseTableLockedExclusive = $010E;

  {session related}
  ffnmSessionAdd           = $0200;
  ffnmSessionClose         = $0201;
  ffnmSessionGetCurrent    = $0202;
  ffnmSessionSetCurrent    = $0203;
  ffnmSessionSetTimeout    = $0204;
  ffnmSessionCloseInactTbl = $0205;                                    {!!.06}

  {rebuild processes}
  ffnmReindexTable         = $0300;
  ffnmPackTable            = $0301;
  ffnmRestructureTable     = $0302;
  ffnmGetRebuildStatus     = $0303;

  {transaction stuff}
  ffnmStartTransaction     = $0400;
  ffnmEndTransaction       = $0401;
  ffnmStartTransactionWith = $0402;                                    {!!.10}

  {table stuff}
  ffnmOpenTable            = $0500;
  ffnmAcqTableLock         = $0510;
  ffnmRelTableLock         = $0511;
  ffnmIsTableLocked        = $0512;
  ffnmGetTableDictionary   = $0513;
  ffnmBuildTable           = $0514;
  ffnmDeleteTable          = $0515;
  ffnmRenameTable          = $0516;
  ffnmGetTableRecCount     = $0517;
  ffnmEmptyTable           = $0518;
  ffnmAddIndex             = $0519;
  ffnmDropIndex            = $051A;
  ffnmSetTableAutoIncValue = $051B;
  ffnmGetTableAutoIncValue = $051C;
  ffnmGetTableRecCountAsync= $051D;                                    {!!.10}
  ffnmGetTableVersion      = $051E;                                    {!!.11}

  {BLOB stuff}
  ffnmCreateBLOB           = $0600;
  ffnmDeleteBLOB           = $0601;
  ffnmReadBLOB             = $0602;
  ffnmGetBLOBLength        = $0603;
  ffnmTruncateBLOB         = $0604;
  ffnmWriteBLOB            = $0605;
  ffnmAddFileBLOB          = $0607;
  ffnmFreeBLOB             = $0608;
  ffnmListBLOBFreeSpace    = $0609;                                    {!!.03}
  ffnmListBLOBSegments     = $060A;                                    {!!.03}

  {cursor stuff}
  ffnmCursorSetToBegin     = $0700;
  ffnmCursorSetToEnd       = $0701;
  ffnmCursorClose          = $0702;
  ffnmCursorGetBookmark    = $0703;
  ffnmCursorSetToBookmark  = $0704;
  ffnmCursorSetToKey       = $0705;
  ffnmCursorSwitchToIndex  = $0706;
  ffnmCursorSetRange       = $0707;
  ffnmCursorResetRange     = $0708;
  ffnmCursorCompareBMs     = $0709;
  ffnmCursorClone          = $070A;
  ffnmCursorSetToCursor    = $070B;
  ffnmCursorSetFilter      = $070C;
  ffnmCursorOverrideFilter = $070D;
  ffnmCursorRestoreFilter  = $070E;
  ffnmCursorSetTimeout     = $070F;
  ffnmCursorCopyRecords    = $0710;                                    {!!.02}
  ffnmCursorDeleteRecords  = $0711;                                    {!!.06}

  {record stuff}
  ffnmRecordGet            = $0800;
  ffnmRecordGetNext        = $0801;
  ffnmRecordGetPrev        = $0802;
  ffnmRecordRelLock        = $0803;
  ffnmRecordDelete         = $0804;
  ffnmRecordInsert         = $0805;
  ffnmRecordModify         = $0806;
  ffnmRecordExtractKey     = $0807;
  ffnmRecordGetForKey      = $0808;
  ffnmRecordGetBatch       = $0809;
  ffnmRecordInsertBatch    = $080A;
  ffnmRecordGetForKey2     = $080C;
  ffnmRecordDeleteBatch    = $080D;
  ffnmRecordIsLocked       = $080E;

  {SQL stuff}
  ffnmSQLAlloc             = $0900;
  ffnmSQLExec              = $0901;
  ffnmSQLExecDirect        = $0902;
  ffnmSQLFree              = $0903;
  ffnmSQLPrepare           = $0904;
  ffnmSQLSetParams         = $0905;

  {Server Operations}
  ffnmServerRestart        = $0A00;
  ffnmServerShutdown       = $0A01;
  ffnmServerStartup        = $0A02;
  ffnmServerStop           = $0A03;

  { Server Info }
  ffnmServerIsReadOnly     = $0B00;
  ffnmServerStatistics     = $0B01;                                   {!!.10}
  ffnmCmdHandlerStatistics = $0B02;                                   {!!.10}
  ffnmTransportStatistics  = $0B03;                                   {!!.10}

  ffnmUser                 = $4000;

{===Network message types===}
type
  PffnmHeader = ^TffnmHeader;
  TffnmHeader = packed record {General message header}
    nmhMsgID    : longint;    {..message identifier}
    nmhMsgLen   : longint;    {..size of this message, incl. header}
    nmhTotalSize: longint;    {..total size of data over all messages}
    nmhClientID : TffClientID;{..client ID (either from or to)}
    nmhRequestID : longInt;   {..client's requestID}
    nmhErrorCode: TffResult;  {..BDE error code, or 0 for success}
    nmhTimeout  : longInt;    {..timeout in milliseconds}
    nmhFirstPart: boolean;    {..is this the 1st part of the message?}
    nmhLastPart : boolean;    {..is this the last part of the message?}
    nmhDataType : TffNetMsgDataType; {..is message bytearray or stream?}
    nmhMsgType  : byte;       {..is this a request or a reply?  Declared as
                                 byte so that you may create additional msg
                                 types. }
    nmhData     : byte;       {..data marker}
  end;

  PffsmHeader = ^TffsmHeader;
  TffsmHeader = packed record {Sub-message header}
    smhMsgID    : longint;    {..message identifier}
    smhReplyLen : longint;    {..size of this reply (header + data)}
    smhErrorCode: TffWord16;  {..BDE error code, or 0 for success}
    smhDataType : TffNetMsgDataType; {..is message bytearray or stream?}
    smhFiller   : byte;       {..filler}
    smhData     : byte;       {..data marker}
  end;

const
  ffc_NetMsgHeaderSize = sizeof(TffnmHeader) - sizeof(byte);
  ffc_SubMsgHeaderSize = sizeof(TffsmHeader) - sizeof(byte);

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
type
  {attach to server}
  PffnmAttachServerReq = ^TffnmAttachServerReq;
  TffnmAttachServerReq = packed record
{Begin !!.03}
{$IFDEF IsDelphi}
    ClientName : TffNetName;
{$ELSE}
    ClientName : TffNetNameShr;
{$ENDIF}
{End !!.03}
    UserID     : TffName;
    Timeout    : longInt;
    ClientVersion : longInt;
  end;
  PffnmAttachServerRpy = ^TffnmAttachServerRpy;
  TffnmAttachServerRpy = packed record
    ClientID : TffClientID;
    VersionNumber : longint;
    Code : longint;
    LastMsgIntvl : longint;
    KAIntvl      : longint;
    KARetries    : longint;
    IsSecure : boolean;
  end;

  {request server name - DATAGRAM ONLY}
  PffnmRequestServerName = ^TffnmRequestServerName;
  TffnmRequestServerName = packed record
    MsgID : longint; {always ffnmRequestServerName}
  end;

  {server name reply - DATAGRAM ONLY}
  PffnmServerNameReply = ^TffnmServerNameReply;
  TffnmServerNameReply = packed record
    MsgID : longint; {always ffnmServerNameReply}
{Begin !!.03}
{$IFDEF IsDelphi}
    ServerLocalName : TffNetName;
    ServerNetName   : TffNetName;
{$ELSE}
    ServerLocalName : TffNetNameShr;
    ServerNetName   : TffNetNameShr;
{$ENDIF}
{End !!.03}
  end;

  PffnmGetServerDateTimeRpy = ^TffnmGetServerDateTimeRpy;
  TffnmGetServerDateTimeRpy = packed record
    ServerNow : TDateTime;
  end;

  PffnmGetServerSystemTimeRpy = ^TffnmGetServerSystemTimeRpy;  {begin !!.10}
  TffnmGetServerSystemTimeRpy = packed record
    ServerNow : TSystemTime;
  end;

  PffnmGetServerGUIDRpy = ^TffnmGetServerGUIDRpy;
  TffnmGetServerGUIDRpy = packed record
    GUID : TGUID;
  end;

  PffnmGetServerIDRpy = ^TffnmGetServerIDRpy;
  TffnmGetServerIDRpy = packed record
    UniqueID : TGUID;
  end;                                                           {end !!.10}

  PffnmCallServerReq = ^TffnmCallServerReq;
  TffnmCallServerReq = packed record
{Begin !!.03}
{$IFDEF IsDelphi}
    ServerName : TffNetName;
{$ELSE}
    ServerName : TffNetNameShr;
{$ENDIF}
{End !!.03}
  end;

  PffnmCallServerRpy = ^TffnmCallServerRpy;
  TffnmCallServerRpy = packed record
    ClientID : TffClientID;
  end;

  { Set a client's timeout value.}
  PffnmClientSetTimeoutReq = ^TffnmClientSetTimeoutReq;
  TffnmClientSetTimeoutReq = packed record
    Timeout : longInt;
  end;
  { Reply as an error in message header. }


{===database related=================================================}
type
  {open database}
  PffnmDatabaseOpenReq = ^TffnmDatabaseOpenReq;
  TffnmDatabaseOpenReq = packed record
    Alias     : TffName;
    OpenMode  : TffOpenMode;
    ShareMode : TffShareMode;
    Timeout   : longInt;
  end;
  PffnmDatabaseOpenRpy = ^TffnmDatabaseOpenRpy;
  TffnmDatabaseOpenRpy = packed record
    DatabaseID : TffDatabaseID;
  end;

  {close database (reply packet contained in header)}
  PffnmDatabaseCloseReq = ^TffnmDatabaseCloseReq;
  TffnmDatabaseCloseReq = packed record
    DatabaseID : TffDatabaseID;
  end;

  {get list of tables in database (reply packet is a stream)}
  PffnmDatabaseTableListReq = ^TffnmDatabaseTableListReq;
  TffnmDatabaseTableListReq = packed record
    DatabaseID : TffDatabaseID;
    Mask       : TffFileNameExt;
  end;

  {add new alias database}
  PffnmDatabaseAddAliasReq = ^TffnmDatabaseAddAliasReq;
  TffnmDatabaseAddAliasReq = packed record
    Alias     : TffName;
    Path      : TffPath;
{Begin !!.11}
    CheckDisk : Boolean;
  end;
  {reply as error in message header}
  PffnmOldDatabaseAddAliasReq = ^TffnmOldDatabaseAddAliasReq;
  TffnmOldDatabaseAddAliasReq = packed record
    Alias     : TffName;
    Path      : TffPath;
  end;
  { Used for backwards compatibility. }
{End !!.11}

  {open database without alias}
  PffnmDatabaseOpenNoAliasReq = ^TffnmDatabaseOpenNoAliasReq;
  TffnmDatabaseOpenNoAliasReq = packed record
    Path      : TffPath;
    OpenMode  : TffOpenMode;
    ShareMode : TffShareMode;
    Timeout   : longInt;
  end;
  PffnmDatabaseOpenNoAliasRpy = ^TffnmDatabaseOpenNoAliasRpy;
  TffnmDatabaseOpenNoAliasRpy = packed record
    DatabaseID : TffDatabaseID;
  end;

  {delete an alias}
  PffnmDatabaseDeleteAliasReq = ^TffnmDatabaseDeleteAliasReq;
  TffnmDatabaseDeleteAliasReq = packed record
    Alias : TffName;
  end;
  {reply as error in message header}

  {retrieve the alias' path}
  PffnmDatabaseGetAliasPathReq = ^TffnmDatabaseGetAliasPathReq;
  TffnmDatabaseGetAliasPathReq = packed record
    Alias : TffName;
  end;
  PffnmDatabaseGetAliasPathRpy = ^TffnmDatabaseGetAliasPathRpy;
  TffnmDatabaseGetAliasPathRpy = packed record
    Path : TffPath;
  end;

  PffnmDatabaseChgAliasPathReq = ^TffnmDatabaseChgAliasPathReq;
  TffnmDatabaseChgAliasPathReq = packed record
    Alias     : TffName;
    NewPath   : TffPath;
{Begin !!.11}
    CheckDisk : Boolean;
  end;
  {reply as error in message header}
  PffnmOldDatabaseChgAliasPathReq = ^TffnmOldDatabaseChgAliasPathReq;
  TffnmOldDatabaseChgAliasPathReq = packed record
    Alias     : TffName;
    NewPath   : TffPath;
  end;
  { Used for backwards compatibility. }
{End !!.11}

  PffnmDatabaseSetTimeoutReq = ^TffnmDatabaseSetTimeoutReq;
  TffnmDatabaseSetTimeoutReq = packed record
    DatabaseID : TffDatabaseID;
    Timeout : longInt;
  end;
  { Reply as error in message header. }

  PffnmDatabaseGetFreeSpaceReq = ^TffnmDatabaseGetFreeSpaceReq;
  TffnmDatabaseGetFreeSpaceReq = packed record
    DatabaseID : TffDatabaseID;
  end;

  PffnmDatabaseGetFreeSpaceRpy = ^TffnmDatabaseGetFreeSpaceRpy;
  TffnmDatabaseGetFreeSpaceRpy = packed record
    FreeSpace : Longint;
  end;

  PffnmDatabaseModifyAliasReq = ^TffnmDatabaseModifyAliasReq;
  TffnmDatabaseModifyAliasReq = packed record
    ClientID  : TffClientID;
    Alias     : TffName;
    NewName   : TffName;
    NewPath   : TffPath;
{Begin !!.11}
    CheckDisk : Boolean;
  end;
  {reply as error in message header}
  PffnmOldDatabaseModifyAliasReq = ^TffnmOldDatabaseModifyAliasReq;
  TffnmOldDatabaseModifyAliasReq = packed record
    ClientID  : TffClientID;
    Alias     : TffName;
    NewName   : TffName;
    NewPath   : TffPath;
  end;
  { Used for backwards compatibility. }
{End !!.11}

  PffnmDatabaseTableExistsReq = ^TffnmDatabaseTableExistsReq;
  TffnmDatabaseTableExistsReq = packed record
    DatabaseID : TffDatabaseID;
    TableName  : TffTableName;
  end;
  PffnmDatabaseTableExistsRpy = ^TffnmDatabaseTableExistsRpy;
  TffnmDatabaseTableExistsRpy = packed record
    Exists : Boolean;
  end;

  PffnmDatabaseTableLockedExclusiveReq = ^TffnmDatabaseTableLockedExclusiveReq;
  TffnmDatabaseTableLockedExclusiveReq = packed record
    DatabaseID : TffDatabaseID;
    TableName  : TffTableName;
  end;
  PffnmDatabaseTableLockedExclusiveRpy = ^TffnmDatabaseTableLockedExclusiveRpy;
  TffnmDatabaseTableLockedExclusiveRpy = packed record
    Locked : Boolean;
  end;

{===session related==================================================}
type
  {add session}
  PffnmSessionAddReq = ^TffnmSessionAddReq;
  TffnmSessionAddReq = packed record
    Timeout : longInt;
  end;
  PffnmSessionAddRpy = ^TffnmSessionAddRpy;
  TffnmSessionAddRpy = packed record
    SessionID : TffSessionID;
  end;

  {close session (reply packet contained in header)}
  PffnmSessionCloseReq = ^TffnmSessionCloseReq;
  TffnmSessionCloseReq = packed record
    SessionID : TffSessionID;
  end;

{Begin !!.06}
  { Close unused tables }
  PffnmSessionCloseInactiveTblReq = ^TffnmSessionCloseInactiveTblReq;
  TffnmSessionCloseInactiveTblReq = packed record
    SessionID : TffSessionID;
  end;
{End !!.06}

  {get current session ID (request packet contained in header)}
  PffnmSessionGetCurrentRpy = ^TffnmSessionGetCurrentRpy;
  TffnmSessionGetCurrentRpy = packed record
    SessionID : TffSessionID;
  end;

  {set current session ID (reply packet contained in header)}
  PffnmSessionSetCurrentReq = ^TffnmSessionSetCurrentReq;
  TffnmSessionSetCurrentReq = packed record
    SessionID : TffSessionID;
  end;

  { Set session's timeout value. }
  PffnmSessionSetTimeoutReq = ^TffnmSessionSetTimeoutReq;
  TffnmSessionSetTimeoutReq = packed record
    SessionID : TffSessionID;
    Timeout : longInt;
  end;
  { Reply as error in message header. }


{===rebuild processes================================================}
type
  {reindex table}
  PffnmReindexTableReq = ^TffnmReindexTableReq;
  TffnmReindexTableReq = packed record
    DatabaseID : TffDatabaseID;
    TableName  : TffTableName;
    IndexName  : TffDictItemName;
    IndexNumber: longint;
  end;
  PffnmReindexTableRpy = ^TffnmReindexTableRpy;
  TffnmReindexTableRpy = packed record
    RebuildID : longint;
  end;

  {pack table}
  PffnmPackTableReq = ^TffnmPackTableReq;
  TffnmPackTableReq = packed record
    DatabaseID : TffDatabaseID;
    TableName  : TffTableName;
  end;
  PffnmPackTableRpy = ^TffnmPackTableRpy;
  TffnmPackTableRpy = packed record
    RebuildID : longint;
  end;

  {restructure table}
  PffnmRestructureTableRpy = ^TffnmRestructureTableRpy;
  TffnmRestructureTableRpy = packed record
    RebuildID : longint;
  end;

  {get rebuild status}
  PffnmGetRebuildStatusReq = ^TffnmGetRebuildStatusReq;
  TffnmGetRebuildStatusReq = packed record
    RebuildID : longint;
  end;
  PffnmGetRebuildStatusRpy = ^TffnmGetRebuildStatusRpy;
  TffnmGetRebuildStatusRpy = packed record
    Status    : TffRebuildStatus;
    IsPresent : boolean;
  end;

{===transaction stuff================================================}
type
  PffnmStartTransactionReq = ^TffnmStartTransactionReq;
  TffnmStartTransactionReq = packed record
    DatabaseID : TffDatabaseID;
    FailSafe   : boolean;
  end;

  PffnmEndTransactionReq = ^TffnmEndTransactionReq;
  TffnmEndTransactionReq = packed record
    DatabaseID : TffTransID;
    ToBeCommitted : boolean;
  end;

{===table stuff======================================================}
type
  PffnmOpenTableReq = ^TffnmOpenTableReq;
  TffnmOpenTableReq = packed record
    DatabaseID : TffDatabaseID;
    TableName  : TffTableName;
    IndexName  : TffDictItemName;
    IndexNumber: longint;
    OpenMode   : TffOpenMode;
    ShareMode  : TffShareMode;
    Timeout    : longInt;
  end;
  {open table replies with a stream}

  PffnmAcqTableLockReq = ^TffnmAcqTableLockReq;
  TffnmAcqTableLockReq = packed record
    CursorID : TffCursorID;
    LockType : TffLockType;
  end;
  {reply as error in message header}

  PffnmRelTableLockReq = ^TffnmRelTableLockReq;
  TffnmRelTableLockReq = packed record
    CursorID : TffCursorID;
    AllLocks : boolean;
    LockType : TffLockType;
  end;
  {reply as error in message header}

  PffnmIsTableLockedReq = ^TffnmIsTableLockedReq;
  TffnmIsTableLockedReq = packed record
    CursorID : TffCursorID;
    LockType : TffLockType;
  end;
  PffnmIsTableLockedRpy = ^TffnmIsTableLockedRpy;
  TffnmIsTableLockedRpy = packed record
    IsLocked : boolean;
  end;

  PffnmGetTableDictionaryReq = ^TffnmGetTableDictionaryReq;
  TffnmGetTableDictionaryReq = packed record
    DatabaseID : TffDatabaseID;
    TableName  : TffTableName;
  end;
  {reply is a stream containing the dictionary}

  PffnmDeleteTableReq = ^TffnmDeleteTableReq;
  TffnmDeleteTableReq = packed record
    DatabaseID : TffDatabaseID;
    TableName  : TffTableName;
  end;
  {reply as error in message header}

  PffnmRenameTableReq = ^TffnmRenameTableReq;
  TffnmrenameTableReq = packed record
    DatabaseID   : TffDatabaseID;
    OldTableName : TffTableName;
    NewTableName : TffTableName;
  end;
  {reply as error in message header}

  PffnmGetTableRecCountReq = ^TffnmGetTableRecCountReq;
  TffnmGetTableRecCountReq = packed record
    CursorID : TffCursorID;
  end;
  PffnmGetTableRecCountRpy = ^TffnmGetTableRecCountRpy;
  TffnmGetTableRecCountRpy = packed record
    RecCount : longint;
  end;

{Begin !!.10}
  PffnmGetTableRecCountAsyncReq = ^TffnmGetTableRecCountAsyncReq;
  TffnmGetTableRecCountAsyncReq = packed record
    CursorID : longint;
  end;
  PffnmGetTableRecCountAsyncRpy = ^TffnmGetTableRecCountAsyncRpy;
  TffnmGetTableRecCountAsyncRpy = packed record
    RebuildID : longint;
  end;
{End !!.10}

  PffnmEmptyTableReq = ^TffnmEmptyTableReq;
  TffnmEmptyTableReq = packed record
    DatabaseID : TffDatabaseID;
    CursorID   : TffCursorID;
    TableName  : TffTableName;
  end;
  {reply as error in message header}

  PffnmAddIndexReq = ^TffnmAddIndexReq;
  TffnmAddIndexReq = packed record
    DatabaseID : TffDatabaseID;
    CursorID   : TffCursorID;
    TableName  : TffTableName;
    IndexDesc  : TffIndexDescriptor;
  end;
  PffnmAddIndexRpy = ^TffnmAddIndexRpy;
  TffnmAddIndexRpy = packed record
    RebuildID : longint;
  end;

  PffnmDropIndexReq = ^TffnmDropIndexReq;
  TffnmDropIndexReq = packed record
    DatabaseID : TffDatabaseID;
    CursorID   : TffCursorID;
    TableName  : TffTableName;
    IndexName  : TffDictItemName;
    IndexNumber: longint;
  end;
  {reply as error in message header}

  PffnmSetTableAutoIncValueReq = ^TffnmSetTableAutoIncValueReq;
  TffnmSetTableAutoIncValueReq = packed record
    CursorID     : TffCursorID;
    AutoIncValue : TffWord32;
  end;
  {reply as error in message header}

  PffnmGetTableAutoIncValueReq = ^TffnmGetTableAutoIncValueReq;
  TffnmGetTableAutoIncValueReq = packed record
    CursorID : TffCursorID;
  end;
  PffnmGetTableAutoIncValueRpy = ^TffnmGetTableAutoIncValueRpy;
  TffnmGetTableAutoIncValueRpy = packed record
    AutoIncValue : TffWord32;
  end;

{Begin !!.11}
  { Get table version. }
  PffnmGetTableVersionReq = ^TffnmGetTableVersionReq;
  TffnmGetTableVersionReq = packed record
    DatabaseID : TffDatabaseID;
    TableName  : TffTableName;
  end;
  PffnmGetTableVersionRpy = ^TffnmGetTableVersionRpy;
  TffnmGetTableVersionRpy = packed record
    Version : Longint;
  end;
{End !!.11}

{===BLOB stuff=======================================================}
type
  PffnmCreateBLOBReq = ^TffnmCreateBLOBReq;
  TffnmCreateBLOBReq = packed record
    CursorID : TffCursorID;
  end;
  PffnmCreateBLOBRpy = ^TffnmCreateBLOBRpy;
  TffnmCreateBLOBRpy = packed record
    BLOBNr : TffInt64;
  end;

  PffnmDeleteBLOBReq = ^TffnmDeleteBLOBReq;
  TffnmDeleteBLOBReq = packed record
    CursorID : TffCursorID;
    BLOBNr : TffInt64;
  end;
  {reply as error in message header}

  PffnmGetBLOBLengthReq = ^TffnmGetBLOBLengthReq;
  TffnmGetBLOBLengthReq = packed record
    CursorID : TffCursorID;
    BLOBNr : TffInt64;
  end;
  PffnmGetBLOBLengthRpy = ^TffnmGetBLOBLengthRpy;
  TffnmGetBLOBLengthRpy = packed record
    BLOBLength : longint;
  end;

  PffnmTruncateBLOBReq = ^TffnmTruncateBLOBReq;
  TffnmTruncateBLOBReq = packed record
    CursorID     : TffCursorID;
    BLOBNr       : TffInt64;
    BLOBLength   : longint;
  end;
  {reply as error in message header}

  PffnmReadBLOBReq = ^TffnmReadBLOBReq;
  TffnmReadBLOBReq = packed record
    CursorID     : TffCursorID;
    BLOBNr       : TffInt64;
    Offset       : longint;
    Len          : longint;
  end;
  PffnmReadBLOBRpy = ^TffnmReadBLOBRpy;
  TffnmReadBLOBRpy = packed record
    BytesRead    : TffWord32;                                          {!!.06}
    BLOB         : TffVarMsgField;
  end;

  PffnmWriteBLOBReq = ^TffnmWriteBLOBReq;
  TffnmWriteBLOBReq = packed record
    CursorID     : TffCursorID;
    BLOBNr       : TffInt64;
    Offset       : longint;
    Len          : longint;
    BLOB         : TffVarMsgField;
  end;
  {reply as error in message header}

  PffnmFreeBLOBReq = ^TffnmFreeBLOBReq;
  TffnmFreeBLOBReq = packed record
    CursorID     : longint;
    BLOBNr       : TffInt64;
    ReadOnly     : boolean;
  end;
  {reply as error in message header}

  PffnmAddFileBLOBReq = ^TffnmAddFileBLOBReq;
  TffnmAddFileBLOBReq = packed record
    CursorID : TffCursorID;
    FileName : TffFullFileName;
  end;
  PffnmAddFileBLOBRpy = ^TffnmAddFileBLOBRpy;
  TffnmAddFileBLOBRpy = packed record
    BLOBNr : TffInt64;
  end;

  {Begin !!.03}
  {get list of free BLOB segments - reply is stream}
  PffnmGetBLOBFreeSpaceReq = ^TffnmGetBLOBFreeSpaceReq;
  TffnmGetBLOBFreeSpaceReq = packed record
    CursorID : TffCursorID;
    InMemory : Boolean;
  end;

  {get list of segments used by BLOB - reply is stream}
  PffnmListBLOBSegmentsReq = ^TffnmListBLOBSegmentsReq;
  TffnmListBLOBSegmentsReq = packed record
    CursorID : TffCursorID;
    BLOBNr : TffInt64;
  end;
  {End !!.03}

{===Cursor stuff=====================================================}
type
  PffnmCursorSetToBeginReq = ^TffnmCursorSetToBeginReq;
  TffnmCursorSetToBeginReq = packed record
    CursorID     : TffCursorID;
  end;
  {reply as error in message header}

  PffnmCursorSetToEndReq = ^TffnmCursorSetToEndReq;
  TffnmCursorSetToEndReq = packed record
    CursorID     : TffCursorID;
  end;
  {reply as error in message header}

  PffnmCursorCloseReq = ^TffnmCursorCloseReq;
  TffnmCursorCloseReq = packed record
    CursorID     : TffCursorID;
  end;
  {reply as error in message header}

  PffnmCursorGetBookmarkReq = ^TffnmCursorGetBookmarkReq;
  TffnmCursorGetBookmarkReq = packed record
    CursorID     : TffCursorID;
    BookmarkSize : longint;
  end;
  {reply is a byte Array}

  PffnmCursorSetToBookmarkReq = ^TffnmCursorSetToBookmarkReq;
  TffnmCursorSetToBookmarkReq = packed record
    CursorID     : TffCursorID;
    BookmarkSize : longint;
    Bookmark     : TffVarMsgField;
  end;
  {reply as error in message header}

  PffnmCursorCompareBMsReq = ^TffnmCursorCompareBMsReq;
  TffnmCursorCompareBMsReq = packed record
    CursorID     : TffCursorID;
    BookmarkSize : longint;
    Bookmark1    : TffVarMsgField;
    Bookmark2    : TffVarMsgField;
  end;
  PffnmCursorCompareBMsRpy = ^TffnmCursorCompareBMsRpy;
  TffnmCursorCompareBMsRpy = packed record
    CompareResult : longint;
  end;

  PffnmCursorSetToKeyReq = ^TffnmCursorSetToKeyReq;
  TffnmCursorSetToKeyReq = packed record
    CursorID   : TffCursorID;
    Action     : TffSearchKeyAction;
    DirectKey  : boolean;
    FieldCount : longint;
    PartialLen : longint;
    KeyDataLen : longint;
    KeyData    : TffVarMsgField;
  end;
  {reply as error in message header}

  PffnmCursorSwitchToIndexReq = ^TffnmCursorSwitchToIndexReq;
  TffnmCursorSwitchToIndexReq = packed record
    CursorID   : TffCursorID;
    IndexName  : TffDictItemName;
    IndexNumber: longint;
    PosnOnRec  : boolean;
  end;
  {reply as error in message header}

  PffnmCursorResetRangeReq = ^TffnmCursorResetRangeReq;
  TffnmCursorResetRangeReq = packed record
    CursorID   : TffCursorID;
  end;
  {reply as error in message header}

  PffnmCursorSetRangeReq = ^TffnmCursorSetRangeReq;
  TffnmCursorSetRangeReq = packed record
    CursorID     : TffCursorID;
    DirectKey    : boolean;
    FieldCount1  : longint;
    PartialLen1  : longint;
    KeyLen1      : longint;
    KeyIncl1     : boolean;
    FieldCount2  : longint;
    PartialLen2  : longint;
    KeyLen2      : longint;
    KeyIncl2     : boolean;
    KeyData1     : TffVarMsgField;  {key or record data depending on Direct Key}
    KeyData2     : TffVarMsgField;  {key or record data depending on Direct Key}
  end;
  {reply as an error in message header}

  PffnmCursorCloneReq = ^TffnmCursorCloneReq;
  TffnmCursorCloneReq = packed record
    CursorID      : TffCursorID;
    OpenMode      : TffOpenMode;
  end;
  PffnmCursorCloneRpy = ^TffnmCursorCloneRpy;
  TffnmCursorCloneRpy = packed record
    CursorID      : TffCursorID;
  end;

  PffnmCursorSetToCursorReq = ^TffnmCursorSetToCursorReq;
  TffnmCursorSetToCursorReq = packed record
    DestCursorID : TffCursorID;
    SrcCursorID  : TffCursorID;
  end;
  {reply as an error in message header}

  PffnmCursorSetFilterReq = ^TffnmCursorSetFilterReq;
  TffnmCursorSetFilterReq = packed record
    CursorID : TffCursorID;
    Timeout  : TffWord32;
    ExprTree : TffVarMsgField;
  end;

  PffnmCursorOverrideFilterReq = ^TffnmCursorOverrideFilterReq;
  TffnmCursorOverrideFilterReq = packed record
    CursorID : longint;
    Timeout  : TffWord32;
    ExprTree : TffVarMsgField;
  end;

  PffnmCursorRestoreFilterReq = ^TffnmCursorRestoreFilterReq;
  TffnmCursorRestoreFilterReq = packed record
    CursorID : longint;
  end;

  { Set a cursor's timeout value. }
  PffnmCursorSetTimeoutReq = ^TffnmCursorSetTimeoutReq;
  TffnmCursorSetTimeoutReq = packed record
    CursorID : TffCursorID;
    Timeout : longInt;
  end;
  { Reply as an error in message header. }

{Begin !!.02}
  { Copy records from one cursor to another. }
  PffnmCursorCopyRecordsReq = ^TffnmCursorCopyRecordsReq;
  TffnmCursorCopyRecordsReq = packed record
    SrcCursorID  : TffCursorID;
    DestCursorID : TffCursorID;
    CopyBLOBs : Boolean;
  end;
  { Reply as an error in message header. }
{End !!.02}

{Begin !!.06}
  { Delete records from cursor. }
  PffnmCursorDeleteRecordsReq = ^TffnmCursorDeleteRecordsReq;
  TffnmCursorDeleteRecordsReq = packed record
    CursorID  : TffCursorID;
  end;
  { Reply as an error in message header. }
{End !!.06}

{===Record stuff=====================================================}
type
  PffnmRecordGetReq = ^TffnmRecordGetReq;
  TffnmRecordGetReq = packed record
    CursorID     : TffCursorID;
    RecLen       : longint;
    BookmarkSize : longint;
    LockType     : TffLockType;
  end;
  {reply is a byte Array}

  PffnmRecordGetNextReq = ^TffnmRecordGetNextReq;
  TffnmRecordGetNextReq = packed record
    CursorID     : TffCursorID;
    RecLen       : longint;
    BookmarkSize : longint;
    LockType     : TffLockType;
  end;
  {reply is a byte Array}

  PffnmRecordGetPrevReq = ^TffnmRecordGetPrevReq;
  TffnmRecordGetPrevReq = packed record
    CursorID     : TffCursorID;
    RecLen       : longint;
    BookmarkSize : longint;
    LockType     : TffLockType;
  end;
  {reply is a Byte Array}

  PffnmRecordRelLockReq = ^TffnmRecordRelLockReq;
  TffnmRecordRelLockReq = packed record
    CursorID : TffCursorID;
    AllLocks : Boolean;
  end;
  {reply as error in message header}

  PffnmRecordDeleteReq = ^TffnmRecordDeleteReq;
  TffnmRecordDeleteReq = packed record
    CursorID     : TffCursorID;
    RecLen       : longint; {if non 0, record is returned}
  end;
  {reply is a Byte Array}

  PffnmRecordInsertReq = ^TffnmRecordInsertReq;
  TffnmRecordInsertReq = packed record
    CursorID     : TffCursorID;
    RecLen       : longint;
    BookmarkSize : longint;
    LockType     : TffLockType;
    Data         : TffVarMsgField;
  end;
  {reply as error in message header}

  PffnmRecordModifyReq = ^TffnmRecordModifyReq;
  TffnmRecordModifyReq = packed record
    CursorID     : TffCursorID;
    RecLen       : longint;
    BookmarkSize : longint;
    RelLock      : Boolean;
    Data         : TffVarMsgField;
  end;
  {reply as error in message header}

  PffnmRecordExtractKeyReq = ^TffnmRecordExtractKeyReq;
  TffnmRecordExtractKeyReq = packed record
    CursorID         : TffCursorID;
    KeyLen           : longint;
    ForCurrentRecord : boolean;
    Data             : TffVarMsgField;
  end;
  {reply is a byte array}

  PffnmRecordGetForKeyReq = ^TffnmRecordGetForKeyReq;
  TffnmRecordGetForKeyReq = packed record
    CursorID   : TffCursorID;
    DirectKey  : boolean;
    FieldCount : longint;
    PartialLen : longint;
    RecLen     : longint;
    KeyDataLen : longint;
    BookmarkSize : longint;
    KeyData    : TffVarMsgField;
  end;
  {reply is a byte array}

  PffnmRecordGetForKeyReq2 = ^TffnmRecordGetForKeyReq2;
  TffnmRecordGetForKeyReq2 = packed record
    CursorID   : longint;
    DirectKey  : boolean;
    FieldCount : longint;
    PartialLen : longint;
    RecLen     : longint;
    KeyDataLen : longint;
    BookmarkSize : longint;
    FirstCall  : Boolean;
    KeyData    : TffVarMsgField;
  end;
  {reply is a byte array}


  PffnmRecordGetBatchReq = ^TffnmRecordGetBatchReq;
  TffnmRecordGetBatchReq = packed record
    CursorID : TffCursorID;
    RecLen   : longint;
    RecCount : longint; {count of records requested}
                        {note: RecLen*RecCount < 64K}
  end;
  PffnmRecordGetBatchRpy = ^TffnmRecordGetBatchRpy;
  TffnmRecordGetBatchRpy = packed record
    RecCount : longint;        {count of records read}
    Error    : TffResult;      {Result of the last GetRecord call}
    RecArray : TffVarMsgField; {place holder for array of records}
  end;

  PffnmRecordDeleteBatchReq = ^TffnmRecordDeleteBatchReq;
  TffnmRecordDeleteBatchReq = packed record
    CursorID : TffCursorID;
    BMCount  : Longint;
    BMLen    : Longint;
    BMArray  : TffVarMsgField;
  end;
  {reply as a longint array with BMCount elements}

  PffnmRecordIsLockedReq = ^TffnmRecordIsLockedReq;
  TffnmRecordIsLockedReq = packed record
    CursorID : TffCursorID;
    LockType : TffLockType;
  end;
  PffnmRecordIsLockedRpy = ^TffnmRecordIsLockedRpy;
  TffnmRecordIsLockedRpy = packed record
    IsLocked : Boolean;
  end;

  PffnmRecordInsertBatchReq = ^TffnmRecordInsertBatchReq;
  TffnmRecordInsertBatchReq = packed record
    CursorID : TffCursorID;
    RecLen   : longint;
    RecCount : longint; {count of records requested}
                        {note: RecLen*RecCount < 64K}
    RecArray : TffVarMsgField; {place holder for array of records}
  end;
  {reply is a longint array with RecCount elements}


{===SQL stuff========================================================}
type

  PffnmSQLAllocReq = ^TffnmSQLAllocReq;
  TffnmSQLAllocReq = packed record
    DatabaseID : TffDatabaseID;
    Timeout    : longInt;
  end;
  PffnmSQLAllocRpy = ^TffnmSQLAllocRpy;
  TffnmSQLAllocRpy = packed record
    StmtID     : TffSqlStmtID;
  end;

  PffnmSQLExecReq = ^TffnmSQLExecReq;
  TffnmSQLExecReq = packed record
    StmtID     : TffSqlStmtID;
    OpenMode   : TffOpenMode;
  end;
  {Exec replies with a stream.  If the execution succeeded, the first item in
   the stream is the server's cursorID & the second item is the cursor's
   data dictionary.  If the execution failed, the first item in the stream is
   the integer length of an error message.  The second item in the stream is
   the error message. }

  PffnmSQLExecDirectReq = ^TffnmSQLExecDirectReq;
  TffnmSQLExecDirectReq = packed record
    DatabaseID : TffDatabaseID;                                        {!!.03 - Start}
    Timeout    : longInt;
    OpenMode   : TffOpenMode;
    Query      : TffVarMsgField;      {place holder for ZString query text}
  end;                                                                 {!!.03 - End}
  {ExecDirect replies with a stream containing a cursorID,
   a data dictionary, and an optional error message.  If cursorID is zero then
   no data dictionary.  Error message is preceded by its length.  If length is
   zero then no error message. }

  PffnmSQLFreeReq = ^TffnmSQLFreeReq;
  TffnmSQLFreeReq = packed record
    StmtID     : TffSqlStmtID;
  end;
  {reply as error in message header}

  PffnmSQLPrepareReq = ^TffnmSQLPrepareReq;
  TffnmSQLPrepareReq = packed record
    StmtID     : TffSqlStmtID;
    Query      : TffVarMsgField;      { place holder for ZString query text }
  end;
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
  PffnmServerIsReadOnlyRpy = ^TffnmServerIsReadOnlyRpy;
  TffnmServerIsReadOnlyRpy = packed record
    IsReadOnly : boolean;
  end;

  PffnmServerStatisticsRpy = ^TffnmServerStatisticsRpy;        {begin !!.10}
  TffnmServerStatisticsRpy = packed record
    Stats : TffServerStatistics;
  end;

  PffnmCmdHandlerStatisticsReq = ^TffnmCmdHandlerStatisticsReq;
  TffnmCmdHandlerStatisticsReq = packed record
    CmdHandlerIdx : Integer;
  end;

  PffnmCmdHandlerStatisticsRpy = ^TffnmCmdHandlerStatisticsRpy;
  TffnmCmdHandlerStatisticsRpy = packed record
    Stats : TffCommandHandlerStatistics;
  end;

  PffnmTransportStatisticsReq = ^TffnmTransportStatisticsReq;
  TffnmTransportStatisticsReq = packed record
    CmdHandlerIdx : Integer;
    TransportIdx : Integer;
  end;

  PffnmTransportStatisticsRpy = ^TffnmTransportStatisticsRpy;
  TffnmTransportStatisticsRpy = packed record
    Stats : TffTransportStatistics;
  end;                                                           {end !!.10}

implementation


end.
