{*********************************************************}
{* FlashFiler: Server exception conversion to dbiResult  *}
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

unit ffsrcvex;

interface

uses
  Classes,
  Windows,
  SysUtils,
  ffconst,
  ffllbase,
  fflllog,
  ffsrmgr,
  ffllexcp,
  ffsrbase,
  ffsrbde;

function ConvertServerExceptionEx(E         : Exception;
                                  aLog      : TffBaseLog;
                            const aReadOnly : Boolean) : TffResult;

function ConvertServerException(E : Exception; aLog : TffBaseLog) : TffResult;

implementation

function ConvertServerExceptionEx(E         : Exception;
                                  aLog      : TffBaseLog;
                            const aReadOnly : Boolean) : TffResult;

var
  ErrorCode : integer;
begin
  {log it}
  if assigned(aLog) and (not aReadOnly) then
    aLog.WriteString(Format('Exception Cvt: %s', [E.Message]));

  {convert the FlashFiler-specific exceptions}
  if E is EffException then begin
    ErrorCode := EffException(E).ErrorCode;
    case ErrorCode of
      fferrBadStruct      : Result := DBIERR_FF_BadStruct;
      fferrOpenFailed     : Result := DBIERR_FF_OpenFailed;
      fferrOpenNoMem      : Result := DBIERR_FF_OpenNoMem;
      fferrCloseFailed    : Result := DBIERR_FF_CloseFailed;
      fferrReadFailed     : Result := DBIERR_FF_ReadFailed;
      fferrReadExact      : Result := DBIERR_FF_ReadExact;
      fferrWriteFailed    : Result := DBIERR_FF_WriteFailed;
      fferrWriteExact     : Result := DBIERR_FF_WriteExact;
      fferrSeekFailed     : Result := DBIERR_FF_SeekFailed;
      fferrFlushFailed    : Result := DBIERR_FF_FlushFailed;
      fferrSetEOFFailed   : Result := DBIERR_FF_SetEOFFailed;
      fferrCopyFile       : Result := DBIERR_FF_CopyFile;
      fferrDeleteFile     : Result := DBIERR_FF_DeleteFile;
      fferrRenameFile     : Result := DBIERR_FF_RenameFile;
      fferrNotAnFFFile    : Result := DBIERR_UNKNOWNFILE;
      fferrBadBlockNr     : Result := DBIERR_FF_BadBlockNr;
      fferrEncrypted      : Result := DBIERR_TBLENCRYPTED;
      fferrRecDeleted     : Result := DBIERR_FF_RecDeleted;
      fferrBadRefNr       : Result := DBIERR_FF_BadRefNr;
      fferrBadDataBlock   : Result := DBIERR_FF_BadDataBlock;
      fferrBlobDeleted    : Result := DBIERR_BLOBERR;
      fferrBadBlobNr      : Result := DBIERR_INVALIDBLOBHANDLE;
      fferrBadBlobBlock,
      fferrBadBlobSeg     : Result := DBIERR_BLOBERR;
      fferrLenMismatch    : Result := DBIERR_INVALIDBLOBLEN;
      fferrOfsNotInBlob   : Result := DBIERR_INVALIDBLOBOFFSET;
      fferrFileBlobWrite  : Result := DBIERR_READONLYFLD;
      fferrBadStreamBlock : Result := DBIERR_FF_BadStreamBlock;
      fferrBadStreamOrigin: Result := DBIERR_FF_BadStreamOrigin;
      fferrStreamSeekError: Result := DBIERR_FF_StreamSeekError;
      fferrBadInxBlock    : Result := DBIERR_FF_BadInxBlock;
      fferrBadIndex       : Result := DBIERR_FF_BadIndex;
      fferrMaxIndexes     : Result := DBIERR_FF_MaxIndexes;
      fferrBadMergeCall   : Result := DBIERR_FF_BadMergeCall;
      fferrKeyNotFound    : Result := DBIERR_FF_KeyNotFound;
      fferrKeyPresent     : Result := DBIERR_FF_KeyPresent;
      fferrNoKeys         : Result := DBIERR_FF_NoKeys;
      fferrNoSeqAccess    : Result := DBIERR_FF_NoSeqAccess;
      fferrBadApproxPos   : Result := DBIERR_FF_BadApproxPos;
      fferrBadServerName  : Result := DBIERR_FF_BadServerName;
      fferrFFV1File       : Result := DBIERR_FF_V1File;
      fferrCommsNoWinRes,
      fferrCommsCannotCall,
      fferrCommsCantListen,
      fferrWinsock,
      fferrWSNoSocket,
      fferrWSNoLocalAddr  : Result := DBIERR_NETUNKNOWN;
      fferrUnknownClient,
      fferrUnknownSession : Result := DBIERR_INVALIDHNDL;
      fferrUnknownAlias   : Result := DBIERR_UNKNOWNDB;
      fferrUnknownPath    : Result := DBIERR_INVALIDDIR;
      fferrUnknownDB      : Result := DBIERR_INVALIDHNDL;
      fferrUnknownTable   : Result := DBIERR_NOSUCHTABLE;
      fferrUnknownIndex   : Result := DBIERR_NOSUCHINDEX;
      fferrUnknownCursor,
      fferrUnknownTrans   : Result := DBIERR_INVALIDHNDL;
      fferrUnknownMsg     : Result := DBIERR_FF_UnknownMsg;
      fferrTmpStoreFull   : Result := DBIERR_FF_TempStorageFull;
      fferrDBExclusive    : Result := DBIERR_NEEDEXCLACCESS;
      fferrDBReadOnly     : Result := DBIERR_READONLYDB;
      fferrTableExclusive : Result := DBIERR_NEEDEXCLACCESS;
      fferrCursorReadOnly : Result := DBIERR_TABLEREADONLY;
      fferrWriteLocked    : Result := DBIERR_LOCKED;
      fferrReadLocked     : Result := DBIERR_LOCKED;
      fferrCannotUnlock   : Result := DBIERR_UNLOCKFAILED;
      fferrTableLocked    : Result := DBIERR_FILELOCKED;
      fferrRecLocked      : Result := DBIERR_LOCKED;
      fferrNoCurrentRec   : Result := DBIERR_NOCURRREC;
      fferrTableMismatch  : Result := DBIERR_DIFFERENTTABLES;
      fferrNoNextRecord   : Result := DBIERR_EOF;
      fferrNoPriorRecord  : Result := DBIERR_BOF;
      fferrTableExists    : Result := DBIERR_TABLEEXISTS;
      fferrBadFieldXform  : Result := DBIERR_INVALIDXLATION;
      fferrBadBookmark    : Result := DBIERR_INVALIDBOOKMARK;
      fferrTransactionFailed : Result := DBIERR_WRITEERR;
      fferrTableFull      : Result := DBIERR_TABLEFULL;
      fferrDiskFull       : Result := DBIERR_NODISKSPACE;              {!!.11}
      fferrTableVersion : Result := DBIERR_FF_TABLEVERSION;            {!!.11} 
      fferrInvalidSqlStmtHandle : Result := DBIERR_INVALIDHNDL;
      fferrBLOBTooBig     : Result := DBIERR_FF_BLOBTooBig;
      fferrDeadlock       : Result := DBIERR_FF_Deadlock;
      fferrLockTimeout    : Result := DBIERR_LOCKED;
      fferrLockRejected   : Result := DBIERR_LOCKED;                   {!!.02}
      fferrTableLockTimeout : Result := DBIERR_FILELOCKED;
      fferrGeneralTimeout : Result := DBIERR_FF_GeneralTimeout;
      fferrNoSQLEngine    : Result := DBIERR_FF_NoSQLEngine;
      fferrIndexNotSupported : Result := DBIERR_INVALIDINDEXCREATE;
      fferrInvalidTableName : Result := DBIERR_INVALIDTABLENAME;
      fferrRangeNotSupported : Result := DBIERR_FF_RangeNotSupported;
      fferrTableOpen      : Result := DBIERR_TABLEOPEN;
      DBIERR_TABLEREADONLY : Result := ErrorCode;                      {!!.06}
      fferrIncompatDict   : Result := DBIERR_FF_IncompatDict;          {!!.06}
      fferrSameTable      : Result := DBIERR_FF_SameTable;             {!!.06}
    else
      Result := DBIERR_FF_Unknown;
    end;{case}
  end
  {convert out of memory errors}
  else if E is EOutOfMemory then begin
    Result := DBIERR_NOMEMORY;
  end
  {convert all other exceptions to fatal error code}
  else
    Result := DBIERR_FF_UnknownExcp;
end;
{--------}
function ConvertServerException(E : Exception; aLog : TffBaseLog) : TffResult;
begin
  Result := ConvertServerExceptionEx(E, aLog, False);
end;

end.
