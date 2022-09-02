{$I fsdefine.inc}

Unit fssrcvex;

Interface

Uses
  Classes,
  Windows,
  SysUtils,
  fsconst,
  fsllbase,
  fslllog,
  fssrmgr,
  fsllexcp,
  fssrbase,
  fssrbde;

Function ConvertServerExceptionEx(E: Exception;
  aLog: TFSBaseLog;
  Const aReadOnly: Boolean): TffResult;

Function ConvertServerException(E: Exception; aLog: TFSBaseLog): TffResult;

Implementation

Function ConvertServerExceptionEx(E: Exception;
  aLog: TFSBaseLog;
  Const aReadOnly: Boolean): TffResult;

Var
  ErrorCode: Integer;
Begin
  {log it}
  If assigned(aLog) And (Not aReadOnly) Then
    aLog.WriteString(Format('Exception Cvt: %s', [E.Message]));

  {convert the FlashFiler-specific exceptions}
  If E Is EfsException Then
    Begin
      ErrorCode := EfsException(E).ErrorCode;
      Case ErrorCode Of
        fserrBadStruct: Result := DBIERR_FS_BadStruct;
        fserrOpenFailed: Result := DBIERR_FS_OpenFailed;
        fserrOpenNoMem: Result := DBIERR_FS_OpenNoMem;
        fserrCloseFailed: Result := DBIERR_FS_CloseFailed;
        fserrReadFailed: Result := DBIERR_FS_ReadFailed;
        fserrReadExact: Result := DBIERR_FS_ReadExact;
        fserrWriteFailed: Result := DBIERR_FS_WriteFailed;
        fserrWriteExact: Result := DBIERR_FS_WriteExact;
        fserrSeekFailed: Result := DBIERR_FS_SeekFailed;
        fserrFlushFailed: Result := DBIERR_FS_FlushFailed;
        fserrSetEOFFailed: Result := DBIERR_FS_SetEOFFailed;
        fserrCopyFile: Result := DBIERR_FS_CopyFile;
        fserrDeleteFile: Result := DBIERR_FS_DeleteFile;
        fserrRenameFile: Result := DBIERR_FS_RenameFile;
        fserrNotAnFFFile: Result := DBIERR_UNKNOWNFILE;
        fserrBadBlockNr: Result := DBIERR_FS_BadBlockNr;
        fserrEncrypted: Result := DBIERR_TBLENCRYPTED;
        fserrRecDeleted: Result := DBIERR_FS_RecDeleted;
        fserrBadRefNr: Result := DBIERR_FS_BadRefNr;
        fserrBadDataBlock: Result := DBIERR_FS_BadDataBlock;
        fserrBlobDeleted: Result := DBIERR_BLOBERR;
        fserrBadBlobNr: Result := DBIERR_INVALIDBLOBHANDLE;
        fserrBadBlobBlock,
          fserrBadBlobSeg: Result := DBIERR_BLOBERR;
        fserrLenMismatch: Result := DBIERR_INVALIDBLOBLEN;
        fserrOfsNotInBlob: Result := DBIERR_INVALIDBLOBOFFSET;
        fserrFileBlobWrite: Result := DBIERR_READONLYFLD;
        fserrBadStreamBlock: Result := DBIERR_FS_BadStreamBlock;
        fserrBadStreamOrigin: Result := DBIERR_FS_BadStreamOrigin;
        fserrStreamSeekError: Result := DBIERR_FS_StreamSeekError;
        fserrBadInxBlock: Result := DBIERR_FS_BadInxBlock;
        fserrBadIndex: Result := DBIERR_FS_BadIndex;
        fserrMaxIndexes: Result := DBIERR_FS_MaxIndexes;
        fserrBadMergeCall: Result := DBIERR_FS_BadMergeCall;
        fserrKeyNotFound: Result := DBIERR_FS_KeyNotFound;
        fserrKeyPresent: Result := DBIERR_FS_KeyPresent;
        fserrNoKeys: Result := DBIERR_FS_NoKeys;
        fserrNoSeqAccess: Result := DBIERR_FS_NoSeqAccess;
        fserrBadApproxPos: Result := DBIERR_FS_BadApproxPos;
        fserrBadServerName: Result := DBIERR_FS_BadServerName;
        fserrFFV1File: Result := DBIERR_FS_V1File;
        fserrCommsNoWinRes,
          fserrCommsCannotCall,
          fserrCommsCantListen,
          fserrWinsock,
          fserrWSNoSocket,
          fserrWSNoLocalAddr: Result := DBIERR_NETUNKNOWN;
        fserrUnknownClient,
          fserrUnknownSession: Result := DBIERR_INVALIDHNDL;
        fserrUnknownAlias: Result := DBIERR_UNKNOWNDB;
        fserrUnknownPath: Result := DBIERR_INVALIDDIR;
        fserrUnknownDB: Result := DBIERR_INVALIDHNDL;
        fserrUnknownTable: Result := DBIERR_NOSUCHTABLE;
        fserrUnknownIndex: Result := DBIERR_NOSUCHINDEX;
        fserrUnknownCursor,
          fserrUnknownTrans: Result := DBIERR_INVALIDHNDL;
        fserrUnknownMsg: Result := DBIERR_FS_UnknownMsg;
        fserrTmpStoreFull: Result := DBIERR_FS_TempStorageFull;
        fserrDBExclusive: Result := DBIERR_NEEDEXCLACCESS;
        fserrDBReadOnly: Result := DBIERR_READONLYDB;
        fserrTableExclusive: Result := DBIERR_NEEDEXCLACCESS;
        fserrCursorReadOnly: Result := DBIERR_TABLEREADONLY;
        fserrWriteLocked: Result := DBIERR_LOCKED;
        fserrReadLocked: Result := DBIERR_LOCKED;
        fserrCannotUnlock: Result := DBIERR_UNLOCKFAILED;
        fserrTableLocked: Result := DBIERR_FILELOCKED;
        fserrRecLocked: Result := DBIERR_LOCKED;
        fserrNoCurrentRec: Result := DBIERR_NOCURRREC;
        fserrTableMismatch: Result := DBIERR_DIFFERENTTABLES;
        fserrNoNextRecord: Result := DBIERR_EOF;
        fserrNoPriorRecord: Result := DBIERR_BOF;
        fserrTableExists: Result := DBIERR_TABLEEXISTS;
        fserrBadFieldXform: Result := DBIERR_INVALIDXLATION;
        fserrBadBookmark: Result := DBIERR_INVALIDBOOKMARK;
        fserrTransactionFailed: Result := DBIERR_WRITEERR;
        fserrTableFull: Result := DBIERR_TABLEFULL;
        fserrDiskFull: Result := DBIERR_NODISKSPACE; {!!.11}
        fserrTableVersion: Result := DBIERR_FS_TABLEVERSION; {!!.11}
        fserrInvalidSqlStmtHandle: Result := DBIERR_INVALIDHNDL;
        fserrBLOBTooBig: Result := DBIERR_FS_BLOBTooBig;
        fserrDeadlock: Result := DBIERR_FS_Deadlock;
        fserrLockTimeout: Result := DBIERR_LOCKED;
        fserrLockRejected: Result := DBIERR_LOCKED; {!!.02}
        fserrTableLockTimeout: Result := DBIERR_FILELOCKED;
        fserrGeneralTimeout: Result := DBIERR_FS_GeneralTimeout;
        fserrNoSQLEngine: Result := DBIERR_FS_NoSQLEngine;
        fserrIndexNotSupported: Result := DBIERR_INVALIDINDEXCREATE;
        fserrInvalidTableName: Result := DBIERR_INVALIDTABLENAME;
        fserrRangeNotSupported: Result := DBIERR_FS_RangeNotSupported;
        fserrTableOpen: Result := DBIERR_TABLEOPEN;
        DBIERR_TABLEREADONLY: Result := ErrorCode; {!!.06}
        fserrIncompatDict: Result := DBIERR_FS_IncompatDict; {!!.06}
        fserrSameTable: Result := DBIERR_FS_SameTable; {!!.06}
        50100: Result := DBIERR_INVALIDPASSWORD;
        50101: Result := DBIERR_UNKNOWNDB; // temp
        Else
          Result := DBIERR_FS_Unknown;
      End; {case}
    End
      {convert out of memory errors}
  Else If E Is EOutOfMemory Then
    Begin
      Result := DBIERR_NOMEMORY;
    End
      {convert all other exceptions to fatal error code}
  Else
    Result := DBIERR_FS_UnknownExcp;
End;
{--------}

Function ConvertServerException(E: Exception; aLog: TFSBaseLog): TffResult;
Begin
  Result := ConvertServerExceptionEx(E, aLog, False);
End;

End.

