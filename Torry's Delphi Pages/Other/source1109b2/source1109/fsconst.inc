{Note:

  The actual strings are found in the following resource scripts:

         FSSRCNST.STR - server strings
           Range: $00 - $C4 (0 - 196)

         FSLLCNST.STR - General strings that can be used both client-side &
           server-side.
           Range: $100 - $1C3 (256 - 451)

         FSCLCNST.STR - Client strings.
           Range: $3F0 - $452 (1,008 - 1,106)

         FSDBCNST.STR - BDE-like strings & FF-specific client-side strings.
           BDE Range: $2101 - $351A (8,449 - 13,549)
           FF  Range: $3C00 - $3CD2 (15,360 - 15,521)

         FSDSCNST.STR - TDataSet descendant error strings.
           Range: $D500 - $D53A (54,528 - 54,586)

}

Const

  { Constants for string resource range boundaries }

  fsSRCNSTLow = $00;
  fsSRCNSTHigh = $FF;

  fsLLCNSTLow = $100;
  fsLLCNSTHigh = $1FF;

  fsCLCNSTLow = $3F0;
  fsCLCNSTHigh = $4FF;

  fsDBCNSTLow = $2101;
  fsDBCNSTHigh = $3D00;

  fsDSCNSTLow = $D500;
  fsDSCNSTHigh = $D5FF;

  {--- fsSRCNST ---}

    { Basic file I/O }
  fserrBadStruct = $00;
  fserrOpenFailed = $01;
  fserrOpenNoMem = $02;
  fserrCloseFailed = $03;
  fserrReadFailed = $04;
  fserrReadExact = $05;
  fserrWriteFailed = $06;
  fserrWriteExact = $07;
  fserrSeekFailed = $08;
  fserrFlushFailed = $09;
  fserrSetEOFFailed = $0A;

  { Low-level FF Server errors }
  fserrNotAnFFFile = $20;
  fserrBadBlockNr = $21;
  fserrEncrypted = $22;
  fserrRecDeleted = $23;
  fserrBadRefNr = $24;
  fserrBadDataBlock = $25;

  fserrBlobDeleted = $30;
  fserrBadBlobNr = $31;
  fserrBadBlobBlock = $32;
  fserrBadBlobSeg = $33;
  fserrLenMismatch = $34;
  fserrOfsNotInBlob = $35;
  fserrFileBlobWrite = $36;

  fserrBadStreamBlock = $40;
  fserrBadStreamOrigin = $41;
  fserrStreamSeekError = $42;

  fserrBadInxBlock = $50;
  fserrBadIndex = $51;
  fserrMaxIndexes = $52;
  fserrBadMergeCall = $53;
  fserrKeyNotFound = $54;
  fserrKeyPresent = $55;
  fserrNoKeys = $56;
  fserrNoSeqAccess = $57;
  fserrBadApproxPos = $58;

  fserrBadServerName = $70;
  fserrFFV1File = $71;
  fserrIncompatDict = $72;
  fserrBLOBTooBig = $73;

  { Errors to indicate unknown handles, IDs, etc }
  fserrUnknownClient = $90;
  fserrUnknownSession = $91;
  fserrUnknownAlias = $92;
  fserrUnknownPath = $93;
  fserrUnknownDB = $94;
  fserrUnknownTable = $95;
  fserrUnknownIndex = $96;
  fserrUnknownCursor = $97;
  fserrUnknownTrans = $98;
  fserrUnknownMsg = $99; { Unknown message type received from client }

  { Misc. server errors as a result of client messages }
  fserrDBExclusive = $A0;
  fserrDBReadOnly = $A1;
  fserrTableExclusive = $A2;
  fserrCursorReadOnly = $A3;
  fserrWriteLocked = $A4;
  fserrReadLocked = $A5;
  fserrCannotUnlock = $A6;
  fserrTableLocked = $A7;
  fserrRecLocked = $A8;
  fserrNoCurrentRec = $A9;
  fserrDynamicLink = $AA;
  fserrResolveTableLinks = $AB;
  fserrTableMismatch = $AC;
  fserrNoNextRecord = $AD;
  fserrNoPriorRecord = $AE;
  fserrTableExists = $AF;
  fserrDBInTrans = $B0;
  fserrAliasExists = $B1;
  fserrCannotCompare = $B2;
  fserrBadFieldXform = $B3;
  fserrNoTransaction = $B4;
  fserrBadBookmark = $B6;
  fserrTransactionFailed = $B7;
  fserrTableFull = $B8;
  fserrInvalidSqlStmtHandle = $B9;
  fserrDeadlock = $BA;
  fserrLockTimeout = $BB;
  fserrLockRejected = $BC;
  fserrTableLockTimeout = $BD;
  fserrGeneralTimeout = $BE;
  fserrNoSQLEngine = $BF;
  fserrIndexNotSupported = $C0;
  fserrInvalidTableName = $C1;
  fserrRangeNotSupported = $C2;
  fserrTableOpen = $C3;
  fserrSameTable = $C4;
  fserrSortFail = $C5;
  fserrBadDistinctField = $C6;
  fserrDiskFull = $C7; {!!.11}
  fserrTableVersion = $C8; {!!.11}

  {--- fsLLCNST ---}

    {temporary storage errors}
  fserrTmpStoreCreateFail = $100;
  fserrTmpStoreFull = $101;
  fserrMapFileCreateFail = $102;
  fserrMapFileHandleFail = $103;
  fserrMapFileViewFail = $104;

  fserrCopyFile = $110;
  fserrDeleteFile = $111;
  fserrRenameFile = $112;

  {low level client errors}
  fserrReplyTimeout = $120;
  fserrWaitFailed = $121;
  fserrInvalidProtocol = $122;
  fserrProtStartupFail = $123;
  fserrConnectionLost = $124;
  fserrTransportFail = $125;
  fserrPortalTimeout = $126;

  {dictionary errors}
  fserrOutOfBounds = $140;
  fserrDictPresent = $141;
  fserrNotADict = $142;
  fserrNoFields = $143;
  fserrBadFieldRef = $144;
  fserrBadFieldType = $145;
  fserrRecTooLong = $146;
  fserrDiffBlockSize = $147;
  fserrDictReadOnly = $148;
  fserrDictMissing = $149;
  fserrBLOBFileDefd = $14A;
  fserrBaseFile = $14B;
  fserrBadFileNumber = $14C;
  fserrBadBaseName = $14D;
  fserrBadExtension = $14E;
  fserrDupExtension = $14F;
  fserrDataFileDefd = $150;
  fserrNoFieldsInKey = $151;
  fserrBadParameter = $152;
  fserrBadBlockSize = $153;
  fserrKeyTooLong = $154;
  fserrDupFieldName = $155;
  fserrDupIndexName = $156;
  fserrIxHlprRegistered = $157;
  fserrIxHlprNotReg = $158;
  fserrIxHlprNotSupp = $159;
  fserrFileInUse = $160;
  fserrFieldInUse = $161;

  {General comms errors}
  fserrCommsNoWinRes = $170;
  fserrCommsCannotCall = $171;
  fserrCommsCantListen = $172;

  {Winsock errors}
  fserrWinsock = $180;
  fserrWSNoWinsock = $181;
  fserrWSNoSocket = $182;
  fserrWSNoLocalAddr = $183;

  {dialog errors}
  fserrInvalidServerName = $1A0;
  fserrInvalidNameorPath = $1A1;
  fserrDuplicateAliasName = $1A2;
  fserrEmptyValuesNotAllowed = $1A3;

  {miscellaneous constants}
  fsscSeqAccessIndexName = $1B0;
  fsscMainTableFileDesc = $1B1;
  fsscRegistryMainKey = $1B2;

  fsscRebuildPlaceHolder = $1C0;
  fsscRestructPlaceHolder = $1C1;
  fsscImportPlaceHolder = $1C2;
  fsscExportPlaceHolder = $1C3;

  {--- FFCLCNST ---}

    {client miscellaneous constants}
  fsccInvalidParameter = $3F0;
  fsccREG_PRODUCT = $3F1;
  fsccDupItemInColl = $3F2;

  { Import constants }
  fsccImport_NoSchemaFile = $400;
  fsccImport_RECLENGTHRequired = $401;
  fsccImport_NoMatchingFields = $402;
  fsccImport_FILETYPEMissing = $403;
  fsccImport_FILETYPEInvalid = $404;
  fsccImport_BadFieldName = $405;
  fsccImport_BadFieldType = $406;
  fsccImport_BadFloatSize = $407;
  fsccImport_BadIntegerSize = $408;
  fsccImport_BadUIntegerSize = $409;
  fsccImport_NoFields = $40A;
  fsccImport_BadOffset = $40B;
  fsccImport_BadSize = $40C;
  fsccImport_BadDecPl = $40D;
  fsccImport_BadDateMask = $40E;
  fsccImport_BadAutoIncSize = $40F;
  fsccImport_BadSchemaHeader = $410;

  fsccDesign_SLinkMasterSource = $450;
  fsccDesign_SLinkMaster = $451;
  fsccDesign_SLinkDesigner = $452;

  {--- FFDBCNST ---}

    {pseudo-BDE errors for server exceptions}
  ERRCAT_FSSQL = $3C;
  ERRBASE_FSSQL = $3C00;

  ERRCODE_FS_BadStruct = 0;
  ERRCODE_FS_OpenFailed = 1;
  ERRCODE_FS_OpenNoMem = 2;
  ERRCODE_FS_CloseFailed = 3;
  { Use me please            = 4;
    Use me please            = 5;
  }
  ERRCODE_FS_ReadFailed = 6;
  ERRCODE_FS_ReadExact = 7;
  ERRCODE_FS_WriteFailed = 8;
  ERRCODE_FS_WriteExact = 9;
  ERRCODE_FS_SeekFailed = $0A;
  ERRCODE_FS_FlushFailed = $0B;
  ERRCODE_FS_SetEOFFailed = $0C;
  ERRCODE_FS_TempStorageFull = $13;
  ERRCODE_FS_CopyFile = $20;
  ERRCODE_FS_DeleteFile = $21;
  ERRCODE_FS_RenameFile = $22;
  ERRCODE_FS_BadBlockNr = $31;
  ERRCODE_FS_RecDeleted = $33;
  ERRCODE_FS_BadRefNr = $34;
  ERRCODE_FS_BadDataBlock = $35;
  ERRCODE_FS_BadStreamBlock = $3D;
  ERRCODE_FS_BadStreamOrigin = $3E;
  ERRCODE_FS_StreamSeekError = $3F;
  ERRCODE_FS_BadInxBlock = $40;
  ERRCODE_FS_BadIndex = $41;
  ERRCODE_FS_MaxIndexes = $42;
  ERRCODE_FS_BadMergeCall = $43;
  ERRCODE_FS_KeyNotFound = $44;
  ERRCODE_FS_KeyPresent = $45;
  ERRCODE_FS_NoKeys = $46;
  ERRCODE_FS_NoSeqAccess = $47;
  ERRCODE_FS_BadApproxPos = $48;
  ERRCODE_FS_BadServerName = $49;
  ERRCODE_FS_FileBLOBOpen = $50;
  ERRCODE_FS_FileBLOBRead = $51;
  ERRCODE_FS_FileBLOBClose = $52;
  ERRCODE_FS_CorruptTrans = $53;
  ERRCODE_FS_FilterTimeout = $54;
  ERRCODE_FS_ReplyTimeout = $55;
  ERRCODE_FS_WaitFailed = $56;
  ERRCODE_FS_ClientIDFail = $57;
  ERRCODE_FS_NoAddHandler = $58;
  ERRCODE_FS_NoRemHandler = $59;
  ERRCODE_FS_Deadlock = $60;
  ERRCODE_FS_Timeout = $61;
  ERRCODE_FS_LockRejected = $62;
  ERRCODE_FS_ServerUnavail = $63;
  ERRCODE_FS_V1File = $64;
  ERRCODE_FS_GeneralTimeout = $65;
  ERRCODE_FS_NoSQLEngine = $66;
  ERRCODE_FS_TableVersion = $67; {!!.11}
  ERRCODE_FS_IxHlprRegistered = $77;
  ERRCODE_FS_IxHlprNotReg = $78;
  ERRCODE_FS_IxHlprNotSupp = $79;
  ERRCODE_FS_IncompatDict = $80; {!!.06}
  ERRCODE_FS_SameTable = $81; {!!.06}
  ERRCODE_FS_UnknownClient = $90;
  ERRCODE_FS_UnknownSession = $91;
  ERRCODE_FS_UnknownDB = $94;
  ERRCODE_FS_UnknownCursor = $97;
  ERRCODE_FS_Unknown = $A0;
  ERRCODE_FS_UnknownExcp = $A1;
  ERRCODE_FS_UnknownMsg = $A2;
  ERRCODE_FS_RangeNotSupported = $D2;

  DBIERR_FS_BadStruct = $3C00; {ERRBASE_FSSQL + ERRCODE_FS_BadStruct;}
  DBIERR_FS_OpenFailed = $3C01; {ERRBASE_FSSQL + ERRCODE_FS_OpenFailed;}
  DBIERR_FS_OpenNoMem = $3C02; {ERRBASE_FSSQL + ERRCODE_FS_OpenNoMem;}
  DBIERR_FS_CloseFailed = $3C03; {ERRBASE_FSSQL + ERRCODE_FS_CloseFailed;}
  DBIERR_FS_ReadFailed = $3C06; {ERRBASE_FSSQL + ERRCODE_FS_ReadFailed;}
  DBIERR_FS_ReadExact = $3C07; {ERRBASE_FSSQL + ERRCODE_FS_ReadExact;}
  DBIERR_FS_WriteFailed = $3C08; {ERRBASE_FSSQL + ERRCODE_FS_WriteFailed;}
  DBIERR_FS_WriteExact = $3C09; {ERRBASE_FSSQL + ERRCODE_FS_WriteExact;}
  DBIERR_FS_SeekFailed = $3C0A; {ERRBASE_FSSQL + ERRCODE_FS_SeekFailed;}
  DBIERR_FS_FlushFailed = $3C0B; {ERRBASE_FSSQL + ERRCODE_FS_FlushFailed;}
  DBIERR_FS_SetEOFFailed = $3C0C; {ERRBASE_FSSQL + ERRCODE_FS_SetEOFFailed;}
  DBIERR_FS_TempStorageFull = $3C13; {ERRBASE_FSSQL + ERRCODE_FS_TempStorageFull;}
  DBIERR_FS_CopyFile = $3C20; {ERRBASE_FSSQL + ERRCODE_FS_CopyFile;}
  DBIERR_FS_DeleteFile = $3C21; {ERRBASE_FSSQL + ERRCODE_FS_DeleteFile;}
  DBIERR_FS_RenameFile = $3C22; {ERRBASE_FSSQL + ERRCODE_FS_RenameFile;}
  DBIERR_FS_BadBlockNr = $3C31; {ERRBASE_FSSQL + ERRCODE_FS_BadBlockNr;}
  DBIERR_FS_RecDeleted = $3C33; {ERRBASE_FSSQL + ERRCODE_FS_RecDeleted;}
  DBIERR_FS_BadRefNr = $3C34; {ERRBASE_FSSQL + ERRCODE_FS_BadRefNr;}
  DBIERR_FS_BadDataBlock = $3C35; {ERRBASE_FSSQL + ERRCODE_FS_BadDataBlock;}
  DBIERR_FS_BadStreamBlock = $3C3D; {ERRBASE_FSSQL + ERRCODE_FS_BadStreamBlock;}
  DBIERR_FS_BadStreamOrigin = $3C3E; {ERRBASE_FSSQL + ERRCODE_FS_BadStreamOrigin;}
  DBIERR_FS_StreamSeekError = $3C3F; {ERRBASE_FSSQL + ERRCODE_FS_StreamSeekError;}
  DBIERR_FS_BadInxBlock = $3C40; {ERRBASE_FSSQL + ERRCODE_FS_BadInxBlock;}
  DBIERR_FS_BadIndex = $3C41; {ERRBASE_FSSQL + ERRCODE_FS_BadIndex;}
  DBIERR_FS_MaxIndexes = $3C42; {ERRBASE_FSSQL + ERRCODE_FS_MaxIndexes;}
  DBIERR_FS_BadMergeCall = $3C43; {ERRBASE_FSSQL + ERRCODE_FS_BadMergeCall;}
  DBIERR_FS_KeyNotFound = $3C44; {ERRBASE_FSSQL + ERRCODE_FS_KeyNotFound;}
  DBIERR_FS_KeyPresent = $3C45; {ERRBASE_FSSQL + ERRCODE_FS_KeyPresent;}
  DBIERR_FS_NoKeys = $3C46; {ERRBASE_FSSQL + ERRCODE_FS_NoKeys;}
  DBIERR_FS_NoSeqAccess = $3C47; {ERRBASE_FSSQL + ERRCODE_FS_NoSeqAccess;}
  DBIERR_FS_BadApproxPos = $3C48; {ERRBASE_FSSQL + ERRCODE_FS_BadApproxPos;}
  DBIERR_FS_BadServerName = $3C49; {ERRBASE_FSSQL + ERRCODE_FS_BadServerName;}
  DBIERR_FS_FileBLOBOpen = $3C50; {ERRBASE_FSSQL + ERRCODE_FS_FileBLOBOpen;}
  DBIERR_FS_FileBLOBRead = $3C51; {ERRBASE_FSSQL + ERRCODE_FS_FileBLOBRead;}
  DBIERR_FS_FileBLOBClose = $3C52; {ERRBASE_FSSQL + ERRCODE_FS_FileBLOBClose;}
  DBIERR_FS_CorruptTrans = $3C53; {ERRBASE_FSSQL + ERRCODE_FS_CorrupTrans;}

  DBIERR_FS_FilterTimeout = $3C54; {ERRBASE_FSSQL + ERRCODE_FS_FilterTimeout;}
  DBIERR_FS_ReplyTimeout = $3C55; {ERRBASE_FSSQL + ERRCODE_FS_ReplyTimeout;}
  DBIERR_FS_WaitFailed = $3C56; {ERRBASE_FSSQL + ERRCODE_FS_WaitFailed;}
  DBIERR_FS_ClientIDFail = $3C57; {ERRBASE_FSSQL + ERRCODE_FS_ClientIDFail;}
  DBIERR_FS_NoAddHandler = $3C58; {ERRBASE_FSSQL + ERRCODE_FS_NoAddHandler;}
  DBIERR_FS_NoRemHandler = $3C59; {ERRBASE_FSSQL + ERRCODE_FS_NoRemHandler;}

  DBIERR_FS_Deadlock = $3C60; {ERRBASE_FSSQL + ERRCODE_FS_Deadlock;}
  DBIERR_FS_Timeout = $3C61; {ERRBASE_FSSQL + ERRCODE_FS_Timeout;}
  DBIERR_FS_LockRejected = $3C62; {ERRBASE_FSSQL + ERRCODE_FS_LockRejected;}

  DBIERR_FS_ServerUnavail = $3C63; {ERRBASE_FSSQL + ERRCODE_FS_ServerUnavail;}
  DBIERR_FS_V1File = $3C64; {ERRBASE_FSSQL + ERRCODE_FS_V1Table;}
  DBIERR_FS_GeneralTimeout = $3C65; {ERRBASE_FSSQL + ERRCODE_FS_GeneralTimeout;}
  DBIERR_FS_NoSQLEngine = $3C66; {ERRBASE_FSSQL + ERRCODE_FS_NoSQLEngine;}
  DBIERR_FS_TableVersion = $3C67; {ERRBASE_FSSQL + ERRCODE_FS_TableVersion;} {!!.11}

  DBIERR_FS_IxHlprRegistered = $3C77; {ERRBASE_FSSQL + ERRCODE_FS_IxHlprRegistered;}
  DBIERR_FS_IxHlprNotReg = $3C78; {ERRBASE_FSSQL + ERRCODE_FS_IxHlprNotReg;}
  DBIERR_FS_IxHlprNotSupp = $3C79; {ERRBASE_FSSQL + ERRCODE_FS_IxHlprNotSupp;}
  DBIERR_FS_IncompatDict = $3C80; {ERRBASE_FSSQL + ERRCODE_FS_IncompatDict;} {!!.06}
  DBIERR_FS_SameTable = $3C81; {ERRBASE_FSSQL + ERRCODE_FS_SameTable;} {!!.06}

  DBIERR_FS_UnknownClient = $3C90; {ERRBASE_FSSQL + ERRCODE_FS_UnknownClient;}
  DBIERR_FS_UnknownSession = $3C91; {ERRBASE_FSSQL + ERRCODE_FS_UnknownSession;}
  DBIERR_FS_UnknownDB = $3C94; {ERRBASE_FSSQL + ERRCODE_FS_UnknownDB;}
  DBIERR_FS_UnknownCursor = $3C97; {ERRBASE_FSSQL + ERRCODE_FS_UnknownCursor;}
  DBIERR_FS_BLOBTooBig = $3C9A; {ERRBASE_FSSQL + BLOB Size Exceeds Max}

  DBIERR_FS_Unknown = $3CA0; {ERRBASE_FSSQL + ERRCODE_FS_Unknown;}
  DBIERR_FS_UnknownExcp = $3CA1; {ERRBASE_FSSQL + ERRCODE_FS_UnknownExcp;}
  DBIERR_FS_UnknownMsg = $3CA2; {ERRBASE_FSSQL + ERRCODE_FS_UnknownMsg;}

  DBIERR_FS_RangeNotSupported = $3CD2; {ERRBASE_FSSQL + ERRCODE_FS_RangeNotSupported;}

