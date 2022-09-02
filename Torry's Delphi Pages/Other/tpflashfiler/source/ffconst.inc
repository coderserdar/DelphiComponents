{*********************************************************}
{* FlashFiler: Stringtable constants                     *}
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

{Note:

  The actual strings are found in the following resource scripts:

         FFSRCNST.STR - server strings
           Range: $00 - $C4 (0 - 196)

         FFLLCNST.STR - General strings that can be used both client-side &
           server-side.
           Range: $100 - $1C3 (256 - 451)

         FFCLCNST.STR - Client strings.
           Range: $3F0 - $452 (1,008 - 1,106)

         FFDBCNST.STR - BDE-like strings & FF-specific client-side strings.
           BDE Range: $2101 - $351A (8,449 - 13,549)
           FF  Range: $3C00 - $3CD2 (15,360 - 15,521)

         FFDSCNST.STR - TDataSet descendant error strings.
           Range: $D500 - $D53A (54,528 - 54,586)

}

const

  { Constants for string resource range boundaries }

  ffSRCNSTLow  = $00;
  ffSRCNSTHigh = $FF;

  ffLLCNSTLow  = $100;
  ffLLCNSTHigh = $1FF;

  ffCLCNSTLow  = $3F0;
  ffCLCNSTHigh = $4FF;

  ffDBCNSTLow  = $2101;
  ffDBCNSTHigh = $3D00;

  ffDSCNSTLow  = $D500;
  ffDSCNSTHigh = $D5FF;

{--- FFSRCNST ---}

  { Basic file I/O }
  fferrBadStruct       = $00;
  fferrOpenFailed      = $01;
  fferrOpenNoMem       = $02;
  fferrCloseFailed     = $03;
  fferrReadFailed      = $04;
  fferrReadExact       = $05;
  fferrWriteFailed     = $06;
  fferrWriteExact      = $07;
  fferrSeekFailed      = $08;
  fferrFlushFailed     = $09;
  fferrSetEOFFailed    = $0A;

  { Low-level FF Server errors }
  fferrNotAnFFFile     = $20;
  fferrBadBlockNr      = $21;
  fferrEncrypted       = $22;
  fferrRecDeleted      = $23;
  fferrBadRefNr        = $24;
  fferrBadDataBlock    = $25;

  fferrBlobDeleted     = $30;
  fferrBadBlobNr       = $31;
  fferrBadBlobBlock    = $32;
  fferrBadBlobSeg      = $33;
  fferrLenMismatch     = $34;
  fferrOfsNotInBlob    = $35;
  fferrFileBlobWrite   = $36;

  fferrBadStreamBlock  = $40;
  fferrBadStreamOrigin = $41;
  fferrStreamSeekError = $42;

  fferrBadInxBlock     = $50;
  fferrBadIndex        = $51;
  fferrMaxIndexes      = $52;
  fferrBadMergeCall    = $53;
  fferrKeyNotFound     = $54;
  fferrKeyPresent      = $55;
  fferrNoKeys          = $56;
  fferrNoSeqAccess     = $57;
  fferrBadApproxPos    = $58;

  fferrBadServerName   = $70;
  fferrFFV1File        = $71;
  fferrIncompatDict    = $72;
  fferrBLOBTooBig      = $73;

  { Errors to indicate unknown handles, IDs, etc }
  fferrUnknownClient   = $90;
  fferrUnknownSession  = $91;
  fferrUnknownAlias    = $92;
  fferrUnknownPath     = $93;
  fferrUnknownDB       = $94;
  fferrUnknownTable    = $95;
  fferrUnknownIndex    = $96;
  fferrUnknownCursor   = $97;
  fferrUnknownTrans    = $98;
  fferrUnknownMsg      = $99;  { Unknown message type received from client }

  { Misc. server errors as a result of client messages }
  fferrDBExclusive       = $A0;
  fferrDBReadOnly        = $A1;
  fferrTableExclusive    = $A2;
  fferrCursorReadOnly    = $A3;
  fferrWriteLocked       = $A4;
  fferrReadLocked        = $A5;
  fferrCannotUnlock      = $A6;
  fferrTableLocked       = $A7;
  fferrRecLocked         = $A8;
  fferrNoCurrentRec      = $A9;
  fferrDynamicLink       = $AA;
  fferrResolveTableLinks = $AB;
  fferrTableMismatch     = $AC;
  fferrNoNextRecord      = $AD;
  fferrNoPriorRecord     = $AE;
  fferrTableExists       = $AF;
  fferrDBInTrans         = $B0;
  fferrAliasExists       = $B1;
  fferrCannotCompare     = $B2;
  fferrBadFieldXform     = $B3;
  fferrNoTransaction     = $B4;
  fferrBadBookmark       = $B6;
  fferrTransactionFailed = $B7;
  fferrTableFull         = $B8;
  fferrInvalidSqlStmtHandle = $B9;
  fferrDeadlock          = $BA;
  fferrLockTimeout       = $BB;
  fferrLockRejected      = $BC;
  fferrTableLockTimeout  = $BD;
  fferrGeneralTimeout    = $BE;
  fferrNoSQLEngine       = $BF;
  fferrIndexNotSupported = $C0;
  fferrInvalidTableName  = $C1;
  fferrRangeNotSupported = $C2;
  fferrTableOpen         = $C3;
  fferrSameTable         = $C4;
  fferrSortFail          = $C5;
  fferrBadDistinctField  = $C6;
  fferrDiskFull          = $C7;                                        {!!.11}
  fferrTableVersion      = $C8;                                        {!!.11}

{--- FFLLCNST ---}

  {temporary storage errors}
  fferrTmpStoreCreateFail = $100;
  fferrTmpStoreFull       = $101;
  fferrMapFileCreateFail  = $102;
  fferrMapFileHandleFail  = $103;
  fferrMapFileViewFail    = $104;

  fferrCopyFile           = $110;
  fferrDeleteFile         = $111;
  fferrRenameFile         = $112;

  {low level client errors}
  fferrReplyTimeout       = $120;
  fferrWaitFailed         = $121;
  fferrInvalidProtocol    = $122;
  fferrProtStartupFail    = $123;
  fferrConnectionLost     = $124;
  fferrTransportFail      = $125;
  fferrPortalTimeout      = $126;

  {dictionary errors}
  fferrOutOfBounds        = $140;
  fferrDictPresent        = $141;
  fferrNotADict           = $142;
  fferrNoFields           = $143;
  fferrBadFieldRef        = $144;
  fferrBadFieldType       = $145;
  fferrRecTooLong         = $146;
  fferrDiffBlockSize      = $147;
  fferrDictReadOnly       = $148;
  fferrDictMissing        = $149;
  fferrBLOBFileDefd       = $14A;
  fferrBaseFile           = $14B;
  fferrBadFileNumber      = $14C;
  fferrBadBaseName        = $14D;
  fferrBadExtension       = $14E;
  fferrDupExtension       = $14F;
  fferrDataFileDefd       = $150;
  fferrNoFieldsInKey      = $151;
  fferrBadParameter       = $152;
  fferrBadBlockSize       = $153;
  fferrKeyTooLong         = $154;
  fferrDupFieldName       = $155;
  fferrDupIndexName       = $156;
  fferrIxHlprRegistered   = $157;
  fferrIxHlprNotReg       = $158;
  fferrIxHlprNotSupp      = $159;
  fferrFileInUse          = $160;
  fferrFieldInUse         = $161;

  {General comms errors}
  fferrCommsNoWinRes      = $170;
  fferrCommsCannotCall    = $171;
  fferrCommsCantListen    = $172;

  {Winsock errors}
  fferrWinsock            = $180;
  fferrWSNoWinsock        = $181;
  fferrWSNoSocket         = $182;
  fferrWSNoLocalAddr      = $183;

  {dialog errors}
  fferrInvalidServerName     = $1A0;
  fferrInvalidNameorPath     = $1A1;
  fferrDuplicateAliasName    = $1A2;
  fferrEmptyValuesNotAllowed = $1A3;

  {miscellaneous constants}
  ffscSeqAccessIndexName     = $1B0;
  ffscMainTableFileDesc      = $1B1;
  ffscRegistryMainKey        = $1B2;

  ffscRebuildPlaceHolder     = $1C0;
  ffscRestructPlaceHolder    = $1C1;
  ffscImportPlaceHolder      = $1C2;
  ffscExportPlaceHolder      = $1C3;

{--- FFCLCNST ---}

  {client miscellaneous constants}
  ffccInvalidParameter     = $3F0;
  ffccREG_PRODUCT          = $3F1;
  ffccDupItemInColl        = $3F2;

  { Import constants }
  ffccImport_NoSchemaFile        = $400;
  ffccImport_RECLENGTHRequired   = $401;
  ffccImport_NoMatchingFields    = $402;
  ffccImport_FILETYPEMissing     = $403;
  ffccImport_FILETYPEInvalid     = $404;
  ffccImport_BadFieldName        = $405;
  ffccImport_BadFieldType        = $406;
  ffccImport_BadFloatSize        = $407;
  ffccImport_BadIntegerSize      = $408;
  ffccImport_BadUIntegerSize     = $409;
  ffccImport_NoFields            = $40A;
  ffccImport_BadOffset           = $40B;
  ffccImport_BadSize             = $40C;
  ffccImport_BadDecPl            = $40D;
  ffccImport_BadDateMask         = $40E;
  ffccImport_BadAutoIncSize      = $40F;
  ffccImport_BadSchemaHeader     = $410;

  ffccDesign_SLinkMasterSource   = $450;
  ffccDesign_SLinkMaster         = $451;
  ffccDesign_SLinkDesigner       = $452;

{--- FFDBCNST ---}

  {pseudo-BDE errors for server exceptions}
  ERRCAT_FLASHFILER = $3C;
  ERRBASE_FLASHFILER = $3C00;

  ERRCODE_FF_BadStruct       = 0;
  ERRCODE_FF_OpenFailed      = 1;
  ERRCODE_FF_OpenNoMem       = 2;
  ERRCODE_FF_CloseFailed     = 3;
  { Use me please            = 4;
    Use me please            = 5;
  }
  ERRCODE_FF_ReadFailed      = 6;
  ERRCODE_FF_ReadExact       = 7;
  ERRCODE_FF_WriteFailed     = 8;
  ERRCODE_FF_WriteExact      = 9;
  ERRCODE_FF_SeekFailed      = $0A;
  ERRCODE_FF_FlushFailed     = $0B;
  ERRCODE_FF_SetEOFFailed    = $0C;
  ERRCODE_FF_TempStorageFull = $13;
  ERRCODE_FF_CopyFile        = $20;
  ERRCODE_FF_DeleteFile      = $21;
  ERRCODE_FF_RenameFile      = $22;
  ERRCODE_FF_BadBlockNr      = $31;
  ERRCODE_FF_RecDeleted      = $33;
  ERRCODE_FF_BadRefNr        = $34;
  ERRCODE_FF_BadDataBlock    = $35;
  ERRCODE_FF_BadStreamBlock  = $3D;
  ERRCODE_FF_BadStreamOrigin = $3E;
  ERRCODE_FF_StreamSeekError = $3F;
  ERRCODE_FF_BadInxBlock     = $40;
  ERRCODE_FF_BadIndex        = $41;
  ERRCODE_FF_MaxIndexes      = $42;
  ERRCODE_FF_BadMergeCall    = $43;
  ERRCODE_FF_KeyNotFound     = $44;
  ERRCODE_FF_KeyPresent      = $45;
  ERRCODE_FF_NoKeys          = $46;
  ERRCODE_FF_NoSeqAccess     = $47;
  ERRCODE_FF_BadApproxPos    = $48;
  ERRCODE_FF_BadServerName   = $49;
  ERRCODE_FF_FileBLOBOpen    = $50;
  ERRCODE_FF_FileBLOBRead    = $51;
  ERRCODE_FF_FileBLOBClose   = $52;
  ERRCODE_FF_CorruptTrans    = $53;
  ERRCODE_FF_FilterTimeout   = $54;
  ERRCODE_FF_ReplyTimeout    = $55;
  ERRCODE_FF_WaitFailed      = $56;
  ERRCODE_FF_ClientIDFail    = $57;
  ERRCODE_FF_NoAddHandler    = $58;
  ERRCODE_FF_NoRemHandler    = $59;
  ERRCODE_FF_Deadlock        = $60;
  ERRCODE_FF_Timeout         = $61;
  ERRCODE_FF_LockRejected    = $62;
  ERRCODE_FF_ServerUnavail   = $63;
  ERRCODE_FF_V1File          = $64;
  ERRCODE_FF_GeneralTimeout  = $65;
  ERRCODE_FF_NoSQLEngine     = $66;
  ERRCODE_FF_TableVersion    = $67;                                    {!!.11}
  ERRCODE_FF_IxHlprRegistered= $77;
  ERRCODE_FF_IxHlprNotReg    = $78;
  ERRCODE_FF_IxHlprNotSupp   = $79;
  ERRCODE_FF_IncompatDict    = $80;                                    {!!.06}
  ERRCODE_FF_SameTable       = $81;                                    {!!.06}
  ERRCODE_FF_UnknownClient   = $90;
  ERRCODE_FF_UnknownSession  = $91;
  ERRCODE_FF_UnknownDB       = $94;
  ERRCODE_FF_UnknownCursor   = $97;
  ERRCODE_FF_Unknown         = $A0;
  ERRCODE_FF_UnknownExcp     = $A1;
  ERRCODE_FF_UnknownMsg      = $A2;
  ERRCODE_FF_RangeNotSupported = $D2;

  DBIERR_FF_BadStruct       = $3C00; {ERRBASE_FLASHFILER + ERRCODE_FF_BadStruct;}
  DBIERR_FF_OpenFailed      = $3C01; {ERRBASE_FLASHFILER + ERRCODE_FF_OpenFailed;}
  DBIERR_FF_OpenNoMem       = $3C02; {ERRBASE_FLASHFILER + ERRCODE_FF_OpenNoMem;}
  DBIERR_FF_CloseFailed     = $3C03; {ERRBASE_FLASHFILER + ERRCODE_FF_CloseFailed;}
  DBIERR_FF_ReadFailed      = $3C06; {ERRBASE_FLASHFILER + ERRCODE_FF_ReadFailed;}
  DBIERR_FF_ReadExact       = $3C07; {ERRBASE_FLASHFILER + ERRCODE_FF_ReadExact;}
  DBIERR_FF_WriteFailed     = $3C08; {ERRBASE_FLASHFILER + ERRCODE_FF_WriteFailed;}
  DBIERR_FF_WriteExact      = $3C09; {ERRBASE_FLASHFILER + ERRCODE_FF_WriteExact;}
  DBIERR_FF_SeekFailed      = $3C0A; {ERRBASE_FLASHFILER + ERRCODE_FF_SeekFailed;}
  DBIERR_FF_FlushFailed     = $3C0B; {ERRBASE_FLASHFILER + ERRCODE_FF_FlushFailed;}
  DBIERR_FF_SetEOFFailed    = $3C0C; {ERRBASE_FLASHFILER + ERRCODE_FF_SetEOFFailed;}
  DBIERR_FF_TempStorageFull = $3C13; {ERRBASE_FLASHFILER + ERRCODE_FF_TempStorageFull;}
  DBIERR_FF_CopyFile        = $3C20; {ERRBASE_FLASHFILER + ERRCODE_FF_CopyFile;}
  DBIERR_FF_DeleteFile      = $3C21; {ERRBASE_FLASHFILER + ERRCODE_FF_DeleteFile;}
  DBIERR_FF_RenameFile      = $3C22; {ERRBASE_FLASHFILER + ERRCODE_FF_RenameFile;}
  DBIERR_FF_BadBlockNr      = $3C31; {ERRBASE_FLASHFILER + ERRCODE_FF_BadBlockNr;}
  DBIERR_FF_RecDeleted      = $3C33; {ERRBASE_FLASHFILER + ERRCODE_FF_RecDeleted;}
  DBIERR_FF_BadRefNr        = $3C34; {ERRBASE_FLASHFILER + ERRCODE_FF_BadRefNr;}
  DBIERR_FF_BadDataBlock    = $3C35; {ERRBASE_FLASHFILER + ERRCODE_FF_BadDataBlock;}
  DBIERR_FF_BadStreamBlock  = $3C3D; {ERRBASE_FLASHFILER + ERRCODE_FF_BadStreamBlock;}
  DBIERR_FF_BadStreamOrigin = $3C3E; {ERRBASE_FLASHFILER + ERRCODE_FF_BadStreamOrigin;}
  DBIERR_FF_StreamSeekError = $3C3F; {ERRBASE_FLASHFILER + ERRCODE_FF_StreamSeekError;}
  DBIERR_FF_BadInxBlock     = $3C40; {ERRBASE_FLASHFILER + ERRCODE_FF_BadInxBlock;}
  DBIERR_FF_BadIndex        = $3C41; {ERRBASE_FLASHFILER + ERRCODE_FF_BadIndex;}
  DBIERR_FF_MaxIndexes      = $3C42; {ERRBASE_FLASHFILER + ERRCODE_FF_MaxIndexes;}
  DBIERR_FF_BadMergeCall    = $3C43; {ERRBASE_FLASHFILER + ERRCODE_FF_BadMergeCall;}
  DBIERR_FF_KeyNotFound     = $3C44; {ERRBASE_FLASHFILER + ERRCODE_FF_KeyNotFound;}
  DBIERR_FF_KeyPresent      = $3C45; {ERRBASE_FLASHFILER + ERRCODE_FF_KeyPresent;}
  DBIERR_FF_NoKeys          = $3C46; {ERRBASE_FLASHFILER + ERRCODE_FF_NoKeys;}
  DBIERR_FF_NoSeqAccess     = $3C47; {ERRBASE_FLASHFILER + ERRCODE_FF_NoSeqAccess;}
  DBIERR_FF_BadApproxPos    = $3C48; {ERRBASE_FLASHFILER + ERRCODE_FF_BadApproxPos;}
  DBIERR_FF_BadServerName   = $3C49; {ERRBASE_FLASHFILER + ERRCODE_FF_BadServerName;}
  DBIERR_FF_FileBLOBOpen    = $3C50; {ERRBASE_FLASHFILER + ERRCODE_FF_FileBLOBOpen;}
  DBIERR_FF_FileBLOBRead    = $3C51; {ERRBASE_FLASHFILER + ERRCODE_FF_FileBLOBRead;}
  DBIERR_FF_FileBLOBClose   = $3C52; {ERRBASE_FLASHFILER + ERRCODE_FF_FileBLOBClose;}
  DBIERR_FF_CorruptTrans    = $3C53; {ERRBASE_FLASHFILER + ERRCODE_FF_CorrupTrans;}

  DBIERR_FF_FilterTimeout   = $3C54; {ERRBASE_FLASHFILER + ERRCODE_FF_FilterTimeout;}
  DBIERR_FF_ReplyTimeout    = $3C55; {ERRBASE_FLASHFILER + ERRCODE_FF_ReplyTimeout;}
  DBIERR_FF_WaitFailed      = $3C56; {ERRBASE_FLASHFILER + ERRCODE_FF_WaitFailed;}
  DBIERR_FF_ClientIDFail    = $3C57; {ERRBASE_FLASHFILER + ERRCODE_FF_ClientIDFail;}
  DBIERR_FF_NoAddHandler    = $3C58; {ERRBASE_FLASHFILER + ERRCODE_FF_NoAddHandler;}
  DBIERR_FF_NoRemHandler    = $3C59; {ERRBASE_FLASHFILER + ERRCODE_FF_NoRemHandler;}

  DBIERR_FF_Deadlock        = $3C60; {ERRBASE_FLASHFILER + ERRCODE_FF_Deadlock;}
  DBIERR_FF_Timeout         = $3C61; {ERRBASE_FLASHFILER + ERRCODE_FF_Timeout;}
  DBIERR_FF_LockRejected    = $3C62; {ERRBASE_FLASHFILER + ERRCODE_FF_LockRejected;}

  DBIERR_FF_ServerUnavail   = $3C63; {ERRBASE_FLASHFILER + ERRCODE_FF_ServerUnavail;}
  DBIERR_FF_V1File          = $3C64; {ERRBASE_FLASHFILER + ERRCODE_FF_V1Table;}
  DBIERR_FF_GeneralTimeout  = $3C65; {ERRBASE_FLASHFILER + ERRCODE_FF_GeneralTimeout;}
  DBIERR_FF_NoSQLEngine     = $3C66; {ERRBASE_FLASHFILER + ERRCODE_FF_NoSQLEngine;}
  DBIERR_FF_TableVersion    = $3C67; {ERRBASE_FLASHFILER + ERRCODE_FF_TableVersion;} {!!.11}

  DBIERR_FF_IxHlprRegistered= $3C77; {ERRBASE_FLASHFILER + ERRCODE_FF_IxHlprRegistered;}
  DBIERR_FF_IxHlprNotReg    = $3C78; {ERRBASE_FLASHFILER + ERRCODE_FF_IxHlprNotReg;}
  DBIERR_FF_IxHlprNotSupp   = $3C79; {ERRBASE_FLASHFILER + ERRCODE_FF_IxHlprNotSupp;}
  DBIERR_FF_IncompatDict    = $3C80; {ERRBASE_FLASHFILER + ERRCODE_FF_IncompatDict;} {!!.06}
  DBIERR_FF_SameTable       = $3C81; {ERRBASE_FLASHFILER + ERRCODE_FF_SameTable;}    {!!.06}

  DBIERR_FF_UnknownClient   = $3C90; {ERRBASE_FLASHFILER + ERRCODE_FF_UnknownClient;}
  DBIERR_FF_UnknownSession  = $3C91; {ERRBASE_FLASHFILER + ERRCODE_FF_UnknownSession;}
  DBIERR_FF_UnknownDB       = $3C94; {ERRBASE_FLASHFILER + ERRCODE_FF_UnknownDB;}
  DBIERR_FF_UnknownCursor   = $3C97; {ERRBASE_FLASHFILER + ERRCODE_FF_UnknownCursor;}
  DBIERR_FF_BLOBTooBig      = $3C9A; {ERRBASE_FLASHFILER + BLOB Size Exceeds Max}

  DBIERR_FF_Unknown         = $3CA0; {ERRBASE_FLASHFILER + ERRCODE_FF_Unknown;}
  DBIERR_FF_UnknownExcp     = $3CA1; {ERRBASE_FLASHFILER + ERRCODE_FF_UnknownExcp;}
  DBIERR_FF_UnknownMsg      = $3CA2; {ERRBASE_FLASHFILER + ERRCODE_FF_UnknownMsg;}

  DBIERR_FF_RangeNotSupported = $3CD2; {ERRBASE_FLASHFILER + ERRCODE_FF_RangeNotSupported;}

