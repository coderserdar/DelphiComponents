{*********************************************************}
{* Datatypes common to PhysDB, PhysDict, PhysDir, PhysDs *)
(* Direct port of the original PHYSTYPE.HPP source file  *)
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

{$I ffcrdefn.inc}

unit ffcrptyp;

{ This file contains data types common to PhysDb.hpp, PhysDict.hpp,
  PhysDir.hpp and PhysDs.hpp. }

interface

uses
  SysUtils,
  ffcrtype,
  ffcrltyp;

const
  ERR_MSG_BUFFER_LEN         = 255;
  ALIAS_NAME_BUFFER_LEN      = 255;

  { bitflag constants for opening database. }
  INCL_SYSTEM_TABLE          : LongInt = $01;
  CONVERT_DATETIME_TO_STRING : LongInt = $02;
  TRANSLATE_DOS_STRINGS      : LongInt = $04;
  TRANSLATE_DOS_MEMOS        : Longint = $08;
  NEED_PROMPT_FOR_TABLES     : LongInt = $16;
  NEED_PROMPT_FOR_BROWSER    : LongInt = $32;

  MAX_LEN_SQL_INFO           = 255;
  MAX_LEN_DIR_INFO           = 512;
  MAX_LEN_SHORT_DIR_INFO     = 128;

  MAX_READ_FILES             = 255;
  MAX_FILE_LINKS             = 255;
  MAX_FIELDS_PER_FILE_LINK   = 10;

  BUFSIZE                    = 255;

  CRYSTAL_LIKE               = 'CRYSTAL_LIKE';
  CRYSTAL_STARTWITH          = 'CRYSTAL_STARTWITH';

type
  {$Z4 These enumerations are long sized, not word sized}
  TPhysDbError = (errPhysDbNoError,
                  errPhysDbErrMsgReturned,
                  errPhysDbNotEnoughMemory,
                  errPhysDbFileDoesNotExist,
                  errPhysDbFilePermissionError,
                  errPhysDbFileIntegrityError,
                  errPhysDbUserCancelOperation,
                  errPhysDbProgrammingError,
                  errPhysDbNotImplemented,
                  errPhysDbSQLServerError,
                  errPhysDbIncorrectPassword,
                  errPhysDbOpenSessionError,
                  errPhysDbLogOnServerError,
                  errPhysDbErrorHandledByDBDLL,
                  errPhysDbStopProceeding);

  TPhysDbIndexInfoCases = (iiIndexesNeverExist, { e.g. ASCII files }
                           iiIndexesExistButNotKnown,
                           iiSomeIndexesKnown,
                           iiAllIndexesKnown);

  TPhysDbBuildIndexCases = (biCannotBuildIndex,
                            biCanBuildNonMaintainedIndex,
                            biCanBuildMaintainedIndex);

  TPhysDbIndexTypes = (itNoIndex,
                       itdBase3,
                       itdBase4,
                       itClipper,
                       itFoxBase,
                       itFoxPro);
  {$Z2 end of long sized enumerations}

{ Pointers to file handle definition.
  Note: The file handle structure is defined local to the physical
  database or dictionary module (in FFCRLTYP.PAS), since its contents
  vary per implementation.  Crystal will not manipulate this info,
  only pass a pointer to it in and out of the DLL. }

  PPhysDbFileHandle = ^TPhysDbFileHandle;
  TPhysDbFileHandleArray = array[0..32767 div SizeOf(TPhysDbFileHandle)] of TPhysDbFileHandle;
  PPhysDbFileHandleArray = ^TPhysDbFileHandleArray;

  PPhysDbServerHandle = ^TPhysDbServerHandle;

{ Info describing a data field.
  Note: The following fields of this structure are meaningful to the
  InitDataFile functions, but not to FetchDataFileInfo:
       - usedInReadRecord
       - offsetInReadRecord
       - usedInIndexRecord
       - offsetInIndexRecord
  This information can be set to zero or ignored by FetchDataFileInfo.
}

  PPhysDbFieldInfo = ^TPhysDbFieldInfo;
  TPhysDbFieldInfo = packed record
    Name          : PChar;              { field name }
    FieldType     : TFieldValueType;    { generic Brahma field type }
    NBytesInField : Word;               { width of Brahma field type }
    Picture       : PChar;              { picture format }
    Alignment     : TDBFieldAlignment;  { left or right aligned }
    Sortable      : TcrBoolean;

    NativeFieldType     : Word;     { native field type, 0 if not used }
    NativeFieldOffset   : Word;     { offset to native field in phys record }
    NBytesInNativeField : Word;     { width of native field type }
    NDecPlacesInNativeField : Word; { number decimal places in native field }

    UsedInReadRecord    : TcrBoolean; { set by caller of InitDataFile functions }
    OffsetInReadRecord  : Word;       { set by caller of InitDataFile functions }
    UsedInIndexRecord   : TcrBoolean; { set by caller of InitDataFile functions }
    OffsetInIndexRecord : Word;       { set by caller of InitDataFile functions }
  end;
  TPhysDbFieldInfoArray = array[0..32767 div SizeOf(TPhysDbFieldInfo)] of TPhysDbFieldInfo;
  PPhysDbFieldInfoArray = ^TPhysDbFieldInfoArray;

{ Info describing a data file.
  Note: The following fields of this structure are meaningful to the
  InitDataFile functions, but not to FetchDataFileInfo:
       - nBytesInReadRecord
       - nFieldsInReadRecord
       - nBytesInIndexRecord
       - nFieldsInIndexRecord
  This information can be set to zero or ignored by FetchDataFileInfo. }

type
  PPhysDbFileInfo = ^TPhysDbFileInfo;
  TPhysDbFileInfo = packed record
    FileType   : TDBFieldFileType;      { whether flat or recurring records }
    TableName  : PChar;                 { table name, nil if doesn't exist }
    NBytesInPhysRecord : Word;          { physical record length, 0 if not used }
    NFields    : Word;                  { number of fields in data file }
    FieldInfo  : PPhysDbFieldInfoArray; { array of field definitiona }

    NBytesInReadRecord   : Word;     { set by caller of InitDataFile functions }
    NFieldsInReadRecord  : Word;     { set by caller of InitDataFile functions }
    NBytesInIndexRecord  : Word;     { set by caller of InitDataFile functions }
    NFieldsInIndexRecord : Word;     { set by caller of InitDataFile functions }
  end;
  TPhysDbFileInfoArray = array[0..32767 div SizeOf(TPhysDbFileInfo)] of TPhysDbFileInfo;
  PPhysDbFileInfoArray = ^TPhysDbFileInfoArray;

{ Info describing an index. }

  PPhysDbIndexInfo = ^TPhysDbIndexInfo;
  TPhysDbIndexInfo = packed record
    ValuesUnique : TcrBoolean;        { true if indx values known to be
                                        unique (1:1 lookup), else false (1:n) }
    NFields : Word;                   { number of fields in index definition }
    FieldNumInFile : PWordArray;      { array of fields in index definition;
                                        each entry is a (0-origin) index into
                                        the PhysDbFileInfo.fieldInfo array of
                                        fields returned by FetchDataFileInfo }
    IndexExpr : PChar;                { if nFields == 0, fieldNumInFile is not
                                        used and indexExpr is used instead;
                                        it contains a text string describing
                                        the calculated index expression }
    EstimatedNBytesInExpr : Word;     { if indexExpr is used this is the
                                        estimated length of the expression }

    IndexType : Word;                 { index type info }
    DefaultIndexFileName : TcrBoolean;{ true if use default index file name,
                                        false if indexFileName define below }
    DefaultTagName : TcrBoolean;      { true if use default tag, fals eif
                                        tagname defined below }
    IndexFileName : PChar;            { defined if defaultIndexFilename = false }
    TagName       : PChar;            { defined if defaultTagName = false }
    Ascending     : TcrBoolean;
    CaseSensitive : TcrBoolean;
  end;
  TPhysDbIndexInfoArray = array[0..32767 div SizeOf(TPhysDbIndexInfo)] of TPhysDbIndexInfo;
  PPhysDbIndexInfoArray = ^TPhysDbIndexInfoArray;

{ Info describing set of indexes. }

  PPhysDbIndexesInfo = ^TPhysDbIndexesInfo;
  TPhysDbIndexesInfo = packed record
    NIndexes   : Word;                   { number of indexes for data file }
    IndexInfo  : PPhysDbIndexInfoArray;  { array of index definitions }
    IndexInUse : Word;                   { set by caller of OpendataFileAndIndexChoice }
    NIndexesInUse  : Word;               { only valid for SQL table linking }
    IndexInUseList : array[0..MAX_FILE_LINKS - 1] of Word; { a list of index for SQL linking }
  end;
  TPhysDbIndexesInfoArray = array[0..32767 div SizeOf(TPhysDbIndexesInfo)] of TPhysDbIndexesInfo;
  PPhysDbIndexesInfoArray = ^TPhysDbIndexesInfoArray;

{ Info describing a search range. }

  PPhysDbFieldRangeInfo = ^TPhysDbFieldRangeInfo;
  TPhysDbFieldRangeInfo = packed record
    MinFieldValue : Pointer;
    MinInclusive  : TcrBoolean;
    MaxFieldValue : Pointer;
    MaxInclusive  : TcrBoolean;
  end;
  TPhysDbFieldRangeInfoArray = array[0..32767 div SizeOf(TPhysDbFieldRangeInfo)] of TPhysDbFieldRangeInfo;
  PPhysDbFieldRangeInfoArray = ^TPhysDbFieldRangeInfoArray;

  PPhysDbRangeInfo = ^TPhysDbRangeInfo;
  TPhysDbRangeInfo = packed record
    FieldName      : PChar;
    BrahmaType     : TFieldValueType;
    BrahmaFieldLen : TcrInt16u;

    SelectIfWithinRange: TcrBoolean;{ if FALSE, first calculate all ranges
                                      in fieldRanges, then select those outside }
    NFieldRanges : TcrInt16u;       { if >1, these are implicitly OR'ed together }
    FieldRanges  : PPhysDbFieldRangeInfoArray;
  end;
  TPhysDbRangeInfoArray = array[0..32767 div SizeOf(TPhysDbRangeInfo)] of TPhysDbRangeInfo;
  PPhysDbRangeInfoArray = ^TPhysDbRangeInfoArray;

{ Info describing SQL search range.  Multiple field ranges can be
  specified for each table. }

  PPhysDbSQLRangeInfo = ^TPhysDbSQLRangeInfo;
  TPhysDbSQLRangeInfo = packed record
    TableName : PChar;
    RangeInfo : TPhysDbRangeInfo;
  end;

{ Info describing a link. }

  PPhysDbFileLinkInfo = ^TPhysDbFileLinkInfo;
  TPhysDbFilelinkInfo = packed record
    FromFile      : PPhysDbFileInfo;
    ToFile        : PPhysDbFileInfo;
    FromFieldList : array[0..MAX_FIELDS_PER_FILE_LINK - 1] of Word;
    ToFieldList   : array[0..MAX_FIELDS_PER_FILE_LINK - 1] of Word;
    NFields       : Word;
    LookupType    : TDBLinkJoinType;   { defined to pass the join type to DLL }
  end;
  TPhysDbFileLinkInfoArray = array[0..32767 div Sizeof(TPhysDbFileLinkInfo)] of TPhysDbFileLinkInfo;
  PPhysDbFileLinkInfoArray = ^TPhysDbFileLinkInfo;

{$IFDEF INCL_SERVER_OPTIONS}
  PPhysDbServerOption = ^TPhysDbServerOption;
  TPhysDbServerOption = packed record
    ConvertDateTimeToString    : TBoolean;
    CountNRecordsBeforeReading : TBoolean;
    NRecordsThreshold          : Word;
  end;
{$ENDIF}

  PPhysDbServerInfo = ^TPhysDbServerInfo;
  TPhysDbServerInfo = packed record
    ServerType   : array[0..MAX_LEN_SQL_INFO - 1] of char;  { SQL server type name }
    ServerName   : array[0..MAX_LEN_SQL_INFO - 1] of char;
    DatabaseName : array[0..MAX_LEN_SQL_INFO - 1] of char;
    UserID       : array[0..MAX_LEN_SQL_INFO - 1] of char;

    SqlLinIndex  : Word;      { index to sqlLibs }

    UseDictPath  : TcrBoolean;  { ver 1.10 for NetWare SQL }
    UseDataPath  : TcrBoolean;

  {$IFDEF INCL_SERVER_OPTIONS}
    Option: PPhysDbServerOption;
    Pid : HTASK;  (* ?? *)
  {$ENDIF}
  end;

  PPhysDbFileDirectoryInfo = ^TPhysDbFileDirectoryInfo;
  TPhysDbFileDirectoryInfo = packed record
    { Fixed length strings since DbMgr and DLLs can both edit these values,
      and use different memory allocation methods. }
    DirPath: array[0..MAX_LEN_SHORT_DIR_INFO - 1] of char; { path and directory file name (e.g. "C:\ACCESS\SAMPLE.MDB") }
    ConnectBuf: array[0..MAX_LEN_DIR_INFO - 1] of char;    { connection info (e.g. "ODBC;DSN=DSQUERY;UID=user") }
  end;

  PPhysDbFileDictionaryInfo = ^TPhysDbFileDictionaryInfo;
  TPhysDbFileDictionaryInfo = packed record
    { Fixed length string since DbMgr and DLLs can both edit these values,
      and use different memory allocation methods. }
    DictPath: array[0..MAX_LEN_SHORT_DIR_INFO - 1] of char; { path abd dictionary file name (e.g., "C:\BTRIEVE\FILE.DDF") }
  end;

  PPhysDbSessionInfo = ^TPhysDbSessionInfo;
  TPhysDbSessionInfo = packed record
    { Fixed length strings since DbMgr and DLLs can both edit these values,
      and use different memory allocation methods. }
    SessionUserID: array[0..MAX_LEN_SHORT_DIR_INFO - 1] of char;
    SessionPassword: array[0..MAX_LEN_SHORT_DIR_INFO - 1] of char;
    SessionHandle: Cardinal; { if <> 0 use sessionHandle, else use
                               sessionUserID and sessionPassword }
  end;

  PPhysDbLogOnInfo = ^TPhysDbLogOnInfo;
  TPhysDbLogOnInfo = packed record
    { Fixed length string since DbMgr and DLLs can both edit these values,
      and use different memory allocation methods. }
    LogOnPassword: array[0..MAX_LEN_SHORT_DIR_INFO - 1] of char;
  end;

  PPhysDbLookupOptInfo = ^TPhysDbLookupOptInfo;
  TPhysDbLookupOptInfo = packed record
    LookupValueLen : Word;
    PartialMatch   : TcrBoolean;
  end;

const
  { temporary, for debugging purposes }
  PhysDbErrors: array[TPhysDbError] of string[30] =(
    'PhysDbNoError',
    'PhysDbErrMsgReturned',
    'PhysDbNotEnoughMemory',
    'PhysDbFileDoesNotExist',
    'PhysDbFilePermissionError',
    'PhysDbFileIntegrityError',
    'PhysDbUserCancelOperation',
    'PhysDbProgrammingError',
    'PhysDbNotImplemented',
    'PhysDbSQLServerError',
    'PhysDbIncorrectPassword',
    'PhysDbOpenSessionError',
    'PhysDbLogOnServerError',
    'PhysDbErrorHandledByDBDLL',
    'PhysDBStopProceeding');

implementation

end.
