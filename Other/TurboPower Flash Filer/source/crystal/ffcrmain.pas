{*********************************************************}
(* Implementation of all driver functions                *)
(* Direct port of the original PHYSDB.CPP source file    *)
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

unit ffcrmain;

{ The import unit can be built by copying the interface section of this
  unit and globally replacing "stdcall;" with "external 'PDBFF.DLL';" }

{
 This file contains the interface definition used by all Brahma physical
 database DLL's.

 Brahma supports three basic types of DLLs, physical database (PhysDb.hpp),
 physical dictionary (PhysDict.hpp), and physical directory (PhysDir.hpp).

 These DLLs provide some similar functions, but differ as follows:
    PhysDb:   - Supports single physical database tables (assumed to be
                stored in single files), and provides both retrieving of
                database info and reading of database records.
              - May be able to retrieve structural and index info of a
                single table, but has no support for links between multiple
                tables.
              - Performs reading of database records (sequentially or using
                an index).
    PhysDict: - Supports retrieving of database info from multiple database
                tables, but has no support for reading of database records.
              - May be able to retrieve structural, index and link
                information of multiple tables.
              - Is knowledgeable of PhysDb database table types, and informs
                the Database Manager of these types for reading of database
                records.
    PhysDir:  - Supports a directory of multiple database files, but does not
                perform retrieving of database info or reading of database
                records itself.
              - Is knowledgeable of PhysDb and PhysDict DLLs, and informs
                the Database Manager which DLL to use for servicing each
                entry in its directory.

 Since each physical database, dictionary and directory is implemented as a
 DLL, other database types can be defined and linked dynamically to the
 Database Manager in the future.

 Note: As mentioned above, physical database DLLs are responsible for
 individual database tables only, and handling the links between multiple-
 table databases is the responsibility of the Database Manager.  The
 physical database DLL must support multiple open database tables at a
 time however.

 Friendly advice:  No global static data should be used in the
 implementation of a physical database DLL.  This makes it easier to
 support multiple open files per report, and multiple open sets of files
 for multiple reports, by letting the Database Manager save state
 information instead.

 The general rule is that whenever any state information is required
 by the DLL it is dynamically allocated and a reference to it passed back
 to the Database Manager.  The Database Manager is then responsible for
 storing this reference, passing it to the DLL whenever it is needed, and
 calling the DLL to free the associated information.

 Error Messages: When any DLL function cannot complete successfully, it
 has a choice of returning an error code (PhysDbError type) or an error
 message (code PhysDbErrMsgReturned, and returning a message in ErrMsg
 parameter).  The recommended behavior of the DLL is:
      - Return an error string if no error code matches the situation
        well, or if very specific information is available that would help
        the user (e.g. "Please execute DOS share program", "Table is
        corrupted at record 15", etc.).  If an error string is returned it
        will be displayed by the Database Manager.
      - Return an error code in all other cases.  The Database Manager
        will display a standard error message of its own in these cases,
        which will be consistent for all physical database types
        (e.g. "Not enough memory", "File could not be found", etc.).
}

{$DEFINE IDAPI_INTERNAL_LIMITS} 

interface

uses
  ffllbase,
  fflllog,                                                             {!!.12}
  ffcrptyp,
  ffcrtype,
  ffclreng,
  ffstdate,                                                            {!!.02}
  SysUtils;


{ ---------------------  Database Abilities  ------------------------ }

{ Return physical database version number.  }

function PhysDbVersionNumber(
                         var MajorVersionNumber : Word;
                         var MinorVersionNumber : Word;
                             ErrMsg : PAnsiChar) : TPhysDbError; stdcall;

{ Return whether this physical database can recognize data files of
 its own type.  For example, a Paradox physical database DLL can recognize
 a data file passed to it by its file name extension and internal header
 information, whereas an ASCII physical database DLL cannot uniquely
 identify a data file as being of its type.

 If this returns true, the Database Manager may pass arbitrary file names to
 the function OpenDataFileIfRecognizedVer12, and assumes it only opens data
 files belonging to it.  If this is false the function OpenDataFileIfRecognizedVer12
 is only called when the user has confirmed that a file is of this data
 type (via a dialog of FetchDatabaseName names) and can assume that the
 type is correct. }

function CanRecognizeDataFile(var CanRecognize : TcrBoolean;
                                  ErrMsg       : PAnsiChar) : TPhysDbError;  stdcall;

{ Return whether this physical database can retrieve info describing
  an open data file (whether flat or recurring records, number of
  fields in the file, the width & type of each data field, etc.).
  This can be done by the physical database either by "inhaling" the
  data file information (without user interaction), or by displaying
  Windows dialogs to prompt the user for this information.

  If this returns true, the Database Manager calls FetchDataFileInfo
  to retrieve this info, if false the Database Manager uses default Windows
  dialogs of its own to prompt the user to provide this information. }

function CanFetchDataFileInfo(var CanFetchFileInfo : TcrBoolean;
                                  ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

{ Return whether this physical database can fetch index information
  for data files of its type.  There are four possible cases:
      1. indexesNeverExist: e.g. for ASCII files.
      2. indexesExistButNotKnown: e.g. if not implemented yet.
      3. someIndexesKnown: e.g. for dBase, default indexes known, but
            others may exist.
      4. allIndexesKnown: e.g. for Paradox, all indexes known by system.

  In cases 3 and 4 the function FetchDataFileIndexInfo is called to
  retrieve information on all known indexes.  In cases 2 and 3 the
  Database Manager uses default Windows dialogs to allow the user to
  select file names containing indexes. }

function CanFetchDataFileIndexInfo(
                               var CanFetchIndexInfo : TPhysDbIndexInfoCases;
                                   ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

{ Return whether this physical database can build indexes on data files
  if required.  There are three possible cases:
      1. cannotBuildIndex
      2. canBuildNonMaintainedIndex
      3. canBuildMaintainedIndex }

function CanBuildIndex(var CanBuildIndex : TPhysDbBuildIndexCases;
                           ErrMsg        : PAnsiChar) : TPhysDbError;  stdcall;

{ Return whether this physical database can (efficiently) retrieve the
  number of records in an open data file.  This information is required
  by the Database Manager to estimate the % completion of reading of a
  data file.

  If this returns true, the Database Manager calls NRecurringRecordsToRead
  to retrieve the record count, if false it does not.

  Note: It is not recommended to read the entire data file to determine
  the number of records, since performance will be seriously slowed.
  Therefore if the physical database system does not easily provide this
  info, this function should return false indicating that the ability is
  not provided. }

function CanFetchNRecurringRecords(var CanFetchNrecords : TcrBoolean;
                                       ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

{ This function is to tell whether this DLL has SQL functionality,
  three parameters are passed back, isSQLTypeDLL, canBuildAndExecSQLQuery,
  and canExecSQLQuery. }

function SQLCompatible(
                   var IsSQLTypeDLL : TcrBoolean;
                   var CanBuildAndExecSQLQuery : TcrBoolean;
                   var CanExecSQLQuery         : TcrBoolean;
                       ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

{ Return if physical database supports reading of main file using an index.
  This is a speed-up option, since Brahma will not need to sort the report. }

function CanReadSortedOrder(var CanReadSorted : TcrBoolean;
                                ErrMsg        : PAnsiChar) : TPhysDbError;  stdcall;

{ Return if physical database supports selecting records using a range.
  This is a speed-up option, since Brahma will only be given records
  matching the record selection criteria. }

function CanReadRangeOfValues(var CanReadRange : TcrBoolean;
                                  ErrMsg       : PAnsiChar) : TPhysDbError;  stdcall;

{ Each physical database type may or may not support multi-user access.
  If a database does support multi-user access, it may allow a choice of
  either file locking or record locking, or it may always use one method.
  This function returns whether record locking is available for this
  physical database type. }

function CanUseRecordLocking(var RecordLockingPossible  : TcrBoolean;
                             var RecordLockingPreferred : TcrBoolean;
                                 ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

{ This function returns whether file locking is available for this
  physical database type. }

function CanUseFileLocking(var FileLockingPossible  : TcrBoolean;
                           var FileLockingPreferred : TcrBoolean;
                               ErrMsg: PAnsiChar) : TPhysDbError;  stdcall;

{ ----------------  Initialization and Termination  ----------------- }

{ Any database system initialization is performed at this point.
  Note: No global static structures should be allocated, as discussed in
  the program header above. }

function InitPhysicalDatabase(ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

{ Termination of the database system is performed at this point. }

function TermPhysicalDatabase(ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

{ OpenSession and TermSession are called to initialize and terminate on
  a per task basis.  The Database Manager determines when a new task
  attempts to use a DLL, and calls OpenSession at that time. }

function OpenSession(ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

function TermSession(ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;


{ -------------------------  Database Name -------------------------- }

{ Return the text name of this physical database format.  This is used
  in Database Manager dialogs to describe the database type of a data
  file, and to store with a database dictionary to describe which physical
  database DLL to use for a file. }

function FetchDatabaseName(var Name   : PAnsiChar;
                               ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

{ Free the text name of this physical database format. }

function FreeDatabaseName(var Name   : PAnsiChar;
                              ErrMsg : PAnsiChar) : TPhysDbError;  stdcall;

{ ----------------------  Log On and Log Off  ----------------------- }

{ These functions allow the CRPE user to pass log on and log off
  information to a PhysDB DLL.

  Note: These functions are only required if the database supports
  password-protected database files (e.g. Paradox).  Otherwise
  they do not need to be implemented. }

function LogOnServer(ServerInfo   : PPhysDbServerInfo;
                 var ServerHandle : PPhysDbServerHandle;
                     Password     : PAnsiChar;
                     ErrMsg       : PAnsiChar) : TPhysDbError;  stdcall;

function LogOffServer(var ServerHandle : PPhysDbServerHandle;
                          ErrMsg       : PAnsiChar) : TPhysDbError;  stdcall;

{ ------------------- Parse and Rebuild SQL Info -------------------- }

{ These functions are helpers to parse SQL connect info passed
  down from a PhysDir type DLL.

  Note: These functions are only useful for MS Access tables.  These
  functions do not need to be implemented in any other case.  In
  general, for SQL databases the PhysDs.hpp (PDS*.DLL) interface
  should be used.

  SST: These routines must be exported even if they are not used or Crystal
  will not load the driver DLL. }

function ParseLogOnInfo(ConnectBuf : PAnsiChar;
                        BufSize    : Word;
                        ServerInfo : PPhysDbServerInfo;
                        ErrMsg     : PAnsiChar) : TPhysDbError;  stdcall;

function RebuildConnectBuf(ServerInfo : PPhysDbServerInfo;
                           ConnectBuf : PAnsiChar;
                           BufSize    : Word;
                           ErrMsg     : PAnsiChar) : TPhysDbError;  stdcall;

{ --------------------  Open and Close Files  ----------------------- }

{ This function is passed a file name, including its path and extension,
  and determines whether it is a data file of its physical database type,
  and if so opens the data file and returns a file handle.

  This function is called the first time a data file is attempted to be
  opened, and before fetching the file info (from FetchDataFileInfo) and
  index info (from FetchDataFileIndexInfo) structures that describe the
  file.  This function may also be called to open a data file for
  sequential reading (without an index) using ReadNextRecurringRecord.

  The Database Manager may pass arbitrary file names to this function,
  and assumes it only opens data files belonging to it.  If it is false
  this function is only called when the user has confirmed that a file
  is of this data type (via a dialog of FetchDatabaseName names) and this
  function opens the file as if the database type is correct.

  The new parameter logOnInfo can contain a password to use in opening
  password-protected files.

  Note: The parameter sessionInfo is only of use for MS Access DLLs
  that track user session info.  The parameter dirInfo is also only
  currently useful for MS Access DLLs.

  The parameter silentMode is used to tell DLL whether to pop up any
  dialog or message itself or just return an error code.

  The parameter aliasName allows the DLL to pass back its own alias
  name to be used for the file, it can ignore this parameter if it wants
  to use the default alias.

  The parameter calledFromDirDLL indicates whether the user has
  chosen a directory or database type file.  If the user chose a
  directory type file, the directory file says to call this database
  DLL with an internal file name. }

function OpenDataFileIfRecognizedVer113(
           FileName         : PAnsiChar;
           OpenDefaultIndex : TcrBoolean;
       var Recognized       : TcrBoolean;
       var FileHandle       : PPhysDbFileHandle;
           CalledFromDirDLL : TcrBoolean;
       var AliasName        : PAnsiChar;
           SilentMode       : TcrBoolean;
           DirInfo          : PPhysDbFileDirectoryInfo;
           DictInfo         : PPhysDbFileDictionaryInfo;
           SessionInfo      : PPhysDbSessionInfo;
           LogOnInfo        : PPhysDbLogOnInfo;
           ErrMsg           : PAnsiChar) : TPhysDbError;  stdcall;

{ This function is passed both a data file name and index file name,
  including paths and extensions, and determines whether they are of
  its physical database type.  If so it opens the data file using the
  specified index, and returns a file handle.

  This function is called when the user has selected an index file
  to attempt to open, or to open a data file for reading, using either
  ReadNextRecurringRecord (in the order of the chosen index file) or
  LookupMatchingRecurringRecord to search directly for a record (using the
  chosen index file).

  This function will only be called if CanFetchDataFileIndexInfo has
  returned indexesExistButNotKnown or someIndexesKnown.

  The new parameter logOnInfo can contain a password to use in opening
  password-protected files.

  Note: The parameter sessionInfo is only of use for MS Access DLLs
  that track user session info.  The parameter dirInfo is also only
  currently useful for MS Access DLLs.

  The parameter silentMode is used to tell DLL whether to pop up any
  dialog or message itself or just return an error code.

  The parameter aliasName allows the DLL to pass back its own alias
  name to be used for the file, it can ignore this parameter if it wants
  to use the default alias. }

function OpenDataAndIndexFileIfRecogV113(
           FileName    : PAnsiChar;
           IndexName   : PAnsiChar;
       var Recognized  : TcrBoolean;
       var FileHandle  : PPhysDbFileHandle;
       var AliasName   : PAnsiChar;
           SilentMode  : TcrBoolean;
           DirInfo     : PPhysDbFileDirectoryInfo;
           DictInfo    : PPhysDbFileDictionaryInfo;
           SessionInfo : PPhysDbSessionInfo;
           LogOnInfo   : PPhysDbLogOnInfo;
           ErrMsg      : PAnsiChar) : TPhysDbError;  stdcall;

{ This function is passed a file name, including its path and extension,
  and the file info (from FetchDataFileInfo) and index info (from
  FetchDataFileIndexInfo) structures, with usedInRead field set to
  indicate the index file chosen.  This function opens the data file
  using the chosen index and returns a file handle.

  This function is called to open a data file for reading, using either
  ReadNextRecurringRecord (in the order of the chosen index file) or
  LookupMatchingRecurringRecord to search directly for a record (using the
  chosen index file).

  This function can assume that the data file is of its physical
  database type, since it was opened and recognized previously in order
  to fetch the file info and index info passed as parameters.

  The new parameter logOnInfo can contain a password to use in opening
  password-protected files.

  Note: The parameter sessionInfo is only of use for MS Access DLLs
  that track user session info.  The parameter dirInfo is also only
  currently useful for MS Access DLLs.

  The parameter silentMode is used to tell DLL whether to pop up any
  dialog or message itself or just return an error code. }

function OpenDataFileAndIndexChoiceVer113(
            FileName    : PAnsiChar;
            InfoPtr     : PPhysDbFileInfo;
            IndexesPtr  : PPhysDbIndexesInfo;
        var FileHandle  : PPhysDbFileHandle;
            SilentMode  : TcrBoolean;
            DirInfo     : PPhysDbFileDirectoryInfo;
            DictInfo    : PPhysDbFileDictionaryInfo;
            SessionInfo : PPhysDbSessionInfo;
            LogOnInfo   : PPhysDbLogOnInfo;
            ErrMsg      : PAnsiChar) : TPhysDbError;  stdcall;

{ This function closes a data file opened with OpenDataFileIfRecognized,
  OpenDataAndIndexFileIfRecognized or OpenDataFileAndIndexChoice, and
  deletes any allocated memory structures. }

function CloseDataFile(var FileHandle : PPhysDbFileHandle;
                           ErrMsg     : PAnsiChar) : TPhysDbError;  stdcall;


{ ----------------------  Fetch Data File Info  --------------------- }

{ This function is passed a file handle of an open data file, and returns
  info describing its file structure (whether flat or recurring records,
  number of fields in the file, the width & type of each data field,
  etc.)

  This function may retrieve this information by:
      1. "Inhaling" the data file information (without user interaction),
         if it has facilities to query the data file definition directly.
      2. Displaying Windows dialogs to prompt the user for this information,
         and then returning these values as the file structure.

  This function is only called if CanFetchDataFileInfo has previously
  returned true.  If it has not, the Database Manager uses default Windows
  dialogs to allow the user to describe the data file structure (with
  obvious risks of error).

  The parameter infoDefaultsExist is only meaningful in case 2 above.
  (In case 1 the function should always retrieve the most current data
  file definition from the system.)  In case 2 if this parameter is true
  the user has executed this function on this table before, and if
  false this is the first time.  If true, the previous values are passed as
  defaults in the info structure, and the function can display them as
  defaults in its Windows dialogs.

  Note: This function is not responsible for filling in certain information
  in PhysDbFileInfo:
       - nBytesInReadRecord
       - nFieldsInReadRecord
       - nBytesInIndexRecord
       - nFieldsInIndexRecord
  and certain information in PhysDbFieldInfo:
       - usedInReadRecord
       - offsetInReadRecord
       - usedInIndexRecord
       - offsetInIndexRecord
  This information is only meaningful in the InitDataFile functions
  below.  It can be set to zero or ignored by FetchDataFileInfo. }

function FetchDataFileInfo(
           FileHandle        : PPhysDbFileHandle;
           InfoDefaultsExist : TcrBoolean;
       var InfoPtr           : PPhysDbFileInfo;
           ErrMsg            : PAnsiChar) : TPhysDbError;  stdcall;

{ This function frees the file info structure allocated by FetchDataFileInfo. }

function FreeDataFileInfo(
           var InfoPtr : PPhysDbFileInfo;
               ErrMsg  : PAnsiChar) : TPhysDbError;  stdcall;


{ -------------------  Fetch Data File Index Info  ------------------ }

{ This function is passed a file handle of an open data file, and returns
  an index info structure (such as the number of known indexes, which
  fields are used in each index definition, etc.).  The fields in an
  index definition are identified by their (0-origin) index in the
  PhysDbFileInfoPtr->fieldInfo array of fields returned by FetchDataFileInfo.
  The file info structure is passed as a parameter to this function to
  look up these field numbers.

  This function is expected to "inhale" the index information (without user
  interaction) by querying the data file definition directly.

  This function is only called if CanFetchDataFileIndexInfo has previously
  returned someIndexesKnown or allIndexesKnown.  If indexesExistButNotKnown
  or someIndexesKnown the Database Manager uses default Windows dialogs
  to allow the user to select file names containing indexes.

  Note: This function is not responsible for filling in certain information
  in PhysDbIndexInfo:
       - usedInRead
  This information is only meaningful in the function OpenDataFileAndIndexChoice
  above.  It can be set to zero or ignored by FetchDataFileIndexInfo. }

function FetchDataFileIndexInfo(
           FileHandle : PPhysDbFileHandle;
           InfoPtr    : PPhysDbFileInfo;
       var IndexesPtr : PPhysDbIndexesInfo;
           ErrMsg     : PAnsiChar) : TPhysDbError;  stdcall;

{ This function frees the index info structure created by
  FetchDataFileIndexInfo. }

function FreeDataFileIndexInfo(var IndexesPtr : PPhysDbIndexesInfo;
                                   ErrMsg     : PAnsiChar) : TPhysDbError;  stdcall;

{ ----  Initialization and Termination of Reading from Data File  ---- }

{ Note: This function is only useful for non-SQL databases that prefer a
  SQL-type interface.  In general, for SQL databases the PhysDs.hpp
  (PDS*.DLL) interface should be used.

  SST: This routine must be exported even if it is not used or Crystal
  will not load the driver DLL. }

function BuildAndExecSQLQuery(
           FileHandleList  : PPhysDbFileHandleArray;
           FileInfoList    : PPhysDbFileInfoArray;
           LinkNonSQLFlags : PcrBooleanArray;
           IndexesInfoList : PPhysDbIndexesInfoArray;
           RangeInfoList   : PPhysDbRangeInfoArray;
           NFiles          : Word;
           LinkInfoList    : PPhysDbFileLinkInfoArray;
           NFileLinks      : Word;
           SqlDrivingFile  : TcrBoolean;
           ErrMsg          : PAnsiChar) : TPhysDbError;  stdcall;

{ This function is passed a file handle of an open data file, and the
  file info structure (from FetchDataFileInfo) describing this file,
  before starting to read from the file.  This function should perform
  any file initialization, and determine the sets of fields to be read
  for each record.

  During the Database Manager print cycle, the physical database functions
  are called as follows for each data file to be read:
     if (OpenDataFileIfRecognizedVer12 ())  // or OpenDataFileAndIndexChoice ()
        if (InitDataFileForReading ()) // or InitDataFileAndIndexForReading ()
           ...                         // perform reading
           TermDataFileForReading ()
        CloseDataFile ()

  Important: This function must not interfere with other data files
  being read at the same time by this physical database implementation.
  Therefore no global (static) data should be used by this function,
  and all state information needed during reading should be kept local
  to its own file handle.  This function should also not perform any
  global initialization of the database system that will affect other
  open data files, (this can be done during InitPhysicalDatabase instead).

  Translated and Non-Translated Fields:  The Database Manager specifies
  two sets of fields to be read from each data record, using the
  additional information in the file info structure:
       - nBytesInReadRecord
       - nFieldsInReadRecord
       - nBytesInIndexRecord
       - nFieldsInIndexRecord
  and in each field info structure:
       - usedInReadRecord
       - offsetInReadRecord
       - usedInIndexRecord
       - offsetInIndexRecord

  The two sets of fields are required for different purposes.  The
  fields indicated by usedInReadRecord are used in the printed report
  and must be translated to generic Brahma data types before returning.
  The fields flagged by usedInIndexRecord are used in constructing an
  index value for looking up records in another file, and should
  not be translated from their native format.

  The function now allows the main file of the report to be opened using
  an index, to speed up sorting and selection of records.

  The function now also allows an array of range values.

  If indexesPtr is NULL,  OpenDataFileIfRecognizedVer12 was called to
  open the data file.  If indexesPtr is non-NULL, OpenDataFileAndIndexChoice
  was called to open the file, and indexPtr contains the index choice.

  This function returns in canDoLimitRange whether it is able to perform
  the range check on this particular field type. }

function InitDataFileForReadingVer17(
           FileHandle      : PPhysDbFileHandle;
           InfoPtr         : PPhysDbFileInfo;
           IndexesPtr      : PPhysDbIndexesInfo;
           RangeInfoList   : PPhysDbRangeInfoArray;
           NRanges         : TcrInt16u;
       var CanDoRangeLimit : TcrBoolean;
           ErrMsg          : PAnsiChar) : TPhysDbError;  stdcall;

{ This function serves the same purpose as InitDataFileForReading, but
  is called when initializing reading from a file with an index,
  whereas InitDataFileForReading is called when reading from a file
  without.  The index info structure (from FetchDataFileIndexInfo) is
  passed to this function to identify the chosen index. }

function InitDataFileAndIndexForReadV115(
           FileHandle   : PPhysDbFileHandle;
           InfoPtr      : PPhysDbFileInfo;
           IndexesPtr   : PPhysDbIndexesInfo;
           LookupOptPtr : PPhysDbLookupOptInfo;
           ErrMsg       : PAnsiChar) : TPhysDbError;  stdcall;

{ This function frees the read state information allocated by
 InitDataFile functions. }

function TermDataFileForReading(
           FileHandle : PPhysDbFileHandle;
           ErrMsg     : PAnsiChar) : TPhysDbError;  stdcall;


{ ---------------  Number of Records in Data File  ------------------- }

{ This function is passed a file handle of an open data file, and
 returns the number of recurring records in the file. }

function NRecurringRecordsToRead(
           FileHandle     : PPhysDbFileHandle;
       var NRecordsToRead : LongInt;
           ErrMsg         : PAnsiChar) : TPhysDbError;  stdcall;

{ -----------------------  Read Functions  --------------------------- }

{ The following comments apply to all three of the functions for
  data file reading: ReadFlatRecord, ReadNextRecurringRecord, and
  LookupMatchingRecurringRecord.

  Translated and Non-Translated Fields:  The Database Manager requires
  two sets of fields to be returned from each data record, as explained in
  InitDataFile functions above.  The two buffers readRecordBuf and
  indexRecordBuf are passed to these functions for the two sets of field
  values.  As well indexNullFlags is an array of flags indicating whether
  a field has special database "null value" and its indexRecordBuf entry
  should be ignored.

  The two sets of fields are required for different purposes.  The fields
  returned in readRecordBuf are used in the printed report and must be
  translated to generic Brahma data types before returning.  The fields
  returned in indexRecordBuf are used in constructing an index value for
  looking up records in another file, and should not be translated from
  their native format. }

{ ---------------------  Read Flat File Record  ---------------------- }

{ This function is passed a file handle of an open flat data file, and
  reads the first data record. }

function ReadFlatRecordVer15(
           FileHandle     : PPhysDbFileHandle;
           ReadRecordBuf  : PByteArray;
           ReadNullFlags  : PcrBooleanArray;
           IndexRecordBuf : PByteArray;
           IndexNullFlags : PcrBooleanArray;
       var RecordRead     : TcrBoolean;
           ErrMsg         : PAnsiChar) : TPhysDbError;  stdcall;


{ --------  Read Next Recurring Record (Sequential Access)  ---------- }

{ This function is passed a file handle of an open data file, and
  reads the next data record (from its current file position) sequentially.
  It sets recordRead to true if it is successful, and to false if it is
  at end of file. }

function ReadNextRecurringRecordVer15(
           FileHandle      : PPhysDbFileHandle;
           ReadRecordBuf   : PByteArray;
           ReadNullFlags   : PcrBooleanArray;
           IndexRecordBuf  : PByteArray;
           IndexNullFlags  : PcrBooleanArray;
       var RecordRead      : TcrBoolean;
       var NRecordsSkipped : LongInt;
           ErrMsg          : PAnsiChar) : TPhysDbError;  stdcall;

{ -------  Lookup Matching Recurring Record (Random Access)  --------- }

{ This function is passed a file handle of an open data file, a lookup
  value and whether to start searching from first record, and looks up
  a record matching the lookup value.   This function is only called if
  OpenDataFileAndIndexChoice has been called to open the data file.

  The lookup value passed in the parameters lookupValueRecordBuf and
  lookupValueNullFlags agrees in type and ordering with the fields of the
  index chosen in the file open call.  The lookup value fields are not
  translated from the native field format, so no translation needs to occur
  back to their native format when doing record lookup.

  If the parameter startTopOfFile is true this function should begin its
  search from the beginning of the data file, if it is false it should
  search from its current file position.

  This function sets recordRead to true if it is successful, and to false
  if it is at end of file. }

function LookupMatchingRecurringRecVer15(
           FileHandle           : PPhysDbFileHandle;
           LookupValueRecordBuf : PAnsiChar;
           LookupValueNullFlags : PcrBooleanArray;
           LookupValueType      : Word;
           StartTopOfFile       : TcrBoolean;
           ReadRecordBuf        : PByteArray;
           ReadNullFlags        : PcrBooleanArray;
           IndexRecordBuf       : PByteArray;
           IndexNullFlags       : PcrBooleanArray;
       var RecordRead           : TcrBoolean;
           ErrMsg               : PAnsiChar) : TPhysDbError;  stdcall;


{ -------------------------  Memo Fields  ---------------------------- }

{ There are two types of memo fields: transientMemoField and
  persistentMemoField.  A transient memo field is one that must
  be read at the same time as the recurring data record, and a
  persistent memo field is one that can be read at any later point.

  For example, dBase supports persistent memo fields by storing a
  memo field number in the data record that uniquely identifies the
  field value in the memo file.  This field number can be stored
  by the physical database in the recurring record, and then read from
  the memo file at any later point.

  Persistent memo fields are preferred by Brahma, since the (potentially
  very large) variable length text values do not need to be saved with
  the data record (including buffering in memory, sorting, etc.)

  The following functions are used to support memo fields.  The
  functions FetchMemoField and FreeMemoField are only called for fields
  identified as transientMemoField's by this physical database.
  The functions FetchPersistentMemoField and FreePersistentMemoField
  are only called for fields identified as persistentMemoField's by
  this physical database.

  Memo field identifiers are stored in data records returned to Brahma
  by the above Read functions, and these identifiers are used to
  retrieve the memo field value. }

function FetchMemoField(MemoFieldRecordBuf : PAnsiChar;
                    var MemoField          : PAnsiChar;
                        ErrMsg             : PAnsiChar) : TPhysDbError;  stdcall;

function FreeMemoField(var MemoField : PAnsiChar;
                           ErrMsg    : PAnsiChar) : TPhysDbError;  stdcall;

function FetchPersistentMemoField(FileHandle         : PPhysDbFileHandle;
                                  MemoFieldRecordBuf : PAnsiChar;
                              var MemoField          : PAnsiChar;
                                  ErrMsg             : PAnsiChar) : TPhysDbError;  stdcall;

function FreePersistentMemoField(FileHandle : PPhysDbFileHandle;
                             var MemoField  : PAnsiChar;
                                 ErrMsg     : PAnsiChar) : TPhysDbError;  stdcall;

{ ---------------------  Multi-User Access  -------------------------- }

{ This function is called to tell the physical database functions to use
  record locking when reading from the database file(s). }

function UseRecordLocking(
           FileHandle : PPhysDbFileHandle;
           ErrMsg     : PAnsiChar) : TPhysDbError;  stdcall;

{ This function is called to tell the physical database functions to use
  file locking when reading from the database file(s). }

function UseFileLocking(
           FileHandle : PPhysDbFileHandle;
           ErrMsg     : PAnsiChar) : TPhysDbError;  stdcall;

{===DEBUG LOGGING===}
procedure StartLog;
procedure EndLog;
procedure AddToLog(const S : string);
procedure AddToLogFmt(const S : string; args : array of const);        {!!.12}
procedure AddBlockToLog(const S : string; Buf : pointer; BufLen : TffMemSize);
procedure AddResultToLog(aResult : TPhysDbError);


implementation

uses
  Dialogs,
  Forms,
  Classes,
  Windows,
  ffclbde,
  ffsrbde,
  ffclconv,
  ffcrltyp,
  ffcrutil,
  ffllunc,
  ffdb,
  fflleng,
  ffdbbase;

type
  TTaskListItem = record
    TaskHandle         : THandle;
    AlreadyInitialized : Boolean;
  end;
  PTaskListItem = ^TTaskListItem;

  TTaskList = class(TList)
    function AddTask(TaskHandle: THandle) : TPhysDbError;
    function DeleteTask(var TaskFound: Boolean;
                        var AlreadyInitialized: Boolean;
                            ErrMsg: PAnsiChar) : TPhysDbError;
    function FindTask(TaskHandle: THandle) : integer;
    function NewTask(var TaskFound: Boolean;
                     var TaskIndex: integer;
                         ErrMsg: PAnsiChar) : TPhysDbError;
  end;

var
  TaskList      : TTaskList;
  IsTaskSuccess : Boolean;
  DebugBuff     : array[0..1023] of AnsiChar;
  Log           : TffEventLog;                                         {!!.12}

{$IFDEF IDAPI_INTERNAL_LIMITS}
const
  MAX_DBS_PER_SESSION = 32;
  nOpenDatabase: Word = 0;
{$ENDIF}

function ServerEngine : TffBaseServerEngine;
{return the default sessions server engine}
begin
  Result := FFSession.ServerEngine;
end;

{ ----------------------- TTaskList methods ------------------------- }

function TTaskList.AddTask(TaskHandle: THandle) : TPhysDbError;
var
  Item : PTaskListItem;
begin
  try
    FFGetMem(Item, sizeof(TTaskListItem));
    Item^.TaskHandle := TaskHandle;
    Item^.AlreadyInitialized := False;
    Add(Item);
    Result := errPhysDbNoError;
  except
    Result := errPhysDbNotEnoughMemory;
  end;
end;

function TTaskList.DeleteTask(var TaskFound: Boolean;
                              var AlreadyInitialized: Boolean;
                                  ErrMsg: PAnsiChar) : TPhysDbError;
var
  Item       : PTaskListItem;
  TaskHandle : THandle;
  TaskIndex  : integer;
begin
  TaskFound := False;
  AlreadyInitialized := False;

  TaskHandle := HInstance;  {!!GetCurrentTask }
  TaskIndex := FindTask(TaskHandle);
  if TaskIndex <> -1 then begin
    TaskFound := True;
    Item := PTaskListItem(TaskList.Items[TaskIndex]);
    AlreadyInitialized := Item^.AlreadyInitialized;
    FFFreeMem(Item, sizeof(TTaskListItem));
    Delete(TaskIndex);
  end;
  Result := errPhysDbNoError;
end;

function TTaskList.FindTask(TaskHandle: THandle) : integer;
var
  i : integer;
begin
  Result := -1;
  for i := 0 to pred(Count) do
    if PTaskListItem(Items[i])^.TaskHandle = TaskHandle then begin
      Result := i;
      Break;
    end;
end;

function TTaskList.NewTask(var TaskFound : Boolean;
                           var TaskIndex : integer;
                               ErrMsg    : PAnsiChar) : TPhysDbError;
var
  TaskHandle : THandle;
begin
  TaskFound := False;

  { See if current task is already in the list of tasks }
  TaskHandle := HInstance; {GetCurrentTask;}
  TaskIndex := FindTask(TaskHandle);
  if TaskIndex <> -1 then begin
    TaskFound := True;
    Result := errPhysDbNoError;
    Exit;
  end;

  { If not, then add it }
  TaskIndex := Count;
  Result := AddTask(TaskHandle);
end;

{ -----------------------  Helper Routines  ------------------------- }

function IDAPIError(ErrCode: TffResult; var ErrMsg: PAnsiChar) : TPhysDbError;
begin
  with EffDatabaseError.CreateViaCode(ErrCode, False) do
    try
      StrPCopy(ErrMsg, ErrorString);
    finally
      Free;
    end;
  AddToLogFmt('  IDAPI Error: [%s]', [ErrMsg]);
  Result := errPhysDbErrMsgReturned;
end;

function Convert2BrahmaType(FileHandle  : PPhysDbFileHandle;
                            NativeType  : TcrInt16u;
                        var NativeWidth : TcrInt16u;
                        var BrahmaType  : TFieldValueType;
                        var BrahmaWidth : TcrInt16u;
                            ErrMsg      : PAnsiChar) : TPhysDbError;
var
  BookmarkSize : Integer;
  FFError      : TffResult;
begin
  Result := errPhysDbNoError;
  case NativeType of
    fldZSTRING:
      begin
        BrahmaType := ftStringField;
        if NativeWidth = 1 then begin  { Handle Char types }
          BrahmaWidth := 2;
        end
        else begin
          BrahmaWidth := NativeWidth;
(*          Dec(NativeWidth);*)
        end;
      end;
    fldDATE:
      begin
        BrahmaType := ftDateField;
        BrahmaWidth := SizeOf(TcrDate);
        NativeWidth := SizeOf(TcrDate);
      end;
    fldBLOB, fldstBINARY, fldstGRAPHIC, fldstTYPEDBINARY:
      begin
        BrahmaType := ftBlobField;

        { Get bookmark size }
        FFError := ServerEngine.CursorGetBookmarkSize(FileHandle^.CursorID, BookmarkSize);
        if FFError <> DBIERR_NONE then begin
          Result := IDAPIError(FFError, ErrMsg);
          Exit;
        end;

        BrahmaWidth := SizeOf(TcrInt16u) + BookmarkSize;
        NativeWidth := SizeOf(TcrInt16u) + BookmarkSize;
      end;
    fldstMEMO, fldstFMTMEMO:
      { Memo field, or variable length char string.  save only the FieldNo
        in this field. }
      begin
        BrahmaType := ftPersistentMemoField;

        { Get bookmark size }
        FFError := ServerEngine.CursorGetBookmarkSize(FileHandle^.CursorID, BookmarkSize);
        if FFError <> DBIERR_NONE then begin
          Result := IDAPIError(FFError, ErrMsg);
          Exit;
        end;

        BrahmaWidth := SizeOf(TcrInt16u) + BookmarkSize + 100;
        NativeWidth := SizeOf(TcrInt16u) + BookmarkSize + 100;
      end;
    fldBOOL:
      begin
        BrahmaType := ftBooleanField;
        BrahmaWidth := SizeOf(TcrBoolean);
      end;
    fldTIME:
      begin
        BrahmaType := ftTimeField;
        BrahmaWidth := SizeOf(TcrTime);
        NativeWidth := SizeOf(TDbiTime);
      end;
    fldTIMESTAMP:
      begin
        BrahmaType := ftStringField;
        BrahmaWidth := SIZEOF_DATETIME_FIELD_STRING;
        NativeWidth := SizeOf(TDbiTimeStamp);
      end;
    fldINT16, fldUINT16:
      begin
        BrahmaType := ftInt16sField;
        BrahmaWidth := SizeOf(TcrInt16s);
      end;
    fldINT32, fldUINT32:
      begin
        BrahmaType := ftInt32sField;
        BrahmaWidth := SizeOf(TcrInt32s);
      end;
    fldFLOAT:
      begin
        BrahmaType := ftNumberField;
        BrahmaWidth := SizeOf(TcrNumber);
      end;
    fldstMONEY:
      begin
        BrahmaType := ftCurrencyField;
        BrahmaWidth := SizeOf(TcrNumber);
      end;
    else
      begin
        BrahmaType := ftUnknownField;
        BrahmaWidth := 1;
        NativeWidth := 1;
      end;
  end;
end;

function DoubleToNumber(const D: Double) : TcrNumber;
begin
  Result := D * NUMBER_SCALING_FACTOR;
end;

function NumberToDouble(const N : TcrNumber) : Double;
begin
  Result := (N / NUMBER_SCALING_FACTOR);
end;

procedure ConvertTimestampToDateTimeString(
            aDate : TDbiDate;
            aTime : TDbiTime;
            aBrahmaValue : PAnsiChar);
var
  Year     : TcrInt16u;
  Fraction : TcrInt16s;
  Hour     : TcrInt16u;
  Minute   : TcrInt16u;
  Second   : TcrInt16u;
  Millisec : TcrInt16u;
  Month    : TcrInt16u;
  Day      : TcrInt16u;
  I        : TcrInt16u;
  ZeroOrd  : Integer;
begin
  Year := 0;
  Fraction := 0;
  FFBDEDateDecode(aDate, Day, Month, Year);
  FFBDETimeDecode(aTime, Hour, Minute, MilliSec);
  Second := Millisec div 1000;
  ZeroOrd := Ord('0');

  { Translate year to string }
  for I := 3 downto 0 do begin
    aBrahmaValue[I] := Chr((Year mod 10) + ZeroOrd);
    Year := Year div 10;
  end;

  aBrahmaValue[4] := '/';
  aBrahmaValue[5] := Chr((Month div 10) + ZeroOrd);
  aBrahmaValue[6] := Chr((Month mod 10) + ZeroOrd);

  aBrahmaValue[7] := '/';
  aBrahmaValue[8] := Chr((Day div 10) + ZeroOrd);
  aBrahmaValue[9] := Chr((Day mod 10) + ZeroOrd);

  aBrahmaValue[10] := ' ';
  aBrahmaValue[11] := Chr((Hour div 10) + ZeroOrd);
  aBrahmaValue[12] := Chr((Hour mod 10) + ZeroOrd);

  aBrahmaValue[13] := ':';
  aBrahmaValue[14] := Chr((Minute div 10) + ZeroOrd);
  aBrahmaValue[15] := Chr((Minute mod 10) + ZeroOrd);

  aBrahmaValue[16] := ':';
  aBrahmaValue[17] := Chr((Second div 10) + ZeroOrd);
  aBrahmaValue[18] := Chr((Second mod 10) + ZeroOrd);

  aBrahmaValue[19] := '.';
  aBrahmaValue[20] := Chr((Fraction div 10) + ZeroOrd);
  aBrahmaValue[21] := Chr((Fraction mod 10) + ZeroOrd);

  aBrahmaValue[22] := #0;
end;

{ ---------------------  Database Abilities  ------------------------ }

{ This is the version number for the driver DLL, not the physical database.
  Crystal Reports uses this number to decide which list of function names
  to expect to be exported from the DLL.

  Crystal Reports OEM Tech Support advised me that this should be
  identical to the version number coded into the PDBXBSE driver.  As such,
  the exported function names should be identical to PDBXBSE as well. }

function PhysDbVersionNumber(
           var MajorVersionNumber : Word;
           var MinorVersionNumber : Word;
               ErrMsg             : PAnsiChar) : TPhysDbError;
begin
  AddToLog('PhysDbVersionNumber');
  MajorVersionNumber := 1;
  MinorVersionNumber := 17;
  Result := errPhysDbNoError;
  AddToLogFmt('  MajMin: [%d.%d]', [MajorVersionNumber, MinorVersionNumber]);
  AddResultToLog(Result);
end;

function CanRecognizeDataFile(
           var CanRecognize : TcrBoolean;
               ErrMsg       : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CanRecognizeDataFile');
  CanRecognize := true;
  Result := errPhysDbNoError;
  AddToLogFmt('  Can?: [%s]', [BoolToStr(CanRecognize)]);
  AddResultToLog(Result);
end;

function CanFetchDataFileInfo(
           var CanFetchFileInfo : TcrBoolean;
               ErrMsg           : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CanFetchDataFileInfo');
  CanFetchFileInfo := true;
  Result := errPhysDbNoError;
  AddToLogFmt('  Can?: [%s]', [BoolToStr(CanFetchFileInfo)]);
  AddResultToLog(Result);
end;

function CanFetchDataFileIndexInfo(
           var CanFetchIndexInfo : TPhysDbIndexInfoCases;
               ErrMsg            : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CanFetchDataFileIndexInfo');
  CanFetchIndexInfo := iiAllIndexesKnown;
  Result := errPhysDbNoError;
  AddToLogFmt('  Can?: [%d]', [Ord(CanFetchIndexInfo)]);
  AddResultToLog(Result);
end;

function CanBuildIndex(
           var CanBuildIndex : TPhysDbBuildIndexCases;
               ErrMsg        : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CanBuildIndex');
  CanBuildIndex := biCannotBuildIndex;
  Result := errPhysDbNoError;
  AddToLogFmt('  Can?: [%d]', [ord(CanBuildIndex)]);
  AddResultToLog(Result);
end;

function CanFetchNRecurringRecords(
           var CanFetchNrecords : TcrBoolean;
               ErrMsg           : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CanFetchNRecurringRecords');
  CanFetchNRecords := true;
  Result := errPhysDbNoError;
  AddToLogFmt('  Can?: [%s]', [BoolToStr(CanFetchNRecords)]);
  AddResultToLog(Result);
end;

function SQLCompatible(
           var IsSQLTypeDLL            : TcrBoolean;
           var CanBuildAndExecSQLQuery : TcrBoolean;
           var CanExecSQLQuery         : TcrBoolean;
               ErrMsg                  : PAnsiChar) : TPhysDbError;
begin
  AddToLog('SQLCompatible');
  IsSQLTypeDLL := false;
  CanBuildAndExecSQLQuery := false;
  CanExecSQLQuery := false;  {true - allow passing down rangeinfolist }
  Result := errPhysDbNoError;
  AddToLogFmt('  IsSQLTypeDLL?:            [%s]', [BoolToStr(IsSQLTypeDLL)]);
  AddToLogFmt('  CanBuildAndExecSQLQuery?: [%s]', [BoolToStr(CanBuildAndExecSQLQuery)]);
  AddToLogFmt('  CanExecSQLQuery?:         [%s]', [BoolToStr(CanExecSQLQuery)]);
  AddResultToLog(Result);
end;

function CanReadSortedOrder(
           var CanReadSorted : TcrBoolean;
               ErrMsg        : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CanReadSortedOrder');
  CanReadSorted := True;
  Result := errPhysDbNoError;
  AddToLogFmt('  Can?: [%s]', [BoolToStr(CanReadSorted)]);
  AddResultToLog(Result);
end;

function CanReadRangeOfValues(
           var CanReadRange : TcrBoolean;
               ErrMsg       : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CanReadRangeOfValues');
  CanReadRange := False;
  Result := errPhysDbNoError;
  AddToLogFmt('  Can?: [%s]', [BoolToStr(CanReadRange)]);
  AddResultToLog(Result);
end;

function CanUseRecordLocking(
           var RecordLockingPossible  : TcrBoolean;
           var RecordLockingPreferred : TcrBoolean;
               ErrMsg                 : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CanUseRecordLocking');
  RecordLockingPossible := false;
  RecordLockingPreferred := false;
  Result := errPhysDbNoError;
  AddToLogFmt('  Record Locking Possible?:  [%s]', [BoolToStr(RecordLockingPossible)]);
  AddToLogFmt('  Record Locking Preferred?: [%s]', [BoolToStr(RecordLockingPreferred)]);
  AddResultToLog(Result);
end;

function CanUseFileLocking(
           var FileLockingPossible  : TcrBoolean;
           var FileLockingPreferred : TcrBoolean;
               ErrMsg               : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CanUseFileLocking');
  FileLockingPossible := false;
  FilelockingPreferred := false;
  Result := errPhysDbNoError;
  AddToLogFmt('  File Locking Possible?:  [%s]', [BoolToStr(FileLockingPossible)]);
  AddToLogFmt('  File Locking Preferred?: [%s]', [BoolToStr(FileLockingPreferred)]);
  AddResultToLog(Result);
end;


{ -----------  Database Initialization and Termination  ------------- }

function InitPhysicalDatabase(ErrMsg : PAnsiChar) : TPhysDbError;
begin
  AddToLog('InitPhysicalDatabase');
  { No special processing to initilize the database.
    But we can't return PhysDbNotImplemented or Crystal will choke.  }
  IsTaskSuccess := True;
  Result := errPhysDbNoError;
  AddResultToLog(Result);
end;

function TermPhysicalDatabase(ErrMsg : PAnsiChar) : TPhysDbError;
begin
  AddToLog('TermPhysicalDatabase');
  { No special processing to deinitialize the database.
    But we can't return PhysDbNotImplemented or Crystal will choke.  }
  Result := errPhysDbNoError;
  AddResultToLog(Result);
end;

function OpenSession(ErrMsg : PAnsiChar) : TPhysDbError;
var
  TaskFound : Boolean;
  TaskIndex : integer;
begin
  AddToLog('OpenSession');
  Result := errPhysDbNoError;
  TaskIndex := -1;
  {handling in the except block? }
  try
    Result := TaskList.NewTask(TaskFound, TaskIndex, ErrMsg);
    if (Result = errPhysDbNoError) then
      if not TaskFound then
        FFSession.Open;
  except
    on EOutOfMemory do begin
      Result := errPhysDbNotEnoughMemory;
      StrPCopy(ErrMsg, '');
    end;

    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      if not Assigned(ErrMsg) then
        StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function TermSession(ErrMsg : PAnsiChar) : TPhysDbError;
var
  TaskFound          : Boolean;
  AlreadyInitialized : Boolean;
begin
  AddToLog('TermSession');
  Result := TaskList.DeleteTask(TaskFound, AlreadyInitialized, ErrMsg);
  if (Result = errPhysDbNoError) then
    if TaskFound then
      if not AlreadyInitialized then
        FFSession.Close;
  AddResultToLog(Result);
end;


{ -------------------------  Database Name -------------------------- }

function FetchDatabaseName(var Name   : PAnsiChar;
                               ErrMsg : PAnsiChar) : TPhysDbError;
begin
  AddToLog('FetchDatabaseName');
  try
    Name := FFStrNew('FlashFiler 2');
    Result := errPhysDbNoError;
  except
    Result := errPhysDbNotEnoughMemory;
  end;
  AddToLogFmt('  Name: [%s]', [Name]);
  AddResultToLog(Result);
end;

function FreeDatabaseName(var Name   : PAnsiChar;
                              ErrMsg : PAnsiChar) : TPhysDbError;
begin
  AddToLog('FreeDatabaseName');
  AddToLogFmt('  Name: [%s]', [Name]);
  Result := errPhysDbNoError;
  try
    FFStrDispose(Name);
    Name := nil;
  except
    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;


{ ----------------------  Log On and Log Off  ----------------------- }

function LogOnServer(ServerInfo   : PPhysDbServerInfo;
                 var ServerHandle : PPhysDbServerHandle;
                     Password     : PAnsiChar;
                     ErrMsg       : PAnsiChar) : TPhysDbError;
begin
  AddToLog('LogOnServer');
  { Can't return PhysDbNotImplemented or Crystal will choke. }
  Result := errPhysDbNoError;
  AddToLogFmt('  Server Handle: [%d]', [ServerHandle]);
  AddResultToLog(Result);
end;

function LogOffServer(
                  var ServerHandle : PPhysDbServerHandle;
                      ErrMsg       : PAnsiChar) : TPhysDbError;
begin
  AddToLog('LogOffServer');
  { Can't return PhysDbNotImplemented or Crystal will choke. }
  Result := errPhysDbNoError;
  AddToLogFmt('  Server Handle: [%d]', [ServerHandle]);
  AddResultToLog(Result);
end;

{ ------------------- Parse and Rebuild SQL Info -------------------- }

function ParseLogOnInfo(
           connectBuf : PAnsiChar;
           bufSize    : Word;
           serverInfo : PPhysDbServerInfo;
           ErrMsg     : PAnsiChar) : TPhysDbError;
begin
  AddToLog('ParseLogOnInfo');
  Result := errPhysDbNotImplemented;
  AddResultToLog(Result);
end;

function RebuildConnectBuf(
           serverInfo : PPhysDbServerInfo;
           connectBuf : PAnsiChar;
           bufSize    : Word;
           ErrMsg     : PAnsiChar) : TphysDbError;
begin
  AddToLog('RebuildConnectBuf');
  Result := errPhysDbNotImplemented;
  AddResultToLog(Result);
end;


{ --------------------  Open and Close Files  ----------------------- }

function InitDataFileHandle(FileName       : PAnsiChar;
                        var FileHandle     : PPhysDbFileHandle;
                            DatabaseHandle : TffDatabaseID;
                            hCursor        : TffcursorID;
                            ErrMsg         : PAnsiChar) : TPhysDbError;
var
  vNotXlateDOSString : Boolean;
  vNotXlateDOSMemo   : Boolean;
begin
  Result := errPhysDbNoError;
  try
    {By default these two flags are FALSE : always convert OEM to ANSI,
     check it now}
    vNotXlateDOSString :=
       (LongInt(FileHandle) and TRANSLATE_DOS_STRINGS) = 0;
    vNotXlateDOSMemo :=
       (LongInt(FileHandle) and TRANSLATE_DOS_MEMOS) = 0;

    FFGetZeroMem(FileHandle, sizeof(TPhysDbFileHandle));
    FileHandle^.DatabaseID := DatabaseHandle;
    FileHandle^.CursorID := hCursor;
    FileHandle^.NotXlateDOSString := vNotXlateDOSString;
    FileHandle^.NotXlateDOSMemo := vNotXlateDOSMemo;

    FileHandle^.PathAndFileName := FFStrAllocCopy(FileName);
  except
    on EOutOfMemory do begin
      Result := errPhysDbNotEnoughMemory;
      ServerEngine.CursorClose(hCursor);
      ServerEngine.DatabaseClose(DatabaseHandle);
      if Assigned(FileHandle) then
        FFFreeMem(FileHandle, sizeof(TPhysDbFileHandle));
      StrPCopy(ErrMsg, '');
    end;

    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      ServerEngine.CursorClose(hCursor);
      ServerEngine.DatabaseClose(DatabaseHandle);
      if Assigned(FileHandle) then
        FFFreeMem(FileHandle, sizeof(TPhysDbFileHandle));
      if not Assigned(ErrMsg) then  {not assigned? }
        StrPCopy(ErrMsg, E.Message);
    end;
  end;
end;

function OpenDatabase(DBName         : PAnsiChar;
                  var DatabaseHandle : TffDatabaseID;
                      ErrMsg         : PAnsiChar) : TPhysDbError;
var
  FFError   : TffResult;
  DBNameUNC : TffShStr;
begin
  if not Assigned(DBName) then begin
    Result := errPhysDbProgrammingError;
    Exit;
  end;

  DBNameUNC := FFExpandUNCFilename(FFStrPas(DBName));
  if (length(DBNameUNC) > 3) and
     (DBNameUNC[length(DBNameUNC)] = '\') then
    dec(DBNameUNC[0]);

  FFSession.Open;
  FFError := ServerEngine.DatabaseOpenNoAlias(FFSession.Client.ClientID,
                                              DBNameUNC,
                                              omReadOnly,
                                              smShared,
                                              DefaultTimeOut, {2000}{-1} {!!.05}
                                              DatabaseHandle);
  if FFError <> DBIERR_NONE then begin
    Result := IDAPIError(FFError, ErrMsg);
    Exit;
  end;
  Result := errPhysDbNoError;
end;

function OpenDataFile(DatabaseHandle : TffDatabaseID;
                      FileName       : PAnsiChar;
                  var FileHandle     : PPhysDbFileHandle;
                      IndexFileName  : PAnsiChar;
                      TagName        : PAnsiChar;
                      IndexId        : Word;
                      ErrMsg         : PAnsiChar) : TPhysDbError;
var
  FFError   : TffResult;
  hCursor   : TffCursorID;
  TableName : TffShStr;
  IndexName : TffShStr;
  Stream    : TMemoryStream;
begin
  TableName := FFExtractTableName(FFStrPas(FileName));
  if (IndexFileName = nil) then
    IndexName := ''
  else
    IndexName := FFStrPas(IndexFilename);

  AddToLogFmt('  TableName: [%s]', [TableName]);
  AddToLogFmt('  IndexName: [%s]', [IndexName]);

  Stream := TMemoryStream.Create;
  try
    FFError := ServerEngine.TableOpen(DatabaseHandle,
                                      TableName,
                                      False,
                                      IndexName,
                                      IndexId,
                                      omReadOnly,
                                      smShared,
                                      DefaultTimeOut, {2000}{-1}      {!!.05}
                                      hCursor,
                                      Stream);
  finally
    Stream.Free;
  end;
  if FFError <> DBIERR_NONE then begin
    ServerEngine.DatabaseClose(DatabaseHandle);
    Result := IDAPIError(FFError, ErrMsg);
    Exit;
  end;

  Result := InitDataFileHandle(FileName, FileHandle, DatabaseHandle,
                               hCursor, ErrMsg);
end;

function OpenDataFileIfRecognizedVer113(
               FileName         : PAnsiChar;
               OpenDefaultIndex : TcrBoolean;
           var Recognized       : TcrBoolean;
           var FileHandle       : PPhysDbFileHandle;
               CalledFromDirDLL : TcrBoolean;
           var AliasName        : PAnsiChar;
               SilentMode       : TcrBoolean;
               DirInfo          : PPhysDbFileDirectoryInfo;
               DictInfo         : PPhysDbFileDictionaryInfo;
               SessionInfo      : PPhysDbSessionInfo;
               LogOnInfo        : PPhysDbLogOnInfo;
               ErrMsg           : PAnsiChar) : TPhysDbError;
var
  FileNameStr    : TffShStr;
  DatabaseHandle : TffDatabaseID;
  DBNameOem      : array[0..255] of AnsiChar;
begin
  AddToLog('OpenDataFileIfRecognizedVer113');
  AddToLogFmt('  File Name:    [%s]', [FileName]);
  AddToLogFmt('  OpenDefIndex: [%s]', [BoolToStr(OpenDefaultIndex)]);

  Result := errPhysDbNoError;
  Recognized := false;
  if not IsTaskSuccess then begin
    AddToLog('  IsTaskSuccess is false');
    AddToLogFmt('  Recognized?   [%s]', [BoolToStr(Recognized)]);
    AddResultToLog(Result);
    Exit;
  end;

  FileHandle := nil;
  DBNameOem[0] := #0;

  if (AliasName <> nil) then
    AliasName[0] := #0;

  try

    { Return error if file does not exist. }
    FileNameStr := FFStrPas(FileName);
    if not FileExists(FileNameStr) then begin
      Result := errPhysDbFileDoesNotExist;
      AddToLogFmt('  Recognized?   [%s]', [BoolToStr(Recognized)]);
      AddResultToLog(Result);
      Exit;
    end;

    { Check to see if the file name has an FF2 extension. If not, then
      we assume that it's not a FF table (this avoids the time-
      consuming protocol and FF client initialization stuff).}
    if (FFCmpShStrUC(FFExtractExtension(FileNameStr),
                     ffc_ExtForData, ffcl_Extension) <> 0) then begin
      { No error, but file is not recognized }
      Result := errPhysDbNoError;
      AddToLogFmt('  Recognized?   [%s]', [BoolToStr(Recognized)]);
      AddResultToLog(Result);
      Exit;
    end;

    {$IFDEF IDAPI_INTERNAL_LIMITS}
    if NOpenDatabase >= MAX_DBS_PER_SESSION then begin
      Recognized := false;
      AddToLogFmt('  Recognized?   [%s]', [BoolToStr(Recognized)]);
      AddResultToLog(Result);
      Exit;
    end;
    {$ENDIF}

    FFStrPCopy(DBNameOem, FFExtractPath(FFStrPas(FileName)));
    Result := OpenDatabase(DBNameOem, DatabaseHandle, ErrMsg);
    if Result <> errPhysDbNoError then begin
      AddToLogFmt('  Recognized?   [%s]', [BoolToStr(Recognized)]);
      AddResultToLog(Result);
      Exit;
    end;

    {$IFDEF IDAPI_INTERNAL_LIMITS}
      Inc(NOpenDatabase);
    {$ENDIF}

    {convert filename to oem? }
    Recognized := OpenDataFile(DatabaseHandle, FileName, FileHandle, nil, nil, 0, ErrMsg) = errPhysDbNoError;
    AddToLogFmt('  CursorID:     [%d]', [FileHandle^.CursorID]);
    Result := errPhysDbNoError;
  except
    on EOutOfMemory do begin
      Result := errPhysDbNotEnoughMemory;
      CloseDataFile(FileHandle, ErrMsg);
      StrPCopy(ErrMsg, '');
    end;

    on E: Exception do begin
      CloseDataFile(FileHandle, ErrMsg);
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  AddToLogFmt('  Recognized?   [%s]', [BoolToStr(Recognized)]);
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg:       [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function OpenDataAndIndexFileIfRecogV113(
               FileName   : PAnsiChar;
               IndexName  : PAnsiChar;
           var Recognized : TcrBoolean;
           var FileHandle : PPhysDbFileHandle;
           var AliasName  : PAnsiChar;
               SilentMode : TcrBoolean;
               DirInfo    : PPhysDbFileDirectoryInfo;
               DictInfo   : PPhysDbFileDictionaryInfo;
               SessionInfo: PPhysDbSessionInfo;
               LogOnInfo  : PPhysDbLogOnInfo;
               ErrMsg     : PAnsiChar) : TPhysDbError;
var
  FFError : TffResult;
begin
  AddToLog('OpenDataAndIndexFileIfRecogV113');
  AddToLogFmt('  FName:   [%s]', [FileName]);
  AddToLogFmt('  InxName: [%s]', [IndexName]);
  Result := errPhysDbNoError;
  try
    Recognized := false;
    AliasName := nil;

    { Open the data file first }
    Result := OpenDataFileIfRecognizedVer113(FileName, False, Recognized,
                FileHandle, False, FileName, SilentMode, DirInfo,
                DictInfo, SessionInfo, LogOnInfo, ErrMsg);
    if Result <> errPhysDbNoError then Exit;
    AddToLogFmt('  FName:     [%s]', [FileHandle^.PathAndFileName]);
    AddToLogFmt('  Cursor ID: [%d]', [FileHandle^.CursorID]);
    FFError := ServerEngine.CursorSwitchToIndex(FileHandle^.CursorID,
                                                IndexName,
                                                0,
                                                True);
    if (FFError = DBIERR_NOCURRREC) then
      FFError := ServerEngine.CursorSwitchToIndex(FileHandle^.CursorID,
                                                  IndexName,
                                                  0,
                                                  False);

    if FFError <> DBIERR_NONE then
      Result := IDAPIError(FFError, ErrMsg);
  except
    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function OpenDataFileAndIndexChoiceVer113(
                FileName   : PAnsiChar;
                InfoPtr    : PPhysDbFileInfo;
                IndexesPtr : PPhysDbIndexesInfo;
            var FileHandle : PPhysDbFileHandle;
                SilentMode : TcrBoolean;
                DirInfo    : PPhysDbFileDirectoryInfo;
                DictInfo   : PPhysDbFileDictionaryInfo;
                SessionInfo: PPhysDbSessionInfo;
                LogOnInfo  : PPhysDbLogOnInfo;
                ErrMsg     : PAnsiChar) : TPhysDbError;
var
  DatabaseHandle: TffDatabaseID;
  FileNameOem: array[0..MAX_PATH] of AnsiChar;
  DBNameOem: array[0..MAX_PATH] of AnsiChar;
begin
  AddToLog('OpenDataFileandIndexChoiceVer113');

  Result := errPhysDbNoError;
  try
    {$IFDEF IDAPI_INTERNAL_LIMITS}
      if NOpenDatabase >= MAX_DBS_PER_SESSION then begin
        Result := errPhysDbErrorHandledByDBDLL;
        Exit;
      end;
    {$ENDIF}

    StrPCopy(DBNameOem, ExtractFilePath(StrPas(FileName)));
    Result := OpenDatabase(DBNameOem, DatabaseHandle, ErrMsg);
    AddToLogFmt('  DatabaseID: [%d]', [DatabaseHandle]);
    if Result = errPhysDbNoError then begin
      {$IFDEF IDAPI_INTERNAL_LIMITS}
        Inc(NOpenDatabase);
      {$ENDIF}

      StrCopy(FileNameOem, FileName);

      with IndexesPtr^.IndexInfo^[IndexesPtr^.IndexInUse] do
        Result := OpenDataFile(DatabaseHandle, FilenameOem, FileHandle,
                    IndexFilename, TagName, IndexesPtr^.IndexInUse, ErrMsg);
      AddToLogFmt('  FName:     [%s]', [FileHandle^.PathAndFileName]);
      AddToLogFmt('  CursorID:  [%d]', [FileHandle^.CursorID]);
    end;
  except
    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function CloseDataFile(var FileHandle : PPhysDbFileHandle;
                           ErrMsg     : PAnsiChar) : TPhysDbError;
begin
  AddToLog('CloseDataFile');
  Result := errPhysDbNoError;
  try
    if Assigned(FileHandle) then begin
      AddToLogFmt('  FName:    [%s]', [FileHandle^.PathAndFileName]);
      AddToLogFmt('  CursorID: [%d]', [FileHandle^.CursorID]);
      with FileHandle^ do begin
        ServerEngine.CursorClose(CursorID);
        if DatabaseID > 0 then  { not sure why DbiCloseCursor is clearing this }
          ServerEngine.DatabaseClose(DatabaseID);
        {$IFDEF IDAPI_INTERNAL_LIMITS}
          if NOpenDatabase > 0 then
            Dec(NOpenDatabase);
        {$ENDIF}

        FFStrDispose(PathAndFileName);
        FFStrDispose(IndexFileName);
        FFStrDispose(TagName);
      end;
      FFFreeMem(FileHandle, sizeof(TPhysDbFileHandle));
    end;
    FileHandle := nil;
  except
    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;


{ ----------------------  Fetch Data File Info  --------------------- }

function FetchDataFileInfo(
               FileHandle        : PPhysDbFileHandle;
               InfoDefaultsExist : TcrBoolean;
           var InfoPtr           : PPhysDbFileInfo;
               ErrMsg            : PAnsiChar) : TPhysDbError;
var
  I             : Integer;
  FieldOffset   : LongInt;

  Buffer        : TffShStr;
  FFFieldType   : TffFieldType;

  BDEType       : Word;
  BDESubType    : Word;
  LogSize       : Word;
begin
  AddToLog('FetchDataFileInfo');
  AddToLogFmt('  File Name: [%s]', [FileHandle^.PathAndFileName]);
  AddToLogFmt('  CursorID:  [%d]', [FileHandle^.CursorID]);

  Result := errPhysDbNoError;
  try
    try

      { Allocate the file info structure }
      FFGetZeroMem(InfoPtr, sizeof(TPhysDbFileInfo));

      with InfoPtr^ do begin
        NFields := 0;
        FieldInfo := nil;
        { Always set the file to the recurring type, even if file contains only
          0 or 1 records, since the file may grow in size. }
        FileType := ftRecurringFile;
        { Set tablename to nil so the file name will be used by default }
        TableName := nil;

        { Get number of fields in table }
        NFields := TFFProxyCursor(FileHandle^.CursorID).Dictionary.FieldCount;
        NBytesInPhysRecord := TFFProxyCursor(FileHandle^.CursorID).PhysicalRecordSize;

        if NFields > 0 then begin
          { Retrieve field info }

          { Allocate the field info array structure }
          FFGetZeroMem(FieldInfo, SizeOf(TPhysDbFieldInfo) * NFields);

          { Build the field info array }
          FieldOffset := 0;
          for I := 0 to pred(NFields) do begin
            with FieldInfo^[I], TFFProxyCursor(FileHandle^.CursorID) do begin
              { Allocate space for the field name }
              Name := FFStrNew(Dictionary.FieldName[I]);
              { Determine Brahma data type and width }
              NBytesInNativeField := Dictionary.FieldLength[I];
              FFFieldType := Dictionary.FieldType[I];
              MapFFTypeToBDE(FFFieldType, NBytesInNativeField, BDEType, BDESubType, LogSize);
              NativeFieldType := BDEType;
              if NativeFieldType = fldBLOB then
                NativeFieldType := BDESubType;
              if (NativeFieldType = fldFLOAT) and (BDESubType = fldstMONEY) then
                NativeFieldType := BDESubType;

              Result := Convert2BrahmaType(FileHandle,
                                           NativeFieldType,
                                           NBytesInNativeField,
                                           FieldType,
                                           NBytesInField,
                                           ErrMsg);
              if Result <> errPhysDbNoError then Exit;

              if FieldType = ftUnknownField then begin
                AddToLog('Convert2BrahmaType: Unknown field');
                AddToLogFmt('  Field:                      [%s]', [Dictionary.FieldName[I]]);
                AddToLogFmt('  Type : [%d]',               [NativeFieldType]);
              end;

              case FFFieldType of
                fftShortString,
                fftShortAnsiStr : NativeFieldOffset := Succ(FieldOffset);
              else
                NativeFieldOffset := FieldOffset;
              end;
              NDecPlacesInNativeField := Dictionary.FieldUnits[I];
              Picture := nil;
              Alignment := alLeftAlignedChars;
              Sortable := true;
            end;

            { Calculate the offset for the next field }
            Inc(FieldOffset, FieldInfo^[I].NBytesInNativeField);
          end;
        end;

        { these are not set by this routine }
        NBytesInReadRecord := 0;
        NFieldsInReadRecord := 0;
        NBytesInIndexRecord := 0;
        NFieldsInIndexRecord := 0;
      end;
    except  { InfoPtr error handler }
      on EOutOfMemory do begin
        Result := errPhysDbNotEnoughMemory;
        FreeDataFileInfo(InfoPtr, ErrMsg);
        StrPCopy(ErrMsg, '');
      end;

      on E: Exception do begin
        FreeDataFileInfo(InfoPtr, ErrMsg);
        if Result = errPhysDbNoError then
          Result := errPhysDbErrMsgReturned;
        StrPCopy(ErrMsg, E.Message);
      end;
    end;
  finally
    Buffer := PhysDbErrors[Result]; { this seems necessary for 32-bit, debug mode only }
  end;
  if (InfoPtr <> nil) then begin
    with InfoPtr^ do begin
      AddToLogFmt('  InfoPtr.NFields:            [%d]', [NFields]);
      AddToLogFmt('  InfoPtr.NBytesInPhysRecord: [%d]', [NBytesInPhysRecord]);
      for i := 0 to pred(NFields) do begin
        AddToLogFmt('  FieldName[%d]:               [%s]', [i, FieldInfo^[i].Name]);
      end;
      AddBlockToLog('  InfoPtr.FieldInfo', FieldInfo, sizeOf(TPhysDbFieldInfo) * NFields);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function FreeDataFileInfo(
           var InfoPtr : PPhysDbFileInfo;
               ErrMsg  : PAnsiChar) : TPhysDbError;
var
  i : Integer;
begin
  AddToLog('FreeDataFileInfo');
  Result := errPhysDbNoError;
  try
    if Assigned(InfoPtr) then begin
      with InfoPtr^ do begin
        FFStrDispose(TableName);
        if Assigned(FieldInfo) then begin
          for I := 0 to pred(NFields) do begin
            FFStrDispose(FieldInfo^[I].Name);
            FFStrDispose(FieldInfo^[I].Picture);
          end;
          FFFreeMem(FieldInfo, Sizeof(TPhysDbFieldInfo) * NFields);
        end;
      end;
      FFFreeMem(InfoPtr, sizeof(TPhysDbFileInfo));
    end;
    InfoPtr := nil;
  except
    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function FetchDataFileIndexInfo(
           FileHandle: PPhysDbFileHandle;
           InfoPtr: PPhysDbFileInfo;
           var IndexesPtr: PPhysDbIndexesInfo;
           ErrMsg: PAnsiChar) : TPhysDbError;
var
  I : Integer;


  function FetchIndexInfo: TPhysDbError;
  var
    Index       : integer;
    IndexDesc   : IDXDesc;
    FFIndexDesc : TffIndexDescriptor;
    FieldN      : Integer;
  begin
    with IndexesPtr^ do begin
      for Index := 1 to NIndexes do begin                             {!!.02}
        FFIndexDesc := TFFProxyCursor(FileHandle^.CursorID).Dictionary.IndexDescriptor[Index]^;
        GetBDEIndexDescriptor(FFIndexDesc, IndexDesc);

        with IndexInfo^[Pred(Index)] do begin                         {!!.02}
          ValuesUnique := IndexDesc.bUnique;
          Ascending := not IndexDesc.bDescending;

          { Allocate space for the filename }
          if StrLen(IndexDesc.szName) <> 0 then begin
            IndexFileName := FFStrAlloc(StrLen(IndexDesc.szName) + 1);
            OemToAnsi(IndexDesc.szName, IndexFilename);
          end;

          { Allocate space for the tagname }
          if StrLen(IndexDesc.szTagName) <> 0 then begin
            TagName := FFStrAlloc(StrLen(IndexDesc.szTagName) + 1);
            OemToAnsi(IndexDesc.szTagName, TagName);
          end;

          IndexType := IndexDesc.iKeyExpType;
          CaseSensitive := not IndexDesc.bCaseInsensitive;

          if IndexDesc.bExpIdx then begin
            { omitted a bunch}
          end
          else begin
            DefaultIndexFileName := not Assigned(IndexFileName);
            DefaultTagName := not Assigned(TagName);
            IndexExpr := nil;
            EstimatedNBytesInexpr := 0;

            NFields := IndexDesc.iFldsInKey;

            { Allocate the output list structure }
            FFGetZeroMem(FieldNumInFile, SizeOf(TcrInt16u) * NFields);

            for FieldN := 0 to pred(NFields) do
              FieldNuminFile^[FieldN] := IndexDesc.aiKeyFld[FieldN] - 1; {!!.02}
          end;
        end;
      end;
    end;
    Result := errPhysDbNoError;
  end;

begin
  AddToLog('FetchDataFileIndexInfo');
  Result := errPhysDbNoError;

  { Allocate the index info structure }
  try
    FFGetZeroMem(IndexesPtr, SizeOf(TPhysDbIndexesInfo));
    with IndexesPtr^ do begin

      { Get number of indexes in the table minus the SEQ Idx}         {!!.02}
      NIndexes := TFFProxyCursor(FileHandle^.CursorID).Dictionary.IndexCount - 1;  {!!.02}
      AddToLogFmt('  File Name:           [%s]', [FileHandle^.PathAndFileName]);
      AddToLogFmt('  CursorID:            [%d]', [FileHandle^.CursorID]);

      if NIndexes > 0 then begin

        { Allocate the index info structures }
        FFGetZeroMem(IndexInfo, SizeOf(TPhysDbIndexInfo) * NIndexes);

        Result := FetchIndexInfo;
        if Result <> errPhysDbNoError then SysUtils.Abort;
      end;
    end;
  except  { InfoPtr error handler }
    on EOutOfMemory do begin
      Result := errPhysDbNotEnoughMemory;
      FreeDataFileIndexInfo(IndexesPtr, ErrMsg);
      StrPCopy(ErrMsg, '');
    end;

    on E: Exception do begin
      FreeDataFileIndexInfo(IndexesPtr, ErrMsg);
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (IndexesPtr <> nil) then begin
    with IndexesPtr^ do begin
      AddToLogFmt('  IndexesPtr.NIndexes: [%d]', [NIndexes]);
      for i := 0 to pred(NIndexes) do begin
        AddToLogFmt('  IndexName[%d]:        [%s]', [i, IndexInfo^[i].IndexFileName]);
      end;
      AddBlockToLog('  IndexesPtr.IndexInfo', IndexInfo, sizeOf(TPhysDbIndexInfo) * NIndexes);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function FreeDataFileIndexInfo(
           var IndexesPtr: PPhysDbIndexesInfo;
           ErrMsg: PAnsiChar) : TPhysDbError;
var
  I: Integer;
begin
  AddToLog('FreeDataFileIndexInfo');
  Result := errPhysDbNoError;
  try
    if Assigned(IndexesPtr) then begin
      if Assigned(IndexesPtr^.IndexInfo) then begin
        for I := 0 to pred(IndexesPtr^.NIndexes) do
          with IndexesPtr^.IndexInfo^[I] do begin
            FFFreeMem(FieldNumInFile, SizeOf(Word) * NFields);
            FFStrDispose(IndexExpr);
            FFStrDispose(IndexFileName);
            FFStrDispose(TagName);
          end;

        FFFreeMem(IndexesPtr^.IndexInfo, SizeOf(TPhysDbIndexInfo) * IndexesPtr^.NIndexes);
      end;
      FFFreeMem(IndexesPtr, SizeOf(TPhysDbIndexesInfo));
      IndexesPtr := nil;
    end;
  except
    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function BuildAndExecSQLQuery(
           FileHandleList: PPhysDbFileHandleArray;
           FileInfoList: PPhysDbFileInfoArray;
           LinkNonSQLFlags: PcrBooleanArray;
           IndexesInfoList: PPhysDbIndexesInfoArray;
           RangeInfoList: PPhysDbRangeInfoArray;
           NFiles: Word;
           LinkInfoList: PPhysDbFileLinkInfoArray;
           NFileLinks: Word;
           SqlDrivingFile: TcrBoolean;
           ErrMsg: PAnsiChar) : TPhysDbError;
begin
  AddToLog('BuildAndExecSQLQuery');
  { This is what PDBBDE returns }
  Result := errPhysDbNotImplemented;
  AddResultToLog(Result);
end;

function InitDataFileForReadingVer17(
           FileHandle    : PPhysDbFileHandle;
           InfoPtr       : PPhysDbFileInfo;
           IndexesPtr    : PPhysDbIndexesInfo;
           RangeInfoList : PPhysDbRangeInfoArray;
           NRanges       : TcrInt16u;
       var CanDoRangeLimit : TcrBoolean;
           ErrMsg          : PAnsiChar) : TPhysDbError;
var
  FFError : TffResult;

  function CanDoRangeLimitOnField(
             FileHandle    : PPhysDbFileHandle;
             InfoPtr       : PPhysDbFileInfo;
             IndexesPtr    : PPhysDbIndexesInfo;
             RangeInfoList : PPhysDbRangeInfoArray;
             NRanges       : Word;
         var CanDoRangeLimit : TcrBoolean;
             ErrMsg          : PAnsiChar) : TPhysDbError;
  var
    IndexInfo             : TPhysDbIndexInfo;
    ContinueBuildStopKey  : Boolean;
    ContinueBuildStartKey : Boolean;
    StopKeyOffset         : integer;
    StartKeyLen           : integer;
    NFieldsInStartKey     : integer;
    MinInclusive          : Boolean;

    RangeN      : integer;
    FieldN      : integer;
    TempPtr     : Pointer;
    TempBool    : TcrBoolean;
    IndexFieldN : integer;
    RangeFieldN : integer;

    SearchCond : TffSearchKeyAction;

    function InitLimitRangeInfo(
               FileHandle    : PPhysDbFileHandle;
               RangeInfoList : PPhysDbRangeInfoArray;
               RangeIndex    : integer;
               RangeFieldN   : integer;
               FieldInfo     : PPhysDbFieldInfo;
           var ContinueBuildStartKey : Boolean;
           var StartKeyLen           : integer;
           var ContinueBuildStopKey  : Boolean;
           var StopKeyOffset         : integer;
               ErrMsg                : PAnsiChar) : TPhysDbError;
    begin
      Result := errPhysDbNoError;

      with FileHandle^.ReadInfo^.RangeFieldInfo^[RangeIndex] do begin
        FieldNo             := RangeFieldN;
        OffsetInRecord      := FieldInfo^.OffsetInIndexRecord;
        FieldLength         := FieldInfo^.NBytesInNativeField;
        FieldType           := FieldInfo^.FieldType;
        NativeFieldOffset   := FieldInfo^.NativeFieldOffset;
        NativeFieldType     := FieldInfo^.NativeFieldType;
        NBytesInNativeField := FieldINfo^.NBytesInNativeField;
      end;

      with RangeInfoList^[RangeIndex].FieldRanges^[0] do begin
        if ContinueBuildStartKey and Assigned(MinFieldValue) then
          Inc(StartKeyLen, FieldInfo^.NBytesInNativeField)
        else
          ContinueBuildStartKey := False;

        if ContinueBuildStopKey and Assigned(MaxFieldValue) then begin
          with FileHandle^.ReadInfo^.RangeFieldInfo^[RangeIndex] do begin
            { Makes no sense to me; we already did this }
            FieldNo                 := RangeFieldN;
            OffsetInRecord          := FieldInfo^.OffsetInIndexRecord;
            FieldLength             := FieldInfo^.NBytesInNativeField;
            FieldType               := FieldInfo^.FieldType;
            NativeFieldOffset       := FieldInfo^.NativeFieldOffset;
            NativeFieldType         := FieldInfo^.NativeFieldType;
            NBytesInNativeField     := FieldInfo^.NBytesInNativeField;
            NDecPlacesInNativeField := FieldInfo^.NDecPlacesInNativeField;
            OffsetInStopKeyBuf      := StopKeyOffset;
            StopInclusive           := RangeInfoList^[RangeIndex].FieldRanges^[0].MaxInclusive;
          end;
          Inc(StopKeyOffset, FieldInfo^.NBytesInNativeField);
          FileHandle^.ReadInfo^.StopKeyLen := StopKeyOffset;
          Inc(FileHandle^.ReadInfo^.NStopKeyRanges);
        end
        else
          ContinueBuildStopKey := False;
      end;
    end;

    function BuildStringRanges(
               FileHandle: PPhysDbFileHandle;
               RangeInfoList: PPhysDbRangeInfoArray;
               RangeIndex: TcrInt16u;
               RangeFieldN: TcrInt16u;
               FieldInfo: PPhysDbFieldInfo;
               ErrMsg: PAnsiChar) : TPhysDbError;
    var
      SavedOffset: TcrINt16u;
      KeyBuf,
      KeyBufOem,
      StartKeyBuf,
      StopKeyBuf: PAnsiChar;
    begin
      SavedOffset := StopKeyOffset;
      Result := InitLimitRangeInfo(FileHandle, RangeInfoList, RangeIndex,
                                   RangeFieldN, FieldInfo, ContinueBuildStartKey,
                                   StartKeyLen, ContinueBuildStopKey, StopKeyOffset,
                                   ErrMsg);
      if Result <> errPhysDbNoError then Exit;

      if ContinueBuildStartKey then begin
        KeyBuf := RangeInfoList^[RangeIndex].FieldRanges^[0].MinFieldValue;
        try
          KeyBufOem := FFStrAllocCopy(KeyBuf);
        except
          Result := errPhysDbNotEnoughMemory;
          Exit;
        end;

        try
          AnsiToOem(keyBufOem, keyBufOem);

          TFFProxyCursor(FileHandle^.CursorID).Dictionary.SetRecordField(RangeFieldN,
                                                                         FileHandle^.ReadInfo^.PhysRecordBuf,
                                                                         KeyBufOem);
        finally
          FFStrDispose(KeyBufOem);
        end;
      end;

      if ContinueBuildStopKey then begin
        KeyBuf := @FileHandle^.ReadInfo^.StopKeyBuf[SavedOffset];
        StrCopy(KeyBuf, @RangeInfoList^[RangeIndex].FieldRanges^[0].MaxFieldValue);
        AnsiToOem(keyBuf, keyBuf);
      end;

       { If current field of min and max range in index are not equal, do not
         try to build stop key. }

       with RangeInfoList^[RangeIndex].FieldRanges^[0] do begin
         StartKeyBuf := @MinFieldValue;
         StopKeyBuf := @MaxFieldValue;
       end;

       if not Assigned(StartKeyBuf) or
          not Assigned(StopKeyBuf) or
          (StrComp(StartKeyBuf, StopKeyBuf) <> 0) then
         ContinueBuildStopKey := False;
    end;

    function BuildDateRanges(
               FileHandle: PPhysDbFileHandle;
               RangeInfoList: PPhysDbRangeInfoArray;
               RangeIndex: TcrInt16u;
               RangeFieldN: TcrInt16u;
               FieldInfo: PPhysDbFieldInfo;
               ErrMsg: PAnsiChar) : TPhysDbError;
    var
      SavedOffset : TcrInt16u;
      Year        : TcrInt16s;
      Month       : TcrInt16u;
      Day         : TcrInt16u;
      DateValue   : TDbiDate;
      FieldLen    : TcrInt16u;
      KeyBuf      : PDBIDate;
      StartKeyBuf : PDBIDate;
      StopKeyBuf  : PDbiDate;
    begin
      SavedOffset := StopKeyOffset;

      Result := InitLimitRangeInfo(FileHandle, RangeInfoList, RangeIndex,
                                   RangeFieldN, FieldInfo, ContinueBuildStartKey,
                                   StartKeyLen, ContinueBuildStopKey, StopKeyOffset,
                                   ErrMsg);
      if Result <> errPhysDbNoError then Exit;

      if ContinueBuildStartKey then begin
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          CrDateToYearMonthDay(TcrDate(MinFieldValue^), Year, Month, Day);
        DateValue := FFBDEDateEncode(Day, Month, Year);
        TFFProxyCursor(FileHandle^.CursorID).Dictionary.SetRecordField(RangeFieldN,
                                                                       FileHandle^.ReadInfo^.PhysRecordBuf,
                                                                       @DateValue);
      end;

      if ContinueBuildStopKey then begin
        FieldLen := StopKeyOffset - SavedOffset;
        KeyBuf := PDbiDate(@FileHandle^.ReadInfo^.StopKeyBuf[SavedOffset]);
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          CrDateToYearMonthDay(TcrDate(MaxFieldValue^), Year, Month, Day);
        DateValue := FFBDEDateEncode(Day, Month, Year);
        Move(DateValue, KeyBuf^, FieldLen);
      end;

      { If current field of min and max range in index are not equal, do not
        try to build stop key. }

       with RangeInfoList^[RangeIndex].FieldRanges^[0] do begin
         StartKeyBuf := PDbiDate(@MinFieldValue);
         StopKeyBuf := PDbiDate(@MaxFieldValue);
       end;

       if not Assigned(StartKeyBuf) or
          not Assigned(StopKeyBuf) or
          (StartKeyBuf^ <> StopKeyBuf^) then
         ContinueBuildStopKey := False;
    end;

    function BuildIntegerRanges(
               FileHandle: PPhysDbFileHandle;
               RangeInfoList: PPhysDbRangeInfoArray;
               RangeIndex: TcrInt16u;
               RangeFieldN: TcrInt16u;
               FieldInfo: PPhysDbFieldInfo;
               ErrMsg: PAnsiChar) : TPhysDbError;
    var
      SavedLen,
      SavedOffset: TcrInt16u;
      FieldLen: TcrInt16u;
      KeyBuf: PAnsiChar;
      StartKeyValue,
      StopKeyValue: TcrInt32s;
      ShortValue: TcrInt16s;
      LongValue: TcrInt32s;
    begin
      SavedLen := StartKeyLen;
      SavedOffset := StopKeyOffset;

      Result := InitLimitRangeInfo(FileHandle, RangeInfoList, RangeIndex,
                                   RangeFieldN, FieldInfo, ContinueBuildStartKey,
                                   StartKeyLen, ContinueBuildStopKey, StopKeyOffset,
                                   ErrMsg);
      if Result <> errPhysDbNoError then Exit;

      StartKeyValue := 0;
      StopKeyValue := 0;

      if ContinueBuildStartKey then begin
        FieldLen := StartKeyLen - SavedLen;
        if FieldLen = 2 then begin
          with RangeInfoList^[RangeIndex].FieldRanges^[0] do
            ShortValue := TcrInt16s(MinFieldValue^);
          StartKeyValue := ShortValue;
          TFFProxyCursor(FileHandle^.CursorID).Dictionary.SetRecordField(RangeFieldN,
                                                                         FileHandle^.ReadInfo^.PhysRecordBuf,
                                                                         @ShortValue);
        end
        else begin
          with RangeInfoList^[RangeIndex].FieldRanges^[0] do
            LongValue := TcrInt32s(MinFieldValue^);
          StartKeyValue := LongValue;
          TFFProxyCursor(FileHandle^.CursorID).Dictionary.SetRecordField(RangeFieldN,
                                                                         FileHandle^.ReadInfo^.PhysRecordBuf,
                                                                         @LongValue);
        end;
      end;

      if ContinueBuildStopKey then begin
        FieldLen := stopKeyOffset - SavedOffset;
        KeyBuf := @FileHandle^.ReadInfo^.StopKeyBuf[SavedOffset];
        if FieldLen = 2 then begin
          with RangeInfoList^[RangeIndex].FieldRanges^[0] do
            ShortValue := TcrInt16s(MaxFieldValue^);
          StopKeyValue := ShortValue;
          Move(ShortValue, KeyBuf^, FieldLen);
        end
        else begin
          with RangeInfoList^[RangeIndex].FieldRanges^[0] do
            LongValue := TcrInt32s(MaxFieldValue^);
          StopKeyValue := LongValue;
          Move(LongValue, KeyBuf^, FieldLen);
        end;
      end;

      { If current field of min and max range in index are not equal, do not
        try to build stop key. }
      if ContinueBuildStopKey and ContinueBuildStartKey then
        if StartKeyValue <> StopKeyValue then
          ContinueBuildStopKey := False;
    end;

    function BuildDoubleRanges(
               FileHandle: PPhysDbFileHandle;
               RangeInfoList: PPhysDbRangeInfoArray;
               RangeIndex: TcrInt16u;
               RangeFieldN: TcrInt16u;
               FieldInfo: PPhysDbFieldInfo;
               ErrMsg: PAnsiChar) : TPhysDbError;
    var
      SavedOffset: TcrInt16u;
      DoubleValue: Double;
      FieldLen: TcrInt16u;
      KeyBuf: PAnsiChar;
      StartKeyBuf,
      StopKeyBuf: PcrNumber;
    begin
      SavedOffset := StopKeyOffset;

      Result := InitLimitRangeInfo(FileHandle, RangeInfoList, RangeIndex,
                                   RangeFieldN, FieldInfo, ContinueBuildStartKey,
                                   StartKeyLen, ContinueBuildStopKey, StopKeyOffset,
                                   ErrMsg);
      if Result <> errPhysDbNoError then Exit;

      if ContinueBuildStartKey then begin
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          DoubleValue := NumberToDouble(TcrNumber(MinFieldValue^));
        TFFProxyCursor(FileHandle^.CursorID).Dictionary.SetRecordField(RangeFieldN,
                                                                       FileHandle^.ReadInfo^.PhysRecordBuf,
                                                                       @DoubleValue);
      end;

      if ContinueBuildStopKey then begin
        FieldLen := StopKeyOffset - SavedOffset;
        KeyBuf := @FileHandle^.ReadInfo^.StopKeyBuf[SavedOffset];
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          DoubleValue := NumberToDouble(TcrNumber(MaxFieldValue^));
        Move(DoubleValue, KeyBuf^, FieldLen);
      end;

      { If current field of min and max range in index are not equal, do not
        try to build stop key. }

      with RangeInfoList^[RangeIndex].FieldRanges^[0] do begin
        StartKeyBuf := PcrNumber(@MinFieldValue);
        StopKeyBuf := PcrNumber(@MaxFieldValue);
      end;

      if not Assigned(StartKeyBuf) or
         not Assigned(StopKeyBuf) or
         (StartKeyBuf^ <> StopKeyBuf^) then
        ContinueBuildStopKey := False;
    end;

    function BuildDecimalRanges(
               FileHandle: PPhysDbFileHandle;
               RangeInfoList: PPhysDbRangeInfoArray;
               RangeIndex: TcrInt16u;
               RangeFieldN: TcrInt16u;
               FieldInfo: PPhysDbFieldInfo;
               ErrMsg: PAnsiChar) : TPhysDbError;
(*
    var
      SavedLen,
      SavedOffset: TcrInt16u;
      FieldLen: TcrInt16u;
      KeyBuf: PAnsiChar;
      DoubleValue: Double;
      StartKeyBuf,
      StopKeyBuf: PcrNumber;
*)
    begin
      Result := errPhysDbNoError;
(*      SavedLen := StartKeyLen;
      SavedOffset := StopKeyOffset;

      Result := InitLimitRangeInfo(FileHandle, RangeInfoList, RangeIndex,
                                   RangeFieldN, FieldInfo, ContinueBuildStartKey,
                                   StartKeyLen, ContinueBuildStopKey, StopKeyOffset,
                                   ErrMsg);
      if Result <> errPhysDbNoError then Exit;

      if ContinueBuildStartKey then begin
        FieldLen := StartKeyLen - SavedLen;
        KeyBuf := @FileHandle^.ReadInfo^.KeyBuf[SavedLen];
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          DoubleValue := NumberToDouble(TcrNumber(MinFieldValue^));
        if Doublevalue < 0 then begin
          ContinueBuildStartKey := False;
          StartKeyLen := SavedLen;
        end
        else
          {
          DoubleToDecimal(FileHandle, DoubleValue, KeyBuf, FieldLen,
                          FieldInfo^.NDecPlacesInNativeField)};
      end;

      if ContinueBuildStopKey then begin
        FieldLen := StopKeyOffset - SavedOffset;
        KeyBuf := @FileHandle^.ReadInfo^.StopKeyBuf[SavedOffset];
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          DoubleValue := NumberToDouble(TcrNumber(MaxFieldValue^));
        Move(DoubleValue, KeyBuf^, FieldLen);
      end;

      { If current field of min and max range in index are not equal, do not
        try to build stop key. }

      with RangeInfoList^[RangeIndex].FieldRanges^[0] do begin
        StartKeyBuf := PcrNumber(@MinFieldValue);
        StopKeyBuf := PcrNumber(@MaxFieldValue);
      end;

      if not Assigned(StartKeyBuf) or
         not Assigned(StopKeyBuf) or
         (StartKeyBuf^ <> StopKeyBuf^) then
        ContinueBuildStopKey := False;*)
    end;

    function BuildTimeRanges(
               FileHandle: PPhysDbFileHandle;
               RangeInfoList: PPhysDbRangeInfoArray;
               RangeIndex: TcrInt16u;
               RangeFieldN: TcrInt16u;
               FieldInfo: PPhysDbFieldInfo;
               ErrMsg: PAnsiChar) : TPhysDbError;
(*
    var
      SavedLen,
      SavedOffset: TcrInt16u;
      KeyBuf: PAnsiChar;
      TimeValue: TcrInt32s;
      TimeValueN: TcrNumber;
      StartKeyBuf,
      StopKeyBuf: PcrNumber;
*)
    begin
      Result := errPhysDbNoError;
(*     SavedLen := StartKeyLen;
      SavedOffset := StopKeyOffset;

      Result := InitLimitRangeInfo(FileHandle, RangeInfoList, RangeIndex,
                                   RangeFieldN, FieldInfo, ContinueBuildStartKey,
                                   StartKeyLen, ContinueBuildStopKey, StopKeyOffset,
                                   ErrMsg);
      if Result <> errPhysDbNoError then Exit;

      if ContinueBuildStartKey then begin
        KeyBuf := @FileHandle^.ReadInfo^.KeyBuf[SavedLen];
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          TimeValue := TBrahmaNumber(MinFieldValue^);
        if TimeValue < 0 then begin
          ContinueBuildStartKey := False;
          StartKeyLen := SavedLen;
        end
        else
          Convert2BTTime(TimeValue, KeyBuf);
      end;

      if ContinueBuildStopKey then begin
        KeyBuf := @FileHandle^.ReadInfo^.StopKeyBuf[SavedOffset];
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          TimeValueN := TBrahmaNumber(MaxFieldValue^);
        if TimeValue < 0 then begin
          ContinueBuildStopKey := False;
          StartKeyLen := SavedLen;
        end
        else
          Move(TimeValueN, KeyBuf, SizeOf(TBrahmaNumber));
      end;

      { If current field of min and max range in index are not equal, do not
        try to extend the stop key. }

      with RangeInfoList^[RangeIndex].FieldRanges^[0] do begin
        StartKeyBuf := PBrahmaNumber(@MinFieldValue);
        StopKeyBuf := PBrahmaNumber(@MaxFieldValue);
      end;

      if not Assigned(StartKeyBuf) or
         not Assigned(StopKeyBuf) or
         (StartKeyBuf^ <> StopKeyBuf^) then
        ContinueBuildStopKey := False;*)
    end;

    function BuildLogicalRanges(
               FileHandle: PPhysDbFileHandle;
               RangeInfoList: PPhysDbRangeInfoArray;
               RangeIndex: TcrInt16u;
               RangeFieldN: TcrInt16u;
               FieldInfo: PPhysDbFieldInfo;
               ErrMsg: PAnsiChar) : TPhysDbError;
    var
      SavedLen,
      SavedOffset,
      FieldLen: TcrInt16u;
      KeyBuf: PAnsiChar;
      LogicalValue: TcrBoolean;
      StartKeyValue,
      StopKeyValue: TcrBoolean;
    begin
      SavedLen := StartKeyLen;
      SavedOffset := StopKeyOffset;

      Result := InitLimitRangeInfo(FileHandle, RangeInfoList, RangeIndex,
                                   RangeFieldN, FieldInfo, ContinueBuildStartKey,
                                   StartKeyLen, ContinueBuildStopKey, StopKeyOffset,
                                   ErrMsg);
      if Result <> errPhysDbNoError then Exit;

      if ContinueBuildStartKey then begin
        FieldLen := StartKeylen - SavedLen;
        KeyBuf := @FileHandle^.ReadInfo^.KeyBuf[SavedLen];
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          LogicalValue := TcrBoolean(MinFieldValue^);
        if FieldLen = 1 then
          TcrInt8u(KeyBuf^) := Ord(LogicalValue)
        else
          PcrInt16u(KeyBuf)^ := Ord(LogicalValue);
      end;

      if ContinueBuildStopKey then begin
        FieldLen := StopKeyOffset - SavedOffset;
        KeyBuf := @FileHandle^.ReadInfo^.StopKeyBuf[SavedOffset];
        with RangeInfoList^[RangeIndex].FieldRanges^[0] do
          LogicalValue := TcrBoolean(MaxFieldValue^);
        if FieldLen = 1 then
          TcrInt8u(KeyBuf^) := Ord(LogicalValue)
        else
          PcrInt16u(KeyBuf)^ := Ord(LogicalValue);
      end;

      { If current field of min and max range in index are not equal, do not
        try to extend the stop key. }

      with RangeInfoList^[RangeIndex].FieldRanges^[0] do begin
        StartKeyValue := TcrBoolean(@MinFieldValue);
        StopKeyValue := TcrBoolean(@MaxFieldValue);
      end;

      if StartKeyValue = StopKeyValue then
        ContinueBuildStopKey := False;
    end;

  begin
    CanDoRangeLimit := false;
    Result := errPhysDbNoError;
    if NRanges = 0 then Exit;

    with FileHandle^.ReadInfo^ do
      FillChar(KeyBuf, SizeOf(KeyBuf), #0);

    StopKeyOffset         := 0;
    StartKeyLen           := 0;
    NFieldsInStartKey     := 0;
    ContinueBuildStopKey  := True;
    ContinueBuildStartKey := True;
    MinInclusive          := True;

    FileHandle^.ReadInfo^.AscendingIndex :=
      IndexesPtr^.IndexInfo^[IndexesPtr^.IndexInUse].Ascending;

    { Swap the begin and end range key, when descending index }
    if not FileHandle^.ReadInfo^.AscendingIndex then begin
      AddToLog('  swapping begin and end range key');
      for RangeN := 0 to pred(NRanges) do begin
        with RangeInfoList^[RangeN] do begin
          for FieldN := 0 to pred(RangeInfoList^[RangeN].NFieldRanges) do begin
            with RangeInfoList^[RangeN].FieldRanges^[FieldN] do begin
              TempPtr := MinFieldValue;
              MinFieldValue := MaxFieldValue;
              MaxFieldValue := TempPtr;

              TempBool := MinInclusive;
              MinInclusive := MaxInclusive;
              MaxInclusive := TempBool;
            end;
          end;
        end;
      end;
    end;

    { Start to do the range search }
    IndexInfo := IndexesPtr^.IndexInfo^[IndexesPtr^.IndexInUse];
    for RangeN := 0 to pred(NRanges) do begin
      RangeFieldN := IndexInfo.FieldNumInFile^[RangeN];

      if not RangeInfoList^[RangeN].SelectIfWithinRange or
         (RangeInfoList^[RangeN].NFieldRanges <> 1) then
        Break;

      case InfoPtr^.FieldInfo^[RangeFieldN].NativeFieldType of
        fldZSTRING:
          begin
            CanDoRangeLimit := true;
            Result := BuildStringRanges(FileHandle, RangeInfoList, RangeN,
                        RangeFieldN, @InfoPtr^.FieldInfo^[RangeFieldN], ErrMsg);
          end;

        fldDATE:
          begin
            CanDoRangeLimit := true;
            Result := BuildDateRanges(FileHandle, RangeInfoList, RangeN,
                        RangeFieldN, @InfoPtr^.FieldInfo^[RangeFieldN], ErrMsg);
          end;

        fldINT16, fldINT32:
          begin
            CanDoRangeLimit := true;
            Result := BuildIntegerRanges(FileHandle, RangeInfoList, RangeN,
                        RangeFieldN, @InfoPtr^.FieldInfo^[RangeFieldN], ErrMsg);
          end;

        fldFLOAT, fldstMONEY:
          begin
            CanDoRangeLimit := true;
            Result := BuildDoubleRanges(FileHandle, RangeInfoList, RangeN,
                        RangeFieldN, @InfoPtr^.FieldInfo^[RangeFieldN], ErrMsg);
          end;

        fldBCD:
          begin
            CanDoRangeLimit := true;
            ShowMessage('BCD datatypes not supported for ranges');
            {Result := BuildDecimalRanges(FileHandle, RangeInfoList, RangeN,
                        RangeFieldN, @InfoPtr^.FieldInfo^[RangeFieldN], ErrMsg);}
          end;

        fldTIME:
          begin
            CanDoRangeLimit := true;
            ShowMessage('Time datatypes are not supported for ranges');
            {Result := BuildTimeRanges(FileHandle, RangeInfoList, RangeN,
                        RangeFieldN, @InfoPtr^.FieldInfo^[RangeFieldN], ErrMsg);}
          end;

        fldBOOL:
          begin
            CanDoRangeLimit := true;
            Result := BuildLogicalRanges(FileHandle, RangeInfoList, RangeN,
                        RangeFieldN, @InfoPtr^.FieldInfo^[RangeFieldN], ErrMsg);
          end;

        else CanDoRangeLimit := false;
      end;

      if ContinueBuildStartKey and
         not RangeInfoList^[RangeN].FieldRanges^[0].MinInclusive then
        MinInclusive := False;

      if (Result <> errPhysDbNoError) or not CanDoRangeLimit then
        Break;

      if ContinueBuildStartKey then
        Inc(NFieldsInStartKey)
      else
        TFFProxyCursor(FileHandle^.CursorID).Dictionary.SetRecordField(RangeFieldN,
                                                                       FileHandle^.ReadInfo^.PhysRecordBuf,
                                                                       nil);
    end;

    { Clear the remaining fields in index }
    for FieldN := NFieldsInStartKey to pred(IndexInfo.NFields) do begin
      IndexFieldN := IndexInfo.FieldNumInFile^[FieldN];
      TFFProxyCursor(FileHandle^.CursorID).Dictionary.SetRecordField(IndexFieldN,
                                                                     FileHandle^.ReadInfo^.PhysRecordBuf,
                                                                     nil);
    end;

    if (Result = errPhysDbNoError) and CanDoRangeLimit then begin
      if StartKeyLen > 0 then begin
        with TFFProxyCursor(FileHandle^.CursorID) do begin
          Dictionary.ExtractKey(IndexID,
                                @FileHandle^.ReadInfo^.PhysRecordBuf,
                                @FileHandle^.ReadInfo^.KeyBuf);
        end;

        if MinInclusive then
          SearchCond := skaEqual
        else
          SearchCond := skaGreater;

        FFError := ServerEngine.CursorSetToKey(FileHandle^.CursorID,
                                               SearchCond,
                                               True,
                                               NFieldsInStartKey,
                                               0,
                                               @FileHandle^.ReadInfo^.KeyBuf);
        AddToLogFmt('  CursorSetToKey: [%d]', [FFError]);
        if FFError = DBIERR_NONE then begin
          FFError := ServerEngine.CursorSetRange(FileHandle^.CursorID,
                                                 True,
                                                 NFieldsInStartKey,
                                                 0,
                                                 @FileHandle^.ReadInfo^.KeyBuf,
                                                 MinInclusive,
                                                 0,
                                                 0,
                                                 nil,
                                                 True);
          AddToLogFmt('  CursorSetRange: [%d]', [FFError]);
        end;
      end else begin
        FFError := ServerEngine.CursorSetToBegin(FileHandle^.CursorID);
        AddToLogFmt('  CursorSetRange: [%d]', [FFError]);
      end;

      if FFError <> DBIERR_NONE then SysUtils.Abort;

      FileHandle^.RangeLimit := True;
    end;
  end;

  function InitReadInfoForRange(
             FileHandle    : PPhysDbFileHandle;
             InfoPtr       : PPhysDbFileInfo;
             IndexesPtr    : PPhysDbIndexesInfo;
             RangeInfoList : PPhysDbRangeInfoArray;
             NRanges       : Word;
         var CanDoRangeLimit : TcrBoolean;
             ErrMsg          : PAnsiChar) : TPhysDbError;
  begin
    Result := errPhysDbNoError;
    with fileHandle^ do begin
      RangeLimit := False;
      with ReadInfo^ do begin
        RangeFieldInfo := nil;
        NStopKeyRanges := 0;
        StopKeyLen := 0;

        if NRanges > 0 then begin
          IndexCaseSensitive := IndexesPtr^.IndexInfo^[IndexesPtr^.IndexInUse].CaseSensitive;

          { Allocate structure for range field info }
          FFGetZeroMem(RangeFieldInfo, SizeOf(TPhysDbReadFieldInfo) * NRanges);
          Result := CanDoRangeLimitOnField(FileHandle, InfoPtr, IndexesPtr,
                                           RangeInfoList, NRanges,
                                           CanDoRangeLimit, ErrMsg);
        end;
      end;
    end;
  end;

var
  ReadFieldNo  : integer;
  IndexFieldNo : integer;
  FieldN       : integer;
begin  { InitDataFileForReadingVer17 }
  AddToLog('InitDataFileForReadingVer17');
  AddToLogFmt('  PathAndFilename:        [%s]', [FileHandle^.PathAndFileName]);
  AddToLogFmt('  CursorID:               [%d]', [FileHandle^.CursorID]);
  if Assigned(IndexesPtr) then
    AddToLogFmt('  IndexesPtr^.IndexInUse: [%d]', [IndexesPtr^.IndexInUse]);
  AddToLogFmt('  NRanges:                [%d]', [NRanges]);
  if Assigned(RangeInfoList) then begin
    AddBlockToLog('  RangeInfoList^[0]: ', @RangeInfoList^[0], SizeOf(TPhysDbRangeInfo));
    with RangeInfoList^[0] do begin
      AddToLogFmt('  RangeInfoList^[0].FieldName:           [%s]', [FieldName]);
      AddToLogFmt('  RangeInfoList^[0].BrahmaType:          [%s]', [FieldValueTypes[BrahmaType]]);
      AddToLogFmt('  RangeInfoList^[0].BrahmaFieldLen:      [%d]', [BrahmaFieldLen]);
      AddToLogFmt('  RangeInfoList^[0].SelectIfWithinRange: [%s]', [BoolToStr(SelectIfWithinRange)]);
      AddToLogFmt('  RangeInfoList^[0].NFieldRanges:        [%d]', [NFieldRanges]);
    end;
  end;

  Result := errPhysDbNoError;
  try
    try
      CanDoRangeLimit := false;
      with FileHandle^ do begin
        ReadInfo := nil;

        { Allocate structure for read state info }
        FFGetZeroMem(ReadInfo, SizeOf(TPhysDbReadInfo));

        MainFile := True;
        ReadInfo^.NumRanges := NRanges;

        with ReadInfo^ do begin
          ValuesUnique := true;
          NBytesInReadRecord := InfoPtr^.NBytesInReadRecord;
          NFieldsInReadRecord := InfoPtr^.NFieldsInReadRecord;
          NBytesInIndexRecord := InfoPtr^.NBytesInIndexRecord;
          NFieldsInIndexRecord := InfoPtr^.NFieldsInIndexRecord;
          NBytesInPhysRecord := InfoPtr^.NBytesInPhysRecord + 1;

          { Allocate the physical record buffer }
          FFGetZeroMem(PhysRecordBuf, NBytesInPhysRecord);

          { Position at first record of file for subsequent reading }
          CurrentRecord := 0;

          { Allocate structure for read state information per translated field }
          FFGetZeroMem(FieldInfo, SizeOf(TPhysDbReadFieldInfo) * InfoPtr^.NFieldsInReadRecord);

          { Allocate structure for read state information per untranslated field }
          FFGetZeroMem(IndexFieldInfo, SizeOf(TPhysDbReadFieldInfo) * InfoPtr^.NFieldsInIndexRecord);

          NFieldsInIndexDefn := 0;
          IndexDefnInfo := nil;

          { Pass through complete file info structure to find all (translated)
            read record fields and (untranslated) index record fields. }
          ReadFieldNo := 0;
          IndexFieldNo := 0;
          for FieldN := 0 to pred(InfoPtr^.NFields) do begin
            with InfoPtr^.FieldInfo^[FieldN] do begin
              if UsedInReadRecord then begin

                { At a field to be translated in read record }
                FieldInfo^[ReadFieldNo].FieldNo := FieldN;
                FieldInfo^[ReadFieldNo].ReadFieldNo := ReadFieldNo;
                FieldInfo^[ReadFieldNo].OffsetInRecord := OffsetInReadRecord;
                FieldInfo^[ReadFieldNo].FieldType := FieldType;
                FieldInfo^[ReadFieldNo].FieldLength := NBytesInField;
                FieldInfo^[ReadFieldNo].NativeFieldOffset := NativeFieldOffset;
                FieldInfo^[ReadFieldNo].NativeFieldType := NativeFieldType;
                FieldInfo^[ReadFieldNo].NBytesInNativeField := NBytesInNativeField;
                FieldInfo^[ReadFieldNo].NDecPlacesInNativeField := NDecPlacesInNativeField;
                Inc(ReadFieldNo);
              end;

              if UsedInIndexRecord then begin

                { At a field to be untranslated in index record }
                IndexFieldInfo^[IndexFieldNo].FieldNo := FieldN;
                IndexFieldInfo^[IndexFieldNo].ReadFieldNo := IndexFieldNo;
                IndexFieldInfo^[IndexFieldNo].OffsetInRecord := OffsetInIndexRecord;
                IndexFieldInfo^[IndexFieldNo].FieldType := FieldType;
                IndexFieldInfo^[IndexFieldNo].FieldLength := NBytesInNativeField;
                IndexFieldInfo^[IndexFieldNo].NativeFieldOffset := NativeFieldOffset;
                IndexFieldInfo^[IndexFieldNo].NativeFieldType := NativeFieldType;
                IndexFieldInfo^[IndexFieldNo].NBytesInNativeField := NBytesInNativeField;
                IndexFieldInfo^[IndexFieldNo].NDecPlacesInNativeField := NDecPlacesInNativeField;
                Inc(IndexFieldNo);
              end;
            end;
          end;
        end;
      end;

      Result := InitReadInfoForRange(FileHandle, InfoPtr, IndexesPtr,
                  RangeInfoList, NRanges, CanDoRangeLimit, ErrMsg);
      if Result <> errPhysDbNoError then
        raise Exception.Create(StrPas(ErrMsg));
      AddToLogFmt('  CanDoRangeLimit:        [%s]', [BoolToStr(CanDoRangeLimit)]);
    except
      on EOutOfMemory do begin
        Result := errPhysDbNotEnoughMemory;
        TermDataFileForReading(FileHandle, ErrMsg);
        StrPCopy(ErrMsg, '');
      end;

      on E: Exception do begin
        if Result = errPhysDbNoError then
          Result := errPhysDbErrMsgReturned;
        TermDataFileForReading(FileHandle, ErrMsg);
        StrPCopy(ErrMsg, E.Message);
        if FFError <> DBIERR_NONE then
          Result := IDAPIError(FFError, ErrMsg);
      end;
    end;
  finally
    StrPCopy(DebugBuff, PhysDbErrors[Result]); { this seems necessary for 32-bit }
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function InitDataFileAndIndexForReadV115(
           FileHandle: PPhysDbFileHandle;
           InfoPtr: PPhysDbFileInfo;
           IndexesPtr: PPhysDbIndexesInfo;
           LookupOptPtr: PPhysDbLookupOptInfo;
           ErrMsg: PAnsiChar) : TPhysDbError;
var
  CanDoRangeLimit: TcrBoolean;

{ This function serves the same purpose as InitDataFileForReading, but
  is called when initializing reading from a file with an index,
  whereas InitDataFileForReading is called when reading from a file
  without.  The index info structure (from FetchDataFileIndexInfo) is
  passed to this function to identify the chosen index. }

  function InitReadInfoForIndex: TPhysDbError;
  var
    IndexInfo: TPhysDbIndexInfo;
    IndexOffset,
    FieldIndex,
    FieldN: integer;
  begin
    Result := errPhysDbNoError;

    if IndexesPtr^.NIndexes = 0 then begin
      Result := errPhysDbFileIntegrityError;
      Exit;
    end;

    { Allocate structure to save information on index fields }
    IndexInfo := IndexesPtr^.IndexInfo^[IndexesPtr^.IndexInUse];

    with FileHandle^.ReadInfo^ do begin
      ValuesUnique := IndexInfo.ValuesUnique;
      IndexCaseSensitive := IndexInfo.CaseSensitive;
      NFieldsInIndexDefn := IndexInfo.NFields;
      FFGetZeroMem(IndexDefnInfo, SizeOf(TPhysDbReadFieldInfo) * IndexInfo.NFields);

      { Default number of lookup fields to same as index }
      NFieldsInLookupValue := NFieldsInIndexDefn;
      LookupValueLen := LookupOptPtr^.LookupValueLen;
      LastLookupFieldLen := 0;
      LastLookupFieldIsSubstr := false;

      IndexOffset := 0;
      for FieldN := 0 to pred(IndexInfo.NFields) do begin
        FieldIndex := IndexInfo.FieldNumInFile^[FieldN];
        IndexDefnInfo^[FieldN].FieldNo := FieldIndex;
        IndexDefnInfo^[FieldN].OffsetInRecord := IndexOffset;
        IndexDefnInfo^[FieldN].FieldLength := InfoPtr^.FieldInfo^[FieldIndex].NBytesInNativeField;
        IndexDefnInfo^[FieldN].FieldType := InfoPtr^.FieldInfo^[FieldIndex].FieldType;

        { Detect if we have link on partial number of fields }
        if IndexDefnInfo^[FieldN].OffsetInRecord >= LookupOptPtr^.LookupValueLen then
          if NFieldsInLookupValue = NFieldsInIndexDefn then
            if FieldN > 0 then
              NFieldsInLookupValue := FieldN;
        IndexOffset := IndexOffset + InfoPtr^.FieldInfo^[FieldIndex].NBytesInNativeField;
      end;

      { Detect if we have link to a partial string field at the end
        of lookup value. }
      if (IndexDefnInfo^[IndexInfo.NFields - 1].FieldType = ftStringField) and
         LookupOptPtr^.PartialMatch then
        LastLookupFieldIsSubstr := True;
    end;
  end;

begin
  AddToLog('InitDataFileAndIndexForReadV115');
  AddToLogFmt('  PathAndFilename: [%s]', [FileHandle^.PathAndFileName]);
  AddToLogFmt('  CursorID:        [%d]', [FileHandle^.CursorID]);
  AddBlockToLog('  LookupOptPtr^:', LookupOptPtr, SizeOf(LookupOptPtr^));

  Result := errPhysDbNoError;
  try

    { Perform same initialization as for no index file }
    Result := InitDataFileForReadingVer17(FileHandle, InfoPtr, IndexesPtr,
                                          nil, 0, CanDoRangeLimit, ErrMsg);
    if Result = errPhysDbNoError then begin

      { Perform index specific initialization }
      Result := InitReadInfoForIndex;
      if Result <> errPhysDbNoError then
        TermDataFileForReading(FileHandle, ErrMsg);
    end;

    FileHandle^.MainFile := False;
  except
    on EOutOfMemory do begin
      Result := errPhysDbNotEnoughMemory;
      StrPCopy(ErrMsg, '');
    end;

    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function TermDataFileForReading(
           FileHandle: PPhysDbFileHandle;
           ErrMsg: PAnsiChar) : TPhysDbError;
begin
  AddToLog('TermDataFileForReading');

  Result := errPhysDbNoError;
  try
    if Assigned(FileHandle) then begin
      AddToLogFmt('  FileName: [%s]', [FileHandle^.PathAndFileName]);
      AddToLogFmt('  CursorID: [%d]', [FileHandle^.CursorID]);
      with FileHandle^ do begin
        if Assigned(ReadInfo) then begin
          with ReadInfo^ do begin
            FFFreeMem(PhysRecordBuf, NBytesInPhysRecord);
            PhysRecordBuf := nil;

            FFFreeMem(FieldInfo, SizeOf(TPhysDbReadFieldInfo) * NFieldsInReadRecord);
            FieldInfo := nil;

            FFFreeMem(IndexFieldInfo, SizeOf(TPhysDbReadFieldInfo) * NFieldsInIndexRecord);
            IndexFieldInfo := nil;

            FFFreeMem(RangeFieldInfo, SizeOf(TPhysDbReadFieldInfo) * NumRanges);
            RangeFieldInfo := nil;
          end;

          FFFreeMem(ReadInfo, sizeof(TPhysDbReadInfo));
          ReadInfo := nil;
        end;
      end;
    end;
  except
    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function NRecurringRecordsToRead(
           FileHandle: PPhysDbFileHandle;
           var NRecordsToRead: LongInt;
           ErrMsg: PAnsiChar) : TPhysDbError;
var
  NRecords : TcrInt32u;
  FFError  : TffResult;
begin
  AddToLog('NRecurringRecordsToRead');
  AddToLogFmt('  FName:         [%s]', [FileHandle^.PathAndFileName]);
  AddToLogFmt('  CursorID:      [%d]', [FileHandle^.CursorID]);

  Result := errPhysDbNoError;
  try
    FFError := ServerEngine.TableGetRecCount(FileHandle^.CursorID, NRecords);
    if FFError <> DBIERR_NONE then begin
      Result := IDAPIError(FFError, ErrMsg);
      Exit;
    end;

    NRecordsToRead := NRecords;
  except
    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  AddToLogFmt('  Records count: [%d]', [NRecordsToRead]);
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

{ Translate and copy fields requested by Database Manager to read
  record buffer. }

function FetchReadRecFields(
           ReadInfo          : PPhysDbReadInfo;
           HCursor           : TffCursorID;
           NotXlateDOSString : Boolean;
           ReadRecordBuf     : PByteArray;
           ReadNullFlags     : PcrBooleanArray;
           ErrMsg            : PAnsiChar) : TPhysDbError;
var
  FFError       : TffResult;
  I             : integer;
  ReadRecOffset : integer;
  BoolValue     : Bool;
  DoubleValue   : Double;
  SingleValue   : Single;                                             {!!.02}
  CompValue     : Comp;                                               {!!.02}
  ExtendedValue : Extended;                                           {!!.02}
  CurrencyValue : Currency;                                           {!!.02}
  Int16Value    : TcrInt16s;
  Int32Value    : TcrInt32s;
  UInt16Value   : TcrInt16u;
  UInt32Value   : TcrInt32u;
  DateValue     : TDbiDate;
  Year          : TcrInt16u;
  Month, Day    : TcrInt16u;
  SYear         : Integer;                                            {!!.02}
  SMonth, SDay  : Integer;                                            {!!.02}
  TimeValue     : TDbiTime;
  Millisec      : TcrInt16u;
  SHours,                                                             {!!.02}
  SMinutes,                                                           {!!.02}
  SSeconds      : Byte;                                               {!!.02}
  HourL,
  MinuteL       : TcrInt32u;
  DateTime      : TDateTime;                                          {!!.02}
  CrTime        : TcrTime;
  CrTimeArray   : array[1..4] of Byte absolute CrTime;
  IsNull        : boolean;
  FType         : TffFieldType;                                       {!!.02}
  aByte         : Byte;                                               {!!.02}
begin
//  AddToLog('FetchReadRecFields');
//  AddToLogFmt('  CursorID:      [%d]', [HCursor]);

  if hCursor = 0 then begin
    Result := IDAPIError(DBIERR_NOTINITIALIZED, ErrMsg);
    Exit;
  end;

  Result := errPhysDbNoError;

  { Translate and copy fields requested by Database Manager to read
    record buffer. }
  for I := 0 to pred(ReadInfo^.NFieldsInReadRecord) do begin
    with ReadInfo^.FieldInfo^[I] do begin
      ReadRecOffset := OffsetInRecord;
      case NativeFieldType of
        fldZSTRING:
          begin
            FType := TFFProxyCursor(HCursor).Dictionary.FieldType[FieldNo];
            if (FType = fftNullString) or
               (FType = fftNullAnsiStr) or                            {!!.02}
               (FType = fftChar) then begin                           {!!.02}
               TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                  ReadInfo^.PhysRecordBuf,
                                                  IsNull,
                                                  @ReadRecordBuf^[ReadRecOffset]);
//               AddToLogFmt('  read Null String field:   [%s]',
//                           [PChar(@ReadRecordBuf^[ReadRecOffset])]);
            end
            else if (FType = fftWideChar) or                          {!!.02}
               (FType = fftWideString) then                           {!!.02}
                ShowMessage('Widestring types not supported')         {!!.02}
            else begin
              TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                                ReadInfo^.PhysRecordBuf,
                                                                IsNull,
                                                                @ReadRecordBuf^[Pred(ReadRecOffset)]);
//               AddToLogFmt('  read String field:   [%s]',
//                           [PChar(@ReadRecordBuf^[Pred(ReadRecOffset)])]);
            end;
            ReadNullFlags^[I] := IsNull;

            if not NotXlateDOSString then
              OemToAnsi(@ReadRecordBuf^[ReadRecOffset],
                        @ReadRecordBuf^[ReadRecOffset]);
            TrimStrR(@ReadRecordBuf^[ReadRecOffset]);
          end;

        fldBOOL:
          begin
            TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                              ReadInfo^.PhysRecordBuf,
                                                              IsNull,
                                                              @BoolValue);

            if IsNull then
              ReadNullFlags^[I] := true
            else begin
              ReadNullFlags^[I] := false;
              PcrBoolean(@ReadRecordBuf^[ReadRecOffset])^ := BoolValue;
            end;
//             AddToLogFmt('  read Bool field:   [%s]',
//                         [BoolToStr(BoolValue)]);
          end;

        fldFLOAT,
        fldstMONEY:
          begin
            FType := TFFProxyCursor(HCursor).Dictionary.FieldType[FieldNo];{begin !!.02}
            case FType of
              fftSingle :
                begin
                  TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                                    ReadInfo^.PhysRecordBuf,
                                                                    IsNull,
                                                                    @SingleValue);
                  DoubleValue := SingleValue;
                end;
              fftComp :
                begin
                  TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                                    ReadInfo^.PhysRecordBuf,
                                                                    IsNull,
                                                                    @CompValue);
                  DoubleValue := CompValue;
                end;
              fftExtended :
                begin
                  TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                                    ReadInfo^.PhysRecordBuf,
                                                                    IsNull,
                                                                    @ExtendedValue);
                  DoubleValue := ExtendedValue;
                end;
              fftCurrency :
                begin
                  TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                                    ReadInfo^.PhysRecordBuf,
                                                                    IsNull,
                                                                    @CurrencyValue);
                  DoubleValue := CurrencyValue;
                end;
              else
                TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                                  ReadInfo^.PhysRecordBuf,
                                                                  IsNull,
                                                                  @DoubleValue);
            end;                                                      {end !!.02}


            if IsNull then
              ReadNullFlags^[I] := true
            else begin
              ReadNullFlags^[I] := false;
              PcrNumber(@ReadRecordBuf^[ReadRecOffset])^ :=
                DoubleToNumber(DoubleValue);
            end;
          end;

        fldINT16:
          begin
            TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                              ReadInfo^.PhysRecordBuf,
                                                              IsNull,
                                                              @Int16Value);
            if IsNull then
              ReadNullFlags^[I] := true
            else begin
              ReadNullFlags^[I] := false;
              PcrInt16s(@ReadRecordBuf^[ReadRecOffset])^ := Int16Value;
            end;
          end;

        fldINT32:
          begin
            TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                              ReadInfo^.PhysRecordBuf,
                                                              IsNull,
                                                              @Int32Value);

            if IsNull then
              ReadNullFlags^[I] := true
            else begin
              ReadNullFlags^[I] := false;
              PcrInt32s(@ReadRecordBuf^[ReadRecOffset])^ := Int32Value;
            end;
          end;

        fldUINT16:
          begin
            FType := TFFProxyCursor(HCursor).Dictionary.FieldType[FieldNo]; {begin !!.02}
            if FType <> fftByte then
              TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                                ReadInfo^.PhysRecordBuf,
                                                                IsNull,
                                                                @UInt16Value)
            else begin
              TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                                ReadInfo^.PhysRecordBuf,
                                                                IsNull,
                                                                @aByte);
              UInt16Value := aByte;
            end;                                                      {end !!.02}
            if IsNull then
              ReadNullFlags^[I] := true
            else begin
              ReadNullFlags^[I] := false;
              PcrInt16u(@ReadRecordBuf^[ReadRecOffset])^ := UInt16Value;
            end;
          end;

        fldUINT32:
          begin
            TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                              ReadInfo^.PhysRecordBuf,
                                                              IsNull,
                                                              @UInt32Value);

            if IsNull then
              ReadNullFlags^[I] := true
            else begin
              ReadNullFlags^[I] := false;
              PcrInt32s(@ReadRecordBuf^[ReadRecOffset])^ := UInt32Value;
            end;
          end;

        fldDATE:
          begin
            TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                              ReadInfo^.PhysRecordBuf,
                                                              IsNull,
                                                              @DateValue);

            if IsNull then
              ReadNullFlags^[I] := true
            else begin
              ReadNullFlags^[I] := false;
              FType := TFFProxyCursor(HCursor).Dictionary.FieldType[FieldNo]; {begin !!.02}
              if FType = fftStDate then begin
                StDateToDMY(TStDate(DateValue), SDay, SMonth, SYear);
                Day := SDay;
                Month := SMonth;
                Year := SYear;
              end else
                FFBDEDateDecode(DateValue, Day, Month, Year);         {end !!.02}
              PcrDate(@ReadRecordBuf^[ReadRecOffset])^ :=
                YearMonthDayToCrDate(Year, Month, Day);
            end;
          end;

        fldTIME:
          begin
            TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                              ReadInfo^.PhysRecordBuf,
                                                              IsNull,
                                                              @TimeValue);

            if IsNull then
              ReadNullFlags^[I] := true
            else begin
              ReadNullFlags^[I] := false;
              StTimeToHMS(TimeValue, SHours, SMinutes, SSeconds);     {begin !!.02}
              HourL := SHours;
              MinuteL := SMinutes;
              Millisec := SSeconds * 1000;

              { Compute Brahma time (number of hundredths of seconds) }
              CrTime := (HourL * 360000 + MinuteL * 6000 + (Millisec div 10)) div 100; {end !!.02}
              PcrTime(@ReadRecordBuf^[ReadRecOffset])^ := CrTime;
            end;
          end;

        fldTIMESTAMP:
          begin
            TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo, {begin !!.02}
                                                              ReadInfo^.PhysRecordBuf,
                                                              IsNull,
                                                              @DateTime);
            StrPCopy(PChar(@ReadRecordBuf^[ReadRecOffset]),
              FormatDateTime('yyyy/mm/dd hh:nn:zz', DateTime - 693594.0)); {end !!.02}
          end;

        fldBCD:
          begin
            ShowMessage('BCD datatypes not supported');
          end;

        fldBLOB,
        fldstMEMO,
        fldstFMTMEMO,
        fldstBINARY,
        fldstOLEOBJ,
        fldstGRAPHIC,
        fldstTYPEDBINARY:
          begin

(*
            { Check the unstable bookmark }
            FFError := DbiGetCursorProps(HCursor, CursorProps);
            if not CursorProps.bBookMarkStable then begin
              Result := IDAPIError(90, ErrMsg);  { 90? }
              Exit;
            end;

            { Check any primary index, sometimes bBookMarkStable doesn't work }
            HasPrimaryIndex := False;
            for IndexN := 0 to CursorProps.iIndexes do begin
              DbiGetIndexDesc(HCursor, IndexN + 1, IndexDesc);
              if IndexDesc.bPrimary = True then begin
                HasPrimaryIndex := True;
                Break;
              end;
            end;

(*
            if not HasPrimaryIndex then begin
              Result := IDAPIError(90, ErrMsg);
              Exit;
            end;
*)
            { Save the field info and RecNo for memo read. }
            PcrInt16u(@ReadRecordBuf^[ReadRecOffset])^:= FieldNo;

            FFError := ServerEngine.CursorGetBookmark(HCursor,
                                                      @ReadRecordBuf^[ReadRecOffset + SizeOf(TcrInt16u)]);
            if FFError <> DBIERR_NONE then begin
              Result := IDAPIError(FFError, ErrMsg);
              Exit;
            end;
          end;
        else
          Break;
      end;
    end;
  end;
end;

{ Copy fields (without translating) requested by Database Manager to
  index record buffer. }

function FetchIndexRecFields(
           ReadInfo : PPhysDbReadInfo;
           HCursor  : TffCursorID;
           IndexRecordBuf : PByteArray;
           IndexNullFlags : PcrBooleanArray;
           ErrMsg         : PAnsiChar) : TPhysDbError;
var
  I      : integer;
  IsNull : Boolean;
begin
//  AddToLog('FetchIndexRecFields');
//  AddToLogFmt('  CursorID:      [%d]', [HCursor]);
//  AddToLogFmt('  Field count:   [%d]', [ReadInfo^.NFieldsInIndexRecord]);
  Result := errPhysDbNoError;
  for I := 0 to pred(ReadInfo^.NFieldsInIndexRecord) do begin
    IndexNullFlags^[I] := false;
//    AddToLogFmt('  Field:       [%d]', [ReadInfo^.IndexFieldInfo^[I].FieldNo]);
    with ReadInfo^.IndexFieldInfo^[I] do begin
      TFFProxyCursor(HCursor).Dictionary.GetRecordField(FieldNo,
                                                        ReadInfo^.PhysRecordBuf,
                                                        IsNull,
                                                        @IndexRecordBuf^[OffsetInRecord]);
      IndexNullFlags^[I] := IsNull;
    end;
  end;
end;

function ReadFlatRecordVer15(
           FileHandle: PPhysDbFileHandle;
           ReadRecordBuf: PByteArray;
           ReadNullFlags: PcrBooleanArray;
           IndexRecordBuf: PByteArray;
           IndexNullFlags: PcrBooleanArray;
           var RecordRead: TcrBoolean;
           ErrMsg: PAnsiChar) : TPhysDbError;
var
  NRecordsSkipped : TcrInt32u;
  FFError         : TffResult;
begin
  AddToLog('ReadFlatRecordVer15');
  AddToLogFmt('  FName:         [%s]', [FileHandle^.PathAndFileName]);
  AddToLogFmt('  CursorID:      [%d]', [FileHandle^.CursorID]);

  Result := errPhysDbNoError;
  try
    if FileHandle^.ReadInfo^.CurrentRecord > 0 then begin
      FileHandle^.ReadInfo^.CurrentRecord := 0;

      { Position at the first record of file for subsequent reading }
      FFError := ServerEngine.CursorSetToBegin(FileHandle^.CursorID);
      if FFError <> DBIERR_NONE then begin
        Result := IDAPIError(FFError, ErrMsg);
        Exit;
      end;
    end;

    Result := ReadNextRecurringRecordVer15(
                FileHandle,
                ReadRecordBuf,
                ReadNullFlags,
                IndexRecordBuf,
                IndexNullFlags,
                RecordRead,
                NRecordsSkipped,
                ErrMsg);
  except
    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function ReadNextRecurringRecordVer15(
           FileHandle: PPhysDbFileHandle;
           ReadRecordBuf: PByteArray;
           ReadNullFlags: PcrBooleanArray;
           IndexRecordBuf: PByteArray;
           IndexNullFlags: PcrBooleanArray;
           var RecordRead: TcrBoolean;
           var NRecordsSkipped: LongInt;
           ErrMsg: PAnsiChar) : TPhysDbError;

  function RecordWithinRange(
             FileHandle: PPhysDbFileHandle;
             StopHere: Boolean;
             ErrMsg: PAnsiChar) : TPhysDbError;
  var
    RangeFieldInfo : TPhysDbReadFieldInfo;
    RangeN         : integer;
    StopKeyOffset  : integer;
    KeyBuf         : Pointer;
    StopKeyBuf     : PAnsiChar;
    NullField      : Boolean;

    function TestRangeLimitForOneField(
               KeyBuf : PAnsiChar;
               IndexCaseSensitive,
               AscendingIndex : TcrBoolean;
           var StopHere       : Boolean) : TPhysDbError;
    var
      SaveKeyCh     : AnsiChar;
      SaveStopKeyCh : AnsiChar;
      CompResult    : Integer;
      MinLen        : Integer;
      DateKey,
      StopDate      : TDbiDate;
      ShortKey,
      StopShort     : TcrInt16s;
      LongKey,
      StopLong      : TcrInt32s;
      DoubleKey,
      StopDouble    : Double;
      StopKeyLen    : TcrInt16u;
      Evaluate      : Boolean;
    begin
      Result := errPhysDbNoError;
      StopHere := False;
      Evaluate := True;
      CompResult := 0;

      case RangeFieldInfo.NativeFieldType of
        fldZSTRING:
          begin
            StopKeyLen := StrLen(StopKeyBuf);
            if RangeFieldInfo.NBytesInNativeField > StopKeyLen then
              MinLen := StopKeyLen
            else
              MinLen := RangeFieldInfo.NBytesInNativeField;

            SaveKeyCh := KeyBuf[MinLen];
            KeyBuf[MinLen] := #0;

            SaveStopKeyCh := StopKeyBuf[RangeFieldInfo.NBytesInNativeField];
            StopKeyBuf[RangeFieldInfo.NBytesInNativeField] := #0;

            if IndexCaseSensitive then
              CompResult := StrComp(KeyBuf, StopKeyBuf)
            else
              CompResult := StrIComp(KeyBuf, StopKeyBuf);

            KeyBuf[MinLen] := SaveKeyCh;
            StopKeyBuf[RangeFieldInfo.NBytesInNativeField] := SaveStopKeyCh;
          end;

        fldDATE:
          begin
            DateKey := PDbiDate(KeyBuf)^;
            StopDate := PDbiDate(StopKeyBuf)^;

            CompResult := -1;
            if DateKey = StopDate then CompResult := 0
            else if DateKey > StopDate then CompResult := 1;
          end;

        fldINT16:
          begin
            ShortKey := PcrInt16s(KeyBuf)^;
            StopShort := PcrInt16s(StopKeyBuf)^;

            CompResult := -1;
            if ShortKey = StopShort then CompResult := 0
            else if ShortKey > StopShort then CompResult := 1;
          end;

        fldINT32:
          begin
            LongKey := PcrInt32s(KeyBuf)^;
            StopLong := PcrInt32s(StopKeyBuf)^;

            CompResult := -1;
            if LongKey = StopLong then CompResult := 0
            else if LongKey > StopLong then CompResult := 1;
          end;

        fldFLOAT,
        fldstMONEY:
          begin
            DoubleKey := PDouble(KeyBuf)^;
            StopDouble := PDouble(StopKeyBuf)^;

            CompResult := -1;
            if DoubleKey = StopDouble then CompResult := 0
            else if DoubleKey > StopDouble then CompResult := 1;
          end;

        fldTIME:
          begin
          {}
          end;

        fldBOOL:
          begin
            Evaluate := False;
            if TcrBoolean(StopKeyBuf^) then
              if TcrBoolean(KeyBuf^) then
                StopHere := False
              else
                StopHere := True
            else
              if TcrBoolean(KeyBuf^) then
                StopHere := True
              else
                StopHere := False;
          end;
        else
          begin
            Result := errPhysDbProgrammingError;
            Exit;
          end;
      end;

      if Evaluate then begin
        if RangeFieldInfo.StopInclusive then
          if AscendingIndex then
            StopHere := (CompResult > 0)
          else
            StopHere := (CompResult < 0)
        else
          if AscendingIndex then
            StopHere := (CompResult >= 0)
          else
            StopHere := (CompResult <= 0);
      end;
    end;

  begin
    Result := errPhysDbNoError;
    if FileHandle^.ReadInfo^.StopKeyLen = 0 then Exit;

    { Loop through all the range fields for the current index }
    for RangeN := 0 to pred(FileHandle^.ReadInfo^.NStopKeyRanges) do begin
      RangeFieldInfo := FileHandle^.ReadInfo^.RangeFieldInfo^[RangeN];
      StopHere := False;

      { KeyBuf points to the values from the current current.
        stopKeyBuf points to the values that define the end of the range. }
      StopKeyOffset := RangeFieldInfo.OffsetInStopKeyBuf;
      KeyBuf := Addr(FileHandle^.ReadInfo^.KeyBuf[StopKeyOffset]);
      StopKeyBuf := Addr(FileHandle^.ReadInfo^.StopKeyBuf[StopKeyOffset]);

      { Get the range value values out of the current record and into
        the comparison buffer in their native format. }
      TFFProxyCursor(FileHandle^.CursorID).Dictionary.GetRecordField(RangeFieldInfo.FieldNo,
                                                                     FileHandle^.ReadInfo^.PhysRecordBuf,
                                                                     NullField,
                                                                     KeyBuf);

      if NullField then
        Continue;

      { Test this record for out of range on the current range field }
      Result := TestRangeLimitForOneField(
                  KeyBuf,
                  FileHandle^.ReadInfo^.IndexCaseSensitive,
                  FileHandle^.ReadInfo^.AscendingIndex,
                  StopHere);
      if Result <> errPhysDbNoError then Exit;

      { Once we've found a field with an out of range value, we needn't look
        at the remaining range fields }
      if StopHere then
        Break;
    end;
  end;

var
  FFError  : TffResult;
  StopHere : Boolean;
  Buffer   : TffShStr;
begin
//  AddToLog('ReadNextRecurringRecordVer15');
//  AddToLogFmt('  FName:         [%s]', [FileHandle^.PathAndFileName]);
//  AddToLogFmt('  CursorID:      [%d]', [FileHandle^.CursorID]);
  Result := errPhysDbNoError;
  try
    try
      RecordRead := false;
      NRecordsSkipped := 0;

      while not RecordRead do begin

        { Advance to the next recurring record, skipping if it is locked
          or deleted by another user }
        FFError := ServerEngine.RecordGetNext(FileHandle^.CursorID,
                                              ffltNoLock,
                                              nil);
        if (FFError = DBIERR_RECDELETED) or (FFError = DBIERR_RECNOTFOUND) then begin
          Inc(FileHandle^.ReadInfo^.CurrentRecord);
          Inc(NRecordsSkipped);
          Continue;  { Try the next record }
        end;

        if FFError = DBIERR_EOF then
          Exit;

        if FFError <> DBIERR_NONE then begin
          Result := IDAPIError(FFError, ErrMsg);
          Exit;
        end;

        FFError := ServerEngine.RecordGet(FileHandle^.CursorID,
                                          ffltNoLock,
                                          FileHandle^.ReadInfo^.PhysRecordBuf);
        if FFError = DBIERR_NONE then begin
          { Test if index fields still in range.  If in range, break, else return }
          if FileHandle^.RangeLimit then begin
            StopHere := False;
            if RecordWithinRange(FileHandle, StopHere, ErrMsg) <> errPhysDbNoError then
              Break;
            if StopHere then
              Exit;
          end;

          RecordRead := true;
          Inc(FileHandle^.ReadInfo^.CurrentRecord);
        end
        else begin
          if (FileHandle^.ReadInfo^.CurrentRecord > 0) and
             ((FFError = DBIERR_RECDELETED) or (FFError = DBIERR_RECNOTFOUND)) then
            Inc(NRecordsSkipped)
          else begin
            Result := IDAPIError(FFError, ErrMsg);
            Exit;
          end;
        end;
      end;

      Result := FetchReadRecFields(FileHandle^.ReadInfo, FileHandle^.CursorID,
                                   FileHandle^.NotXlateDOSString, ReadRecordBuf, ReadNullFlags, ErrMsg);
      if Result <> errPhysDbNoError then Exit;

      RecordRead := true;
      Result := FetchIndexRecFields(FileHandle^.ReadInfo, FileHandle^.CursorID,
                                    IndexRecordBuf, IndexNullFlags, ErrMsg);

    except
      on E: Exception do begin
        if Result = errPhysDbNoError then
          Result := errPhysDbErrMsgReturned;
        StrPCopy(ErrMsg, E.Message);
      end;
    end;
  finally
    Buffer := PhysDbErrors[Result]; { this seems necessary for 32-bit }
  end;
end;

function LookupMatchingRecurringRecVer15(
           FileHandle: PPhysDbFileHandle;
           LookupValueRecordBuf: PAnsiChar;
           LookupValueNullFlags: PcrBooleanArray;
           LookupValueType: TcrInt16u;
           StartTopOfFile: TcrBoolean;
           ReadRecordBuf: PByteArray;
           ReadNullFlags: PcrBooleanArray;
           IndexRecordBuf: PByteArray;
           IndexNullFlags: PcrBooleanArray;
           var RecordRead: TcrBoolean;
           ErrMsg: PAnsiChar) : TPhysDbError;

  function CompareLookupResult(
             FileHandle : PPhysDbFileHandle;
             LookupValueRecordBuf : PAnsiChar;
             LookupValueNullFlags : PcrBooleanArray;
         var Match  : Boolean;
             ErrMsg : PAnsiChar) : TPhysDbError;
  var
    FFError        : TffResult;
    I              : integer;
    FieldNo        : integer;
    LookupOffset   : integer;
    LookupValueLen : DWORD;
    FieldLen       : integer;
    CompareLen     : DWORD;
    NFields        : integer;
    LookupNullFlag : Boolean;
    NullField      : Boolean;
  begin
    Result := errPhysDbNoError;
    Match := False;

    { Ensure that fields are in system buffer }
    FFError := ServerEngine.RecordGet(FileHandle^.CursorID,
                                      ffltNoLock,
                                      FileHandle^.ReadInfo^.PhysRecordBuf);
    if FFError <> DBIERR_NONE then begin
      Result := IDAPIError(FFError, ErrMsg);
      Exit;
    end;

    { Fetch fields from system record buffer }
    NFields := FileHandle^.ReadInfo^.NFieldsInLookupValue;
    LookupNullFlag := False;
    if NFields > 0 then
      LookupNullFlag := LookupValueNullFlags^[0];

    for I := 0 to pred(NFields) do begin
      FieldNo := FileHandle^.ReadInfo^.IndexDefnInfo^[I].FieldNo;
      LookupOffset := FileHandle^.ReadInfo^.IndexDefnInfo^[I].OffsetInRecord;
      FieldLen := FileHandle^.ReadInfo^.IndexDefnInfo^[I].FieldLength;
      NullField := False;
      TFFProxyCursor(FileHandle^.CursorID).Dictionary.GetRecordField(FieldNo,
                                                  FileHandle^.ReadInfo^.PhysRecordBuf,
                                                  NullField,
                                                  @FileHandle^.ReadInfo^.KeyBuf[LookupOffset]);
      if FFError <> DBIERR_NONE then begin
        Result := IDAPIError(FFError, ErrMsg);
        Exit;
      end;

      if LookupNullFlag or NullField then Exit;

      { Compare each individual field to see if matches lookup value.
        Only compare as much data as present if substring field }
      CompareLen := FieldLen;
      if FileHandle^.ReadInfo^.LookupValueLen < (LookupOffset + FieldLen) then
        CompareLen := FileHandle^.ReadInfo^.LookupValueLen - LookupOffset;
      if FileHandle^.ReadInfo^.IndexDefnInfo^[I].FieldType = ftStringField then begin
        LookupValueLen := StrLen(@LookupValueRecordBuf[LookupOffset]);
        if LookupValueLen < CompareLen then begin
          if I = NFields - 1 then begin
            if FileHandle^.ReadInfo^.LastLookupFieldIsSubstr then
              CompareLen := LookupValueLen
            else if LookupValueLen <> StrLen(@FileHandle^.ReadInfo^.KeyBuf[LookupOffset]) then
              Exit
            else
              CompareLen := LookupValueLen;
          end;
        end;
      end;

(*      if FileHandle^.ReadInfo^.IndexCaseSensitive then begin*)
        if (CompareLen = 0) or (FFCmpBytes(PffByteArray(@FileHandle^.ReadInfo^.KeyBuf[LookupOffset]),
                                           PffByteArray(@LookupValueRecordBuf[LookupOffset]),
                                           CompareLen) <> 0) then begin
          Exit;
        end;
    end;

    Match := True;
  end;
var
  FFError : TffResult;
  Match   : Boolean;
  I       : integer;
  FieldN  : integer;
  NFields : integer;
  LookupNullFlag : Boolean;
begin
  AddToLog('LookupMatchingRecurringRecVer15');
  AddToLogFmt('  FileName:             [%s]', [FileHandle^.PathAndFileName]);
  AddToLogFmt('  CursorID:             [%d]', [FileHandle^.CursorID]);
  Result := errPhysDbNoError;
  try

    { Set up for search }
    RecordRead := false;

    with FileHandle^.ReadInfo^ do begin
      if not StartTopOfFile then begin
        AddToLog('  StartTopOfFile        [False]');
        if ValuesUnique and
           not LastLookupFieldIsSubstr and
           (NFieldsInLookupValue > NFieldsInIndexDefn) then
          Exit;

        { See if next record also matches }
        FFError := ServerEngine.RecordGetNext(FileHandle^.CursorID,
                                              ffltNoLock,
                                              nil);
        AddToLogFmt('  RecordGetNext Result  [%d]', [FFError]);

        if FFError <> DBIERR_NONE then
          Exit;

        Result := CompareLookupResult(FileHandle,
                                      LookupValueRecordBuf,
                                      LookupValueNullFlags,
                                      Match,
                                      ErrMsg);
        AddToLogFmt('  Match Result          [%s]', [BoolToStr(Match)]); {!!.12}
        if (Result <> errPhysDbNoError) or not Match then Exit;
      end else begin
        AddToLog('  StartTopOfFile        [True]');
        { Clear all the fields in index }
        for FieldN := 0 to pred(NFieldsInIndexDefn) do begin
          TFFProxyCursor(FileHandle^.CursorID).Dictionary.SetRecordField(
                                                 IndexDefnInfo^[FieldN].FieldNo,
                                                 PhysRecordBuf,
                                                 nil);
        end;

        { Copy fields (without translating) from lookup value buffer to
          system record buffer }
        NFields := NFieldsInLookupValue;
        LookupNullFlag := False;
        if NFields > 0 then
          LookupNullFlag := LookupValueNullFlags^[0];
        FillChar(PhysRecordBuf^, NBytesInPhysRecord, #0);

        for I := 0 to pred(NFields) do begin
          if not LookupNullFlag then begin

            { Copy index record field into system record buffer }
            with IndexDefnInfo^[I] do
              TFFProxyCursor(FileHandle^.CursorID).Dictionary.SetRecordField(
                                                           FieldNo,
                                                           PhysRecordBuf,
                                                           @LookupValueRecordBuf[OffsetInRecord]);
          end;
        end;

        with TFFProxyCursor(FileHandle^.CursorID) do
          Dictionary.ExtractKey(IndexID, PhysRecordBuf, @KeyBuf);

        FFError := ServerEngine.CursorSetToKey(FileHandle^.CursorID,
                                               skaEqual,
                                               True,
                                               NFieldsInLookupValue,
                                               LastLookupFieldLen,
                                               @KeyBuf);
        AddToLogFmt('  CursorSetToKey Result [%d]', [FFError]);
       
        { Test if exact lookup succeeeded }
        if (FFError = DBIERR_EOF) or
           (FFError = DBIERR_OUTOFRANGE) or
           (FFError = DBIERR_RECNOTFOUND) or
           (FFError = DBIERR_RECDELETED) then begin
          Result := errPhysDbNoError;
          Exit;
        end
        else if FFError <> DBIERR_NONE then begin
          Result := IDAPIError(FFError, ErrMsg);
          Exit;
        end;

        { read in the current record }
        FFError := ServerEngine.RecordGetNext(FileHandle^.CursorID,
                                              ffltNoLock,
                                              FileHandle^.ReadInfo^.PhysRecordBuf);
        AddToLogFmt('  RecordGetNext Result here  [%d]', [FFError]);
        if FFError <> DBIERR_NONE then begin
          Result := IDAPIError(FFError, ErrMsg);
          Exit;
        end;
      end;

      Result := FetchReadRecFields(FileHandle^.ReadInfo,
                                   FileHandle^.CursorID,
                                   FileHandle^.NotXlateDOSString,
                                   ReadRecordBuf,
                                   ReadNullFlags,
                                   ErrMsg);
      if Result <> errPhysDbNoError then Exit;

      RecordRead := true;
      Result := FetchIndexRecFields(FileHandle^.ReadInfo,
                                    FileHandle^.CursorID,
                                    IndexRecordBuf,
                                    IndexNullFlags,
                                    ErrMsg);
    end;
  except
    on EOutOfMemory do begin
      Result := errPhysDbNotEnoughMemory;
      StrPCopy(ErrMsg, '');
    end;

    on E: Exception do begin
      if Result = errPhysDbNoError then
        Result := errPhysDbErrMsgReturned;
      StrPCopy(ErrMsg, E.Message);
    end;
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

{ -------------------------  Memo Fields  ---------------------------- }

function FetchMemoField(
           MemoFieldRecordBuf: PAnsiChar;
           var MemoField: PAnsiChar;
           ErrMsg: PAnsiChar) : TPhysDbError;
begin
  AddToLog('FetchMemoField');
  MemoField := nil;
  Result := errPhysDbNoError;
  AddResultToLog(Result);
end;

function FreeMemoField(
           var MemoField: PAnsiChar;
           ErrMsg: PAnsiChar) : TPhysDbError;
begin
  AddToLog('FreeMemoField');
  FFStrDispose(MemoField);
  MemoField := nil;
  Result := errPhysDbNoError;
  AddResultToLog(Result);
end;

function FetchPersistentMemoField(FileHandle : PPhysDbFileHandle;
                                  MemoFieldRecordBuf : PAnsiChar;
                              var MemoField : PAnsiChar;
                                  ErrMsg    : PAnsiChar) : TPhysDbError;
var
  FFError       : TffResult;
  NativeType    : TcrInt16u;
  FieldN        : integer;
  FieldNo       : integer;
  ValueType     : TFieldValueType;
  CmpResult     : Integer;
  BlobSize      : TcrInt32u;
  NBytesReturned : TffWord32;
  BlobHandle     : THandle;
  Handle         : THandle;
  BlobFieldPtr   : PByteArray;
  FinalBlobFieldPtr : PByteArray;
  Size          : TcrInt32u;
  SavedBlobSize : TcrInt32u;
  FirstTime     : Boolean;
  Offset        : TcrInt32u;
  NBytesCopied  : TcrInt32u;
  StartPos      : TcrInt32u;
  BookmarkSize  : Integer;
  IsNull        : Boolean;
  aBlobNr       : TffInt64;
  TempI64       : TffInt64;
  BookmarkBuf   : Pointer;
begin
  AddToLog('FetchPersistentMemoField');
  AddBlockToLog('  Memo Data', MemoFieldRecordBuf, 12);

  Result := errPhysDbNoError;
  MemoField := nil;
  try
    try

      { Restore the field info from brahma buffer }
      FieldNo := TcrInt16s(MemoFieldRecordBuf^);
      ValueType := ftPersistentMemoField;
      NativeType := 0;
      with FileHandle^.ReadInfo^ do
        for FieldN := 0 to pred(NFieldsInReadRecord) do begin
          if FieldInfo^[FieldN].FieldNo = FieldNo then begin
            NativeType := FieldInfo^[FieldN].NativeFieldType;
            if FieldInfo^[FieldN].FieldType = ftBlobField then
              ValueType := ftBlobField;
          end;
        end;

      { Get the current bookmark }
      FFError := ServerEngine.CursorGetBookmarkSize(FileHandle^.CursorID, BookmarkSize);
      if FFError <> DBIERR_NONE then begin
        Result := IDAPIError(FFError, ErrMsg);
        Exit;
      end;

      FFGetMem(BookmarkBuf, BookmarkSize + 1);
      try
        FFError := ServerEngine.CursorGetBookmark(FileHandle^.CursorID,
                                                  BookmarkBuf);
        if FFError <> DBIERR_NONE then begin
          Result := IDAPIError(FFError, ErrMsg);
          Exit;
        end;

        ServerEngine.CursorCompareBookmarks(FileHandle^.CursorID,
                                            BookmarkBuf,
                                            @MemoFieldRecordBuf[SizeOf(TcrInt16u)],
                                            CmpResult);

      finally
        FFFreeMem(BookmarkBuf, BookmarkSize+1);
      end;

      { If it is not the current position, reposition to the old position }
      if CmpResult <> 0 then begin
        FFError := ServerEngine.CursorSetToBookmark(FileHandle^.CursorID,
                                                    @MemoFieldRecordBuf[SizeOf(TcrInt16u)]);
        if FFError = DBIERR_NONE then
          FFError := ServerEngine.RecordGet(FileHandle^.CursorID,
                                            ffltNoLock,
                                            FileHandle^.ReadInfo^.PhysRecordBuf);

        if FFError <> DBIERR_NONE then begin
          Result := IDAPIError(FFError, ErrMsg);
          Exit;
        end;
      end;

      TempI64.iLow := 0;
      TempI64.iHigh := 0;
      TFFProxyCursor(FileHandle^.CursorID).Dictionary.GetRecordField(
        FieldNo, FileHandle^.ReadInfo^.PhysRecordBuf, IsNull, @aBLOBNr);

      if (not IsNull) and (ffCmpI64(aBLOBNr, TempI64) = 0) then
        FFError := DBIERR_INVALIDBLOBHANDLE
      else
        FFError := DBIERR_NONE;

      if FFError <> DBIERR_NONE then begin
        Result := IDAPIError(FFError, ErrMsg);
        Exit;
      end;

      if not IsNull then begin                                        {!!.02}
        try
          FFError := ServerEngine.BLOBGetLength(FileHandle^.CursorID,
                                                aBlobNr,
                                                BlobSize);
          if FFError <> DBIERR_NONE then begin
            Result := IDAPIError(FFError, ErrMsg);
            Exit;
          end;

          if BlobSize = 0 then
            Exit;

          if ValueType = ftPersistentMemoField then begin
            {Handle only 64K memos for now }
            BlobSize := BlobSize;
            Handle := GlobalAlloc(GHND or GMEM_SHARE, BlobSize);
            if Handle = 0 then
              raise EOutOfMemory.Create('');
            try
              BlobHandle := GlobalAlloc(GHND or GMEM_SHARE, BlobSize + 1);
              BlobFieldPtr := GlobalLock(Handle);
              try
                if Assigned(BlobFieldPtr) then begin
                  FinalBlobFieldPtr := GlobalLock(BlobHandle);
                  try
                    FFError := ServerEngine.BLOBRead(FileHandle^.CursorID,
                                                     aBlobNr,
                                                     0,
                                                     BlobSize,
                                                     BlobFieldPtr^,
                                                     NBytesReturned);
                    if FFError <> DBIERR_NONE then begin
                      Result := IDAPIError(FFError, ErrMsg);
                      Exit;
                    end;

                    if NativeType = fldstFMTMEMO then begin
                      if BlobSize > 44 then begin
                        if StrLComp(PAnsiChar(BlobFieldPtr), #1#0#0#0#$C7#0#0#0, 8) = 0 then begin
                          Move(BlobFieldPtr^[8], FinalBlobFieldPtr^, BlobSize - 8);
                          FinalBlobFieldPtr[NBytesReturned - 8] := $0;
                        end else begin
                          Move(BlobFieldPtr^[44], FinalBlobFieldPtr^, BlobSize - 44);
                          FinalBlobFieldPtr[NBytesReturned - 44] := $0;
                        end;
                      end else begin
                        Move(BlobFieldPtr^, FinalBlobFieldPtr^, BlobSize);
                        FinalBlobFieldPtr[NBytesReturned] := $0;
                      end;
                    end else begin
                      Move(BlobFieldPtr^, FinalBlobFieldPtr^, BlobSize);
                      FinalBlobFieldPtr[NBytesReturned] := $0;
                    end;

                    if not FileHandle^.NotXlateDOSMemo then
                      OemToAnsi(PAnsiChar(FinalBlobFieldPtr), PAnsiChar(FinalBlobFieldPtr));

                    MemoField := PAnsiChar(FinalBlobFieldPtr);
                  finally
                    GlobalUnlock(BlobHandle);
                  end;
                end;
              finally
                GlobalUnlock(Handle)
              end;
            finally
              GlobalFree(Handle);
            end;

          end else begin
            { Nonmemo BLOB, may be a bitmap }
            Handle := GlobalAlloc(GHND or GMEM_SHARE, BlobSize);
            if Handle = 0 then
              raise EOutOfMemory.Create('');
            try
              if NativeType = fldstBINARY then
                { No BLOB_INFO }
                BlobHandle := GlobalAlloc(GHND or GMEM_SHARE, BlobSize - SizeOf(TBitmapFileHeader))
              else
                BlobHandle := GlobalAlloc(GHND or GMEM_SHARE, BlobSize - BLOB_INFO_SIZE - SizeOf(TBitmapFileHeader));
              BlobFieldPtr := GlobalLock(Handle);
              try
                if Assigned(BlobFieldPtr) then begin
                  FinalBlobFieldPtr := GlobalLock(BlobHandle);
                  try
                    NBytesReturned := 0;
                    SavedBlobSize := BlobSize;
                    Size := FFMinDW(SavedBlobSize, $FFE0);
                    FirstTime := True;
                    Offset := 0;
                    NBytesCopied := 0;
                    while Size <> 0 do begin
                      FFError := ServerEngine.BLOBRead(FileHandle^.CursorID,
                                                       aBlobNr,
                                                       Offset,
                                                       Size,
                                                       BlobFieldPtr^,
                                                       NBytesReturned);
                      if FFError <> DBIERR_NONE then begin
                        Result := IDAPIError(FFError, ErrMsg);
                        Exit;
                      end;

                      Inc(Offset, NBytesReturned);
                      Dec(SavedBlobSize, NBytesReturned);
                      StartPos := 0;
                      if FirstTime then begin
                        if NativeType <> fldstBINARY then
                          Inc(StartPos, BLOB_INFO_SIZE);

                        { If it is not a bitmap, return nil }
                        if Copy(StrPas(PAnsiChar(@BlobFieldPtr^[StartPos])), 1 ,2) <> 'BM' then
                          Exit;

                        Inc(StartPos, SizeOf(TBitmapFileHeader));
                      end;

                      { Copy the bitmap data of size FFE0 or less depending on the size
                        of whole bitmap }
                      Move(BlobFieldPtr^[StartPos], FinalBlobFieldPtr^[NBytesCopied], Size - StartPos);

                      Inc(NBytesCopied, Size - StartPos);
                      { The size of data to be got }
                      Size := FFMinDW(SavedBlobSize, $FFE0);
                      FirstTime := False
                    end;
                  finally
                    GlobalUnlock(BlobHandle);
                  end;
                end;
              finally
                GlobalUnlock(Handle);
              end;
            finally
              GlobalFree(Handle);
            end;

            { Pass back the handle to the bitmap to Crystal.  Allegedly, Crystal
              will handle freeing it }
            MemoField := PAnsiChar(BlobHandle);
          end;
        finally
          ServerEngine.BLOBFree(FileHandle^.CursorID,
                                aBlobNr,
                                True);
        end;
      end;                                                            {!!.02}
    except
      on EOutOfMemory do begin
        Result := errPhysDbNotEnoughMemory;
        StrPCopy(ErrMsg, '');
      end;
      on E: Exception do begin
        if Result = errPhysDbNoError then
          Result := errPhysDbErrMsgReturned;
        StrPCopy(ErrMsg, E.Message);
      end;
    end;
  finally
    StrPCopy(DebugBuff, PhysDbErrors[Result]); { this seems necessary for 32-bit }
  end;
  if (Result = errPhysDbErrMsgReturned) then
    AddToLogFmt('  ErrMsg: [%s]', [ErrMsg]);
  AddResultToLog(Result);
end;

function FreePersistentMemoField(
           FileHandle: PPhysDbFileHandle;
           var MemoField: PAnsiChar;
           ErrMsg: PAnsiChar) : TPhysDbError;
begin
  AddToLog('FreePersistentMemoField');

  GlobalFree(THandle(MemoField));
  MemoField := nil;
  Result := errPhysDbNoError;
  AddResultToLog(Result);
end;


{ ---------------------  Multi-User Access  -------------------------- }

function UseRecordLocking(FileHandle : PPhysDbFileHandle;
                          ErrMsg     : PAnsiChar) : TPhysDbError;
begin
  AddToLog('UseRecordLocking');
  Result := errPhysDbNotImplemented;
  AddResultToLog(Result);
end;

function UseFileLocking(FileHandle : PPhysDbFileHandle;
                        ErrMsg     : PAnsiChar) : TPhysDbError;
begin
  AddToLog('UseFileLocking');
  Result := errPhysDbNotImplemented;
  AddResultToLog(Result);
end;


{===Debug logging====================================================}
{Begin !!.12}
procedure StartLog;
begin
{$IFDEF Debug}
  Log := TffEventLog.Create(nil);
  Log.FileName := FFMakeFullFileName(FFExtractPath(FFGetExeName), 'FFDRIVER.LOG');
  Log.Enabled := True;
  Log.WriteString('FF server log started');
{$ELSE}
  Log := nil;
{$ENDIF}
end;
{--------}
procedure EndLog;
begin
  if Log <> nil then
    Log.Free;
end;
{--------}
procedure AddToLog(const S : string);
begin
  if Log <> nil then
    Log.WriteString(S);
end;
{--------}
procedure AddToLogFmt(const S : string; args : array of const);
begin
  if Log <> nil then
    Log.WriteStringFmt(S, args);
end;
{--------}
procedure AddBlockToLog(const S : string; Buf : pointer; BufLen : TffMemSize);
begin
  if Log <> nil then
    Log.WriteBlock(S, Buf, BufLen);
end;
{--------}
procedure AddResultToLog(aResult : TPhysDbError);
{$IFDEF Debug}
var
  S : string;
{$ENDIF}
begin
{$IFDEF Debug}
  case aResult of
    errPhysDbNoError             : S := 'errPhysDbNoError';
    errPhysDbErrMsgReturned      : S := 'errPhysDbErrMsgReturned';
    errPhysDbNotEnoughMemory     : S := 'errPhysDbNotEnoughMemory';
    errPhysDbFileDoesNotExist    : S := 'errPhysDbFileDoesNotExist';
    errPhysDbFilePermissionError : S := 'errPhysDbFilePermissionError';
    errPhysDbFileIntegrityError  : S := 'errPhysDbFileIntegrityError';
    errPhysDbUserCancelOperation : S := 'errPhysDbUserCancelOperation';
    errPhysDbProgrammingError    : S := 'errPhysDbProgrammingError';
    errPhysDbNotImplemented      : S := 'errPhysDbNotImplemented';
    errPhysDbSQLServerError      : S := 'errPhysDbSQLServerError';
    errPhysDbIncorrectPassword   : S := 'errPhysDbIncorrectPassword';
    errPhysDbOpenSessionError    : S := 'errPhysDbOpenSessionError';
    errPhysDbLogOnServerError    : S := 'errPhysDbLogOnServerError';
    errPhysDbErrorHandledByDBDLL : S := 'errPhysDbErrorHandledByDBDLL';
    errPhysDbStopProceeding      : S := 'errPhysDbStopProceeding';
  else
    S := '***Unknown***';
  end;{case}
  Log.WriteStringFmt('  Result: %s [%d]', [S, ord(aResult)]);
{$ENDIF}
end;
{End !!.12}
{====================================================================}

procedure UnitEnterProc;
begin
  TaskList := TTaskList.Create;
  StartLog;
end;

procedure UnitExitProc;
begin
  EndLog;
  TaskList.Free;
end;

initialization
  UnitEnterProc;

finalization
  UnitExitProc;

end.

