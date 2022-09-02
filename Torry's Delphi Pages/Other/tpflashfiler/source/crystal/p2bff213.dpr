{*********************************************************}
{* 32-bit Crystal Reports Driver Project File            *}
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

{ NOTICE:  This is the source code for a database DLL driver
  to allow Crystal Reports 4.5 to 7.x  to directly access
  TurboPower's FlashFiler database tables.  Although this is
  a driver for a third-party product, Seagate Software (Crystal
  Reports) has no obligation to support this driver (and will
  not in any way).  All tech support concerns regarding the
  FlashFiler driver for Crystal Reports should be directed
  to TurboPower Software Company. }

library p2bff213;

{$I ffdefine.inc}

{$I ffcrdefn.inc}

uses
  {$IFDEF USETeDEBUG}
  TeDebug,
  {$ENDIF}
  Windows,
  Forms,
  SysUtils,
  ffllbase,
  ffcrmain in 'ffcrmain.pas' { Main routines for processing CRW requests },
  ffcrtype in 'ffcrtype.pas' { Principal datatypes and structures },
  ffcrptyp in 'ffcrptyp.pas' { Datatypes shared between driver and CRW },
  ffcrltyp in 'ffcrltyp.pas' { Datatypes specific to this physical database },
  ffcrutil in 'ffcrutil.pas' { General utility routines };

{$R *.RES}

exports
  PhysDbVersionNumber               index 1,
  CanRecognizeDataFile              index 2,
  CanFetchDataFileInfo              index 3,
  CanFetchDataFileIndexInfo         index 4,
  CanBuildIndex                     index 5,
  CanFetchNRecurringRecords         index 6,
  SQLCompatible                     index 7,
  CanReadSortedOrder                index 8,
  CanReadRangeOfValues              index 9,
  CanUseRecordLocking               index 10,
  CanUseFileLocking                 index 11,
  InitPhysicalDatabase              index 12,
  TermPhysicalDatabase              index 13,
  OpenSession                       index 14,
  TermSession                       index 15,
  FetchDatabaseName                 index 16,
  FreeDatabaseName                  index 17,
  LogOnServer                       index 18,
  LogOffServer                      index 19,
  ParseLogOnInfo                    index 20,
  RebuildConnectBuf                 index 21,
  OpenDataFileIfRecognizedVer113    index 22,
  OpenDataAndIndexFileIfRecogV113   index 23,
  OpenDataFileAndIndexChoiceVer113  index 24,
  CloseDataFile                     index 25,
  FetchDataFileInfo                 index 26,
  FreeDataFileInfo                  index 27,
  FetchDataFileIndexInfo            index 28,
  FreeDataFileIndexInfo             index 29,
  BuildAndExecSQLQuery              index 30,
  InitDataFileForReadingVer17       index 31,
  InitDataFileAndIndexForReadV115   index 32,
  TermDataFileForReading            index 33,
  NRecurringRecordsToRead           index 34,
  ReadFlatRecordVer15               index 35,
  ReadNextRecurringRecordVer15      index 36,
  LookupMatchingRecurringRecVer15   index 37,
  FetchMemoField                    index 38,
  FreeMemoField                     index 39,
  FetchPersistentMemoField          index 40,
  FreePersistentMemoField           index 41,
  UseRecordLocking                  index 42,
  UseFileLocking                    index 43;


var
  ExitSave : Pointer;

procedure DLLExitProc; far;
begin
  ExitProc := ExitSave;
  AddToLog('Unloading FlashFiler 2 driver');
end;

begin
  AddToLog(Format('Loading FlashFiler 2 driver; Version: [%d]', [ffVersionNumber]));
  ExitSave := ExitProc;
  ExitProc := Addr(DLLExitProc);
end.

