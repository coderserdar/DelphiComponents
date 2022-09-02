{*********************************************************}
{* FlashFiler: Non-native BDE Client Interface Routines  *}
{*********************************************************}
{NOTE:                                                    }
{ The FFDbiRoutines are slowly being phased out. Their    }
{ functions have been added to the appropriate FF         }
{ components. These functions are provided for backwards  }
{ compatiblity, and may be removed in the next major      }
{ version of FlashFiler. USE AT YOUR OWN RISK!            }
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

unit ffclintf;

interface

uses
  ffsrbde,
  ffllbase,
  fflldict,
  ffllprot,
  ffdb,
  ffclbase,
  Classes;

function FFDbiAddAlias(aSession : TffSession;
                 const aAlias   : TffName;
                 const aPath    : TffPath) : TffResult;
  {-Add a new permanent alias}
  {-TffSession.AddAliasEx should now be used instead}

function FFDbiAddFileBLOB(aTable    : TffDataSet;
                    const iField    : Word;
                    const aFileName : TffFullFileName) : TffResult;
  {-Add a file BLOB to a FlashFiler table}
  {-TffTable.AddFileBlobEx should now be used instead}

function FFDbiAddIndex(aTable     : TffBaseTable;
                 const aIndexDesc : TffIndexDescriptor;
                   var aTaskID    : LongInt) : TffResult;
  {-Add an index to a FlashFiler table}
  {-TffTable.AddIndexEx should now be used instead}

function FFDbiCreateTable(aDatabase   : TffDatabase;
                    const aOverWrite  : Boolean;
                    const aTableName  : TffTableName;
                          aDictionary : TffDataDictionary) : TffResult;
  {-Create a FlashfFiler table}
  {-TffDatabase.CreateTableEx should now be used instead}

function FFDbiDeleteAlias(aSession : TffSession;
                    const aAlias   : TffName) : TffResult;
  {-Delete an alias permanently}
  {-TffSession.DeleteAliasEx should now be used instead}


function FFDbiGetRecordBatch(aTable        : TffDataSet;
                       const aRequestCount : LongInt;
                         var aReturnCount  : LongInt;
                             pRecBuff      : Pointer) : TffResult;
  {-get a batch of records}
  { pRecBuff must be allocated to hold RequestCount * RecordLength recs}
  {-TffTable.GetRecordBatch should now be used instead}

function FFDbiGetRecordBatchEx(aTable        : TffDataSet;
                         const aRequestCount : LongInt;
                           var aReturnCount  : LongInt;
                               pRecBuff      : Pointer;
                           var aError        : TffResult) : TffResult;
  {-get a batch of records}
  { pRecBuff must be allocated to hold RequestCount * RecordLength recs}
  {-TffTable.GetRecordBatchEx should now be used instead}

function FFDbiGetServerDateTime(aSession   : TffSession;
                            var aServerNow : TDateTime) : TffResult;
  {-get the current date and time at the server}
  { NOTE: the returned date and time is with respect to the time zone
          of the SERVER, not the CLIENT. If the server and client are
          in different time zones, you are responsible for any
          conversion.}
  {-TffSession.GetServerDateTime should now be used instead}

function FFDbiGetTaskStatus(aSession   : TffSession;
                      const aTaskID    : LongInt;
                        var aCompleted : Boolean;
                        var aStatus    : TffRebuildStatus) : TffResult;
  {-Query the status of a given pack, reindex, or restructure operation}
  {-TffSession.GetTaskStatus should now be used instead}

function FFDbiInsertRecordBatch(aTable    : TffDataSet;
                          const aCount    : LongInt;
                                pRecBuff  : Pointer;
                            var aErrors   : PffLongIntArray) : TffResult;
  {-insert a batch of records}
  {Errors must be allocated to hold Count * sizeof( LongInt )}
  {-TffTable.InsertRecordBatch should now be used instead}

function FFDbiOverrideFilter(aTable    : TffDataSet;
                             aExprTree : pCANExpr;
                             aTimeout  : TffWord32) : TffResult;
  {-Used internally to override a cursor's existing filter with a new filter.
    Occurs when a locate must used a ranged dataset. }

function FFDbiPackTable(aDatabase  : TffDatabase;
                  const aTableName : TffTableName;
                    var aTaskID    : LongInt) : TffResult;
  {-Recover disk space occupied by deleted records in a table}
  {-TffDatabase.PackTable or}
  {-TffTable.PackTableEx should now be used instead}

function FFDbiReindexTable(aDatabase  : TffBaseDatabase;
                     const aTableName : TffTableName;
                     const aIndexNum  : Integer;
                       var aTaskID    :  LongInt) : TffResult;
  {-Reconstruct key values for an index on a given table}
  {-TffDatabase.ReindexTable or}
  {-TffTable.ReindexTableEx should now be used instead}

function FFDbiRestoreFilter(aTable : TffDataSet) : TffResult;
  {-After a locate has finished overriding the server-side filter, this
    method is used to restore the cursor's original filter. }

function FFDbiRestructureTable(aDatabase   : TffDatabase;
                         const aTableName  : TffTableName;
                               aDictionary : TffDataDictionary;
                               aFieldMap   : TStrings;
                           var aTaskID     : LongInt) : TffResult;
  {-Change the layout of an existing FF table}
  {-TffDatabase.RestructureTable or}
  {-TffTable.RestructureTableEx should now be used instead}


function FFDbiSetFailSafeTransaction(aDatabase : TffBaseDatabase;
                               const aFailSafe : Boolean) : TffResult;
  {-Enable/disable failsafe transactions}
  {TffDatabase.FailSafe property should now be used instead}

function FFDbiSetFilter(aTable    : TffDataSet;
                        aExprTree : pCANExpr;
                  const aTimeout  : TffWord32) : TffResult;
  {-set the serverside filter for this cursor}
  {-TffTable.SetFilterEx should now be used instead}

procedure FFDbiSetProtocol(aProtocol : TffCommsProtocolClass);
  {-change the protocol type of future FlashFiler client sessions}

procedure FFDbiSetLoginRetries(const aRetries : Byte);
  {-change the allowable number of login retries by a client}

procedure FFDbiSetLoginParameters(const aUser     : TffName;
                                  const aPassword : TffName );
  {-change the client username and password for future FF client sessions}

function FFDbiSetTableAutoIncValue(aTable : TffDataSet;
                             const aValue: TffWord32) : TffResult;
  {-Set the autoinc seed value for a FF table}
  {TffTable.SetTableAutoIncValue should now be used instead}

implementation
uses
  SysUtils;

function FFDbiAddAlias(aSession : TffSession;
                 const aAlias   : TffName;
                 const aPath    : TffPath) : TffResult;
begin
  Result := aSession.AddAliasEx(aAlias, aPath, False);                 {!!.11}
end;

function FFDbiAddFileBLOB(aTable    : TffDataSet;
                    const iField    : Word;
                    const aFileName : TffFullFileName) : TffResult;
begin
  Result := aTable.AddFileBlob(iField, aFileName);
end;

function FFDbiAddIndex(aTable     : TffBaseTable;
                 const aIndexDesc : TffIndexDescriptor;
                   var aTaskID    : LongInt) : TffResult;
begin
  Result := aTable.AddIndexEx(aIndexDesc, aTaskID);
end;

function FFDbiCreateTable(aDatabase   : TffDatabase;
                    const aOverWrite  : Boolean;
                    const aTableName  : TffTableName;
                          aDictionary : TffDataDictionary) : TffResult;
begin
  Result := aDatabase.CreateTable(aOverWrite, aTableName, aDictionary);
end;

function FFDbiDeleteAlias(aSession : TffSession;
                    const aAlias   : TffName) : TffResult;
begin
  Result := aSession.DeleteAliasEx(aAlias);
end;

function FFDbiGetRecordBatch(aTable        : TffDataSet;
                       const aRequestCount : LongInt;
                         var aReturnCount  : LongInt;
                             pRecBuff      : Pointer) : TffResult;
begin
  Result := aTable.GetRecordBatch(aRequestCount,
                                  aReturnCount,
                                  pRecBuff);
end;

function FFDbiGetRecordBatchEx(aTable        : TffDataSet;
                         const aRequestCount : LongInt;
                           var aReturnCount  : LongInt;
                               pRecBuff      : Pointer;
                           var aError        : TffResult) : TffResult;
begin
  Result := aTable.GetRecordBatchEx(aRequestCount,
                                    aReturnCount,
                                    pRecBuff,
                                    aError );
end;

function FFDbiGetServerDateTime(aSession   : TffSession;
                            var aServerNow : TDateTime) : TffResult;
begin
  Result := aSession.GetServerDateTime(aServerNow);
end;


function FFDbiGetTaskStatus(aSession   : TffSession;
                      const aTaskID    : LongInt;
                        var aCompleted : Boolean;
                        var aStatus    : TffRebuildStatus) : TffResult;
begin
  Result := aSession.GetTaskStatus(aTaskID, aCompleted, aStatus);
end;

function FFDbiInsertRecordBatch(aTable    : TffDataSet;
                          const aCount    : LongInt;
                                pRecBuff : Pointer;
                            var aErrors   : PffLongIntArray) : TffResult;
begin
  Result := aTable.InsertRecordBatch(aCount,
                                     pRecBuff,
                                     aErrors);
end;

function FFDbiOverrideFilter(aTable    : TffDataSet;
                             aExprTree : pCANExpr;
                             aTimeout  : TffWord32) : TffResult;
begin
  Result := aTable.OverrideFilterEx(aExprTree, aTimeout);
end;

function FFDbiPackTable(aDatabase  : TffDatabase;
                  const aTableName : TffTableName;
                    var aTaskID    : LongInt) : TffResult;
begin
  Result := aDatabase.PackTable(aTableName, aTaskID);
end;

function FFDbiReindexTable(aDatabase  : TffBaseDatabase;
                     const aTableName : TffTableName;
                     const aIndexNum  : Integer;
                       var aTaskID    : LongInt) : TffResult;
begin
  Result := aDatabase.ReIndexTable(aTableName, aIndexNum, aTaskID);
end;

function FFDbiRestoreFilter(aTable : TffDataSet) : TffResult;
begin
  Result := aTable.RestoreFilterEx;
end;

function FFDbiRestructureTable(aDatabase   : TffDatabase;
                         const aTableName  : TffTableName;
                               aDictionary : TffDataDictionary;
                               aFieldMap   : TStrings;
                           var aTaskID     : LongInt) : TffResult;
begin
  Result := aDatabase.RestructureTable(aTableName,
                                       aDictionary,
                                       aFieldMap,
                                       aTaskID);
end;

function FFDbiSetTableAutoIncValue(aTable : TffDataSet;
                             const aValue: TffWord32) : TffResult;
begin
  Result := aTable.SetTableAutoIncValue(aValue);
end;

function FFDbiSetFailSafeTransaction(aDatabase : TffBaseDatabase;
                               const aFailSafe : Boolean) : TffResult;
begin
  aDatabase.FailSafe := aFailSafe;
  Result := DBIERR_NONE;
end;

function FFDbiSetFilter(aTable    : TffDataSet;
                        aExprTree : pCANExpr;
                  const aTimeout  : TffWord32) : TffResult;
begin
  Result := aTable.SetFilterEx(aExprTree, aTimeout);
end;

procedure FFDbiSetProtocol(aProtocol : TffCommsProtocolClass);
begin
  ffclProtocol := aProtocol;
end;

procedure FFDbiSetLoginRetries(const aRetries : Byte);
begin
  if aRetries > 0 then
    ffclLoginRetries := aRetries;
end;

procedure FFDbiSetLoginParameters(const aUser     : TffName;
                                  const aPassword : TffName );
begin
  ffclUsername := aUser;
  ffclPassword := aPassword;
end;

end.

