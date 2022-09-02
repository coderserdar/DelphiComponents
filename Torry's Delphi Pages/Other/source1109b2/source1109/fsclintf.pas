{*********************************************************}
{* FSSQL: Non-native BDE Client Interface Routines  *}
{*********************************************************}
{NOTE:                                                    }
{ The FSDbiRoutines are slowly being phased out. Their    }
{ functions have been added to the appropriate FF         }
{ components. These functions are provided for backwards  }
{ compatiblity, and may be removed in the next major      }
{ version of FSSQL. USE AT YOUR OWN RISK!            }
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
 * The Original Code is TurboPower FSSQL
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

Unit fsclintf;

Interface

Uses
  fssrbde,
  fsllbase,
  fslldict,
  fsllprot,
  fsdb,
  fsclbase,
  Classes;

Function FSDbiAddAlias(aSession: TFSSession;
  Const aAlias: TffName;
  Const aPath: TffPath): TffResult;
{-Add a new permanent alias}
{-TFSSession.AddAliasEx should now be used instead}

Function FSDbiAddFileBLOB(aTable: TFSDataSet;
  Const iField: Word;
  Const aFileName: TffFullFileName): TffResult;
{-Add a file BLOB to a FSSQL table}
{-TfsTable.AddFileBlobEx should now be used instead}

Function FSDbiAddIndex(aTable: TFSBaseTable;
  Const aIndexDesc: TffIndexDescriptor;
  Var aTaskID: Longint): TffResult;
{-Add an index to a FSSQL table}
{-TfsTable.AddIndexEx should now be used instead}

Function FSDbiCreateTable(aDatabase: TFSDatabase;
  Const aOverWrite: Boolean;
  Const aTableName: TfsTableName;
  aDictionary: TFSInfoDict): TffResult;
{-Create a FlashfFiler table}
{-TFSDatabase.CreateTableEx should now be used instead}

Function FSDbiDeleteAlias(aSession: TFSSession;
  Const aAlias: TffName): TffResult;
{-Delete an alias permanently}
{-TFSSession.DeleteAliasEx should now be used instead}

Function FSDbiGetRecordBatch(aTable: TFSDataSet;
  Const aRequestCount: Longint;
  Var aReturnCount: Longint;
  pRecBuff: Pointer): TffResult;
{-get a batch of records}
{ pRecBuff must be allocated to hold RequestCount * RecordLength recs}
{-TfsTable.GetRecordBatch should now be used instead}

Function FSDbiGetRecordBatchEx(aTable: TFSDataSet;
  Const aRequestCount: Longint;
  Var aReturnCount: Longint;
  pRecBuff: Pointer;
  Var aError: TffResult): TffResult;
{-get a batch of records}
{ pRecBuff must be allocated to hold RequestCount * RecordLength recs}
{-TfsTable.GetRecordBatchEx should now be used instead}

Function FSDbiGetServerDateTime(aSession: TFSSession;
  Var aServerNow: TDateTime): TffResult;
{-get the current date and time at the server}
{ NOTE: the returned date and time is with respect to the time zone
        of the SERVER, not the CLIENT. If the server and client are
        in different time zones, you are responsible for any
        conversion.}
{-TFSSession.GetServerDateTime should now be used instead}

Function FSDbiGetTaskStatus(aSession: TFSSession;
  Const aTaskID: Longint;
  Var aCompleted: Boolean;
  Var aStatus: TffRebuildStatus): TffResult;
{-Query the status of a given pack, reindex, or restructure operation}
{-TFSSession.GetTaskStatus should now be used instead}

Function FSDbiInsertRecordBatch(aTable: TFSDataSet;
  Const aCount: Longint;
  pRecBuff: Pointer;
  Var aErrors: PffLongIntArray): TffResult;
{-insert a batch of records}
{Errors must be allocated to hold Count * sizeof( LongInt )}
{-TfsTable.InsertRecordBatch should now be used instead}

Function FSDbiOverrideFilter(aTable: TFSDataSet;
  aExprTree: pCANExpr;
  aTimeout: TffWord32): TffResult;
{-Used internally to override a cursor's existing filter with a new filter.
  Occurs when a locate must used a ranged dataset. }

Function FSDbiPackTable(aDatabase: TFSDatabase;
  Const aTableName: TfsTableName;
  Var aTaskID: Longint): TffResult;
{-Recover disk space occupied by deleted records in a table}
{-TFSDatabase.PackTable or}
{-TfsTable.PackTableEx should now be used instead}

Function FSDbiReindexTable(aDatabase: TFSBaseDatabase;
  Const aTableName: TfsTableName;
  Const aIndexNum: Integer;
  Var aTaskID: Longint): TffResult;
{-Reconstruct key values for an index on a given table}
{-TFSDatabase.ReindexTable or}
{-TfsTable.ReindexTableEx should now be used instead}

Function FSDbiRestoreFilter(aTable: TFSDataSet): TffResult;
{-After a locate has finished overriding the server-side filter, this
  method is used to restore the cursor's original filter. }

Function FSDbiRestructureTable(aDatabase: TFSDatabase;
  Const aTableName: TfsTableName;
  aDictionary: TFSInfoDict;
  aFieldMap: TStrings;
  Var aTaskID: Longint;
  aRangeError: boolean): TffResult;
{-Change the layout of an existing FF table}
{-TFSDatabase.RestructureTable or}
{-TfsTable.RestructureTableEx should now be used instead}

Function FSDbiSetFailSafeTransaction(aDatabase: TFSBaseDatabase;
  Const aFailSafe: Boolean): TffResult;
{-Enable/disable failsafe transactions}
{TFSDatabase.FailSafe property should now be used instead}

Function FSDbiSetFilter(aTable: TFSDataSet;
  aExprTree: pCANExpr;
  Const aTimeout: TffWord32): TffResult;
{-set the serverside filter for this cursor}
{-TfsTable.SetFilterEx should now be used instead}

Procedure FSDbiSetProtocol(aProtocol: TfsCommsProtocolClass);
{-change the protocol type of future FSSQL client sessions}

Procedure FSDbiSetLoginRetries(Const aRetries: Byte);
{-change the allowable number of login retries by a client}

Procedure FSDbiSetLoginParameters(Const aUser: TffName;
  Const aPassword: TffName);
{-change the client username and password for future FF client sessions}

Function FSDbiSetTableAutoIncValue(aTable: TFSDataSet;
  Const aValue: Int64; Const aStep: Longint): TffResult;
{-Set the autoinc seed value for a FF table}
{TfsTable.SetTableAutoIncValue should now be used instead}

Implementation
Uses
  SysUtils;

Function FSDbiAddAlias(aSession: TFSSession;
  Const aAlias: TffName;
  Const aPath: TffPath): TffResult;
Begin
  Result := aSession.AddAliasEx(aAlias, aPath, False); {!!.11}
End;

Function FSDbiAddFileBLOB(aTable: TFSDataSet;
  Const iField: Word;
  Const aFileName: TffFullFileName): TffResult;
Begin
 // Result := aTable.AddFileBlob(iField, aFileName);
 Result := TffResult(0) ; 
End;

Function FSDbiAddIndex(aTable: TFSBaseTable;
  Const aIndexDesc: TffIndexDescriptor;
  Var aTaskID: Longint): TffResult;
Begin
  Result := aTable.AddIndexEx(aIndexDesc, aTaskID);
End;

Function FSDbiCreateTable(aDatabase: TFSDatabase;
  Const aOverWrite: Boolean;
  Const aTableName: TfsTableName;
  aDictionary: TFSInfoDict): TffResult;
Begin
  Result := aDatabase.CreateTable(aOverWrite, aTableName, aDictionary);
End;

Function FSDbiDeleteAlias(aSession: TFSSession;
  Const aAlias: TffName): TffResult;
Begin
  Result := aSession.DeleteAliasEx(aAlias);
End;

Function FSDbiGetRecordBatch(aTable: TFSDataSet;
  Const aRequestCount: Longint;
  Var aReturnCount: Longint;
  pRecBuff: Pointer): TffResult;
Begin
  Result := aTable.GetRecordBatch(aRequestCount,
    aReturnCount,
    pRecBuff);
End;

Function FSDbiGetRecordBatchEx(aTable: TFSDataSet;
  Const aRequestCount: Longint;
  Var aReturnCount: Longint;
  pRecBuff: Pointer;
  Var aError: TffResult): TffResult;
Begin
  Result := aTable.GetRecordBatchEx(aRequestCount,
    aReturnCount,
    pRecBuff,
    aError);
End;

Function FSDbiGetServerDateTime(aSession: TFSSession;
  Var aServerNow: TDateTime): TffResult;
Begin
  Result := aSession.GetServerDateTime(aServerNow);
End;

Function FSDbiGetTaskStatus(aSession: TFSSession;
  Const aTaskID: Longint;
  Var aCompleted: Boolean;
  Var aStatus: TffRebuildStatus): TffResult;
Begin
  Result := aSession.GetTaskStatus(aTaskID, aCompleted, aStatus);
End;

Function FSDbiInsertRecordBatch(aTable: TFSDataSet;
  Const aCount: Longint;
  pRecBuff: Pointer;
  Var aErrors: PffLongIntArray): TffResult;
Begin
  Result := aTable.InsertRecordBatch(aCount,
    pRecBuff,
    aErrors);
End;

Function FSDbiOverrideFilter(aTable: TFSDataSet;
  aExprTree: pCANExpr;
  aTimeout: TffWord32): TffResult;
Begin
  Result := aTable.OverrideFilterEx(aExprTree, aTimeout);
End;

Function FSDbiPackTable(aDatabase: TFSDatabase;
  Const aTableName: TfsTableName;
  Var aTaskID: Longint): TffResult;
Begin
  Result := aDatabase.PackTable(aTableName, aTaskID, False, False);
End;

Function FSDbiReindexTable(aDatabase: TFSBaseDatabase;
  Const aTableName: TfsTableName;
  Const aIndexNum: Integer;
  Var aTaskID: Longint): TffResult;
Begin
  Result := aDatabase.ReIndexTable(aTableName, aIndexNum, aTaskID);
End;

Function FSDbiRestoreFilter(aTable: TFSDataSet): TffResult;
Begin
  Result := aTable.RestoreFilterEx;
End;

Function FSDbiRestructureTable(aDatabase: TFSDatabase;
  Const aTableName: TfsTableName;
  aDictionary: TFSInfoDict;
  aFieldMap: TStrings;
  Var aTaskID: Longint;
  aRangeError: boolean): TffResult;
Begin
  Result := aDatabase.RestructureTable(aTableName,
    aDictionary,
    aFieldMap,
    aTaskID,
    aRangeError);
End;

Function FSDbiSetTableAutoIncValue(aTable: TFSDataSet;
  Const aValue: Int64; Const aStep: Longint): TffResult;
Begin
  Result := aTable.SetTableAutoIncValue(aValue, aStep);
End;

Function FSDbiSetFailSafeTransaction(aDatabase: TFSBaseDatabase;
  Const aFailSafe: Boolean): TffResult;
Begin
  aDatabase.FailSafe := aFailSafe;
  Result := DBIERR_NONE;
End;

Function FSDbiSetFilter(aTable: TFSDataSet;
  aExprTree: pCANExpr;
  Const aTimeout: TffWord32): TffResult;
Begin
  Result := aTable.SetFilterEx(aExprTree, aTimeout);
End;

Procedure FSDbiSetProtocol(aProtocol: TfsCommsProtocolClass);
Begin
  fsclProtocol := aProtocol;
End;

Procedure FSDbiSetLoginRetries(Const aRetries: Byte);
Begin
  If aRetries > 0 Then
    fsclLoginRetries := aRetries;
End;

Procedure FSDbiSetLoginParameters(Const aUser: TffName;
  Const aPassword: TffName);
Begin
  fsclUsername := aUser;
  fsclPassword := aPassword;
End;

End.

