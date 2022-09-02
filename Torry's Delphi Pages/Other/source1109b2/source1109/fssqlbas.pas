{*********************************************************}
{* FSSQL: SQL Symbol Table                               *}
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

{ symbol table, expression tree, etc}

{$I fsdefine.inc}

Unit fssqlbas;

Interface

Uses
  Classes,
  fsllbase,
  fsllcomp,
  fslleng;

Type

  { The following type defines the data needed for a SQL parameter. }
  PfsSqlParamInfo = ^TfsSqlParamInfo;
  TfsSqlParamInfo = Record {Information block for SQL parameter}
    piNum: Word; {..parameter number (1..n)}
    piName: String[fscl_GeneralNameSize]; {..parameter name }
    piType: TfsFieldType; {..data type}
    piOffset: Longint; {..offset in record}
    piLength: Longint; {..length in bytes}
    piIsNull: boolean;
  End;

  TfsSqlParamInfoList = Array[0..1023] Of TfsSqlParamInfo;
  PfsSqlParamInfoList = ^TfsSqlParamInfoList;

  { This is the base class for a FlashFiler SQL engine.  A SQL engine must
    support the following operations:

    1. Execute a prepared statement, returning a cursor to the client.
       Client calls:
         Alloc
         Prepare
         SetParams (optional)
         Exec
         FreeStmt
    2. Execute an unprepared statement, returning a cursor to the client:
       Client calls:
         ExecDirect
  }
  TFSBaseSQLEngine = Class(TfsLoggableComponent)
  Protected

  Public
    Destructor Destroy; Override; {!!.11}

    Function Alloc(anEngine: TFSBaseServerEngine;
      aClientID: TffClientID;
      aDatabaseID: TffDatabaseID;
      aTimeout: Longint;
      Var aStmtID: TffSqlStmtID): TffResult; Virtual; Abstract;
    { Allocates a statement handle for a query.  This method must be
      called prior to Prepare, Exec, etc.  When the statement is no longer
      needed, you must free the statement handle using FreeStmt. }

{Begin !!.01}
    Procedure CollectGarbage; Virtual; Abstract;
    { Use this method to perform garbage collection. Invoked when server
      engine's garbage collection is invoked. }
{End !!.01}

    Function Exec(aStmtID: TffSqlStmtID;
      aOpenMode: TffOpenMode;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult; Virtual; Abstract;
    { Executes a previously-prepared statement.  Prior to calling this
      method, the client must allocate a statement handle using Alloc and
      specify the query using Prepare.

      aCursorID is set to zero if no result set returned otherwise it is a
      handle to the result set.  If a result set is returned, aStream
      contains a data dictionary defining the structure of the resultset. }

    Function ExecDirect(anEngine: TFSBaseServerEngine;
      aClientID: TffClientID;
      aDatabaseID: TffDatabaseID;
      aQueryText: PChar;
      aOpenMode: TffOpenMode;
      aTimeout: Longint;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult; Virtual; Abstract;
    { Prepares and executes a query immediately.

      aCursorID is set to zero if no result set returned otherwise it is a
      handle to the result set.  If a result set is returned, aStream
      contains a data dictionary defining the structure of the resultset. }

{Begin !!.03}
    Procedure RequestClose; Virtual; Abstract;
    { Ask the remaining SQL prepared statements to close. This occurs
      when preparing for shutdown with the goal of preventing a cursor
      being freed before its SQL table proxy is freed. }
{End !!.03}

    Function FreeStmt(aStmtID: TffSqlStmtID): TffResult; Virtual; Abstract;
    { Frees the resources associated with the specified statement.  Must
      be called after a statement has been allocated and executed. }

    Function Prepare(aStmtID: TffSqlStmtID;
      aQueryText: PChar;
      aStream: TStream): TffResult; Virtual; Abstract;
    { Use this method to prepare a query for execution.  The client must
      first allocate a statement using Alloc.  Parameters:

        aStmtID is the handle to the statement created by Alloc.
        aQueryText is the SQL statement to prepare.
        aStream is for error reporting.  If the SQL statement cannot be
          prepared (i.e., it is incorrectly formed) or some other error
          occurs, write a descriptive error message to the stream.  The error
          text is used when forming an exception on the client side.  }

{Begin !!.03}
    Procedure RemoveForClient(Const aClientID: TffClientID); Virtual; Abstract;
    { Remove the prepared statements associated with a particular client. }
{End !!.03}

    Function SetParams(aStmtID: TffSqlStmtID;
      aNumParams: Word;
      aParamDescs: PfsSqlParamInfoList;
      aDataBuffer: PffByteArray;
      aStream: TStream): TffResult; Virtual; Abstract;
    { Use this method to associate values with parameters embedded within
      a prepared statement.  The client must allocate a statement using Alloc
      and prepare the statement using Prepare.  Parameters:

        aStmtID is the handle to the statement created by Alloc.
        aNumParams is the number of parameters being passed to this method.
        aParamDescs is a pointer to the array of parameters.
        aDataBuffer is an array containing the parameter values.  Each param
          description points to its corresponding value in this buffer.
        aStream is for error reporting.  If one or more parameters are invalid
          or some other error occurs, write a descriptive error message to the
          stream.  The error text is used when forming an exception on the
          client side. }

  Published

  End;

Implementation

{===TFSBaseSQLEngine======================================================}

Destructor TFSBaseSQLEngine.Destroy;
Begin
  FFNotifyDependents(ffn_Destroy);
  Inherited;
End;
{=========================================================================}

End.

