{*********************************************************}
{* FlashFiler: SQL Symbol Table                          *}
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

{$I ffdefine.inc}

unit ffsqlbas;

interface

uses
  Classes,
  ffllbase,
  ffllcomp,
  fflleng;

type

  { The following type defines the data needed for a SQL parameter. }
  PffSqlParamInfo = ^TffSqlParamInfo;
  TffSqlParamInfo = record          {Information block for SQL parameter}
    piNum         : word;           {..parameter number (1..n)}
    piName        : string[ffcl_GeneralNameSize]; {..parameter name }
    piType        : TffFieldType;   {..data type}
    piOffset      : word;           {..offset in record}
    piLength      : word;           {..length in bytes}
  end;

  TffSqlParamInfoList = array[0..1023] of TffSqlParamInfo;
  PffSqlParamInfoList = ^TffSqlParamInfoList;

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
  TffBaseSQLEngine = class(TffLoggableComponent)
  protected

  public

    destructor Destroy; override;                                      {!!.11}

    function Alloc(anEngine    : TffBaseServerEngine;
                   aClientID   : TffClientID;
                   aDatabaseID : TffDatabaseID;
                   aTimeout    : Longint;
                   var aStmtID : TffSqlStmtID): TffResult; virtual; abstract;
    { Allocates a statement handle for a query.  This method must be
      called prior to Prepare, Exec, etc.  When the statement is no longer
      needed, you must free the statement handle using FreeStmt. }

{Begin !!.01}
    procedure CollectGarbage; virtual; abstract;
      { Use this method to perform garbage collection. Invoked when server
        engine's garbage collection is invoked. }
{End !!.01}

    function Exec(aStmtID: TffSqlStmtID;
                  aOpenMode: TffOpenMode;
                  var aCursorID: TffCursorID;
                  aStream: TStream): TffResult; virtual; abstract;
    { Executes a previously-prepared statement.  Prior to calling this
      method, the client must allocate a statement handle using Alloc and
      specify the query using Prepare.

      aCursorID is set to zero if no result set returned otherwise it is a
      handle to the result set.  If a result set is returned, aStream
      contains a data dictionary defining the structure of the resultset. }

    function ExecDirect(anEngine    : TffBaseServerEngine;
                        aClientID   : TffClientID;
                        aDatabaseID : TffDatabaseID;
                        aQueryText  : PChar;
                        aOpenMode   : TffOpenMode;
                        aTimeout    : Longint;
                    var aCursorID   : TffCursorID;
                        aStream     : TStream): TffResult; virtual; abstract;
    { Prepares and executes a query immediately.

      aCursorID is set to zero if no result set returned otherwise it is a
      handle to the result set.  If a result set is returned, aStream
      contains a data dictionary defining the structure of the resultset. }

{Begin !!.03}
    procedure RequestClose; virtual; abstract;
    { Ask the remaining SQL prepared statements to close. This occurs
      when preparing for shutdown with the goal of preventing a cursor
      being freed before its SQL table proxy is freed. }
{End !!.03}

    function FreeStmt(aStmtID: TffSqlStmtID): TffResult; virtual; abstract;
    { Frees the resources associated with the specified statement.  Must
      be called after a statement has been allocated and executed. }

    function Prepare(aStmtID: TffSqlStmtID;
                     aQueryText: PChar;
                     aStream : TStream): TffResult; virtual; abstract;
    { Use this method to prepare a query for execution.  The client must
      first allocate a statement using Alloc.  Parameters:

        aStmtID is the handle to the statement created by Alloc.
        aQueryText is the SQL statement to prepare.
        aStream is for error reporting.  If the SQL statement cannot be
          prepared (i.e., it is incorrectly formed) or some other error
          occurs, write a descriptive error message to the stream.  The error
          text is used when forming an exception on the client side.  }

{Begin !!.03}
    procedure RemoveForClient(const aClientID : TffClientID); virtual; abstract;
    { Remove the prepared statements associated with a particular client. }
{End !!.03}

    function SetParams(aStmtID     : TffSqlStmtID;
                       aNumParams  : Word;
                       aParamDescs : PffSqlParamInfoList;
                       aDataBuffer : PffByteArray;
                       aStream     : TStream): TffResult; virtual; abstract;
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

  published

  end;


implementation

{===TffBaseSQLEngine======================================================}
destructor TffBaseSQLEngine.Destroy;
begin
  FFNotifyDependents(ffn_Destroy);
  inherited;
end;
{=========================================================================}

end.
