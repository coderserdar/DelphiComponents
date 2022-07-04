{$INCLUDE DbxOpenOdbc.inc}

// Conditional defines can be hard-coded in here, or set using project options dialog
// (Project / Options... / Directories/Conditionals / Conditional defines).

// Compiler options:
// Define _RELEASE_ or _DEBUG_ conditional here or in project options,
// or leave undefined to use global project settings
{.$DEFINE _RELEASE_}
{.$DEFINE _DEBUG_}

// Define _DENT_ for DENT driver (Dmitry Arefiev)
{.$DEFINE _DENT_}

{$R-} // Compiled with not range-checking (improve productivity).

{$IFDEF _RELEASE_}
  // Release options:
  {$O+} // Optimization on
  {$R-} // Compiled with not range-checking (improve productivity).
  {$UNDEF _DEBUG_}
  {$UNDEF _TRACE_CALLS_}
{$ENDIF}

{$UNDEF _debug_emulate_stmt_per_con_}
{$UNDEF _debug_blocking_}
{$IFDEF _DEBUG_}
  // Debug options:
  {$O-} // Optimization off
  {$D+,L+} // Include Debug Info
  {$R+}   // Compiled with range-checking (worse productivity).
  {$UNDEF _INLINE_} // Allows to debug a inline code
  {.$R-}    // Compiled with not range-checking (improve productivity).
  // Developer options (only for debugging):
  {.$DEFINE _TRACE_CALLS_} // logging of calls (optional)
  {.$DEFINE _debug_emulate_stmt_per_con_} // It is only for developer testing _InternalCloneConnection_ (emulated fStatementPerConnection).
    {$ifdef _debug_emulate_stmt_per_con_}
      {.$define _debug_blocking_} // High probability of blocking (SQL Server/Read Commited).
      {
        scenario of blocking table:
        ===========================
      1) dbxExplor connect to SQLServer (define _debug_emulate_stmt_per_con_, _debug_blocking_)
      2) open any table
      3) start transaction
      4) change and update data
      5) refresh or ( press "detach query", "refresh")
      6) reopen same table (table is blocked)
      //}
    {$endif}
{$ENDIF} //of: {$IFDEF _DEBUG_}

{$DEFINE _MULTIROWS_FETCH_}
// Explanation of _MULTIROWS_FETCH_
// Support for ODBC "block fetching" (SQL_ATTR_ROW_ARRAY_SIZE)
// Not all ODBC drivers support this mode (SQL_GD_BLOCK).
// We automatically query the ODBC driver to check if it supports this option,
// so you can safely include this option and block fetching will be used if
// the ODBC driver supports it.
// Even if the ODBC driver indicates that it supports block fetching,
// block fetching cannot be used if there are any late-bound fields in the query
// (large fields e.g. MEMO, BLOB are normally late-bound).
// If you have late-bound fields, and you want to use block fetching, you will need
// to use _MIXED_FETCH_ option in addition to _MULTIROWS_FETCH_ (see below).

{$DEFINE _MIXED_FETCH_}
{$IFDEF _MIXED_FETCH_}
  {$IFNDEF _MULTIROWS_FETCH_}
  {$UNDEF _MIXED_FETCH_}
    {$MESSAGE  Warn '_MIXED_FETCH_ ignored because _MULTIROWS_FETCH_ not defined'}
  {$ENDIF}
{$ENDIF}
// Explanation of _MIXED_FETCH_
// (This option only applies if _MULTIROWS_FETCH_ is also set.)
// Set the _MIXED_FETCH_ option if you you want to use block cursors for queries
// with late-bound fields (i.e. large fields).
// For fetching late-bound columns the following ODBC API functions are used:
// SqlBindCol + SqlSetPos + SqlGetData.
// If you have late-bound fields and you want to use block fetching,
// it is necessary to use a static cursor (SQL_CURSOR_STATIC), whereas normally
// we use the more efficient forward-only cursor (SQL_CURSOR_FORWARD_ONLY).
// But with the forward-only cursor, block fetching is not possible if the are
// and late-bound columns.
// You can dynamically control this parameter by using the connection parameter
// coMixedFetch. You can change this parameter before execution of query.
//
// Warnings:
//
// - Static and keyset-driven cursors increase the usage of tempdb. Static server cursors
//   build the entire cursor in tempdb; keyset-driven cursors build the keyset in tempdb.
//
// - Some ODBC drivers incorrectly handle this mode of operation.
//   We have detected bugs with the following drivers:
//   PervasiveSQL ODBC, Version: '08.10.0117 017'.
//   Microsoft SQL 2000, version '08.00.0194'.
//   (In Developer Edition version '08.00.0384' this error is already not present).
//
// - When the ODBC driver does not support SQL_GD_ANY_COLUMN, and you have large fields,
//   it is more efficient to specify the simple fields before large fields (MEMO, BLOB)
//   in the select statment, because all fields beyond the first large field will be late-bound.
//
// You can adjust support of this adjustment separately for each driver.
// Look for procedure RetrieveDriverName.
//
{$DEFINE _RegExprParser_}
{$IFDEF _DisableRegExprParser_}
  {$UNDEF _RegExprParser_}
{$ENDIF}
// Explanation of _RegExprParser_
// For Informix multi-part names, this option must be defined
// For any other RDBMS, can be defined not not essential
// When defined, multi-part table names are parsed using DbxObjectParser
// (which itself uses RegExpr module by Andrey V. Sorokin / Henry Spencer)

{$DEFINE _InternalCloneConnection_}
{$IFDEF _DisableInternalCloneConnection_}
  {$UNDEF _InternalCloneConnection_}
{$ENDIF}
// Explanation of _InternalCloneConnection_
// If the DBMS limits the number simultaneous statements per connection,
// DbExpress will clone the connection externally where necessary.
// If _InternalCloneConnection_ is defined, we report to DbExpress that
// statements per connection are unlimited, and automatically clone a
// new connection internally where necessary. This is particularly useful
// for prompted connects, because we hold the connection string returned by
// the first connect, and we use this for the subsequent cloned connects, so
// the user is prompted only once for connect info. (This is also needed for
// old version of DbExpress which query max simultaneous statements before
// any connect, but Borland fixed this in Delphi 6 SP2.)
// It is possible to set _InternalCloneConnection_ in a line of connection or in
// properties of connection:
//    SQLConnection.Params.Values['Custom String']  := 'coICloneCon=1;...

{$IFOPT R+}
  {$DEFINE RANGECHECKS_ON}
{$ELSE}
  {$DEFINE RANGECHECKS_OFF}
{$ENDIF}
