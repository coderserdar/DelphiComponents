{*********************************************************}
{* FSSQL: SQL Engine class                               *}
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

{$I fsdefine.inc}

{$DEFINE SQLSupported}

Unit fssqleng;

Interface

Uses
  Classes,
  windows,
  SysUtils,
  fsconst,
  fsllbase,
  fslleng,
  fsllexcp,
  fslldict,
  fssql,
  //Activex,
  fssqlbas,
  fssrbase,
  fssrbde,
  fssrcvex,
  fssqldb, {!!.11}
  fsdictserveraccess,
  fsserverclass,
  fsfunInterp;

Type
  { A prepared statement is an SQL query in tokenized, parsed, executable form.
    All SQL statements must become prepared statements before they can be
    executed (whether the client specifically "prepares" them or not).  Trigger
    and stored procedure code is loaded from the relevant system tables into
    a persistent prepared statement so that it can be executed on demand. }
  TfsSqlPreparedStatement = Class(TfsBasePreparedStmt) {!!.01} {!!.10}
  Protected {private}
    spsParser: TfsSql;
    //      FTimeout : Longint;                                            {Deleted !!.01}
    spsDatabaseProxy: TfsSqlDatabaseProxy; {!!.11}
  Public
    Constructor Create2(anEngine: TFSServer; {!!.01}
      aClientID: TffClientID;
      aDatabaseID: TffDatabaseID;
      aTimeout: Longint);
    Destructor Destroy; Override;
    Procedure Bind; Override; {!!.11}
    Function Execute(Var aLiveResult: Boolean; {!!.10}
      Var aCursorID: TffCursorID;
      Var aRowsAffected: Integer;
      Var aRecordsRead: Integer): TffResult; Override; {!!.10}
    Function Parse(aQuery: PChar): Boolean; Override; {!!.10}
    Function SetParams(aNumParams: Word; aParamInfo: PfsSqlParamInfoList; aDataBuffer: PffByteArray): TffResult;
  End;

  { The SQL engine orchestrates all the activity in the server relevant to
    SQL processing. The TFSServer class hands off SQL requests to the
    SQL engine for processing, which in turn hands off results back to the
    TFSServer for return to the client.

    There is only one instance of TFSSQLEngine and that is SQLEngine declared
    within this unit.  All communication with the SQL Engine can take place
    through this global variable. }
  TFSSQLEngine = Class(TFSBaseSQLEngine)
  Protected { private}
  Protected
    sqlPreparedStatementsList: TfsSrcStmtList;
  Public
    // source execute
    Engine: TFSServer; // for alloc and prepare and exec
    Query: String;
    ClientID: TffClientID;
    DatabaseID: TffDatabaseID;
    Timeout: Longint;
    OpenMode: TffOpenMode;
    StmtID: TffSqlStmtID;
    SrcCursor: TfsSrBaseCursor;

    Constructor Create(aOwner: TComponent); Override;
    Destructor Destroy; Override;

    Function Alloc(anEngine: TFSBaseServerEngine;
      aClientID: TffClientID;
      aDatabaseID: TffDatabaseID;
      aTimeout: Longint;
      Var aStmtID: TffSqlStmtID): TffResult; Override;
    {- Implementation of DbiQAlloc }

{Begin !!.01}
    Procedure CollectGarbage; Override;
    { Clears out SQL prepared statements that were still active when
      the client sent a free statement request. }
{End !!.01}

    Function Exec(aStmtID: TffSqlStmtID;
      aOpenMode: TffOpenMode;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult; Override;
    {- Implementation of DbiQExec.  aCursorID is 0 if no result set
       returned. aStream is used to pass back dictionary for the
       result set (only if aCursorID <> 0). }

    Function ExecDirect(anEngine: TFSBaseServerEngine;
      aClientID: TffClientID;
      aDatabaseID: TffDatabaseID;
      aQueryText: PChar;
      aOpenMode: TffOpenMode;
      aTimeout: Longint;
      Var aCursorID: TffCursorID;
      aStream: TStream): TffResult; Override;
    {- Implementation of DbiQExecDirect.  aCursorID is 0 if no result set
       is returned. aStream is used to pass back dictionary for the
       result set. }

    Function FreeStmt(aStmtID: TffSqlStmtID): TffResult; Override;
    {- Implementation of DbiQFree }

    Function Prepare(aStmtID: TffSqlStmtID;
      aQueryText: PChar;
      aStream: TStream): TffResult; Override;
    {- Implementation of DbiQPrepare }

{Begin !!.03}
    Procedure RemoveForClient(Const aClientID: TffClientID); Override;
    {- Remove the prepared statements associated with a particular client. }

    Procedure RequestClose; Override;
    {- Ask the remaining SQL prepared statements to close. This occurs
       when preparing for shutdown with the goal of preventing a cursor
       being freed before its SQL table proxy is freed. }
{End !!.03}

    Function SetParams(aStmtID: TffSqlStmtID;
      aNumParams: Word;
      aParamDescs: PfsSqlParamInfoList;
      aDataBuffer: PffByteArray;
      aStream: TStream): TffResult; Override;
    {- Implementation of DbiQSetParams }

  End;

Implementation

Uses
  {$IFDEF DCC6OrLater}
  Variants,
  {$ENDIF}
  fsstdate,
  fsutil;

Type
  PComp = ^Comp;

  {===TfsSqlPreparedStatement==========================================}

Constructor TfsSqlPreparedStatement.Create2(anEngine: TFSServer; {!!.01}
  aClientID: TffClientID;
  aDatabaseID: TffDatabaseID;
  aTimeout: Longint);
Var {!!.10}
  parentDB: TfsSrcDatabase; {!!.10}
Begin
  Inherited Create(aTimeout); {!!.01}
  bpsEngine := anEngine;
  bpsDatabaseID := aDatabaseID;
  spsParser := TfsSql.Create(Nil);
  bpsClientID := aClientID;
  soClient := TfsSrcClient(aClientID); {!!.01} {!!.10}
  parentDB := TfsSrcDatabase(bpsDatabaseID);
  parentDB.StmtList.BeginWrite;
  Try
    parentDB.StmtList.AddStmt(Self);
  Finally
    parentDB.StmtList.EndWrite;
  End;
End;
{--------}

Destructor TfsSqlPreparedStatement.Destroy;
Begin
  spsDatabaseProxy.Free; {!!.12}
  If assigned(spsParser) Then
    Begin
      If spsParser.RootNode <> Nil Then
        Begin
          spsParser.RootNode.Free;
          spsParser.RootNode := Nil;
        End;
      spsParser.Free;
    End;
  Inherited Destroy;
End;
{Begin !!.11}
{--------}

Procedure TfsSqlPreparedStatement.Bind;
Begin
  If (spsParser <> Nil) And
    (spsParser.RootNode <> Nil) Then
    Begin
      spsDatabaseProxy := TfsSqlDatabaseProxy.Create(bpsEngine, bpsDatabaseID);
      spsParser.RootNode.Bind(bpsClientID, 0, spsDatabaseProxy);
    End;
End;
{--------}

Function TfsSqlPreparedStatement.Execute(Var aLiveResult: Boolean;
  Var aCursorID: TffCursorID;
  Var aRowsAffected: Integer;
  Var aRecordsRead: Integer): TffResult;
Begin
  Result :=
    spsParser.RootNode.Execute(aLiveResult, aCursorID,
    aRowsAffected, aRecordsRead);
End;
{End !!.11}
{--------}

Function TfsSqlPreparedStatement.Parse(aQuery: PChar): Boolean;
Begin
  If spsParser.RootNode <> Nil Then
    Begin
      spsParser.RootNode.Free;
      spsParser.RootNode := Nil;
    End;
  spsParser.SourceStream.SetSize(StrLen(aQuery) + 1);
  move(aQuery^, spsParser.SourceStream.Memory^, StrLen(aQuery) + 1);
  spsParser.Execute;
  Result := spsParser.Successful;
End;
{--------}

Function TfsSqlPreparedStatement.SetParams(aNumParams: Word;
  aParamInfo: PfsSqlParamInfoList;
  aDataBuffer: PffByteArray): TffResult;
Var
  I: Integer;
  Value: Variant;
  FieldBuffer: PffByteArray;
  D: Double;
  W: WideString;
  WC: WideChar;
  DT: TDateTime;
  VPtr: PByte;
Begin
  Result := DBIERR_NONE;
  For I := 0 To aNumParams - 1 Do
    Begin
      With aParamInfo^[I] Do
        Begin
          If piName = '' Then { named parameter }
            piName := ':' + IntToStr(piNum); { unnamed parameter }
          If Not piIsNull Then
            Begin
              FieldBuffer := PffByteArray(DWord(aDataBuffer) + piOffset);
              Case piType Of
                fstBoolean:
                  Value := Boolean(FieldBuffer^[0]);
                fstSingleChar:
                  Value := Char(FieldBuffer^[0]);
                fstSingleWideChar:
                  Begin
                    WC := PWideChar(FieldBuffer)^;
                    W := WC;
                    Value := W;
                  End;
                fstUInt8:
                  Value := PByte(FieldBuffer)^;
                fstUInt16:
                  Value := PWord(FieldBuffer)^;
                fstUInt32:
                  Begin
                    D := PDWord(FieldBuffer)^;
                    Value := D;
                  End;
                fstInt8:
                  Value := PShortInt(FieldBuffer)^;
                fstInt16:
                  Value := PSmallInt(FieldBuffer)^;
                fstInt32:
                  Value := PLongint(FieldBuffer)^;
                fstAutoInc32:
                  Value := PLongint(FieldBuffer)^;
                fstSingle:
                  Value := PSingle(FieldBuffer)^;
                fstDouble:
                  Value := PDouble(FieldBuffer)^;
                fstExtended:
                  Value := PExtended(FieldBuffer)^;
                fstInt64, fstAutoInc64, fstRecVersion:
                  Begin
                    {$IFDEF IsNoVariantInt64}
                    TVarData(Value).VType := VT_DECIMAL;
                    Decimal(Value).lo64 := pint64(FieldBuffer)^;
                    {$ELSE}
                    Value := pint64(FieldBuffer)^;
                    {$ENDIF}
                  End;
                fstCurrency:
                  Value := PCurrency(FieldBuffer)^;
                //fstBcd:
                 // Value := PBcd(FieldBuffer)^;
                fstDate:
                  Value := StDateToDateTime(PStDate(FieldBuffer)^);
                fstTime:
                  Value := StTimeToDateTime(PStTime(FieldBuffer)^);
                fstDateTime:
                  Begin
                    DT := PffDateTime(FieldBuffer)^;
                    VarCast(Value, DT - 693594, varDate); {!!.11}
                  End;
                fstShortString:
                  Value := PShortString(FieldBuffer)^;
                fstVarNullString,fstNullString:
                  Value := String(PChar(FieldBuffer));
                fstWideString,fstVarWideString:
                  Value := WideString(PWideChar(FieldBuffer));
                {Begin !!.13}
                fstBLOB..fstBLOBGraphic:
                  If piLength = 0 Then
                    Value := ''
                  Else
                    Begin
                      Value := VarArrayCreate([1, piLength], varByte);
                      VPtr := VarArrayLock(Value);
                      Try
                        Move(FieldBuffer^, VPtr^, piLength);
                      Finally
                        VarArrayUnlock(Value);
                      End;
                    End;
                {End !!.13}
                Else
                  Raise Exception.Create('Unsupported field type');
              End;
            End
          Else
            Value := null;
          spsParser.RootNode.SetParameter(I, Value);
        End;
    End;
End;
{====================================================================}

{===TFSSQLEngine=====================================================}

Constructor TFSSQLEngine.Create(aOwner: TComponent);
Begin
  Inherited Create(aOwner);

  {create list for prepared statements}
  sqlPreparedStatementsList := TfsSrcStmtList.Create;
End;
{--------}

Destructor TFSSQLEngine.Destroy;
Begin
  sqlPreparedStatementsList.Free;
  Inherited Destroy;
End;
{--------}

Function TFSSQLEngine.Exec(aStmtID: TffSqlStmtID;
  aOpenMode: TffOpenMode;
  Var aCursorID: TffCursorID;
  aStream: TStream): TffResult;
Var
  L: Integer;
  RowsAffected: Integer; {!!.10}
  RecordsRead: Integer; {!!.10}
  LiveResult: Boolean; {!!.10}
  aProc, aProcParam: String;
  Cursor: TfsSrBaseCursor;
  aIndex: Word;
Begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  Try
    aCursorID := 0;
    OpenMode := aOpenMode;
    StmtID := aStmtID;
    aIndex := 0;
    Result := DBIERR_NONE;
    // first search procedure , if exists then begin procedure  else
    aProc := IsProcedure(Query, aProcParam, True);
    If aProc <> '' Then
      Begin
        FFSetRetry(Timeout);
        LiveResult := aOpenMode = omReadWrite;
        Cursor := TfsSrcCursor.Create(TFSServer(Engine), TfsSrcDatabase(DatabaseID), Timeout);
        Try
          Cursor.ExecProcedure(aProc, aProcParam, aCursorID);
          If aCursorID <> 0 Then
            TfsSrcCursor(aCursorID).TableName := AnsiUpperCase(aProc);

          If Assigned(aStream) Then
            Begin
              If (aCursorID <> 0) Then
                Begin
                  // select
                  RowsAffected := -1;
                  RecordsRead := 1;
                  Assert(TObject(ACursorID) Is TfsSrBaseCursor);
                  aStream.Write(aCursorID, SizeOf(aCursorID));
                  TfsSrBaseCursor(ACursorID).Dictionary.WriteToStream(aStream);
                  SrcCursor := TfsSrBaseCursor(ACursorID);
                  aIndex := SrcCursor.IndexID;
                  aStream.Write(LiveResult, SizeOf(LiveResult));
                  aStream.Write(RecordsRead, SizeOf(RecordsRead));
                  aStream.Write(aIndex, SizeOf(aIndex));
                End
              Else
                Begin
                  RowsAffected := -1;
                  RecordsRead := 0;
                  // delete update insert
                  SrcCursor := TfsSrBaseCursor(ACursorID);
                  aStream.Write(aCursorID, SizeOf(aCursorID)); //zero
                  aStream.Write(RowsAffected, SizeOf(RowsAffected));
                  aStream.Write(RecordsRead, SizeOf(RecordsRead));
                  aStream.Write(aIndex, SizeOf(aIndex));
                End;
            End;
        Finally
          Cursor.Free;
        End;
      End
    Else
      Begin
        With sqlPreparedStatementsList.Stmt[ftFromID, aStmtID] Do
          Begin
            Activate;
            Try
              FFSetRetry(Timeout);
              LiveResult := aOpenMode = omReadWrite;
              Result := Execute(LiveResult, aCursorID, RowsAffected,
                RecordsRead);
              If Result <> 0 Then
                Exit;
              If Assigned(aStream) Then
                Begin
                  If (aCursorID <> 0) Then
                    Begin
                      // select
                      Assert(TObject(ACursorID) Is TfsSrBaseCursor);
                      aStream.Write(aCursorID, SizeOf(aCursorID));
                      TfsSrBaseCursor(ACursorID).Dictionary.WriteToStream(aStream);
                      SrcCursor := TfsSrBaseCursor(ACursorID);
                      aIndex := SrcCursor.IndexID;
                      aStream.Write(LiveResult, SizeOf(LiveResult));
                      aStream.Write(RecordsRead, SizeOf(RecordsRead));
                      aStream.Write(aIndex, SizeOf(aIndex));
                    End
                  Else
                    Begin
                      // delete update insert
                      SrcCursor := TfsSrBaseCursor(ACursorID);
                      aStream.Write(aCursorID, SizeOf(aCursorID)); //zero
                      aStream.Write(RowsAffected, SizeOf(RowsAffected));
                      aStream.Write(RecordsRead, SizeOf(RecordsRead));
                      aStream.Write(aIndex, SizeOf(aIndex));
                    End;
                End;
            Finally
              Deactivate;
            End;
          End;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerException(E, FEventLog);
        L := length(E.Message);
        If aStream <> Nil Then
          Begin
            aStream.Write(L, sizeof(L));
            aStream.Write(E.Message[1], L);
          End;
      End;
  End;
  {$ENDIF}
End;
{--------}

Function TFSSQLEngine.ExecDirect(anEngine: TFSBaseServerEngine;
  aClientID: TffClientID;
  aDatabaseID: TffDatabaseID;
  aQueryText: PChar;
  aOpenMode: TffOpenMode;
  aTimeout: Longint;
  Var aCursorID: TffCursorID;
  aStream: TStream): TffResult;
Var
  Statement: TfsSqlPreparedStatement;
  L: Integer;
  RowsAffected: Integer; {!!.10}
  RecordsRead: Integer; {!!.10}
  LiveResult: Boolean; {!!.10}
  aProc, aProcParam: String;
  Cursor: TfsSrBaseCursor;
  aIndex: Word;
Begin
  // obs³uga skryptu begin end lub execute procedure
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}

  Assert(anEngine Is TFSServer); {!!.01}
  Query := Trim(strpas(aQueryText));
  ClientID := aClientID;
  DatabaseID := aDatabaseID;
  Timeout := aTimeout;
  OpenMode := aOpenMode;
  StmtID := 0;
  Engine := TFSServer(anEngine);
  SrcCursor := Nil;
  Statement := Nil;
  aIndex := 0;
  Try
    aCursorID := 0;

    // first search procedure, if exists then begin procedure  else
    Result := DBIERR_NONE;
    // first search procedure , if exists then begin procedure  else
    aProc := IsProcedure(Query, aProcParam);
    If aProc <> '' Then
      Begin
        FFSetRetry(Timeout);
        LiveResult := aOpenMode = omReadWrite;
        Cursor := TfsSrcCursor.Create(TFSServer(Engine), TfsSrcDatabase(DatabaseID), Timeout);
        Try
          Cursor.ExecProcedure(aProc, aProcParam, aCursorID);
          If aCursorID <> 0 Then
            TfsSrcCursor(aCursorID).TableName := AnsiUpperCase(aProc);

          If Assigned(aStream) Then
            Begin
              If (aCursorID <> 0) Then
                Begin
                  // select
                  RowsAffected := -1;
                  RecordsRead := 1;
                  Assert(TObject(ACursorID) Is TfsSrBaseCursor);
                  aStream.Write(aCursorID, SizeOf(aCursorID));
                  TfsSrBaseCursor(ACursorID).Dictionary.WriteToStream(aStream);
                  SrcCursor := TfsSrBaseCursor(ACursorID);
                  aIndex := SrcCursor.IndexID;
                  aStream.Write(LiveResult, SizeOf(LiveResult));
                  aStream.Write(RecordsRead, SizeOf(RecordsRead));
                  aStream.Write(aIndex, SizeOf(aIndex));
                End
              Else
                Begin
                  RowsAffected := -1;
                  RecordsRead := 0;
                  // delete update insert
                  SrcCursor := TfsSrBaseCursor(ACursorID);
                  aStream.Write(aCursorID, SizeOf(aCursorID)); //zero
                  aStream.Write(RowsAffected, SizeOf(RowsAffected));
                  aStream.Write(RecordsRead, SizeOf(RecordsRead));
                  aStream.Write(aIndex, SizeOf(aIndex));
                End;
            End;
        Finally
          Cursor.Free;
        End;
      End
    Else
      Begin
        Statement := TfsSqlPreparedStatement.Create2(TFSServer(anEngine), {!!.01}
          aClientID, {!!.01}
          aDatabaseID, aTimeout);
        Try
          If Statement.Parse(aQueryText) Then
            Begin
              LiveResult := aOpenMode = omReadWrite;
              Result := Statement.Execute(LiveResult,
                aCursorID, RowsAffected, RecordsRead);
              If Assigned(aStream) Then
                Begin
                  If (aCursorID <> 0) Then
                    Begin
                      {query}
                      aStream.Write(aCursorID, SizeOf(aCursorID));
                      TfsSrBaseCursor(ACursorID).Dictionary.WriteToStream(aStream);
                      aStream.Write(LiveResult, SizeOf(LiveResult));
                      aStream.Write(RecordsRead, SizeOf(RecordsRead));
                      SrcCursor := TfsSrBaseCursor(ACursorID);
                      aIndex := SrcCursor.IndexID;
                      aStream.Write(aIndex, SizeOf(aIndex));
                    End
                  Else
                    Begin
                      {data manipulation}
                      aStream.Write(aCursorID, SizeOf(aCursorID)); {zero}
                      aStream.Write(RowsAffected, SizeOf(RowsAffected));
                      aStream.Write(RecordsRead, SizeOf(RecordsRead));
                      aStream.Write(aIndex, SizeOf(aIndex));
                    End;
                End;
            End
          Else
            Raise Exception.Create('SQL Syntax error');
        Finally
          Statement.Free;
        End;
      End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerException(E, FEventLog);
        If Statement <> Nil Then
          Begin
            L := Statement.spsParser.ListStream.Size;
            aStream.Write(L, sizeof(L));
            Statement.spsParser.ListStream.Seek(0, 0);
            aStream.CopyFrom(Statement.spsParser.ListStream, L);
          End
        Else
          Begin
            L := 0;
            aStream.Write(L, sizeof(L));
          End;
      End;
  End;
  {$ENDIF}
End;
{--------}

Function TFSSQLEngine.FreeStmt(aStmtID: TffSqlStmtID): TffResult;
Var {!!.10}
  parentDB: TfsSrcDatabase; {!!.10}
  Stmt: TfsBasePreparedStmt; {!!.13}
Begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  Result := DBIERR_NONE;
  Try
    {Begin !!.01}
    Stmt := sqlPreparedStatementsList.Stmt[ftFromID, aStmtID]; {!!.13}
    If Stmt <> Nil Then {!!.13}
      With Stmt Do
        Begin {!!.13}
          If CanClose(True) Then
            Begin
              sqlPreparedStatementsList.DeleteStmt(aStmtID);
              parentDB := TfsSrcDatabase(DatabaseID); {!!.10}
              parentDB.StmtList.BeginWrite; {!!.10}
              Try
                parentDB.StmtList.DeleteStmt(aStmtID); {!!.10}
              Finally
                parentDB.StmtList.EndWrite; {!!.10}
              End;
            End
          Else
            RequestClose;
        End; { with }
    {End !!.01}
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerException(E, FEventLog);
      End;
  End;
  {$ENDIF}
End;
{--------}

Function TFSSQLEngine.Alloc(anEngine: TFSBaseServerEngine;
  aClientID: TffClientID;
  aDatabaseID: TffDatabaseID;
  aTimeout: Longint;
  Var aStmtID: TffSqlStmtID): TffResult;
Var
  Statement: TfsSqlPreparedStatement;
Begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  aStmtID := 0;
  Result := DBIERR_NONE;
  Assert(anEngine Is TFSServer); {!!.01}
  Query := '';
  ClientID := aClientID;
  DatabaseID := aDatabaseID;
  Timeout := aTimeout;
  OpenMode := omReadWrite;
  StmtID := 0;
  SrcCursor := Nil;
  Engine := TFSServer(anEngine);
  Try
    Statement := TfsSqlPreparedStatement.Create2(TFSServer(anEngine), {!!.01}
      aClientID, {!!.01}
      aDatabaseID, aTimeout);
    Try
      sqlPreparedStatementsList.AddStmt(Statement); {!!.10}
      aStmtID := Statement.Handle; {!!.10}
    Except
      Statement.Free;
      Raise;
    End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerException(E, FEventLog);
      End;
  End;
  {$ENDIF}
End;
{Begin !!.01}
{--------}

Procedure TFSSQLEngine.CollectGarbage;
Begin
  sqlPreparedStatementsList.RemoveUnused;
End;
{End !!.01}
{--------}

Function TFSSQLEngine.Prepare(aStmtID: TffSqlStmtID;
  aQueryText: PChar;
  aStream: TStream): TffResult;
Var
  L: Integer;
  aProc, aProcParam: String;
  Stmt: TfsSqlPreparedStatement;
Begin
  // obs³uga skryptu begin end lub execute procedure
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  Try
    {Begin !!.01}
    Result := DBIERR_NONE;
    Query := Trim(strpas(aQueryText));
    StmtID := aStmtID;
    SrcCursor := Nil;
    aProc := IsProcedure(Query, aProcParam, True);
    If aProc <> '' Then Exit;

    With sqlPreparedStatementsList.Stmt[ftFromID, aStmtID] Do
      Begin
        Activate;
        Try
          {Begin !!.11}
          If Parse(aQueryText) Then
            Bind
          Else
            Raise Exception.Create('SQL syntax error');
          {End !!.11}
        Finally
          Deactivate;
        End;
      End;
    {End !!.01}
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerException(E, FEventLog);
        Stmt := TfsSqlPreparedStatement(sqlPreparedStatementsList.Stmt[ftFromID, aStmtID]);
        If Assigned(aStream) Then
          Begin
            L := Stmt.spsParser.ListStream.Size;
            aStream.Write(L, sizeof(L));
            Stmt.spsParser.ListStream.Seek(0, 0);
            aStream.CopyFrom(Stmt.spsParser.ListStream, L);
          End;
      End;
  End;
  {$ENDIF}
End;
{Begin !!.03}
{--------}

Procedure TFSSQLEngine.RemoveForClient(Const aClientID: TffClientID);
Begin
  sqlPreparedStatementsList.RemoveForClient(aClientID);
End;
{--------}

Procedure TFSSQLEngine.RequestClose;
Begin
  { Free up the remaining SQL prepared statements. }
  sqlPreparedStatementsList.RequestClose;
  sqlPreparedStatementsList.RemoveUnused;
End;
{End !!.03}
{--------}

Function TFSSQLEngine.SetParams(aStmtID: TffSqlStmtID;
  aNumParams: Word;
  aParamDescs: PfsSqlParamInfoList;
  aDataBuffer: PffByteArray;
  aStream: TStream): TffResult;
Var
  Stmt: TfsSqlPreparedStatement;
Begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  Try
    Stmt := TfsSqlPreparedStatement(sqlPreparedStatementsList.Stmt[ftFromID, aStmtID]);
    Stmt.Activate;
    Try
      Result := Stmt.SetParams(aNumParams, aParamDescs, aDataBuffer);
    Finally
      Stmt.Deactivate;
    End;
  Except
    On E: Exception Do
      Begin
        Result := ConvertServerException(E, FEventLog);
      End;
  End;
  {$ENDIF}
End;
{====================================================================}

End.

