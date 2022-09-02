{*********************************************************}
{* FlashFiler: SQL Engine class                          *}
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

{$DEFINE SQLSupported} 

unit ffsqleng;

interface

uses
  classes,
  windows,
  sysutils,
  ffconst,
  ffllbase,
  fflleng,
  ffllexcp,
  fflldict,
  ffsql,
  ffsqlbas,
  ffsrbase,
  ffsrbde,
  ffsrcvex,
  ffsqldb,                                                             {!!.11}
  fftbdict,
  ffsreng;


type
  { A prepared statement is an SQL query in tokenized, parsed, executable form.
    All SQL statements must become prepared statements before they can be
    executed (whether the client specifically "prepares" them or not).  Trigger
    and stored procedure code is loaded from the relevant system tables into
    a persistent prepared statement so that it can be executed on demand. }
  TffSqlPreparedStatement = class(TffBasePreparedStmt)                 {!!.01}{!!.10}
    protected {private}
      spsParser : TffSQL;
//      FTimeout : Longint;                                            {Deleted !!.01}
      spsDatabaseProxy : TFFSqlDatabaseProxy;                          {!!.11}
    public
      constructor Create2(anEngine : TffServerEngine;                  {!!.01}
                          aClientID : TffClientID;
                          aDatabaseID: TffDatabaseID;
                          aTimeout : Longint);
      destructor Destroy; override;
      procedure Bind; override;                                        {!!.11}
      function Execute(var aLiveResult: Boolean;                       {!!.10}
                       var aCursorID: TffCursorID;
                       var aRowsAffected: Integer;
                       var aRecordsRead: Integer): TffResult; override;{!!.10}
      function Parse(aQuery: PChar): Boolean; override;                {!!.10}
      function SetParams(aNumParams: Word; aParamInfo: PffSqlParamInfoList; aDataBuffer: PffByteArray): TffResult;
    end;

  { The SQL engine orchestrates all the activity in the server relevant to
    SQL processing. The TffServerEngine class hands off SQL requests to the
    SQL engine for processing, which in turn hands off results back to the
    TffServerEngine for return to the client.

    There is only one instance of TffSqlEngine and that is SQLEngine declared
    within this unit.  All communication with the SQL Engine can take place
    through this global variable. }
  TffSqlEngine = class(TffBaseSQLEngine)
    protected { private}
    protected
      sqlPreparedStatementsList: TffSrStmtList;
    public
      constructor Create(aOwner : TComponent); override;

      destructor Destroy; override;

      function Alloc(anEngine : TffBaseServerEngine;
                     aClientID: TffClientID;
                     aDatabaseID: TffDatabaseID;
                     aTimeout : Longint;
                     var aStmtID: TffSqlStmtID): TffResult; override;
      {- Implementation of DbiQAlloc }

{Begin !!.01}
      procedure CollectGarbage; override;
        { Clears out SQL prepared statements that were still active when
          the client sent a free statement request. }
{End !!.01}

      function Exec(aStmtID: TffSqlStmtID;
                    aOpenMode: TffOpenMode;
                    var aCursorID: TffCursorID;
                    aStream: TStream): TffResult; override;
      {- Implementation of DbiQExec.  aCursorID is 0 if no result set
         returned. aStream is used to pass back dictionary for the
         result set (only if aCursorID <> 0). }

      function ExecDirect(anEngine    : TffBaseServerEngine;
                          aClientID   : TffClientID;
                          aDatabaseID : TffDatabaseID;
                          aQueryText  : PChar;
                          aOpenMode   : TffOpenMode;
                          aTimeout    : Longint;
                      var aCursorID   : TffCursorID;
                          aStream     : TStream): TffResult; override;
      {- Implementation of DbiQExecDirect.  aCursorID is 0 if no result set
         is returned. aStream is used to pass back dictionary for the
         result set. }

      function FreeStmt(aStmtID: TffSqlStmtID): TffResult; override;
      {- Implementation of DbiQFree }

      function Prepare(aStmtID: TffSqlStmtID;
                       aQueryText: PChar;
                       aStream : TStream): TffResult; override;
      {- Implementation of DbiQPrepare }

{Begin !!.03}
      procedure RemoveForClient(const aClientID : TffClientID); override;
      {- Remove the prepared statements associated with a particular client. }

      procedure RequestClose; override;
      {- Ask the remaining SQL prepared statements to close. This occurs
         when preparing for shutdown with the goal of preventing a cursor
         being freed before its SQL table proxy is freed. }
{End !!.03}

      function SetParams(aStmtID: TffSqlStmtID;
                         aNumParams: Word;
                         aParamDescs: PffSqlParamInfoList;
                         aDataBuffer: PffByteArray;
                         aStream : TStream): TffResult; override;
      {- Implementation of DbiQSetParams }

    end;

implementation

uses
{$IFDEF DCC6OrLater}
  Variants,
{$ENDIF}
  ffstdate;

type
  PComp = ^Comp;

{===TffSqlPreparedStatement==========================================}
constructor TffSqlPreparedStatement.Create2(anEngine : TffServerEngine; {!!.01}
                                            aClientID : TffClientID;
                                            aDatabaseID: TffDatabaseID;
                                            aTimeout : Longint);
var                                                                    {!!.10}
  parentDB : TffSrDatabase;                                            {!!.10}
begin
  inherited Create(aTimeout);                                          {!!.01}
  bpsEngine := anEngine;
  bpsDatabaseID := aDatabaseID;
  spsParser := TffSql.Create(nil);
  bpsClientID := aClientID;
  soClient := TffSrClient(aClientID);                                  {!!.01}{!!.10}
//  FTimeout := aTimeout;                                              {Deleted !!.01}
{Begin !!.10}
  parentDB := TffSrDatabase(bpsDatabaseID);
  parentDB.StmtList.BeginWrite;
  try
    parentDB.StmtList.AddStmt(Self);
  finally
    parentDB.StmtList.EndWrite;
  end;
{End !!.10}
end;
{--------}
destructor TffSqlPreparedStatement.Destroy;
begin
  spsDatabaseProxy.Free;                                               {!!.12}
  if assigned(spsParser) then begin
    if spsParser.RootNode <> nil then begin
      spsParser.RootNode.Free;
      spsParser.RootNode := nil;
    end;
    spsParser.Free;
  end;
{Begin !!.10}
  { Assumption: By this point, the SQL prepared statement has been removed
    or is in the processor of being removed from the client's statement list. }
{End !!.10}
  inherited Destroy;
end;
{Begin !!.11}
{--------}
procedure TffSqlPreparedStatement.Bind;
begin
  if (spsParser <> nil) and
     (spsParser.RootNode <> nil) then begin
    spsDatabaseProxy := TFFSqlDatabaseProxy.Create(bpsEngine, bpsDatabaseID);
    spsParser.RootNode.Bind(bpsClientID, 0, spsDatabaseProxy);
  end;
end;
{--------}
function TffSqlPreparedStatement.Execute(var aLiveResult: Boolean;    
                                         var aCursorID: TffCursorID;
                                         var aRowsAffected: Integer;
                                         var aRecordsRead: Integer): TffResult;
begin
  {try}                                                                {!!.12}
    Result :=
      spsParser.RootNode.Execute(aLiveResult, aCursorID,
                                 aRowsAffected, aRecordsRead);
  {                                                                     !!.12
  finally
    spsDatabaseProxy.Free;
  end;
  }
end;
{End !!.11}
{--------}
function TffSqlPreparedStatement.Parse(aQuery: PChar): Boolean;
begin
  if spsParser.RootNode <> nil then begin
    spsParser.RootNode.Free;
    spsParser.RootNode := nil;
  end;
  spsParser.SourceStream.SetSize(StrLen(aQuery) + 1);
  move(aQuery^, spsParser.SourceStream.Memory^, StrLen(aQuery) + 1);
  spsParser.Execute;
  Result := spsParser.Successful;
end;
{--------}
function TffSqlPreparedStatement.SetParams(aNumParams: Word;
                                           aParamInfo: PffSqlParamInfoList;
                                           aDataBuffer: PffByteArray): TffResult;
var
  I: Integer;
  Value : Variant;
  FieldBuffer : PffByteArray;
  D : double;
  W : WideString;
  WC : WideChar;
  DT : TDateTime;
  VPtr : PByte;                                                        {!!.13}
begin
  Result := DBIERR_NONE;
  for I := 0 to aNumParams - 1 do begin
    with aParamInfo^[I] do begin
      if piName = '' then                  { named parameter }
        piName := ':' + IntToStr(piNum);   { unnamed parameter }
      FieldBuffer := PffByteArray(DWord(aDataBuffer) + piOffset);
      case piType of
      fftBoolean :
        Value := Boolean(FieldBuffer^[0]);
      fftChar :
        Value := Char(FieldBuffer^[0]);
      fftWideChar :
        begin
          WC := PWideChar(FieldBuffer)^;
          W := WC;
          Value := W;
        end;
      fftByte :
        Value := PByte(FieldBuffer)^;
      fftWord16 :
        Value := PWord(FieldBuffer)^;
      fftWord32 :
        begin
          D := PDWord(FieldBuffer)^;
          Value := D;
        end;
      fftInt8 :
        Value := PShortInt(FieldBuffer)^;
      fftInt16 :
        Value := PSmallInt(FieldBuffer)^;
      fftInt32 :
        Value := PInteger(FieldBuffer)^;
      fftAutoInc :
        Value := PInteger(FieldBuffer)^;
      fftSingle :
        Value := PSingle(FieldBuffer)^;
      fftDouble :
        Value := PDouble(FieldBuffer)^;
      fftExtended :
        Value := PExtended(FieldBuffer)^;
      fftComp :
        Value := PComp(FieldBuffer)^;
      fftCurrency :
        Value := PCurrency(FieldBuffer)^;
      fftStDate :
        Value := StDateToDateTime(PStDate(FieldBuffer)^);
      fftStTime :
        Value := StTimeToDateTime(PStTime(FieldBuffer)^);
      fftDateTime :
        begin
          DT := PffDateTime(FieldBuffer)^;
          VarCast(Value, DT - 693594, varDate);                        {!!.11}
        end;
      fftShortString :
        Value := PShortString(FieldBuffer)^;
      fftShortAnsiStr :
        Value := PShortString(FieldBuffer)^;
      fftNullString :
        Value := StrPas(PChar(FieldBuffer));
      fftNullAnsiStr :
        Value := string(PChar(FieldBuffer));
      fftWideString :
        Value := WideString(PWideChar(FieldBuffer));
{Begin !!.13}
      fftBLOB..fftBLOBTypedBin :
        if piLength = 0 then
          Value := ''
        else begin
          Value := VarArrayCreate([1, piLength], varByte);
          VPtr := VarArrayLock(Value);
          try
            Move(FieldBuffer^, VPtr^, piLength);
          finally
            VarArrayUnlock(Value);
          end;
        end;
{End !!.13}
      else
        raise Exception.Create('Unsupported field type');
      end;
      spsParser.RootNode.SetParameter(I, Value);
    end;
  end;
end;
{====================================================================}

{===TffSqlEngine=====================================================}
constructor TffSqlEngine.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);

  {create list for prepared statements}
  sqlPreparedStatementsList := TffSrStmtList.Create;
end;
{--------}
destructor TffSqlEngine.Destroy;
begin
  sqlPreparedStatementsList.Free;
  inherited Destroy;
end;
{--------}
function TffSqlEngine.Exec(aStmtID: TffSqlStmtID;
                           aOpenMode: TffOpenMode;
                           var aCursorID: TffCursorID;
                           aStream: TStream) : TffResult;
var
  L : Integer;
  RowsAffected: Integer;                                               {!!.10}
  RecordsRead: Integer;                                                {!!.10}
  LiveResult: Boolean;                                                 {!!.10}
  IndexID : Longint;                                                   {!!.11}
begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  try
    aCursorID := 0;
{Begin !!.01}
    with sqlPreparedStatementsList.Stmt[ftFromID, aStmtID] do begin
      Activate;
      try
        FFSetRetry(Timeout);
        LiveResult := aOpenMode = omReadWrite;                         {!!.10}
        Result := Execute(LiveResult, aCursorID, RowsAffected,         {!!.10}
                          RecordsRead);                                {!!.10}
        if Result <> 0 then Exit;
        if Assigned(aStream) then begin
          if (aCursorID <> 0) then begin
            {query}
            Assert(TObject(ACursorID) is TffSrBaseCursor);
            aStream.Write(aCursorID, SizeOf(aCursorID));
            TffSrBaseCursor(ACursorID).Dictionary.WriteToStream(aStream);
{Begin !!.11}
            { If this is a pre-2.11 client then write index ID 0 to the
              stream. }
            if TffSrClient(ClientID).ClientVersion < ffVersion2_10 then begin
              IndexID := 0;
              aStream.Write(IndexID, SizeOf(IndexID));
            end
            else begin
              aStream.Write(LiveResult, SizeOf(LiveResult));
              aStream.Write(RecordsRead, SizeOf(RecordsRead));          
            end
{End !!.11}
          end else begin
            {data manipulation}
            aStream.Write(aCursorID, SizeOf(aCursorID)); {zero}        {!!.10}
            aStream.Write(RowsAffected, SizeOf(RowsAffected));         {!!.10}
            aStream.Write(RecordsRead, SizeOf(RecordsRead));           {!!.10}
          end;
        end;
      finally
        Deactivate;
      end;
    end;  { while }
{End !!.01}
  except
    on E : Exception do begin
      Result := ConvertServerException(E, FEventLog);
      L := length(E.Message);
      aStream.Write(L, sizeof(L));
      aStream.Write(E.Message[1], L);
    end;
  end;
  {$ENDIF}
end;
{--------}
function TffSqlEngine.ExecDirect(anEngine    : TffBaseServerEngine;
                                 aClientID   : TffClientID;
                                 aDatabaseID : TffDatabaseID;
                                 aQueryText  : PChar;
                                 aOpenMode   : TffOpenMode;
                                 aTimeout    : Longint;
                             var aCursorID   : TffCursorID;
                                 aStream     : TStream): TffResult;
var
  Statement: TffSqlPreparedStatement;
  L : Integer;
  RowsAffected: Integer;                                               {!!.10}
  RecordsRead: Integer;                                                {!!.10}
  LiveResult: Boolean;                                                 {!!.10}
begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  Result := DBIERR_NONE;
  Assert(anEngine is TffServerEngine);                                 {!!.01}
  Statement := nil;
  try
    aCursorID := 0;

    Statement := TffSqlPreparedStatement.Create2(TffServerEngine(anEngine), {!!.01}
                                                 aClientID,                 {!!.01}
                                                 aDatabaseID, aTimeout);
    try
      if Statement.Parse(aQueryText) then begin
        LiveResult := aOpenMode = omReadWrite;                         {!!.10}
        Result := Statement.Execute(LiveResult,                        {!!.10}
          aCursorID, RowsAffected, RecordsRead);                       {!!.10}
        if Assigned(aStream) then begin
          if (aCursorID <> 0) then begin
            {query}
            aStream.Write(aCursorID, SizeOf(aCursorID));
            TffSrBaseCursor(ACursorID).Dictionary.WriteToStream(aStream);
            aStream.Write(LiveResult, SizeOf(LiveResult));             {!!.10}
            aStream.Write(RecordsRead, SizeOf(RecordsRead));           {!!.10}
          end else begin
            {data manipulation}
            aStream.Write(aCursorID, SizeOf(aCursorID)); {zero}        {!!.10}
            aStream.Write(RowsAffected, SizeOf(RowsAffected));         {!!.10}
            aStream.Write(RecordsRead, SizeOf(RecordsRead));           {!!.10}
          end;
        end;
      end else
        raise Exception.Create('SQL Syntax error');
    finally
      Statement.Free;
    end;

  except
    on E : Exception do begin
      Result := ConvertServerException(E, FEventLog);
      if Statement <> nil then begin
        L := Statement.spsParser.ListStream.Size;
        aStream.Write(L, sizeof(L));
        Statement.spsParser.ListStream.Seek(0, 0);
        aStream.CopyFrom(Statement.spsParser.ListStream, L);
      end else begin
        L := 0;
        aStream.Write(L, sizeof(L));
      end;
    end;
  end;
  {$ENDIF}
end;
{--------}
function TffSqlEngine.FreeStmt(aStmtID: TffSqlStmtID) : TffResult;
var                                                                    {!!.10}
  parentDB : TffSrDatabase;                                            {!!.10}
  Stmt : TffBasePreparedStmt;                                          {!!.13}
begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  Result := DBIERR_NONE;
  try
{Begin !!.01}
    Stmt := sqlPreparedStatementsList.Stmt[ftFromID, aStmtID];         {!!.13}
    if Stmt <> nil then                                                {!!.13}
      with Stmt do begin                                               {!!.13}
        if CanClose(True) then begin
          sqlPreparedStatementsList.DeleteStmt(aStmtID);
          parentDB := TffSrDatabase(DatabaseID);                       {!!.10}
          parentDB.StmtList.BeginWrite;                                {!!.10}
          try
            parentDB.StmtList.DeleteStmt(aStmtID);                     {!!.10}
          finally
            parentDB.StmtList.EndWrite;                                {!!.10}
          end;
        end
        else
           RequestClose;
      end;  { with }
{End !!.01}
  except
    on E : Exception do begin
      Result := ConvertServerException(E, FEventLog);
    end;
  end;
  {$ENDIF}
end;
{--------}
function TffSqlEngine.Alloc(anEngine : TffBaseServerEngine;
                            aClientID : TffClientID;
                            aDatabaseID : TffDatabaseID;
                            aTimeout : Longint;
                            var aStmtID : TffSqlStmtID): TffResult;
var
  Statement: TffSqlPreparedStatement;
begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  aStmtID := 0;
  Result := DBIERR_NONE;
  Assert(anEngine is TffServerEngine);                                 {!!.01}

  try
    Statement := TffSqlPreparedStatement.Create2(TffServerEngine(anEngine), {!!.01}
                                                 aClientID,                 {!!.01}
                                                 aDatabaseID, aTimeout);
    try
      sqlPreparedStatementsList.AddStmt(Statement);                    {!!.10}
      aStmtID := Statement.Handle;                                     {!!.10}
    except
      Statement.Free;
      raise;
    end;
  except
    on E : Exception do begin
      Result := ConvertServerException(E, FEventLog);
    end;
  end;
  {$ENDIF}
end;
{Begin !!.01}
{--------}
procedure TffSqlEngine.CollectGarbage;
begin
  sqlPreparedStatementsList.RemoveUnused;
end;
{End !!.01}
{--------}
function TffSqlEngine.Prepare(aStmtID: TffSqlStmtID;
                              aQueryText: PChar;
                              aStream : TStream) : TffResult;
var
  L : Integer;
  Stmt : TffSqlPreparedStatement;
begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  try
{Begin !!.01}
    Result := DBIERR_NONE;
    with sqlPreparedStatementsList.Stmt[ftFromID, aStmtID] do begin
      Activate;
      try
{Begin !!.11}
        if Parse(aQueryText) then
          Bind
        else
          raise Exception.Create('SQL syntax error');
{End !!.11}
      finally
        Deactivate;
      end;
    end;
{End !!.01}
  except
    on E : Exception do begin
      Result := ConvertServerException(E, FEventLog);
      Stmt := TffSqlPreparedStatement(sqlPreparedStatementsList.Stmt[ftFromID, aStmtID]);
      L := Stmt.spsParser.ListStream.Size;
      aStream.Write(L, sizeof(L));
      Stmt.spsParser.ListStream.Seek(0, 0);
      aStream.CopyFrom(Stmt.spsParser.ListStream, L);
    end;
  end;
  {$ENDIF}
end;
{Begin !!.03}
{--------}
procedure TffSqlEngine.RemoveForClient(const aClientID : TffClientID);
begin
  sqlPreparedStatementsList.RemoveForClient(aClientID);
end;
{--------}
procedure TffSqlEngine.RequestClose;
begin
  { Free up the remaining SQL prepared statements. }
  sqlPreparedStatementsList.RequestClose;
  sqlPreparedStatementsList.RemoveUnused;
end;
{End !!.03}
{--------}
function TffSqlEngine.SetParams(aStmtID: TffSqlStmtID;
                                aNumParams: Word;
                                aParamDescs: PffSqlParamInfoList;
                                aDataBuffer: PffByteArray;
                                aStream : TStream): TffResult;
var
  Stmt : TffSQLPreparedStatement;
begin
  {$IFNDEF SQLSupported}
  Result := DBIERR_NOTSUPPORTED;
  {$ELSE}
  try
{Begin !!.01}
    Stmt := TffSqlPreparedStatement(sqlPreparedStatementsList.Stmt[ftFromID, aStmtID]);
    Stmt.Activate;
    try
      Result := Stmt.SetParams(aNumParams, aParamDescs, aDataBuffer);
    finally
      Stmt.Deactivate;
    end;
{End !!.01}
  except
    on E : Exception do begin
      Result := ConvertServerException(E, FEventLog);
    end;
  end;
  {$ENDIF}
end;
{====================================================================}

end.
