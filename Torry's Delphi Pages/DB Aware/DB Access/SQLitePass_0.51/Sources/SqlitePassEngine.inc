{ This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

  ---------------------------------------------------------------------------

    Basic sqlite library wrapper for use with fpc compiler
    Author : Luc DAVID Email: luckylazarus@free.fr
    2006 - 2009

    portions of code from Mike Cariotoglou Email: mike@deltasingular.gr

    Major changes are indicated in the \Documentation\Changes.pdf file
    Last update 05.01.2009

  --------------------------------------------------------------------------- }

{.$DEFINE DEBUG_SQLITEPASS}


{ ----- Utilities ----- }
(*function _ExecCallback(cbParam: PSqlitePassExecCallBack; Columns: Integer;
                       ColumnValues, ColumnNames: Ppchar): integer; cdecl;
begin
try
// Result:=cbparam^(columns,columnValues,columnNames);
except
 result:=1;
end;
end; *)


{ TSqlitePassEngine }

constructor TSqlitePassEngine.Create(Database: TSqlitePassDatabase);
begin
FDatabase := Database;
FLibraryLoaded := False;
FLibraryHandle := 0;
FConnectionHandle:= nil;
FQueryTimeOut := 0;
FTransaction := TSqlitePassTransaction.Create(Self);
{$IFDEF DEBUG_SQLITEPASS}
FPreparedStmts := TStringList.Create;
{$ENDIF}
end;

destructor TSqlitePassEngine.Destroy;
begin
CloseDatabase;
{$IFDEF DEBUG_SQLITEPASS}
FPreparedStmts.Free;
{$ENDIF}
FTransaction.Free;
FDatabase := nil;
inherited Destroy;
end;

function TSqlitePassEngine.CheckResult(Const i: Integer; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Integer;
var
ErrorMsg: String;
begin
Result := i;
If not (i in [SQLITE_OK, SQLITE_DONE]) then
   begin
   Case FDatabase.SystemEncoding of
     sysANSI  : ErrorMsg := SqlitePassUtils.UTF8ToAnsi(SqliteDbv3_Errmsg(FConnectionHandle));
     sysUTF8  : ErrorMsg := SqliteDbv3_Errmsg(FConnectionHandle);
     sysUTF16 : ErrorMsg := SqliteDbv3_Errmsg16(FConnectionHandle);
     end;
   FDatabase.FDatabaseError.RaiseExceptionFmt('%d: %s',[i,ErrorMsg], i, FDatabase, VerboseLevel);
   end;
end;

{$IFDEF Delphi2009}
{$WARNINGS OFF}
{$ENDIF}
function TSqlitePassEngine.ExecSQL(Const Sql: String; CallBackFunc: TSqlitePassExecCallBack = nil): Integer;
var
SQLText: UTF8String;
begin
  {SqliteDbv3_Exec is Utf8 only, SQLText is encoded to Utf8 if needed}
  Case FDatabase.SystemEncoding of
       sysANSI  : SetString(SQLText, PAnsiChar(SqlitePassUtils.AnsiToUtf8(Sql)), Length(Sql)); { Delphi 2009 warning is ok here }
       sysUTF8  : SetString(SQLText, PAnsiChar(Sql), Length(Sql)); { Delphi 2009 warning is ok here }
       sysUTF16 : SetString(SQLText, PAnsiChar(SqlitePassUtils.UTF8Encode(Sql)), Length(Sql));
       end;

  repeat
  // TODO
  (*if Assigned(CallBackFunc)
      then result:= SqliteDbv3_Exec(FConnectionHandle,Pchar(Sql),@_ExecCallBack,@@CallBackFunc,nil)
      else*)
  Result:= CheckResult(SqliteDbv3_Exec(FConnectionHandle,pUTF8String(SQLText),nil,nil,nil));
  until (result=SQLITE_OK) or not (result in [SQLITE_BUSY,SQLITE_LOCKED])
        or ((FQueryTimeOut>=0) and (Integer(GetTickCount)>=FStartTime));
end;
{$IFDEF Delphi2009}
{$WARNINGS ON}
{$ENDIF}

procedure TSqlitePassEngine.ExecQueryUTF8(Const Sql: UTF8String; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
var
PSQLText, pRemainingSql: pAnsiChar;
StmtPtr: Pointer;
Status: Integer;
SQLText: AnsiString;
begin
PSQLText := nil;
SetString(SqlText, PAnsiChar(Sql), Length(Sql));
PRemainingSQL := PSQLText;
StmtPtr := nil;

//While (PSQLText <> #0) and (PSql^ <> '') do
While (PSQLText <> nil) do
  begin
  CheckResult(SqliteDbv3_prepare_v2(FConnectionHandle, PSQLText, -1, StmtPtr, PRemainingSQL), VerboseLevel);

    Repeat
    Status := CheckResult(SqliteDbv3_step(StmtPtr), VerboseLevel);
    Until (Status=SQLITE_OK) or not (Status in [SQLITE_BUSY,SQLITE_LOCKED])
           or ((FQueryTimeOut>=0) and (Integer(GetTickCount)>=FStartTime));

  CheckResult(SqliteDbv3_Finalize(StmtPtr), VerboseLevel);
  PSQLText := PRemainingSQL;
  end;
end;

procedure TSqlitePassEngine.ExecQueryUTF16(Const Sql: UTF16WideString; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
var
PSql, pRemainingSql: pWideChar;
StmtPtr: Pointer;
Status: Integer;
begin
PSql := PWideChar(Sql);
PRemainingSQL := PSql;
StmtPtr := nil;

While (PSql <> nil) do
  begin
  CheckResult(SqliteDbv3_prepare16_v2(FConnectionHandle, PSql, -1, StmtPtr, PRemainingSQL), VerboseLevel);

    Repeat
    Status := CheckResult(SqliteDbv3_step(StmtPtr), VerboseLevel);
    Until (Status = SQLITE_OK) or not (Status in [SQLITE_BUSY,SQLITE_LOCKED])
           or ((FQueryTimeOut>=0) and (Integer(GetTickCount)>=FStartTime));

  CheckResult(SqliteDbv3_Finalize(StmtPtr), VerboseLevel);
  PSql := PRemainingSQL;
  end;
end;

procedure TSqlitePassEngine.ExecQuery(Const Sql: String; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow);
begin
 Case FDatabase.SystemEncoding of
       sysANSI  : ExecQueryUTF8 (SqlitePassUtils.AnsiToUTF8(AnsiString(Sql)), VerboseLevel);
       sysUTF8  : ExecQueryUTF8 (AnsiString(Sql), VerboseLevel);
       sysUTF16 : ExecQueryUTF16(Sql, VerboseLevel);
       end;
end;


procedure TSqlitePassEngine.OpenDatabase(Const FullName: String; Const LibraryFile: String);
var
DbFullName: String;
begin
SetString(DbFullName, PChar(FullName), Length(FullName));
FLibraryLoaded := LoadSqliteLibrary(LibraryFile);
Case FDatabase.SystemEncoding of
     sysANSI  : CheckResult(SqliteDbv3_Open(pAnsiChar(SqlitePassUtils.AnsiToUTF8(DbFullName)), FConnectionHandle, SQLITE_OPEN_READWRITE, ''));
     sysUTF8  : CheckResult(SqliteDbv3_Open(pAnsiChar(AnsiString(DbFullName)), FConnectionHandle, SQLITE_OPEN_READWRITE, ''));
     sysUTF16 : CheckResult(SqliteDbv3_Open16(pWideChar(DbFullName), FConnectionHandle));
     end;
end;

procedure TSqlitePassEngine.CloseDatabase;
begin
If Assigned(FConnectionHandle) then
   begin
   Transaction.CommitAll;
   {$IFDEF DEBUG_SQLITEPASS}
   FinalizePendingStmts;
   {$ENDIF}
   if CheckResult(SqliteDbv3_Close(FConnectionHandle)) in [SQLITE_OK, SQLITE_DONE] then
      begin
      FConnectionHandle := nil;
      Dec(EnginesCount);
      end;
   end;
if FLibraryLoaded and (EnginesCount = 0) then
   begin
   FreeLibrary(FLibraryHandle);
   FLibraryLoaded := False;
   end;
end;

Function TSqlitePassEngine.LoadSqliteLibrary(LibraryFile: String): Boolean;
begin
Result := False;
if LibraryFile = ''
   then LibraryFile := DefaultSqliteLibrary;

{ The library is already loaded and we try to load the same file again}
if FLibraryLoaded and (LibraryFile = FLibraryFile)
   then exit;

if FLibraryLoaded
   then FreeLibrary(FLibraryHandle);

FLibraryFile := LibraryFile;
{$IFDEF WIN32}
  FLibraryHandle := LoadLibrary(PChar(FLibraryFile));
{$ELSE}
  FLibraryHandle := LoadLibrary(FLibraryFile);
{$ENDIF}

Result := FLibraryHandle <> 0;
if Result then Try
               LoadFunctions(FLibraryHandle);
               Except
               Result := False;
               Raise;
               end
          else FDatabase.DatabaseError.RaiseExceptionFmt(Msg3001, [FLibraryFile]);
end;

function TSqlitePassEngine.PrepareStmt(var Stmt: pointer; const sql: String; VerboseLevel: TSqlitePassVerboseLevel = vlLogAndShow): Boolean;
var
{$IFDEF DEBUG_SQLITEPASS}
i: Integer;
{$ENDIF}
SQLText: String;
RemainingSql: Pointer;
begin
 RemainingSql := nil;
 UnprepareStmt(Stmt);
 { We 'have to' get a unique String before sending the PChar to the sqlite lib, otherwise
   we could get unpredictable results and memory corruption...}
 SetString(SqlText, PChar(Sql), Length(Sql));

 Case FDatabase.SystemEncoding of
      sysANSI  : Result := (CheckResult(SqliteDbv3_prepare_v2(FConnectionHandle,pAnsiChar(SqlitePassUtils.AnsiToUtf8(AnsiString(SqlText))),-1,Stmt,PAnsiChar(RemainingSql)), VerboseLevel) in [SQLITE_OK, SQLITE_DONE]);
      sysUTF8  : Result := (CheckResult(SqliteDbv3_prepare_v2(FConnectionHandle,pAnsiChar(AnsiString(SqlText)),-1,Stmt,PAnsiChar(RemainingSql)), VerboseLevel) in [SQLITE_OK, SQLITE_DONE]);
      sysUTF16 : Result := (CheckResult(SqliteDbv3_prepare16_v2(FConnectionHandle,pWideChar(SqlText),-1,Stmt,PWideChar(RemainingSql)), VerboseLevel) in [SQLITE_OK, SQLITE_DONE]);
      else Result := False;
      end;

 {$IFDEF DEBUG_SQLITEPASS}
 If Result then
    begin
    i := FPreparedStmts.IndexOfName(IntToStr(Integer(Stmt)));
    if i > -1
       then FPreparedStmts.Strings[i] := IntToStr(Integer(Stmt)) + '=' + Sql
       else FPreparedStmts.Add(IntToStr(Integer(Stmt)) + '=' + Sql);
    end;
 {$ENDIF}
end;

function TSqlitePassEngine.UnprepareStmt(var Stmt: pointer): Boolean;
{$IFDEF DEBUG_SQLITEPASS}
var
i: Integer;
{$ENDIF}
begin
Result := True;
if FConnectionHandle = nil then exit;
if Assigned(Stmt)
   then begin
        try
        Result := (CheckResult(SqliteDbv3_Finalize(Stmt)) in [SQLITE_OK, SQLITE_DONE]);
        {$IFDEF DEBUG_SQLITEPASS}
        i := FPreparedStmts.IndexOfName(IntToStr(Integer(Stmt)));
        if i > -1
           then FPreparedStmts.Delete(i);
        {$ENDIF}
        finally
        Stmt := nil;
        end;
        end;
end;

{ TODO : Workaround to cleanup unused 'garbage' statements
  These statements should not exist, but...I have got to check my code... }
procedure TSqlitePassEngine.FinalizePendingStmts;
//var
//Stmt: Pointer;
begin
// TODO Check this
(*While FPreparedStmts.Count > 0 do
  begin
  Stmt := Pointer(StrToInt(FPreparedStmts.Names[0]));
  UnPrepareStmt(Stmt);
  end;*)
end;

function TSqlitePassEngine.GetChangesCount: Integer;
begin
Result := SqliteDbv3_changes(FConnectionHandle);
end;

function TSqlitePassEngine.GetTotalChangesCount: Integer;
begin
Result := SqliteDbv3_changes(FConnectionHandle);
end;

function TSqlitePassEngine.GetLastInsertRowId: Int64;
begin
Result := SqliteDbv3_last_insert_rowid(FConnectionHandle);
end;

{ User Defined Functions}
function TSqlitePassEngine.CreateFunction(FuncName: String; ArgCount: ShortInt;  DefaultEncoding: Byte;
                                          UserData: Pointer; xFunc, xStep: TFuncHandler; xFinal: TFuncFinalizer): Boolean;
var
Name: String;
begin
 { We 'have to' get a unique String before sending the PChar to the sqlite lib, otherwise
   we could get unpredictable results and memory corruption...}
 SetString(Name, PChar(FuncName), Length(FuncName));

 Case FDatabase.SystemEncoding of
      sysANSI  : Result := (CheckResult(SqliteDbv3_create_function(FConnectionHandle,
                                                                   pAnsiChar(SqlitePassUtils.AnsiToUtf8(AnsiString(Name))),
                                                                   ArgCount, DefaultEncoding, UserData,
                                                                   xFunc, xStep, xFinal)) in [SQLITE_OK, SQLITE_DONE]);
      sysUTF8  : Result := (CheckResult(SqliteDbv3_create_function(FConnectionHandle,
                                                                   pAnsiChar(AnsiString(Name)),
                                                                   ArgCount, DefaultEncoding, UserData,
                                                                   xFunc, xStep, xFinal)) in [SQLITE_OK, SQLITE_DONE]);
      sysUTF16 : Result := (CheckResult(SqliteDbv3_create_function16(FConnectionHandle,
                                                                     pWideChar(Name),
                                                                     ArgCount, DefaultEncoding, UserData,
                                                                     xFunc, xStep, xFinal)) in [SQLITE_OK, SQLITE_DONE]);
      else Result := False;
      end;
end;


{ TSqlitPassTransaction }

constructor TSqlitePassTransaction.Create(Owner: TSqlitePassEngine);
begin
inherited Create;
FEngine := Owner;
FLevel := 0;
end;

destructor TSqlitePassTransaction.Destroy;
begin
FEngine := nil;
Inherited Destroy;
end;

procedure TSqlitePassTransaction.Start;
begin
FEngine.ExecSql('Begin;');
Inc(FLevel);
end;

procedure TSqlitePassTransaction.Commit;
begin
FEngine.ExecSql('Commit;');
Dec(FLevel);
end;

procedure TSqlitePassTransaction.CommitAll;
begin
ResetTransactions(True);
end;

procedure TSqlitePassTransaction.Rollback;
begin
FEngine.ExecSql('Rollback;');
Dec(FLevel);
end;

procedure TSqlitePassTransaction.RollbackAll;
begin
ResetTransactions(False);
end;

procedure TSqlitePassTransaction.ResetTransactions(CommitTransaction: Boolean);
begin
While FLevel > 0 do
    Case CommitTransaction of
         True  : Commit;
         False : Rollback;
         end;
end;

procedure TSqlitePassTransaction.StartInternalTransaction;
begin
if FLevel = 0
   then begin
        FInternalTransaction := True;
        Start;
        end;
end;

procedure TSqlitePassTransaction.CommitInternalTransaction;
begin
if FInternalTransaction = True
   then begin
        FInternalTransaction := False;
        Commit;
        end;
end;

procedure TSqlitePassTransaction.RollbackInternalTransaction;
begin
if FInternalTransaction = True
   then begin
        FInternalTransaction := False;
        Rollback;
        end;
end;

procedure TSqlitePassEngine.StartChrono;
begin
 FStartTime:=Integer(GetTickCount);
end;

procedure TSqlitePassEngine.StopChrono;
begin
 FStopTime:=Integer(GetTickCount);
 FElapsedTime := (FStopTime - FStartTime) / 1000;
end;

